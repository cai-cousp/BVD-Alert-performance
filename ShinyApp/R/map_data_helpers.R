# =============================================================================
# BVD Alerts Dashboard — Interactive notification map helpers
# =============================================================================

notification_ensemble_label <- "Ensemble de la zone affectée"

require_map_columns <- function(data, columns, arg) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    rlang::abort(
      c(
        paste0("`", arg, "` is missing required column(s):"),
        x = paste(missing, collapse = ", ")
      ),
      call = rlang::caller_env()
    )
  }
  invisible(data)
}

format_map_metric <- function(x, digits = 2L) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) {
    return("&mdash;")
  }
  formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
}

#' Summarise the last three completed notification windows
#'
#' The filter uses the same 21-day convention as `R/02_alert_trends.R`: a
#' strict comparison against the latest `threshold_valid_to` keeps exactly the
#' three last completed windows when weekly windows are used.
summarise_recent_notification_metrics <- function(trends, weeks = 3L) {
  required <- c(
    "zone_sante_notification",
    "threshold_valid_to",
    "threshold_time_key",
    "total_alerts",
    "case_adequacy",
    "death_adequacy",
    "aai"
  )
  require_map_columns(trends, required, "trends")
  if (!is.numeric(weeks) || length(weeks) != 1L || weeks < 1L || !is.finite(weeks)) {
    rlang::abort("`weeks` must be one finite positive numeric value.")
  }

  last_window_end <- suppressWarnings(max(as.Date(trends$threshold_valid_to), na.rm = TRUE))
  if (!is.finite(unclass(last_window_end))) {
    rlang::abort("`trends$threshold_valid_to` contains no valid date.")
  }

  trends |>
    dplyr::filter(
      as.Date(.data$threshold_valid_to) > last_window_end - (as.integer(weeks) * 7L),
      .data$zone_sante_notification != notification_ensemble_label
    ) |>
    dplyr::summarise(
      total_alerts = if (all(is.na(.data$total_alerts))) NA_real_ else sum(.data$total_alerts, na.rm = TRUE),
      case_adequacy_recent = if (all(is.na(.data$case_adequacy))) NA_real_ else mean(.data$case_adequacy, na.rm = TRUE),
      death_adequacy_recent = if (all(is.na(.data$death_adequacy))) NA_real_ else mean(.data$death_adequacy, na.rm = TRUE),
      mean_aai_recent = if (all(is.na(.data$aai))) NA_real_ else mean(.data$aai, na.rm = TRUE),
      n_recent_windows = dplyr::n_distinct(.data$threshold_time_key, na.rm = TRUE),
      .by = "zone_sante_notification"
    )
}

#' Prepare Leaflet map data from recent adequacy and canonical geography
#'
#' @param recent_adequacy Latest `02_recent_adequacy.xlsx` data.
#' @param trends `02_trends_smooth_adeq.rds` data.
#' @param geography Canonical GRID3 `sf` object with `province`/`zonesante`.
prepare_notification_map_data <- function(recent_adequacy, trends, geography) {
  required_adequacy <- c(
    "zone_sante_notification", "mean_aai", "adequacy_category"
  )
  require_map_columns(recent_adequacy, required_adequacy, "recent_adequacy")
  require_map_columns(
    trends,
    c("zone_sante_notification", "Province"),
    "trends"
  )
  require_map_columns(geography, c("province", "zonesante"), "geography")
  if (!inherits(geography, "sf")) {
    rlang::abort("`geography` must be an `sf` object.")
  }

  recent_metrics <- summarise_recent_notification_metrics(trends)

  # Join by exact key here; the canonical-geography join below still uses the
  # project's hierarchical fuzzy matcher and its audit logic.
  adequacy_with_metrics <- recent_adequacy |>
    dplyr::select(-dplyr::any_of(c(
      "total_alerts", "case_adequacy_recent", "death_adequacy_recent",
      "mean_aai_recent", "n_recent_windows"
    ))) |>
    dplyr::left_join(recent_metrics, by = "zone_sante_notification") |>
    dplyr::mutate(
      mean_aai_recent = dplyr::coalesce(
        .data$mean_aai_recent,
        .data$mean_aai
      ),
      mean_aai_difference = abs(.data$mean_aai - .data$mean_aai_recent)
    )

  large_difference <- adequacy_with_metrics |>
    dplyr::filter(!is.na(.data$mean_aai_difference), .data$mean_aai_difference > 0.01)
  if (nrow(large_difference) > 0L) {
    rlang::warn(
      paste0(
        nrow(large_difference),
        " health zone(s) differ between source mean AAI and the ",
        "three-window recomputation."
      )
    )
  }

  performance <- prepare_notification_performance_data(
    adequacy_with_metrics,
    trends
  )

  joined <- join_notification_performance_to_health_zones(
    performance$data,
    geography
  )

  map_data <- joined$data |>
    sf::st_make_valid() |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      map_id = sprintf("notification_hz_%04d", dplyr::row_number())
    )

  # GRID3 boundaries contain far more vertices than needed at dashboard zoom.
  # Simplify in a projected CRS, then return Leaflet-compatible EPSG:4326.
  # This reduces the websocket payload from multiple MB to a few hundred KB.
  map_data <- map_data |>
    sf::st_transform(3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 250) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  # Keep only analysed health zones for the dashboard. The static report
  # intentionally includes national grey context, but those additional polygons
  # make the Leaflet payload unnecessarily large and delay the first render.
  map_data <- map_data |>
    dplyr::filter(!is.na(.data$zone_sante_notification))

  attr(map_data, "notification_audit") <- joined$audit
  map_data
}

#' Non-spatial lookup between Leaflet layer IDs and notification keys
notification_map_lookup <- function(map_data) {
  if (is.null(map_data) || nrow(map_data) == 0L) {
    return(tibble::tibble(
      map_id = character(),
      zone_sante_notification = character(),
      province = character()
    ))
  }

  if (!"map_id" %in% names(map_data)) {
    map_data$map_id <- sprintf("notification_hz_%04d", seq_len(nrow(map_data)))
  }

  sf::st_drop_geometry(map_data) |>
    dplyr::filter(!is.na(.data$zone_sante_notification)) |>
    dplyr::transmute(
      .data$map_id,
      .data$zone_sante_notification,
      province = dplyr::coalesce(
        as.character(.data$province_notification),
        as.character(.data$province)
      )
    ) |>
    tibble::as_tibble()
}

#' HTML tooltip for a notification map polygon
build_notification_hover_label <- function(row) {
  category_key <- as.character(row$adequacy_category_recomputed)
  category <- notification_adequacy_labels_fr[[category_key]] %||%
    notification_na_label_fr

  htmltools::HTML(
    sprintf(
      paste0(
        "<div class='notification-map-tooltip'>",
        "<strong>%s</strong><br>",
        "<span>Province :</span> %s<br>",
        "<span>Total des alertes :</span> %s<br>",
        "<span>Adéquation des alertes de cas :</span> %s<br>",
        "<span>Adéquation des alertes de décès :</span> %s<br>",
        "<span>AAI :</span> %s<br>",
        "<span>Catégorie :</span> %s",
        "</div>"
      ),
      htmltools::htmlEscape(row$zone_sante_notification %||% "Pas de données"),
      htmltools::htmlEscape(row$province_notification %||% row$province %||% "&mdash;"),
      format_map_metric(as.numeric(row$total_alerts), digits = 0L),
      format_map_metric(as.numeric(row$case_adequacy_recent)),
      format_map_metric(as.numeric(row$death_adequacy_recent)),
      format_map_metric(as.numeric(row$mean_aai_recent)),
      htmltools::htmlEscape(category)
    )
  )
}

#' Table 1 for a selected health zone, using national-table columns
get_table1_selected_hz_df <- function(trends_smooth_adeq, hz) {
  require_map_columns(
    trends_smooth_adeq,
    c(
      "zone_sante_notification", "threshold_time_key", "case_alerts",
      "death_alerts", "total_alerts", "Alert_case_threshold",
      "Alert_death_threshold", "case_adequacy", "death_adequacy", "aai"
    ),
    "trends_smooth_adeq"
  )
  if (length(hz) != 1L || is.na(hz) || !nzchar(hz)) {
    return(tibble::tibble(
      threshold_time_key = as.Date(character()),
      case_alerts = numeric(),
      death_alerts = numeric(),
      total_alerts = numeric(),
      Alert_case_threshold = numeric(),
      Alert_death_threshold = numeric(),
      case_adequacy = numeric(),
      death_adequacy = numeric(),
      aai = numeric()
    ))
  }

  trends_smooth_adeq |>
    dplyr::filter(
      .data$zone_sante_notification == .env$hz,
      .data$zone_sante_notification != notification_ensemble_label
    ) |>
    dplyr::select(
      "threshold_time_key",
      "case_alerts", "death_alerts", "total_alerts",
      "Alert_case_threshold", "Alert_death_threshold",
      "case_adequacy", "death_adequacy", "aai"
    ) |>
    dplyr::mutate(
      case_adequacy = round(.data$case_adequacy, 2),
      death_adequacy = round(.data$death_adequacy, 2),
      aai = round(.data$aai, 2)
    ) |>
    dplyr::arrange(dplyr::desc(.data$threshold_time_key))
}

#' Table 2 for a selected health zone, using national-table columns
get_table2_selected_hz_df <- function(intermediate_params, trends_smooth_adeq, hz) {
  empty_table <- tibble::tibble(
    threshold_time_key = as.Date(character()),
    beta_c = numeric(),
    beta_d = numeric(),
    n_recent_confirmed = numeric(),
    n_recent_confirmed_nowcast = numeric(),
    detection_rate_adj = numeric(),
    estimated_true_cases_recent = numeric()
  )
  if (length(hz) != 1L || is.na(hz) || !nzchar(hz)) {
    return(empty_table)
  }
  required_params <- c("case_derived_params", "recent_cases")
  missing_params <- setdiff(required_params, names(intermediate_params))
  if (length(missing_params) > 0L) {
    return(empty_table)
  }

  require_map_columns(
    intermediate_params$case_derived_params,
    c(
      "zone_sante_notification", "threshold_time_key", "beta_c", "beta_d",
      "estimated_true_cases_recent"
    ),
    "intermediate_params$case_derived_params"
  )
  require_map_columns(
    intermediate_params$recent_cases,
    c(
      "zone_sante_notification", "threshold_time_key",
      "n_recent_confirmed", "n_recent_confirmed_nowcast"
    ),
    "intermediate_params$recent_cases"
  )

  recent_cases <- intermediate_params$recent_cases |>
    dplyr::filter(.data$zone_sante_notification == .env$hz) |>
    dplyr::select(
      "threshold_time_key",
      "n_recent_confirmed", "n_recent_confirmed_nowcast"
    ) |>
    dplyr::mutate(
      n_recent_confirmed_nowcast = dplyr::coalesce(
        .data$n_recent_confirmed_nowcast,
        .data$n_recent_confirmed
      )
    )

  intermediate_params$case_derived_params |>
    dplyr::filter(.data$zone_sante_notification == .env$hz) |>
    dplyr::select(
      "threshold_time_key", "beta_c", "beta_d",
      "estimated_true_cases_recent"
    ) |>
    dplyr::left_join(recent_cases, by = "threshold_time_key") |>
    dplyr::mutate(
      detection_rate_adj = dplyr::if_else(
        .data$estimated_true_cases_recent > 0,
        .data$n_recent_confirmed_nowcast / .data$estimated_true_cases_recent,
        NA_real_
      ),
      detection_rate_adj = dplyr::if_else(
        is.finite(.data$detection_rate_adj),
        .data$detection_rate_adj,
        NA_real_
      ),
      beta_c = round(.data$beta_c, 3),
      beta_d = round(.data$beta_d, 3),
      detection_rate_adj = round(.data$detection_rate_adj, 3),
      estimated_true_cases_recent = round(.data$estimated_true_cases_recent, 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$threshold_time_key))
}

#' Load and simplify DRC province boundaries shapefile
#'
#' Searches for standard administrative level 1 boundaries in the project Maps
#' directory, standardises the province name column to `province_name`,
#' simplifies geometries for optimal Leaflet rendering, and returns an EPSG:4326
#' `sf` layer.
#'
#' @param maps_base_dir Path to the `Maps` folder (or parent containing shapefiles).
#' @param dTolerance Distance tolerance in meters for polygon simplification.
#'   Default 500m.
#'
#' @return An `sf` object with columns `province_name` and geometry, or `NULL`
#'   if no shapefile is found.
load_province_boundaries <- function(maps_base_dir, dTolerance = 500) {
  if (is.null(maps_base_dir) || !dir.exists(maps_base_dir)) {
    warning("Province map directory does not exist: ", maps_base_dir)
    return(NULL)
  }

  candidates <- c(
    file.path(maps_base_dir, "cod_admin_boundaries.shp", "cod_admin1.shp"),
    file.path(maps_base_dir, "cod_admin_boundaries.geojson", "cod_admin1.geojson"),
    file.path(
      maps_base_dir,
      "ZSandDPSshapefiles",
      "osm_rdc_sante_provinces_211212",
      "OSM_RDC_sante_provinces_211212.shp"
    )
  )

  valid_path <- candidates[file.exists(candidates)][1L]
  if (is.na(valid_path)) {
    warning("No province boundary shapefile found in: ", maps_base_dir)
    return(NULL)
  }

  prov_raw <- tryCatch(
    sf::read_sf(valid_path, quiet = TRUE),
    error = function(e) {
      warning("Failed to load province shapefile: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(prov_raw) || nrow(prov_raw) == 0L) {
    return(NULL)
  }

  name_col <- intersect(
    c("adm1_name", "province", "name", "adm1", "PROVINCE", "NAME"),
    names(prov_raw)
  )[1L]

  if (is.na(name_col)) {
    warning("Could not identify province name column in: ", valid_path)
    return(NULL)
  }

  geom_col <- attr(prov_raw, "sf_column") %||% "geometry"

  prov <- prov_raw |>
    dplyr::select(province_name = dplyr::all_of(name_col), dplyr::all_of(geom_col)) |>
    dplyr::mutate(
      province_name = gsub("(^|[ -])([a-zà-öø-ÿ])", "\\1\\U\\2", as.character(.data$province_name), perl = TRUE)
    ) |>
    sf::st_make_valid()

  if (is.na(sf::st_crs(prov))) {
    sf::st_crs(prov) <- 4326
  }

  # Simplify in projected CRS (EPSG:3379 DRC planar) for snappy Leaflet performance
  prov <- prov |>
    sf::st_transform(3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  prov
}

#' Create label centroid points for province names
#'
#' Computes interior surface coordinates (`lng`, `lat`) for each province in an
#' `sf` layer, used for rendering bold text labels on Leaflet maps.
#'
#' @param province_sf An `sf` object returned by [load_province_boundaries()].
#'
#' @return A `tibble` with `province_name`, `lng`, and `lat` columns.
create_province_label_points <- function(province_sf) {
  if (is.null(province_sf) || !inherits(province_sf, "sf") || nrow(province_sf) == 0L) {
    return(tibble::tibble(
      province_name = character(),
      lng = numeric(),
      lat = numeric()
    ))
  }

  if (!"province_name" %in% names(province_sf)) {
    rlang::abort("`province_sf` must contain `province_name` column.")
  }

  label_pts <- suppressWarnings(sf::st_point_on_surface(province_sf))
  coords <- sf::st_coordinates(label_pts)

  tibble::tibble(
    province_name = as.character(label_pts$province_name),
    lng = unname(coords[, 1L]),
    lat = unname(coords[, 2L])
  ) |>
    dplyr::filter(!is.na(.data$province_name), is.finite(.data$lng), is.finite(.data$lat))
}

