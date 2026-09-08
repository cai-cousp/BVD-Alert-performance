# =============================================================================
# BVD Alerts Dashboard — Interactive notification map helpers
# =============================================================================

notification_adequacy_levels <- c("Under-alerting", "Adequate", "Over-alerting")

notification_adequacy_palette <- c(
  "Under-alerting" = "#D55E00",
  "Adequate"       = "#009E73",
  "Over-alerting"  = "#CC79A7"
)

notification_adequacy_labels_fr <- c(
  "Under-alerting" = "Sous-notification",
  "Adequate"       = "Notification adéquate",
  "Over-alerting"  = "Surnotification"
)

notification_na_fill <- "#F4F4F4"
notification_na_label_fr <- "Pas de données / hors analyse"
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

  # Keep all health zones belonging to the affected provinces for spatial continuity
  affected_provs_trends <- setdiff(unique(trends$Province), c("Ensemble", "Ensemble de la zone affectée"))
  norm_affected <- tolower(gsub("[- ]", "", affected_provs_trends))
  map_data <- map_data |>
    dplyr::filter(
      !is.na(.data$zone_sante_notification) |
      tolower(gsub("[- ]", "", as.character(.data$province))) %in% norm_affected
    )

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
  category <- if (length(category_key) == 1L && !is.na(category_key) && category_key %in% names(notification_adequacy_labels_fr)) {
    notification_adequacy_labels_fr[[category_key]]
  } else {
    notification_na_label_fr
  }

  zone_name <- row$zone_sante_notification
  if (is.null(zone_name) || is.na(zone_name) || !nzchar(zone_name)) {
    zone_name <- row$zonesante
  }
  if (is.null(zone_name) || is.na(zone_name) || !nzchar(zone_name)) {
    zone_name <- "Pas de données"
  }

  prov_name <- row$province_notification
  if (is.null(prov_name) || is.na(prov_name) || !nzchar(prov_name)) {
    prov_name <- row$province
  }
  if (is.null(prov_name) || is.na(prov_name) || !nzchar(prov_name)) {
    prov_name <- "&mdash;"
  }

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
      htmltools::htmlEscape(zone_name),
      htmltools::htmlEscape(prov_name),
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
#' `sf` layer. Prioritises the official OSM RDC santé reference in ZSandDPSshapefiles.
#'
#' @param maps_base_dir Path to the `Maps` folder (or parent containing shapefiles).
#' @param dTolerance Distance tolerance in meters for polygon simplification.
#'   Default 500m.
#' @param provinces Optional character vector of province names to filter.
#'   Matching is tolerant to spaces/hyphens and case.
#'
#' @return An `sf` object with columns `province_name` and geometry, or `NULL`
#'   if no shapefile is found.
load_province_boundaries <- function(maps_base_dir, dTolerance = 500, provinces = NULL) {
  if (is.null(maps_base_dir) || !dir.exists(maps_base_dir)) {
    warning("Province map directory does not exist: ", maps_base_dir)
    return(NULL)
  }

  candidates <- c(
    file.path(
      maps_base_dir,
      "ZSandDPSshapefiles",
      "osm_rdc_sante_provinces_211212",
      "OSM_RDC_sante_provinces_211212.shp"
    ),
    file.path(maps_base_dir, "cod_admin_boundaries.shp", "cod_admin1.shp"),
    file.path(maps_base_dir, "cod_admin_boundaries.geojson", "cod_admin1.geojson")
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
    c("name", "adm1_name", "province", "adm1", "PROVINCE", "NAME"),
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
      province_name = stringr::str_to_title(as.character(.data$province_name))
    ) |>
    sf::st_make_valid()

  if (is.na(sf::st_crs(prov))) {
    sf::st_crs(prov) <- 4326
  }

  # Filter to specified provinces if provided
  if (!is.null(provinces) && length(provinces) > 0L) {
    target_norm <- tolower(gsub("[- ]", "", as.character(provinces)))
    prov <- prov |>
      dplyr::filter(tolower(gsub("[- ]", "", as.character(.data$province_name))) %in% target_norm)
  }

  # Simplify in projected CRS (EPSG:3379 DRC planar) for snappy Leaflet performance
  prov <- prov |>
    sf::st_transform(3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  prov
}

#' Load and simplify the complete DRC national boundary
#'
#' Dissolves province boundaries from ZSandDPSshapefiles or loads cod_admin0,
#' creating a single national boundary outline for Leaflet full-country framing.
#'
#' @param maps_base_dir Path to the `Maps` folder.
#' @param dTolerance Simplification distance tolerance in meters (default: 1000m).
#' @return An `sf` object representing the national boundary, or `NULL`.
load_drc_boundary <- function(maps_base_dir, dTolerance = 1000) {
  if (is.null(maps_base_dir) || !dir.exists(maps_base_dir)) {
    warning("Maps directory does not exist: ", maps_base_dir)
    return(NULL)
  }

  # First check for OSM provinces shapefile to dissolve (ensures 100% boundary coherence)
  osm_prov_path <- file.path(
    maps_base_dir,
    "ZSandDPSshapefiles",
    "osm_rdc_sante_provinces_211212",
    "OSM_RDC_sante_provinces_211212.shp"
  )

  drc_sf <- if (file.exists(osm_prov_path)) {
    tryCatch({
      osm_p <- sf::read_sf(osm_prov_path, quiet = TRUE)
      if (is.na(sf::st_crs(osm_p))) sf::st_crs(osm_p) <- 4326
      drc_geom <- sf::st_union(osm_p)
      sf::st_sf(country = "RDC", geometry = drc_geom)
    }, error = function(e) NULL)
  } else {
    NULL
  }

  if (is.null(drc_sf)) {
    admin0_candidates <- c(
      file.path(maps_base_dir, "cod_admin_boundaries.shp", "cod_admin0.shp"),
      file.path(maps_base_dir, "cod_admin_boundaries.geojson", "cod_admin0.geojson")
    )
    admin0_path <- admin0_candidates[file.exists(admin0_candidates)][1L]
    if (!is.na(admin0_path)) {
      drc_sf <- tryCatch({
        raw0 <- sf::read_sf(admin0_path, quiet = TRUE)
        if (is.na(sf::st_crs(raw0))) sf::st_crs(raw0) <- 4326
        sf::st_sf(country = "RDC", geometry = sf::st_geometry(raw0))
      }, error = function(e) NULL)
    }
  }

  if (is.null(drc_sf)) {
    warning("Could not build DRC national boundary from: ", maps_base_dir)
    return(NULL)
  }

  drc_sf |>
    sf::st_transform(3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
}

#' Load and spatially attribute health zones from ZSandDPSshapefiles
#'
#' Reads the official OSM health zone shapefile, spatio-joins each zone to its
#' province from OSM provinces, and optionally filters to a set of provinces.
#'
#' @param maps_base_dir Path to the `Maps` folder.
#' @param provinces Optional character vector of province names to keep.
#' @param dTolerance Simplification distance tolerance in meters (default: 500m).
#' @return An `sf` object with columns `zonesante`, `province`, and geometry.
load_zsanddps_health_zones <- function(maps_base_dir, provinces = NULL, dTolerance = 500) {
  if (is.null(maps_base_dir) || !dir.exists(maps_base_dir)) {
    warning("Maps directory does not exist: ", maps_base_dir)
    return(NULL)
  }

  zone_shp <- file.path(
    maps_base_dir,
    "ZSandDPSshapefiles",
    "osm_rdc_sante_zones_211212",
    "OSM_RDC_sante_zones_211212.shp"
  )
  prov_shp <- file.path(
    maps_base_dir,
    "ZSandDPSshapefiles",
    "osm_rdc_sante_provinces_211212",
    "OSM_RDC_sante_provinces_211212.shp"
  )

  if (!file.exists(zone_shp) || !file.exists(prov_shp)) {
    warning("ZSandDPSshapefiles not found in: ", maps_base_dir)
    return(NULL)
  }

  zs <- tryCatch(sf::read_sf(zone_shp, quiet = TRUE), error = function(e) NULL)
  dps <- tryCatch(sf::read_sf(prov_shp, quiet = TRUE), error = function(e) NULL)

  if (is.null(zs) || is.null(dps) || nrow(zs) == 0L || nrow(dps) == 0L) {
    return(NULL)
  }

  if (is.na(sf::st_crs(zs))) sf::st_crs(zs) <- 4326
  if (is.na(sf::st_crs(dps))) sf::st_crs(dps) <- 4326

  dps_clean <- dps |>
    dplyr::select(province = "name")

  # Assign province via spatial surface-point intersection
  zs_pts <- suppressWarnings(sf::st_point_on_surface(zs))
  joined_pts <- suppressWarnings(sf::st_join(zs_pts, dps_clean, join = sf::st_intersects))

  zs$province <- as.character(joined_pts$province)
  zs$zonesante <- as.character(zs$name)

  res <- zs |>
    dplyr::select("zonesante", "province", dplyr::all_of(attr(zs, "sf_column") %||% "geometry"))

  if (!is.null(provinces) && length(provinces) > 0L) {
    target_norm <- tolower(gsub("[- ]", "", as.character(provinces)))
    res <- res |>
      dplyr::filter(tolower(gsub("[- ]", "", as.character(.data$province))) %in% target_norm)
  }

  res |>
    sf::st_transform(3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
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


