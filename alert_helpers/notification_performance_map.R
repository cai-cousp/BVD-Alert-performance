# Classification, input-resolution and join helpers for the health-zone
# notification performance map (sous-notification / adequate / surnotification).

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

#' Classify health-zone notification adequacy from mean AAI
#'
#' Applies the project rules from `R/02_alert_trends.R`: AAI < 0.75 is
#' under-notification, AAI > 2 is over-notification, and 0.75-2 (inclusive) is
#' adequate. Missing values return `NA`.
#'
#' @param mean_aai Numeric vector of mean Alert Adequacy Index values.
#'
#' @return Factor with levels `Under-alerting`, `Adequate`, `Over-alerting`.
classify_notification_adequacy <- function(mean_aai) {
  if (!is.numeric(mean_aai)) {
    rlang::abort("`mean_aai` must be numeric.", call = rlang::caller_env())
  }
  invalid <- !is.na(mean_aai) & (!is.finite(mean_aai) | mean_aai < 0)
  if (any(invalid)) {
    rlang::abort(
      "`mean_aai` must contain only missing, finite, non-negative values.",
      call = rlang::caller_env()
    )
  }

  category <- dplyr::case_when(
    is.na(mean_aai) ~ NA_character_,
    mean_aai < 0.75 ~ "Under-alerting",
    mean_aai > 2.0 ~ "Over-alerting",
    .default = "Adequate"
  )
  factor(category, levels = notification_adequacy_levels)
}

#' Resolve the latest complete notification map inputs
#'
#' Selects the newest dated output directory containing both required input
#' files, so an empty current-day directory never breaks the map.
#'
#' @param output_root Root directory containing dated output folders.
#' @param adequacy_path Optional explicit path to `02_recent_adequacy.xlsx`.
#' @param trends_path Optional explicit path to `02_trends_smooth.rds`.
#'
#' @return List with `adequacy_path`, `trends_path` and `output_dir`.
resolve_notification_map_inputs <- function(
  output_root = here::here("output"),
  adequacy_path = NULL,
  trends_path = NULL
) {
  if (xor(is.null(adequacy_path), is.null(trends_path))) {
    rlang::abort(
      "Provide both `adequacy_path` and `trends_path`, or neither.",
      call = rlang::caller_env()
    )
  }

  if (!is.null(adequacy_path)) {
    paths <- c(adequacy_path, trends_path)
    missing <- paths[!file.exists(paths)]
    if (length(missing) > 0L) {
      rlang::abort(
        c("Explicit notification map input(s) do not exist:", x = paste(missing, collapse = ", ")),
        call = rlang::caller_env()
      )
    }
    return(list(
      adequacy_path = adequacy_path,
      trends_path = trends_path,
      output_dir = dirname(adequacy_path)
    ))
  }

  if (!dir.exists(output_root)) {
    rlang::abort(
      c("The output root does not exist.", x = output_root),
      call = rlang::caller_env()
    )
  }

  candidate_dirs <- list.dirs(output_root, recursive = FALSE)
  candidate_dirs <- candidate_dirs[
    grepl("^[0-9]{4}_[0-9]{2}_[0-9]{2}$", basename(candidate_dirs))
  ]
  candidate_dirs <- sort(candidate_dirs)

  complete <- candidate_dirs[
    file.exists(file.path(candidate_dirs, "02_recent_adequacy.xlsx")) &
      file.exists(file.path(candidate_dirs, "02_trends_smooth.rds"))
  ]

  if (length(complete) == 0L) {
    rlang::abort(
      c(
        "No complete notification map inputs found.",
        i = "Run R/02_alert_trends.R first to generate 02_recent_adequacy.xlsx and 02_trends_smooth.rds.",
        x = output_root
      ),
      call = rlang::caller_env()
    )
  }

  selected <- complete[length(complete)]
  today_label <- format(Sys.Date(), "%Y_%m_%d")
  if (basename(selected) != today_label) {
    message(
      "No complete notification inputs for ", today_label,
      "; using ", selected, "."
    )
  }

  list(
    adequacy_path = file.path(selected, "02_recent_adequacy.xlsx"),
    trends_path = file.path(selected, "02_trends_smooth.rds"),
    output_dir = selected
  )
}

#' Fail fast when required data frame columns are missing
#'
#' @keywords internal
#' @param data Data frame to validate.
#' @param columns Required column names.
#' @param arg Argument name used in the error message.
abort_if_missing_columns <- function(data, columns, arg) {
  if (!is.data.frame(data)) {
    rlang::abort(sprintf("`%s` must be a data frame.", arg), call = rlang::caller_env())
  }
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    rlang::abort(
      c(
        sprintf("Missing required column(s) in `%s`:", arg),
        x = paste(missing, collapse = ", ")
      ),
      call = rlang::caller_env()
    )
  }
  invisible(NULL)
}

#' Prepare recent adequacy data for the notification performance map
#'
#' Joins province from the trends output (the recent adequacy workbook has no
#' province column), removes the ensemble row, recomputes the three-level
#' adequacy category from `mean_aai`, and audits disagreements with the source
#' category.
#'
#' @param adequacy Recent adequacy `data.frame` from `02_recent_adequacy.xlsx`.
#' @param trends Trends `data.frame` from `02_trends_smooth.rds`.
#'
#' @return List with `data` and an `audit` list.
prepare_notification_performance_data <- function(adequacy, trends) {
  required_adequacy <- c("zone_sante_notification", "mean_aai", "adequacy_category")
  required_trends <- c("zone_sante_notification", "Province")
  abort_if_missing_columns(adequacy, required_adequacy, "adequacy")
  abort_if_missing_columns(trends, required_trends, "trends")

  province_lookup <- trends |>
    dplyr::filter(
      .data$Province != "Ensemble",
      .data$zone_sante_notification != notification_ensemble_label
    ) |>
    dplyr::summarise(
      n_provinces = dplyr::n_distinct(.data$Province),
      province_notification = dplyr::if_else(
        dplyr::n_distinct(.data$Province) == 1L,
        dplyr::first(.data$Province),
        NA_character_
      ),
      .by = "zone_sante_notification"
    )

  province_lookup_problems <- province_lookup |>
    dplyr::filter(.data$n_provinces > 1L) |>
    dplyr::select(-"n_provinces")

  n_input_rows <- nrow(adequacy)
  ensemble_rows <- adequacy |>
    dplyr::filter(.data$zone_sante_notification == notification_ensemble_label)

  data <- adequacy |>
    dplyr::filter(.data$zone_sante_notification != notification_ensemble_label) |>
    dplyr::left_join(
      dplyr::select(province_lookup, -"n_provinces"),
      by = "zone_sante_notification"
    ) |>
    dplyr::mutate(
      adequacy_category_recomputed = classify_notification_adequacy(.data$mean_aai)
    )

  disagreements <- data |>
    dplyr::filter(
      is.na(.data$adequacy_category) |
        as.character(.data$adequacy_category_recomputed) != .data$adequacy_category
    ) |>
    dplyr::select(
      "zone_sante_notification",
      "mean_aai",
      adequacy_category_source = "adequacy_category",
      "adequacy_category_recomputed"
    )

  audit <- list(
    n_input_rows = n_input_rows,
    n_ensemble_rows_removed = nrow(ensemble_rows),
    n_health_zone_rows = nrow(data),
    n_missing_aai = sum(is.na(data$mean_aai)),
    n_category_disagreements = nrow(disagreements),
    category_disagreements = disagreements,
    province_lookup_problems = province_lookup_problems,
    zones_without_province = data |>
      dplyr::filter(is.na(.data$province_notification)) |>
      dplyr::select("zone_sante_notification")
  )

  list(data = data, audit = audit)
}

#' Join notification performance estimates to GRID3 health-zone polygons
#'
#' Uses hierarchical fuzzy matching and returns both the map data and an audit
#' of unmatched rows and duplicate geography keys.
#'
#' @param performance Prepared performance data from
#'   [prepare_notification_performance_data()].
#' @param health_zones Canonical `sf` geography with `province`/`zonesante`.
#' @param match_threshold Fuzzy match threshold percentage (default 80).
#'
#' @return List with `data` (sf) and `audit`.
join_notification_performance_to_health_zones <- function(
  performance,
  health_zones,
  match_threshold = 80
) {
  required_performance <- c(
    "province_notification",
    "zone_sante_notification",
    "mean_aai",
    "adequacy_category_recomputed"
  )
  required_zones <- c("province", "zonesante")
  abort_if_missing_columns(performance, required_performance, "performance")
  abort_if_missing_columns(health_zones, required_zones, "health_zones")
  if (!inherits(health_zones, "sf")) {
    rlang::abort("`health_zones` must be an sf object.", call = rlang::caller_env())
  }
  if (is.na(sf::st_crs(health_zones))) {
    rlang::abort("`health_zones` must have a defined CRS.", call = rlang::caller_env())
  }

  if (nrow(performance) == 0L) {
    audit <- list(
      n_geometry_rows = nrow(health_zones),
      n_estimates = 0L,
      n_performance_rows = 0L,
      n_matched_rows = 0L,
      n_unmatched_rows = 0L,
      unmatched_performance = performance,
      duplicate_keys = tibble::tibble(),
      geometry_crs = sf::st_crs(health_zones)
    )
    return(list(data = health_zones, audit = audit))
  }

  geo_matched <- match_alert_health_zone_names(
    provinces = performance$province_notification,
    zones = performance$zone_sante_notification,
    health_zones = health_zones,
    match_threshold = match_threshold
  )

  performance_matched <- performance |>
    dplyr::mutate(
      province_matched = geo_matched$province_matched,
      zone_matched = geo_matched$zone_matched
    )

  duplicate_keys <- performance_matched |>
    dplyr::filter(!is.na(.data$province_matched), !is.na(.data$zone_matched)) |>
    dplyr::count(province_matched, zone_matched) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicate_keys) > 0L) {
    rlang::warn(
      c(
        paste(nrow(duplicate_keys), "duplicated geography key(s) after name matching."),
        i = "Polygon rows may be duplicated; inspect the audit output."
      )
    )
  }

  ref_keys <- health_zones |>
    sf::st_drop_geometry() |>
    dplyr::select("province", "zonesante")

  unmatched <- performance_matched |>
    dplyr::anti_join(
      ref_keys,
      by = dplyr::join_by(
        province_matched == province,
        zone_matched == zonesante
      )
    )
  n_unmatched <- nrow(unmatched)
  if (n_unmatched > 0L) {
    rlang::warn(
      paste(n_unmatched, "performance row(s) did not match a health-zone polygon.")
    )
  }

  map_data <- health_zones |>
    dplyr::left_join(
      performance_matched,
      by = dplyr::join_by(
        province == province_matched,
        zonesante == zone_matched
      )
    )

  audit <- list(
    n_geometry_rows = nrow(health_zones),
    n_estimates = sum(!is.na(map_data$adequacy_category_recomputed)),
    n_performance_rows = nrow(performance),
    n_matched_rows = nrow(performance) - n_unmatched,
    n_unmatched_rows = n_unmatched,
    unmatched_performance = unmatched,
    duplicate_keys = duplicate_keys,
    geometry_crs = sf::st_crs(health_zones)
  )

  list(data = map_data, audit = audit)
}

#' Derive the notification map reference date
#'
#' Prefers the trends `evd_max_date` attribute, then the latest threshold end,
#' then the output directory date. Never uses `Sys.Date()` silently.
#'
#' @param trends Trends `data.frame` from `02_trends_smooth.rds`.
#' @param output_dir Selected output directory path.
#'
#' @return A `Date`.
notification_map_reference_date <- function(trends, output_dir) {
  evd_max_date <- attr(trends, "evd_max_date")
  if (!is.null(evd_max_date)) {
    return(as.Date(evd_max_date))
  }
  if ("threshold_valid_to" %in% names(trends)) {
    latest <- suppressWarnings(max(trends$threshold_valid_to, na.rm = TRUE))
    if (is.finite(latest)) {
      return(as.Date(latest))
    }
  }
  as.Date(basename(output_dir), format = "%Y_%m_%d")
}
