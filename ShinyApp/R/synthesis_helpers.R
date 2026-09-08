# =============================================================================
# BVD Alerts Dashboard — Synthesis & Helper Routines
# =============================================================================
# Self-contained helper functions for Table 2 synthesis and metadata parsing.
# Sourced by global.R to avoid external dependencies on alert_helpers/.
# =============================================================================

#' Validate that a data frame contains required columns
#' @param data Data frame to validate.
#' @param required Character vector of column names.
#' @param caller Name of calling function for error message.
#' @return The input data frame invisibly.
alert_required_columns <- function(data, required, caller) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    rlang::abort(c(
      paste0(caller, " requires columns that are missing from the data."),
      x = paste(missing, collapse = ", ")
    ))
  }
  invisible(data)
}

#' Combine detection-rate estimates by health zone
#'
#' Averages valid detection rates from CFR back-calculation and the epi-link
#' method, then recomputes derived metrics from the averaged rate.
#'
#' @param hz_detection_backcalc Dataframe from `compute_detection_cfr_backcalc_by_hz()`.
#' @param hz_detection_epilink Dataframe from `compute_detection_by_hz()`.
#' @param recent_cases Optional dataframe from `compute_recent_confirmed_windows_by_hz()`.
#' @return Tibble with one row per health zone.
combine_detection_estimates_by_hz <- function(hz_detection_backcalc,
                                              hz_detection_epilink,
                                              recent_cases = NULL) {
  alert_required_columns(
    hz_detection_backcalc,
    c("zone_sante_notification", "n_detected", "detection_rate_adj"),
    "combine_detection_estimates_by_hz()"
  )
  alert_required_columns(
    hz_detection_epilink,
    c("zone_sante_notification", "detection_rate_adj"),
    "combine_detection_estimates_by_hz()"
  )
  if (!is.null(recent_cases)) {
    alert_required_columns(
      recent_cases,
      c("zone_sante_notification", "n_recent_confirmed"),
      "combine_detection_estimates_by_hz()"
    )
  }

  backcalc <- hz_detection_backcalc |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::select(
      zone_sante_notification,
      dplyr::any_of("threshold_time_key"),
      n_detected,
      dplyr::any_of(c(
        "n_detected_nowcast",
        "n_detected_nowcast_low",
        "n_detected_nowcast_high"
      )),
      n_observed_deaths,
      dplyr::any_of(c(
        "n_observed_deaths_nowcast",
        "n_observed_deaths_nowcast_low",
        "n_observed_deaths_nowcast_high"
      )),
      cfr_used,
      r_used,
      detection_rate_backcalc_adj = detection_rate_adj,
      under_detection_backcalc = under_detection_rate,
      estimated_true_cases_backcalc = estimated_true_cases,
      estimated_true_cases_backcalc_low = estimated_true_cases_low,
      estimated_true_cases_backcalc_high = estimated_true_cases_high,
      detection_rate_backcalc_low = detection_rate_low,
      detection_rate_backcalc_high = detection_rate_high,
      detection_backcalc_status,
      dplyr::any_of(c(
        "zero_deaths",
        "missing_growth",
        "missing_cfr",
        "detection_gt_one",
        "pooled_cfr_used",
        "pooled_growth_used",
        "predicted_last_count"
      ))
    )

  epilink <- hz_detection_epilink |>
    dplyr::filter(!is.na(zone_sante_notification)) |>
    dplyr::select(
      zone_sante_notification,
      dplyr::any_of("threshold_time_key"),
      n_conf_valid_epilink,
      n_epilink,
      detection_rate_epilink_adj = detection_rate_adj,
      under_detection_epilink = under_detection_rate
    )

  combined <- dplyr::full_join(
    backcalc,
    epilink,
    by = if (all(c("threshold_time_key") %in% names(backcalc)) &&
      all(c("threshold_time_key") %in% names(epilink))) {
      dplyr::join_by(zone_sante_notification, threshold_time_key)
    } else {
      dplyr::join_by(zone_sante_notification)
    },
    relationship = "one-to-one"
  )

  has_recent_cases <- !is.null(recent_cases)

  if (has_recent_cases) {
    recent_metadata_columns <- c(
      "threshold_time_key",
      "threshold_window_id",
      "threshold_window_index",
      "threshold_valid_from",
      "threshold_valid_to",
      "recent_case_window_start",
      "recent_case_window_end",
      "recent_case_window_days",
      "recent_case_anchor_date"
    )
    recent <- recent_cases |>
      dplyr::filter(!is.na(zone_sante_notification)) |>
      dplyr::select(
        zone_sante_notification,
        n_recent_confirmed,
        dplyr::any_of(c(
          "n_recent_confirmed_nowcast",
          "n_recent_confirmed_nowcast_low",
          "n_recent_confirmed_nowcast_high",
          "recent_count_source"
        )),
        dplyr::any_of(recent_metadata_columns)
      )

    combined <- dplyr::full_join(
      combined,
      recent,
      by = if ("threshold_time_key" %in% names(recent) &&
        "threshold_time_key" %in% names(combined)) {
        dplyr::join_by(zone_sante_notification, threshold_time_key)
      } else {
        dplyr::join_by(zone_sante_notification)
      },
      relationship = if ("threshold_time_key" %in% names(recent)) {
        if ("threshold_time_key" %in% names(combined)) {
          "one-to-one"
        } else {
          "one-to-many"
        }
      } else {
        "one-to-one"
      }
    )
  }

  if (!("predicted_last_count" %in% names(combined))) {
    combined$predicted_last_count <- NA_real_
  }
  if (!("n_recent_confirmed_nowcast" %in% names(combined))) {
    combined$n_recent_confirmed_nowcast <- NA_real_
  }
  if (!("n_detected_nowcast" %in% names(combined))) {
    combined$n_detected_nowcast <- NA_integer_
  }
  if (!("n_observed_deaths_nowcast" %in% names(combined))) {
    combined$n_observed_deaths_nowcast <- NA_integer_
  }

  combined <- combined |>
    dplyr::mutate(
      detection_rate_adj = dplyr::case_when(
        !is.na(detection_rate_backcalc_adj) & !is.na(detection_rate_epilink_adj) ~
          (detection_rate_backcalc_adj + detection_rate_epilink_adj) / 2,
        !is.na(detection_rate_backcalc_adj) ~ detection_rate_backcalc_adj,
        !is.na(detection_rate_epilink_adj) ~ detection_rate_epilink_adj,
        TRUE ~ NA_real_
      ),
      detection_source = dplyr::case_when(
        !is.na(detection_rate_backcalc_adj) & !is.na(detection_rate_epilink_adj) ~ "combined",
        !is.na(detection_rate_backcalc_adj) ~ "cfr_backcalc",
        !is.na(detection_rate_epilink_adj) ~ "epilink",
        TRUE ~ "none"
      ),
      under_detection_rate = dplyr::case_when(
        !is.na(detection_rate_adj) & detection_rate_adj > 0 ~ 1 / detection_rate_adj,
        TRUE ~ NA_real_
      )
    )

  combined
}

#' Extract date/timestamp stamp from an EVD file name or snapshot key
#' @param file_name Character scalar.
#' @return Character scalar or NULL.
extract_evd_date_stamp <- function(file_name) {
  if (is.null(file_name) || length(file_name) == 0L || is.na(file_name)) {
    return(NULL)
  }
  fname <- basename(file_name)
  match_int <- stringr::str_match(
    fname,
    "clean_Int_([0-9]{4}_[0-9]{2}_[0-9]{2}(?:_[0-9]+)?)\\.(?:rds|xlsx)$"
  )
  if (!is.na(match_int[1, 2])) {
    return(match_int[1, 2])
  }
  match_analysis <- stringr::str_match(
    fname,
    "(?:01_thresholds_synthesis|01_thresholds_ensemble|01_intermediate_parameters|02_trends_smooth_adeq|02_trends_smooth|02_recent_adequacy|05_nowcast_by_zone|05_nowcast_by_zone_deaths|synthesis_short|EpiSize_BDV)_([0-9]{4}_[0-9]{2}_[0-9]{2}(?:_[0-9]+)?)\\.(?:rds|xlsx|pdf)$"
  )
  if (!is.na(match_analysis[1, 2])) {
    return(match_analysis[1, 2])
  }
  NULL
}

#' Locate the most recent cleaned EVD line list file
#' @param base_path Character path.
#' @param pattern Character regex.
#' @return Character path or NULL.
latest_evd_file <- function(base_path, pattern = "evd.clean_Int_.*\\.rds") {
  if (!dir.exists(base_path)) return(NULL)
  files <- list.files(path = base_path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0L) return(NULL)
  files[which.max(as.numeric(gsub("\\D", "", basename(files))))]
}
