#' Add status-specific alive exposure to case-derived thresholds
#'
#' The all-case estimate is the epidemiological anchor. The status-specific
#' nowcasts supply the alive share, avoiding the strong assumption that alive
#' and dead confirmed cases have identical detection probabilities.
#'
#' @param case_derived_thresholds Detection-adjusted all-case exposure, with
#'   health-zone, time-window and `estimated_true_cases_recent` columns.
#' @param hz_recent_cases All-case recent-window table.
#' @param hz_recent_alive_cases Confirmed-alive recent-window table.
#' @param min_share_denominator Minimum all-case nowcast total required before
#'   using a health-zone/window share.
#'
#' @return `case_derived_thresholds` with `estimated_true_alive_cases_recent`,
#'   share columns and diagnostics.
add_alive_exposure_to_case_thresholds <- function(
  case_derived_thresholds,
  hz_recent_cases,
  hz_recent_alive_cases,
  min_share_denominator = 5
) {
  alert_required_columns(
    case_derived_thresholds,
    c(
      "zone_sante_notification", "threshold_time_key",
      "estimated_true_cases_recent"
    ),
    "add_alive_exposure_to_case_thresholds()"
  )
  alert_required_columns(
    hz_recent_cases,
    c(
      "zone_sante_notification", "threshold_time_key",
      "n_recent_confirmed_nowcast"
    ),
    "add_alive_exposure_to_case_thresholds()"
  )
  alert_required_columns(
    hz_recent_alive_cases,
    c(
      "zone_sante_notification", "threshold_time_key",
      "n_recent_confirmed_alive_nowcast"
    ),
    "add_alive_exposure_to_case_thresholds()"
  )

  key_columns <- c("zone_sante_notification", "threshold_time_key")
  raw_shares <- hz_recent_cases |>
    dplyr::select(
      dplyr::any_of(c(
        key_columns, "n_recent_confirmed_nowcast",
        "n_recent_confirmed_nowcast_low", "n_recent_confirmed_nowcast_high"
      ))
    ) |>
    dplyr::inner_join(
      hz_recent_alive_cases |>
        dplyr::select(
          dplyr::any_of(c(
            key_columns, "n_recent_confirmed_alive_nowcast",
            "n_recent_confirmed_alive_nowcast_low",
            "n_recent_confirmed_alive_nowcast_high"
          ))
        ),
      by = key_columns,
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      alive_share = bounded_ratio(
        .data$n_recent_confirmed_alive_nowcast,
        .data$n_recent_confirmed_nowcast
      ),
      alive_share_low = bounded_ratio(
        .data$n_recent_confirmed_alive_nowcast_low,
        .data$n_recent_confirmed_nowcast_high
      ),
      alive_share_high = bounded_ratio(
        .data$n_recent_confirmed_alive_nowcast_high,
        .data$n_recent_confirmed_nowcast_low
      ),
      share_usable = is.finite(.data$alive_share) &
        .data$n_recent_confirmed_nowcast >= min_share_denominator
    )

  hz_fallback <- raw_shares |>
    dplyr::filter(.data$share_usable) |>
    dplyr::summarise(
      alive_share = sum(.data$n_recent_confirmed_alive_nowcast, na.rm = TRUE) /
        sum(.data$n_recent_confirmed_nowcast, na.rm = TRUE),
      .by = zone_sante_notification
    ) |>
    dplyr::mutate(
      alive_share = bounded_ratio(.data$alive_share, 1),
      alive_share_source = "hz_fallback"
    )

  global_fallback <- raw_shares |>
    dplyr::filter(.data$share_usable) |>
    dplyr::summarise(
      alive_share = sum(.data$n_recent_confirmed_alive_nowcast, na.rm = TRUE) /
        sum(.data$n_recent_confirmed_nowcast, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      alive_share = bounded_ratio(.data$alive_share, 1),
      alive_share_source = "global_fallback"
    )

  share_lookup <- raw_shares |>
    dplyr::filter(.data$share_usable) |>
    dplyr::select(
      dplyr::all_of(key_columns),
      alive_share,
      alive_share_low,
      alive_share_high
    ) |>
    dplyr::mutate(alive_share_source = "hz_window") |>
    dplyr::bind_rows(
      hz_fallback |>
        dplyr::select(
          dplyr::all_of(key_columns[1]),
          alive_share,
          alive_share_source
        )
    ) |>
    dplyr::bind_rows(
      global_fallback |>
        dplyr::select(alive_share, alive_share_source)
    )

  # `join_by()` cannot express the ordered fallback hierarchy in one join, so
  # apply current-window, HZ and global fallbacks in that priority.
  out <- case_derived_thresholds |>
    dplyr::left_join(
      share_lookup |> dplyr::filter(alive_share_source == "hz_window"),
      by = key_columns,
      relationship = "one-to-one"
    )

  needs_hz <- is.na(out$alive_share)
  if (any(needs_hz)) {
    # Remember which rows originally got a window-level share before
    # replacing missing shares with the HZ fallback. `rows_update`
    # overwrites every matching zone-row, so restore the original source
    # label afterwards.
    hz_window_mask <- !is.na(out$alive_share)
    out <- out |>
      dplyr::rows_update(
        hz_fallback,
        by = "zone_sante_notification",
        unmatched = "ignore"
      ) |>
      dplyr::mutate(
        alive_share_source = dplyr::if_else(
          needs_hz & is.finite(.data$alive_share),
          "hz_fallback",
          .data$alive_share_source
        )
      )
    # Restore the hz_window source for rows that came from the window-level
    # lookup (they got clobbered by rows_update above).
    out <- out |>
      dplyr::mutate(
        alive_share_source = dplyr::if_else(
          hz_window_mask & is.finite(.data$alive_share),
          "hz_window",
          .data$alive_share_source
        )
      )
  }

  needs_global <- is.na(out$alive_share)
  if (any(needs_global) && nrow(global_fallback) == 1L) {
    out <- out |>
      dplyr::mutate(
        alive_share = dplyr::if_else(
          needs_global,
          global_fallback$alive_share[[1]],
          .data$alive_share
        ),
        alive_share_source = dplyr::if_else(
          needs_global,
          "global_fallback",
          .data$alive_share_source
        )
      )
  }

  out |>
    dplyr::mutate(
      estimated_true_alive_cases_recent =
        .data$estimated_true_cases_recent * .data$alive_share,
      estimated_true_alive_cases_recent = dplyr::if_else(
        is.finite(.data$estimated_true_alive_cases_recent) &
          .data$estimated_true_alive_cases_recent >= 0,
        .data$estimated_true_alive_cases_recent,
        NA_real_
      )
    )
}

bounded_ratio <- function(numerator, denominator) {
  res <- rep(NA_real_, max(length(numerator), length(denominator)))
  valid <- is.finite(denominator) & denominator > 0
  res[valid] <- numerator[valid] / denominator[valid]
  pmin(pmax(res, 0), 1)
}

pooled_alert_beta <- function(alert_count, exposure) {
  if (is.na(exposure) || !is.finite(exposure) || exposure <= 0) {
    return(NA_real_)
  }
  alert_count / exposure
}
