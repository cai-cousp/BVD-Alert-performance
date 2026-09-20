#' Validate coherence between all-case, alive and death nowcasts
#'
#' Status-specific nowcasts are fitted independently. This helper checks that
#' they can safely be combined in the alert-threshold calculations. It does not
#' overwrite the independently fitted alive nowcast with `all - deaths`.
#'
#' @param nowcasts_all_cases All-case nowcast table.
#' @param nowcasts_alive Confirmed-alive nowcast table.
#' @param nowcasts_deaths Confirmed-death nowcast table.
#' @param warn_above Relative `alive + deaths - all` discrepancy above which a
#'   warning is emitted.
#' @param error_above Relative discrepancy above which an error is emitted.
#' @param series_attrs Character expected `series` attributes. Pass `NULL` to
#'   skip attribute checks (primarily for tests using hand-made nowcasts).
#'
#' @return Invisibly returns a list of input tables with a `coherence`
#'   attribute summarising any discrepancy.
validate_status_nowcasts <- function(
  nowcasts_all_cases,
  nowcasts_alive,
  nowcasts_deaths,
  warn_above = 0.10,
  error_above = 0.25,
  series_attrs = c("confirmed_cases", "confirmed_alive", "confirmed_deaths")
) {
  inputs <- list(
    confirmed_cases = nowcasts_all_cases,
    confirmed_alive = nowcasts_alive,
    confirmed_deaths = nowcasts_deaths
  )
  required_columns <- c(
    "zone_sante_notification", "date", "observed", "nowcast_median",
    "nowcast_lower_90", "nowcast_upper_90"
  )

  purrr::iwalk(inputs, \(data, series) {
    alert_required_columns(data, required_columns, "validate_status_nowcasts()")
    if (!is.null(series_attrs) && !identical(attr(data, "series"), series)) {
      rlang::abort(sprintf(
        "The %s nowcast must have series attribute '%s'.",
        series, series
      ))
    }
  })

  key_columns <- c("zone_sante_notification", "date")
  joined <- inputs$confirmed_cases |>
    dplyr::inner_join(
      inputs$confirmed_alive |>
        dplyr::select(dplyr::all_of(c(key_columns, "nowcast_median"))),
      by = key_columns,
      suffix = c("_all", "_alive")
    ) |>
    dplyr::inner_join(
      inputs$confirmed_deaths |>
        dplyr::select(dplyr::all_of(c(key_columns, "nowcast_median"))),
      by = key_columns
    ) |>
    dplyr::rename(nowcast_median_deaths = nowcast_median)

  if (nrow(joined) == 0L) {
    rlang::abort("No common health-zone/date keys across status-specific nowcasts.")
  }

  summary <- joined |>
    dplyr::summarise(
      n_keys = dplyr::n(),
      median_all = sum(nowcast_median_all, na.rm = TRUE),
      median_alive = sum(nowcast_median_alive, na.rm = TRUE),
      median_deaths = sum(nowcast_median_deaths, na.rm = TRUE),
      max_alive_excess = max(
        nowcast_median_alive - nowcast_median_all,
        na.rm = TRUE
      ),
      max_death_excess = max(
        nowcast_median_deaths - nowcast_median_all,
        na.rm = TRUE
      ),
      .by = zone_sante_notification
    ) |>
    dplyr::mutate(
      status_sum = median_alive + median_deaths,
      relative_discrepancy = dplyr::if_else(
        median_all > 0,
        abs(status_sum - median_all) / median_all,
        dplyr::if_else(status_sum == 0, 0, Inf)
      )
    ) |>
    dplyr::arrange(dplyr::desc(relative_discrepancy), zone_sante_notification)

  zero_all <- summary |>
    dplyr::filter(.data$median_all == 0, .data$status_sum > 0)
  if (nrow(zero_all) > 0L) {
    bad <- zero_all$zone_sante_notification[[1]]
    rlang::abort(
      c(
        "Status-specific nowcasts are not coherent.",
        x = sprintf(
          "For %s, the all-case nowcast is zero but alive + deaths is %g.",
          bad,
          zero_all$status_sum[[1]]
        ),
        i = "Check that every series resolves onset dates with the same fallback columns."
      ),
      class = "nowcast_coherence_error"
    )
  }

  worst <- suppressWarnings(max(summary$relative_discrepancy, na.rm = TRUE))
  if (!is.finite(worst)) {
    rlang::abort(
      "Status-specific nowcasts contain non-finite estimates.",
      class = "nowcast_coherence_error"
    )
  }
  worst_zone <- summary$zone_sante_notification[[which.max(summary$relative_discrepancy)]]

  if (worst > error_above) {
    rlang::abort(
      c(
        "Status-specific nowcasts are not coherent.",
        x = sprintf(
          "For %s, |alive + deaths - all| / all = %.1f%%.",
          worst_zone, 100 * worst
        ),
        i = "Refit the nowcasts or increase `error_above` deliberately."
      ),
      class = "nowcast_coherence_error"
    )
  }

  if (worst > warn_above) {
    rlang::warn(
      sprintf(
        "Status-specific nowcasts differ from additivity by up to %.1f%% (%s).",
        100 * worst, worst_zone
      ),
      class = "nowcast_coherence_warning"
    )
  }

  out <- list(
    confirmed_cases = nowcasts_all_cases,
    confirmed_alive = nowcasts_alive,
    confirmed_deaths = nowcasts_deaths
  )
  attr(out, "coherence") <- summary
  invisible(out)
}
