#' Compute Rt by HZ
#'
#' Estimates the effective reproduction number (Rt) from confirmed/positive
#' cases with symptom onset up to `end_date`. By default the whole dataset
#' period is used; pass `window_days` to restrict to a trailing window of that
#' length in days. By default the anchor is the most recent confirmed/positive
#' onset date in `hz_data`.
#'
#' When `nowcast` is supplied (output of `compute_nowcasts_by_zone()`), the
#' truncated tail of the window is replaced with nowcast counts before Rt is
#' estimated; otherwise raw counts are used unchanged.
#'
#' The value returned is the last `Mean(R)` of the `estimate_R()` sliding
#' windows — the current reproduction number at the end of the series.
#'
#' @param hz_data Dataframe. Data filtered for a specific HZ.
#' @param window_days Numeric. Length in days of the trailing incidence window;
#'   `NULL` (default) uses the whole dataset period up to `end_date`.
#' @param end_date Date. Anchor date for the trailing window; `NULL` uses the
#'   most recent confirmed/positive onset date in `hz_data`.
#' @param case_date_col Character. Preferred case date column.
#' @param fallback_date_col Character. Fallback date column.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param ref_date Date. Reference date used to build the daily series.
#' @param max_delay Integer. Nowcast tail length in days.
#' @param min_date Date. Optional lower bound for the daily series.
compute_rt_hz <- function(hz_data,
                          window_days = NULL,
                          end_date = NULL,
                          case_date_col = "alert_date_debut_symptoms",
                          fallback_date_col = "s2_date_debut_signes_symptomes",
                          nowcast = NULL,
                          ref_date = Sys.Date(),
                          max_delay = 21L,
                          min_date = NULL,
                          return_interval = FALSE) {
  empty_res <- if (isTRUE(return_interval)) {
    tibble::tibble(rt = NA_real_, rt_lower = NA_real_, rt_upper = NA_real_)
  } else {
    NA_real_
  }

  if (!is.null(window_days) &&
      (!is.numeric(window_days) || length(window_days) != 1L || window_days < 1)) {
    rlang::abort("`window_days` must be a positive number or NULL (whole dataset period).")
  }

  hz_full <- tryCatch(
    build_confirmed_daily(
      hz_data,
      min_date = min_date,
      ref_date = ref_date,
      case_date_col = case_date_col,
      fallback_date_col = fallback_date_col,
      min_days = 3L
    ),
    error = function(e) NULL
  )
  if (is.null(hz_full)) return(empty_res)

  # Anchor on confirmed/positive onset dates unless an explicit end date is
  # supplied (e.g. the window end in the cumulative 01b pipeline).
  if (is.null(end_date)) end_date <- max(hz_full$date, na.rm = TRUE)
  end_date <- as.Date(end_date)

  if (is.null(window_days)) {
    hz_cases <- hz_full |>
      dplyr::filter(.data$date <= end_date)
  } else {
    hz_cases <- hz_full |>
      dplyr::filter(
        .data$date >= (end_date - window_days),
        .data$date <= end_date
      )
  }

  if (!is.null(nowcast)) {
    zone <- NULL
    if ("zone_sante_notification" %in% names(hz_data)) {
      zone_values <- unique(
        hz_data$zone_sante_notification[!is.na(hz_data$zone_sante_notification)]
      )
      if (length(zone_values) == 1L) zone <- zone_values[[1L]]
    }
    hz_cases <- splice_nowcast_tail(
      hz_cases,
      nowcast,
      zone = zone,
      ref_date = end_date,
      max_delay = max_delay
    )
  }

  if (sum(hz_cases$I, na.rm = TRUE) < 3) return(empty_res)
  if (nrow(hz_cases) < 3) return(empty_res)

  # Standard SI for EVD: mean 12, sd 5 (Uganda 2000-01)
  tryCatch(
    {
      res <- suppressMessages(suppressWarnings(EpiEstim::estimate_R(
        hz_cases$I,
        method = "parametric_si",
        config = EpiEstim::make_config(list(
          mean_si = 12, std_si = 5
        ))
      )))
      if (isTRUE(return_interval)) {
        tibble::tibble(
          rt = dplyr::last(res$R$`Mean(R)`),
          rt_lower = dplyr::last(res$R$`Quantile.0.025(R)`),
          rt_upper = dplyr::last(res$R$`Quantile.0.975(R)`)
        )
      } else {
        dplyr::last(res$R$`Mean(R)`)
      }
    },
    error = function(e) empty_res
  )
}

#' Compute Rt across time windows with optional stratification
#'
#' Iterates the shared analysis grid (`windows` via `alert_window_grid()`)
#' and estimates the effective reproduction number (Rt) along with its 95%
#' credible interval (from `EpiEstim::estimate_R()`) on cumulative line-list
#' data ending at each window end (or within a trailing window of `window_days`).
#'
#' By default (`by = NULL`), Rt is estimated on the overall dataset without
#' stratification. When `by` is supplied (as bare symbols, character vector,
#' or tidyselect expression), Rt is estimated for each group combination in each
#' window. For groups where the estimate is unavailable (e.g., fewer than 3
#' cases), estimates fall back to the pooled (all-group) Rt and interval for the
#' same window.
#'
#' @param data Dataframe. EVD line list.
#' @param windows Dataframe. Shared time-window grid (distinct window metadata
#'   or a full HZ x window table such as `hz_recent_cases`).
#' @param by Grouping variable(s) specified as bare symbols, character strings,
#'   or tidyselect expression. Defaults to `NULL` (unstratified overall dataset).
#' @param window_days Numeric. Length in days of the trailing incidence
#'   window used by `compute_rt_hz()`; `NULL` (default) uses the whole
#'   dataset period up to each window end.
#' @param case_date_col Character. Preferred case date column.
#' @param fallback_date_col Character. Fallback date column.
#' @param nowcast Dataframe. Optional nowcast table from
#'   `compute_nowcasts_by_zone()`.
#' @param max_delay Integer. Nowcast tail length in days.
#' @return Tibble with window metadata columns, estimated `rt`, 95% interval
#'   (`rt_lower`, `rt_upper`), pooled fallbacks (`pooled_rt`, `pooled_rt_lower`,
#'   `pooled_rt_upper`), and resolved `rt_used`, `rt_used_lower`, `rt_used_upper`.
#'   If `by` is specified, the grouping column(s) are included.
#' @export
compute_rt <- function(data,
                       windows,
                       by = NULL,
                       window_days = NULL,
                       case_date_col = "alert_date_debut_symptoms",
                       fallback_date_col = "s2_date_debut_signes_symptomes",
                       nowcast = NULL,
                       max_delay = 21L) {
  if (is.null(windows)) {
    rlang::abort("`compute_rt()` requires a shared `windows` grid.")
  }

  by_quo <- rlang::enquo(by)
  by_cols <- if (rlang::quo_is_null(by_quo)) {
    NULL
  } else {
    tryCatch(
      names(tidyselect::eval_select(by_quo, data = data)),
      error = function(e) {
        val <- tryCatch(rlang::eval_tidy(by_quo), error = function(e2) NULL)
        if (is.character(val) && all(val %in% names(data))) {
          val
        } else {
          rlang::abort(
            c("Invalid `by` argument in `compute_rt()`.",
              x = conditionMessage(e)),
            parent = e
          )
        }
      }
    )
  }

  windows_meta <- alert_window_grid(windows)
  resolved_date <- alert_resolve_case_date(
    data,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col
  )

  # When stratified, determine distinct group combinations across data
  group_index <- if (!is.null(by_cols)) {
    data |>
      dplyr::select(dplyr::all_of(by_cols)) |>
      dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) |>
      dplyr::distinct() |>
      dplyr::arrange(dplyr::across(dplyr::everything()))
  } else {
    NULL
  }

  out <- purrr::map_dfr(
    seq_len(nrow(windows_meta)),
    \(i) {
      w <- windows_meta[i, ]
      subset <- data |>
        dplyr::mutate(.alert_resolved_date = resolved_date) |>
        dplyr::filter(
          !is.na(.alert_resolved_date),
          .alert_resolved_date <= w$recent_case_window_end
        ) |>
        dplyr::select(-.alert_resolved_date)

      if (is.null(by_cols)) {
        # Unstratified overall estimate
        res_rt <- compute_rt_hz(
          hz_data = subset,
          window_days = window_days,
          end_date = w$recent_case_window_end,
          ref_date = w$recent_case_window_end,
          case_date_col = case_date_col,
          fallback_date_col = fallback_date_col,
          nowcast = nowcast,
          max_delay = max_delay,
          return_interval = TRUE
        )

        tibble::tibble(
          rt = res_rt$rt,
          rt_lower = res_rt$rt_lower,
          rt_upper = res_rt$rt_upper,
          pooled_rt = res_rt$rt,
          pooled_rt_lower = res_rt$rt_lower,
          pooled_rt_upper = res_rt$rt_upper,
          rt_used = res_rt$rt,
          rt_used_lower = res_rt$rt_lower,
          rt_used_upper = res_rt$rt_upper
        ) |>
          alert_bind_window_metadata(w)
      } else {
        # Stratified by by_cols
        rows <- purrr::map_dfr(
          seq_len(nrow(group_index)),
          \(g_idx) {
            g_row <- group_index[g_idx, , drop = FALSE]
            g_subset <- subset |>
              dplyr::semi_join(g_row, by = by_cols)

            nowcast_matched <- nowcast
            if (!is.null(nowcast_matched) && !is.null(by_cols)) {
              if ("province" %in% by_cols && !"province" %in% names(nowcast_matched) && "province_notification" %in% names(nowcast_matched)) {
                nowcast_matched$province <- nowcast_matched$province_notification
              } else if ("province_notification" %in% by_cols && !"province_notification" %in% names(nowcast_matched) && "province" %in% names(nowcast_matched)) {
                nowcast_matched$province_notification <- nowcast_matched$province
              }
            }

            g_nowcast <- if (!is.null(nowcast_matched) &&
                             !is.null(by_cols) &&
                             all(by_cols %in% names(nowcast_matched))) {
              nowcast_matched |>
                dplyr::semi_join(g_row, by = by_cols)
            } else if (!is.null(nowcast_matched) &&
                       "zone_sante_notification" %in% names(nowcast_matched) &&
                       "zone_sante_notification" %in% names(g_subset)) {
              nowcast_matched |>
                dplyr::filter(.data$zone_sante_notification %in% unique(g_subset$zone_sante_notification))
            } else {
              nowcast_matched
            }

            res_rt <- compute_rt_hz(
              hz_data = g_subset,
              window_days = window_days,
              end_date = w$recent_case_window_end,
              ref_date = w$recent_case_window_end,
              case_date_col = case_date_col,
              fallback_date_col = fallback_date_col,
              nowcast = g_nowcast,
              max_delay = max_delay,
              return_interval = TRUE
            )

            dplyr::bind_cols(
              g_row,
              tibble::tibble(
                rt = res_rt$rt,
                rt_lower = res_rt$rt_lower,
                rt_upper = res_rt$rt_upper
              )
            )
          }
        )

        n_valid <- sum(is.finite(rows$rt), na.rm = TRUE)
        pooled_mean <- if (n_valid > 0) mean(rows$rt, na.rm = TRUE) else NA_real_
        pooled_low  <- if (n_valid > 0) mean(rows$rt_lower, na.rm = TRUE) else NA_real_
        pooled_high <- if (n_valid > 0) mean(rows$rt_upper, na.rm = TRUE) else NA_real_

        rows |>
          dplyr::mutate(
            pooled_rt = pooled_mean,
            pooled_rt_lower = pooled_low,
            pooled_rt_upper = pooled_high,
            rt_used = dplyr::if_else(is.finite(rt), rt, pooled_rt),
            rt_used_lower = dplyr::if_else(is.finite(rt), rt_lower, pooled_rt_lower),
            rt_used_upper = dplyr::if_else(is.finite(rt), rt_upper, pooled_rt_upper)
          ) |>
          alert_bind_window_metadata(w)
      }
    }
  )

  if (!is.null(by_cols)) {
    out |>
      dplyr::arrange(
        threshold_valid_from,
        dplyr::across(dplyr::all_of(by_cols))
      )
  } else {
    out |>
      dplyr::arrange(threshold_valid_from)
  }
}

#' Compute Rt per health zone per time window
#'
#' Backward-compatible wrapper around `compute_rt()` with
#' `by = "zone_sante_notification"`.
#'
#' Iterates the shared analysis grid (`hz_recent_cases` / `alert_window_grid`)
#' and estimates Rt for each health zone on the whole dataset period (or a
#' trailing window of `window_days`) ending at each window end, using
#' cumulative line-list data up to that window end. Rows where the
#' zone-specific estimate is unavailable fall back to the pooled (all-zone)
#' Rt for the same window.
#'
#' @inheritParams compute_rt
#' @return Tibble with one row per health zone per time window and columns
#'   `rt`, `rt_lower`, `rt_upper`, `pooled_rt`, `pooled_rt_lower`,
#'   `pooled_rt_upper`, `rt_used`, `rt_used_lower`, and `rt_used_upper`.
#' @export
compute_rt_by_hz <- function(data,
                             windows,
                             window_days = NULL,
                             case_date_col = "alert_date_debut_symptoms",
                             fallback_date_col = "s2_date_debut_signes_symptomes",
                             nowcast = NULL,
                             max_delay = 21L) {
  compute_rt(
    data = data,
    windows = windows,
    by = "zone_sante_notification",
    window_days = window_days,
    case_date_col = case_date_col,
    fallback_date_col = fallback_date_col,
    nowcast = nowcast,
    max_delay = max_delay
  )
}
