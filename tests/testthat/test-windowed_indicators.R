library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/nowcast_by_zone.R"))
source(here::here("alert_helpers/compute_recent_confirmed_windows_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_by_hz.R"))
source(here::here("alert_helpers/compute_detection_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/compute_growth_rate_by_hz.R"))
source(here::here("alert_helpers/compute_detection_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/compute_rt_hz.R"))
source(here::here("alert_helpers/combine_detection_estimates_by_hz.R"))
source(here::here("alert_helpers/compute_last_window_projection.R"))

#' Minimal line list with two populated zones and one sparse zone
make_evd <- function() {
  zone_a_dates <- seq.Date(as.Date("2026-06-01"), as.Date("2026-06-21"), by = "day")
  zone_b_dates <- seq.Date(as.Date("2026-06-05"), as.Date("2026-06-14"), by = "day")

  evd <- tibble::tibble(
    zone_sante_notification = c(
      rep("Zone_A", length(zone_a_dates)),
      rep("Zone_B", length(zone_b_dates)),
      "Zone_C"
    ),
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif",
    alert_date_debut_symptoms = c(
      zone_a_dates,
      zone_b_dates,
      as.Date("2026-06-10")
    ),
    s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
    nature_alerte = "Vivant",
    s6_statut_final_patient = "Vivant",
    alert_lien_epidemiologic = c(
      rep("Oui", length(zone_a_dates) + length(zone_b_dates)),
      NA_character_
    )
  )

  death_days <- as.Date(c("2026-06-03", "2026-06-10", "2026-06-17"))
  death_idx <- which(
    evd$alert_date_debut_symptoms %in% death_days &
      evd$zone_sante_notification == "Zone_A"
  )
  evd$nature_alerte[death_idx] <- "Décédé"
  evd$s6_statut_final_patient[death_idx] <- "Décédé"
  evd
}

#' Three backward-anchored 7-day windows ending 06-07 / 06-14 / 06-21
make_windows <- function() {
  ends <- as.Date(c("2026-06-07", "2026-06-14", "2026-06-21"))
  starts <- ends - 6L
  tibble::tibble(
    threshold_valid_from = starts,
    threshold_valid_to = ends,
    threshold_time_key = format(starts),
    recent_case_window_start = starts,
    recent_case_window_end = ends,
    recent_case_window_days = 7L,
    recent_case_anchor_date = starts,
    threshold_window_id = paste(format(starts), format(ends), sep = "__"),
    threshold_window_index = seq_along(ends)
  ) |>
    dplyr::select(
      threshold_time_key,
      threshold_window_id,
      threshold_window_index,
      threshold_valid_from,
      threshold_valid_to,
      recent_case_window_start,
      recent_case_window_end,
      recent_case_window_days,
      recent_case_anchor_date
    )
}

meta_columns <- c(
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

test_that("compute_cfr_by_hz estimates CFR on cumulative windows", {
  evd <- make_evd()
  windows <- make_windows()

  result <- compute_cfr_by_hz(evd, windows = windows)

  expect_true(all(meta_columns %in% names(result)))
  # 3 zones x 3 windows
  expect_equal(nrow(result), 9L)

  zone_a <- result |>
    dplyr::filter(zone_sante_notification == "Zone_A")
  # Cumulative confirmed counts through each window end
  expect_equal(zone_a$n_conf, c(7L, 14L, 21L))
  expect_equal(zone_a$n_deaths, c(1L, 2L, 3L))
  expect_equal(zone_a$cfr_used, rep(1 / 7, 3), tolerance = 1e-8)

  # Sparse zones fall back to the per-window pooled CFR
  zone_b <- result |>
    dplyr::filter(zone_sante_notification == "Zone_B")
  pooled_w1 <- sum(result$n_deaths[result$threshold_time_key == "2026-06-01"]) /
    sum(result$n_conf[result$threshold_time_key == "2026-06-01"])
  expect_equal(zone_b$cfr_used[[1]], pooled_w1, tolerance = 1e-8)
})

test_that("compute_detection_by_hz keeps one row per zone x window", {
  evd <- make_evd()
  windows <- make_windows()

  result <- compute_detection_by_hz(evd, windows = windows)

  expect_equal(nrow(result), 9L)
  expect_true(all(meta_columns %in% names(result)))

  zone_a <- result |>
    dplyr::filter(zone_sante_notification == "Zone_A")
  expect_equal(zone_a$n_conf_valid_epilink, c(7L, 14L, 21L))
  expect_equal(zone_a$detection_rate_adj, rep(1, 3), tolerance = 1e-8)

  # Zones without any valid epi-link field keep an NA rate
  zone_c <- result |>
    dplyr::filter(zone_sante_notification == "Zone_C")
  expect_true(all(is.na(zone_c$detection_rate_adj)))
})

test_that("compute_cfr_backcalc_by_hz applies nowcast only to recent windows", {
  evd <- make_evd()
  windows <- make_windows()

  # Nowcast tail covers 2026-06-08 .. 2026-06-28; only windows ending on or
  # after 2026-06-08 (latest onset within max_delay of the reference date)
  # can be adjusted.
  nowcast_dates <- seq.Date(as.Date("2026-06-08"), as.Date("2026-06-28"), by = "day")
  nowcasts <- tidyr::crossing(
    zone_sante_notification = c("Zone_A", "Zone_B"),
    date = nowcast_dates
  ) |>
    dplyr::mutate(
      observed = 0L,
      nowcast_median = dplyr::if_else(
        date %in% as.Date(c("2026-06-20", "2026-06-21")),
        2,
        0
      ),
      nowcast_lower_90 = nowcast_median * 0.8,
      nowcast_upper_90 = nowcast_median * 1.2,
      method = "epinow2",
      status = "fit_ok"
    )
  attr(nowcasts, "ref_date") <- as.Date("2026-06-28")

  result <- compute_cfr_backcalc_by_hz(
    evd,
    windows = windows,
    nowcast = nowcasts
  )

  expect_equal(nrow(result), 9L)
  expect_true(all(meta_columns %in% names(result)))

  zone_a <- result |>
    dplyr::filter(zone_sante_notification == "Zone_A")
  expect_equal(zone_a$n_confirmed, c(7L, 14L, 21L))

  # Window 1 (ending 06-07) predates the nowcast horizon -> no adjustment
  expect_true(all(is.na(zone_a$n_confirmed_nowcast[zone_a$threshold_time_key == "2026-06-01"])))
  # Window 3 (ending 06-21) gets the two nowcast days (2 + 2)
  expect_equal(
    zone_a$n_confirmed_nowcast[zone_a$threshold_time_key == "2026-06-15"],
    25L
  )
})

test_that("compute_growth_rate_by_hz shares the grid keys when windows supplied", {
  evd <- make_evd()
  windows <- make_windows()

  result <- compute_growth_rate_by_hz(
    evd,
    windows = windows,
    n_copy_last = 0L
  )

  expect_equal(nrow(result), 9L)
  expect_equal(
    result |>
      dplyr::distinct(threshold_time_key) |>
      dplyr::pull(threshold_time_key),
    c("2026-06-01", "2026-06-08", "2026-06-15")
  )
  expect_true("pooled_r" %in% names(result))
  expect_true("r_used" %in% names(result))

  # Sparse zone falls back to the pooled growth rate where available
  zone_c <- result |>
    dplyr::filter(zone_sante_notification == "Zone_C")
  expect_true(all(zone_c$growth_fallback_reason %in%
    c("pooled_growth_rate", "missing_growth_rate")))
  pooled_matches <- zone_c |>
    dplyr::filter(growth_fallback_reason == "pooled_growth_rate")
  if (nrow(pooled_matches) > 0) {
    expect_equal(pooled_matches$r_used, pooled_matches$pooled_r)
  }
})

test_that("compute_detection_cfr_backcalc_by_hz joins CFR and growth per window", {
  evd <- make_evd()
  windows <- make_windows()

  hz_cfr_bk <- compute_cfr_backcalc_by_hz(evd, windows = windows)
  hz_growth <- compute_growth_rate_by_hz(
    evd,
    windows = windows,
    n_copy_last = 0L
  )

  result <- compute_detection_cfr_backcalc_by_hz(
    evd,
    hz_cfr_bk,
    hz_growth,
    windows = windows
  )

  expect_equal(nrow(result), 9L)
  expect_true(all(meta_columns %in% names(result)))

  zone_a <- result |>
    dplyr::filter(zone_sante_notification == "Zone_A")
  expect_equal(zone_a$n_detected, c(7L, 14L, 21L))
  expect_equal(
    zone_a$threshold_time_key,
    c("2026-06-01", "2026-06-08", "2026-06-15")
  )
})

test_that("compute_rt_by_hz returns one row per zone x window with pooled fallback", {
  evd <- make_evd()
  windows <- make_windows()

  result <- compute_rt_by_hz(evd, windows = windows)

  expect_equal(nrow(result), 9L)
  expect_true(all(meta_columns %in% names(result)))
  expect_true(all(c("rt", "pooled_rt", "rt_used") %in% names(result)))

  # Zone_C has a single case: rt is NA and falls back to the pooled value
  zone_c <- result |>
    dplyr::filter(zone_sante_notification == "Zone_C")
  expect_true(all(is.na(zone_c$rt)))
  expect_equal(zone_c$rt_used, zone_c$pooled_rt)
})

test_that("combine_detection_estimates_by_hz works with all-windowed inputs", {
  evd <- make_evd()

  recent <- compute_recent_confirmed_windows_by_hz(
    evd,
    window_days = 7L,
    analysis_start_date = as.Date("2026-06-01"),
    analysis_end_date = as.Date("2026-06-21")
  )
  grid <- alert_window_grid(recent)

  hz_cfr_bk <- compute_cfr_backcalc_by_hz(evd, windows = grid)
  hz_growth <- compute_growth_rate_by_hz(
    evd,
    windows = grid,
    n_copy_last = 0L
  )
  detection_bk <- compute_detection_cfr_backcalc_by_hz(
    evd,
    hz_cfr_bk,
    hz_growth,
    windows = grid
  )
  detection_epi <- compute_detection_by_hz(evd, windows = grid)

  combined <- combine_detection_estimates_by_hz(
    detection_bk,
    detection_epi,
    recent_cases = recent
  )

  # One row per HZ x window, exactly matching the recent-case grid
  expect_equal(nrow(combined), nrow(recent))
  expect_equal(
    combined |>
      dplyr::distinct(zone_sante_notification, threshold_time_key) |>
      nrow(),
    nrow(recent)
  )
  expect_true("n_recent_confirmed" %in% names(combined))
  expect_true("estimated_true_cases_recent" %in% names(combined))

  # The most recent window per zone is the last key of the grid
  most_recent <- combined |>
    dplyr::filter(
      threshold_time_key == max(threshold_time_key, na.rm = TRUE)
    )
  expect_equal(nrow(most_recent), nrow(alert_hz_index(evd)))
})

test_that("compute_last_window_projection joins growth by window key", {
  recent <- tibble::tibble(
    threshold_time_key = c("w1", "w2"),
    zone_sante_notification = "Zone_A",
    n_recent_confirmed = c(5L, 3L),
    n_recent_confirmed_nowcast = c(NA_integer_, 30L)
  )
  growth <- tibble::tibble(
    threshold_time_key = c("w1", "w2"),
    zone_sante_notification = "Zone_A",
    r_used = c(0.1, 0.1),
    predicted_last_count = c(10, 20)
  )

  out <- compute_last_window_projection(recent, growth)

  expect_equal(out$n_recent_confirmed, c(5L, 30L))
  expect_equal(out$projection_method, c("observed", "nowcast"))
})
