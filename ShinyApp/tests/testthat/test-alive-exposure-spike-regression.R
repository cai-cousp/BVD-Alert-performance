library(testthat)
library(dplyr)
library(tidyr)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/alive_exposure.R"))
source(here::here("alert_helpers/compute_alert_multipliers.R"))
source(here::here("alert_helpers/window_utils.R"))

# Fixture mirroring the 2026-09 spike: consecutive 7-day grid windows where
# the weekly alive nowcast collapses to ~0 in one window (outcomes not yet
# recorded, `below_min_cases` empirical fallback) while alive alerts keep
# flowing. The offset model must not let that week drive beta_c.
make_spike_weekly_inputs <- function() {
  window_keys <- format(as.Date("2026-08-16") + (0:2) * 7)
  dates <- as.Date("2026-08-16") + 0:20

  # All-case nowcast: stable ~1 case/day over the tail.
  all_nowcast <- tibble::tibble(
    zone_sante_notification = "A",
    date = dates,
    observed = rep(c(1L, 0L), length.out = length(dates)),
    nowcast_median = rep(1, length(dates)),
    nowcast_lower_90 = rep(0.8, length(dates)),
    nowcast_upper_90 = rep(1.2, length(dates)),
    method = "empirical_delay_correction",
    status = "fit_ok"
  )
  attr(all_nowcast, "ref_date") <- max(dates)
  attr(all_nowcast, "series") <- "confirmed_cases"

  # Alive nowcast: healthy (~3/day) in the first week, collapsed to ~0.3/day
  # in the trailing two weeks (below_min_cases in real runs).
  alive_median <- dplyr::if_else(
    dates <= as.Date("2026-08-22"),
    3,
    0.3
  )
  alive_nowcast <- tibble::tibble(
    zone_sante_notification = "A",
    date = dates,
    observed = rep(0L, length(dates)),
    nowcast_median = alive_median,
    nowcast_lower_90 = alive_median * 0.8,
    nowcast_upper_90 = alive_median * 1.2,
    method = "empirical_delay_correction",
    status = "below_min_cases"
  )
  attr(alive_nowcast, "ref_date") <- max(dates)
  attr(alive_nowcast, "series") <- "confirmed_alive"

  windows <- tibble::tibble(
    threshold_time_key = window_keys,
    threshold_window_id = window_keys,
    threshold_window_index = seq_along(window_keys),
    recent_case_window_start = as.Date(window_keys),
    recent_case_window_end = as.Date(window_keys) + 6L
  )

  list(
    all_nowcast = all_nowcast,
    alive_nowcast = alive_nowcast,
    windows = windows
  )
}

test_that("window-level alive shares are not clobbered by the HZ fallback", {
  # Regression: rows_update(hz_fallback, by = zone) overwrote valid
  # hz_window shares (e.g. Wamba raw share 0.561 became hz_fallback 1).
  keys <- tibble::tribble(
    ~threshold_time_key, ~zone_sante_notification,
    "2026-08-23", "A",
    "2026-08-30", "A"
  )
  all_cases <- keys |> mutate(
    n_recent_confirmed_nowcast = c(NA, 100),
    n_recent_confirmed_nowcast_low = c(NA, 80),
    n_recent_confirmed_nowcast_high = c(NA, 120)
  )
  alive <- keys |> mutate(
    n_recent_confirmed_alive_nowcast = c(NA, 56),
    n_recent_confirmed_alive_nowcast_low = c(NA, 50),
    n_recent_confirmed_alive_nowcast_high = c(NA, 62)
  )
  case_thresholds <- keys |> mutate(estimated_true_cases_recent = 200)

  out <- add_alive_exposure_to_case_thresholds(
    case_thresholds,
    all_cases,
    alive
  )

  # Both windows inherit the HZ-level fallback share (~0.56); only the
  # missing-window's source should report hz_fallback while the usable
  # window should retain its window-level source.
  share_0830 <- out |>
    filter(threshold_time_key == "2026-08-30") |>
    pull(alive_share)
  src_0830 <- out |>
    filter(threshold_time_key == "2026-08-30") |>
    pull(alive_share_source)
  expect_equal(share_0830, 0.56)
  expect_equal(src_0830, "hz_window")
})

test_that("weekly alive exposure floor prevents a collapsed window from spiking beta", {
  # Regression (Wamba, window ending 05-09): the alive nowcast collapses to
  # ~0 for the trailing week(s) (status below_min_cases) while validated
  # alive alerts keep flowing. Unfettered, the week gets near-zero exposure
  # with non-zero count and drags the offset rate up ~7x.
  inputs <- make_spike_weekly_inputs()
  dates <- as.Date("2026-08-16") + 0:20

  # Confirmed alive cases with onsets concentrated in the first two weeks;
  # the last week carries no alive-case incidence.
  onset_dates <- c(
    as.Date("2026-08-16") + 0:6,
    rep(as.Date("2026-08-23") + 0:6, 3)
  )
  evd_rows <- purrr::map_dfr(onset_dates, \(d) {
    tibble::tibble(
      zone_sante_notification = "A",
      classification_finale = "Cas confirmé",
      lab_resultat_final = "Positif",
      alert_date_debut_symptoms = d,
      s2_date_debut_signes_symptomes = d,
      date_debut_signes_symptomes_impt = as.Date(NA),
      nature_alerte = "Vivant",
      s5_statut_patient_lors_prelev = "Vivant",
      s6_statut_final_patient = "Vivant",
      date_de_deces = as.Date(NA),
      s6_date_deces = as.Date(NA)
    )
  })

  # Alerts are a separate signal stream: validated alive alerts keep flowing
  # into the collapsed week even though the alive-case series is empty.
  val_alerts <- tibble::tibble(
    zone_sante_notification = "A",
    nature_alerte = "Vivant",
    date_heure_notification_alerte = seq(
      as.Date("2026-08-30"),
      as.Date("2026-09-05"),
      by = "day"
    ),
    alert_conlusion = "Validée"
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 60,
    estimated_true_alive_cases_recent = 30,
    threshold_time_key = "2026-09-13",
    recent_case_window_start = as.Date("2026-09-13"),
    recent_case_window_end = as.Date("2026-09-13") + 6L
  )
  hz_cfr <- tibble::tibble(zone_sante_notification = "A", cfr_used = 0.4)

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd_rows,
    windows = inputs$windows,
    nowcast = inputs$all_nowcast,
    nowcast_alive = inputs$alive_nowcast,
    ref_date = max(dates)
  )

  weekly <- attr(result, "weekly_model_data")
  collapsed_week <- weekly |>
    filter(week_bin == as.Date("2026-08-30"))
  other_weeks <- weekly |>
    filter(week_bin != as.Date("2026-08-30"))

  # The collapsed week must be flagged and excluded from the model fit
  # (or its exposure floored), not silently passed with ~0 exposure.
  expect_true(
    all(collapsed_week$case_model_included == FALSE) ||
      all(
        collapsed_week$estimated_true_alive_cases_week >=
          0.05 * max(other_weeks$estimated_true_alive_cases_week, na.rm = TRUE)
      )
  )

  # And the fitted beta must stay within a sane band relative to the ratio
  # implied by the non-degenerate weeks (here: 0 alerts / ~29 exposure).
  beta <- result$beta_c
  expect_true(is.finite(beta))
  expect_true(beta < 0.05)
})

test_that("fit_poisson_offset_rate respects per-week inclusion flags from caller", {
  # When the caller flags degenerate weeks via case_model_included /
  # death_model_included (set upstream by compute_alert_multipliers), the
  # offset-rate fit must honour those flags rather than re-filtering on
  # min_weekly_exposure alone.
  data <- tibble::tibble(
    zone_sante_notification = "SpikeZone",
    week_bin = as.Date(c("2026-08-16", "2026-08-23", "2026-08-30")),
    case_alerts_week = c(5L, 6L, 6L),
    estimated_true_alive_cases_week = c(1000, 900, 0.5),
    case_model_included = c(TRUE, TRUE, FALSE)
  )

  res <- fit_poisson_offset_rate(
    data = data,
    count_col = "case_alerts_week",
    exposure_col = "estimated_true_alive_cases_week",
    model = "case",
    min_model_weeks = 2L,
    confidence_level = 0.95,
    min_total_exposure = 1.0,
    min_weekly_exposure = 0.1,
    beta_max = 25.0
  )

  expect_equal(res$n_model_weeks, 2L)
  expect_true(res$beta < 0.02)
  expect_equal(res$total_exposure, 1900)
})

test_that("pooled beta guards against zero denominators", {
  # 01b pooled fallback divides alert counts by summed exposure; zero
  # exposure must yield NA, not Inf/NaN.
  expect_equal(pooled_alert_beta(10, 0), NA_real_)
  expect_equal(pooled_alert_beta(10, NA_real_), NA_real_)
  expect_equal(pooled_alert_beta(0, 0), NA_real_)
  expect_equal(pooled_alert_beta(10, 100), 0.1)
})
