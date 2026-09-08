library(testthat)
library(dplyr)

source(here::here("R/alert_helpers.R"))
source(here::here("R/alert_plots.R"))

test_that("Ensemble thresholds exclude Approach B from synthesis", {
  # Mock HZ synthesis data
  hz_synth <- tibble::tibble(
    threshold_time_key = c("2026-07-06", "2026-07-06", "2026-07-13", "2026-07-13"),
    zone_sante_notification = c("Zone1", "Zone2", "Zone1", "Zone2"),
    Province = c("Ituri", "Ituri", "Ituri", "Ituri"),
    Population = c(100000, 200000, 100000, 200000),
    expected_weekly_deaths_cmr = c(16, 32, 16, 32),
    death_threshold_lower_A = c(14, 29, 14, 29),
    death_threshold_upper_A = c(18, 35, 18, 35),
    alert_case_threshold_lower_B = c(10, 20, 10, 20),
    alert_case_threshold_upper_B = c(30, 60, 30, 60),
    alert_death_threshold_lower_B = c(5, 10, 5, 10),
    alert_death_threshold_upper_B = c(15, 30, 15, 30),
    alert_case_threshold_C = c(8, 12, 14, 26),
    alert_case_threshold_lower_C = c(6, 10, 10, 20),
    alert_case_threshold_upper_C = c(10, 14, 18, 32),
    alert_death_threshold_C = c(4, 6, 7, 13),
    alert_death_threshold_lower_C = c(3, 5, 5, 10),
    alert_death_threshold_upper_C = c(5, 7, 9, 16),
    Alert_case_lower = c(8, 15, 10, 20),
    Alert_case_upper = c(20, 37, 24, 46),
    Alert_death_lower = c(7, 15, 9, 16),
    Alert_death_upper = c(13, 24, 14, 27),
    Alert_case_threshold = c(14, 26, 17, 33),
    Alert_death_threshold = c(10, 20, 12, 22),
    recent_case_window_start = as.Date(c("2026-07-06", "2026-07-06", "2026-07-13", "2026-07-13")),
    recent_case_window_end = as.Date(c("2026-07-12", "2026-07-12", "2026-07-19", "2026-07-19")),
    recent_case_window_days = c(7L, 7L, 7L, 7L),
    recent_case_anchor_date = as.Date(rep("2026-07-19", 4)),
    threshold_valid_from = as.Date(c("2026-07-06", "2026-07-06", "2026-07-13", "2026-07-13")),
    threshold_valid_to = as.Date(c("2026-07-12", "2026-07-12", "2026-07-19", "2026-07-19")),
    week_start = as.Date(c("2026-07-06", "2026-07-06", "2026-07-13", "2026-07-13"))
  )

  confirmed_dates <- tibble::tibble(
    zone_sante_notification = c("Zone1", "Zone2"),
    first_confirmed_notification_date = as.Date(c("2026-07-01", "2026-07-01"))
  )

  ensemble_synth <- build_ensemble_thresholds(hz_synth, confirmed_dates)

  expect_equal(nrow(ensemble_synth), 2)
  expect_equal(ensemble_synth$Population, c(300000, 300000))
  expect_equal(ensemble_synth$expected_weekly_deaths_cmr, c(48, 48))
  expect_equal(ensemble_synth$Alert_case_lower, c(16, 30))
  expect_equal(ensemble_synth$Alert_case_upper, c(24, 50))
  expect_equal(ensemble_synth$Alert_death_lower, c(25.5, 29))
  expect_equal(ensemble_synth$Alert_death_upper, c(32.5, 39))
  expect_equal(ensemble_synth$Alert_case_threshold, c(20, 40))
  expect_equal(ensemble_synth$Alert_death_threshold, c(29, 34))
  expect_equal(ensemble_synth$alert_case_threshold_lower_B, c(NA_real_, NA_real_))
  expect_equal(ensemble_synth$alert_case_threshold_upper_B, c(NA_real_, NA_real_))
  expect_equal(ensemble_synth$alert_death_threshold_lower_B, c(NA_real_, NA_real_))
  expect_equal(ensemble_synth$alert_death_threshold_upper_B, c(NA_real_, NA_real_))
  expect_equal(unique(ensemble_synth$zone_sante_notification), "Ensemble de la zone affectée")
  expect_equal(unique(ensemble_synth$Province), "Ensemble")
})

test_that("Ensemble activates each health zone from its first confirmed notification", {
  hz_synth <- tibble::tibble(
    threshold_time_key = c(
      "2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"
    ),
    zone_sante_notification = c("Zone1", "Zone2", "Zone1", "Zone2"),
    Population = c(100, 200, 100, 200),
    expected_weekly_deaths_cmr = c(1, 2, 1, 2),
    death_threshold_lower_A = c(9, 18, 9, 18),
    death_threshold_upper_A = c(11, 22, 11, 22),
    alert_case_threshold_lower_C = c(2, 4, 2, 4),
    alert_case_threshold_upper_C = c(3, 5, 3, 5),
    alert_death_threshold_lower_C = c(5, 10, 5, 10),
    alert_death_threshold_upper_C = c(6, 12, 6, 12),
    threshold_valid_from = as.Date(c(
      "2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"
    )),
    threshold_valid_to = as.Date(c(
      "2026-06-07", "2026-06-07", "2026-06-14", "2026-06-14"
    )),
    recent_case_window_start = as.Date(c(
      "2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"
    )),
    recent_case_window_end = as.Date(c(
      "2026-06-07", "2026-06-07", "2026-06-14", "2026-06-14"
    )),
    recent_case_window_days = 7L,
    recent_case_anchor_date = as.Date(c(
      "2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"
    )),
    week_start = as.Date(c(
      "2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"
    ))
  )
  confirmed_dates <- tibble::tibble(
    zone_sante_notification = c("Zone1", "Zone2"),
    first_confirmed_notification_date = as.Date(c("2026-06-01", "2026-06-08"))
  )

  ensemble_synth <- build_ensemble_thresholds(hz_synth, confirmed_dates)

  expect_equal(ensemble_synth$threshold_time_key, c("2026-06-01", "2026-06-08"))
  expect_equal(ensemble_synth$Population, c(100, 300))
  expect_equal(ensemble_synth$expected_weekly_deaths_cmr, c(1, 3))
  expect_equal(ensemble_synth$death_threshold_lower_A, c(9, 27))
  expect_equal(ensemble_synth$alert_case_threshold_lower_C, c(2, 6))
  expect_equal(ensemble_synth$Alert_death_lower, c(7, 21))
})

test_that("Ensemble aggregates beta multipliers and combined detection rate", {
  hz_synth <- tibble::tibble(
    threshold_time_key = c("2026-06-01", "2026-06-08", "2026-06-08"),
    zone_sante_notification = c("Zone1", "Zone1", "Zone2"),
    Population = c(100, 100, 200),
    expected_weekly_deaths_cmr = c(1, 1, 2),
    death_threshold_lower_A = c(9, 9, 18),
    death_threshold_upper_A = c(11, 11, 22),
    alert_case_threshold_lower_C = c(2, 2, 4),
    alert_case_threshold_upper_C = c(3, 3, 5),
    alert_death_threshold_lower_C = c(5, 5, 10),
    alert_death_threshold_upper_C = c(6, 6, 12),
    threshold_valid_from = as.Date(c("2026-06-01", "2026-06-08", "2026-06-08")),
    threshold_valid_to = as.Date(c("2026-06-07", "2026-06-14", "2026-06-14")),
    recent_case_window_start = as.Date(c("2026-06-01", "2026-06-08", "2026-06-08")),
    recent_case_window_end = as.Date(c("2026-06-07", "2026-06-14", "2026-06-14")),
    recent_case_window_days = 7L,
    recent_case_anchor_date = as.Date(c("2026-06-01", "2026-06-08", "2026-06-08")),
    week_start = as.Date(c("2026-06-01", "2026-06-08", "2026-06-08"))
  )
  confirmed_dates <- tibble::tibble(
    zone_sante_notification = c("Zone1", "Zone2"),
    first_confirmed_notification_date = as.Date(c("2026-06-01", "2026-06-08"))
  )
  case_params <- tibble::tibble(
    threshold_time_key = c("2026-06-01", "2026-06-08", "2026-06-08"),
    zone_sante_notification = c("Zone1", "Zone1", "Zone2"),
    beta_c = c(2, 2, 4),
    beta_c_low = c(1, 1, 3),
    beta_c_high = c(3, 3, 5),
    beta_d = c(3, 3, 5),
    beta_d_low = c(2, 2, 4),
    beta_d_high = c(4, 4, 6),
    detection_rate_adj = c(0.4, 0.4, 0.8)
  )

  ensemble_synth <- build_ensemble_thresholds(
    hz_synth,
    confirmed_dates,
    case_derived_params = case_params
  )

  expect_equal(ensemble_synth$beta_c, c(2, 3))
  expect_equal(ensemble_synth$beta_c_low, c(1, 2))
  expect_equal(ensemble_synth$beta_c_high, c(3, 4))
  expect_equal(ensemble_synth$beta_d, c(3, 4))
  expect_equal(ensemble_synth$beta_d_low, c(2, 3))
  expect_equal(ensemble_synth$beta_d_high, c(4, 5))
  expect_equal(ensemble_synth$detection_rate_adj, c(0.4, 0.6))
})

test_that("Ensemble activation dates are validated", {
  hz_synth <- tibble::tibble(
    threshold_time_key = "2026-06-01",
    zone_sante_notification = c("Zone1", "Zone2"),
    Population = c(100, 200),
    expected_weekly_deaths_cmr = c(1, 2),
    death_threshold_lower_A = c(9, 18),
    death_threshold_upper_A = c(11, 22),
    alert_case_threshold_lower_C = c(2, 4),
    alert_case_threshold_upper_C = c(3, 5),
    alert_death_threshold_lower_C = c(5, 10),
    alert_death_threshold_upper_C = c(6, 12),
    threshold_valid_from = as.Date("2026-06-01"),
    threshold_valid_to = as.Date("2026-06-07"),
    recent_case_window_start = as.Date("2026-06-01"),
    recent_case_window_end = as.Date("2026-06-07"),
    recent_case_window_days = 7L,
    recent_case_anchor_date = as.Date("2026-06-01"),
    week_start = as.Date("2026-06-01")
  )

  missing_dates <- tibble::tibble(
    zone_sante_notification = "Zone1",
    first_confirmed_notification_date = as.Date("2026-06-01")
  )
  expect_error(
    build_ensemble_thresholds(hz_synth, missing_dates),
    "Confirmation dates are missing"
  )

  duplicate_dates <- tibble::tibble(
    zone_sante_notification = c("Zone1", "Zone1", "Zone2"),
    first_confirmed_notification_date = as.Date(c(
      "2026-06-01", "2026-06-02", "2026-06-08"
    ))
  )
  expect_error(
    build_ensemble_thresholds(hz_synth, duplicate_dates),
    "one row per health zone"
  )

  invalid_dates <- tibble::tibble(
    zone_sante_notification = c("Zone1", "Zone2"),
    first_confirmed_notification_date = as.Date(c(NA, "2026-06-08"))
  )
  expect_error(
    build_ensemble_thresholds(hz_synth, invalid_dates),
    "valid, non-missing"
  )
})

test_that("Ensemble trend and adequacy computation works end-to-end", {
  mock_trend_data <- tibble::tibble(
    threshold_time_key = c("2026-07-06", "2026-07-13", "2026-07-20"),
    recent_case_window_start = as.Date(c("2026-07-06", "2026-07-13", "2026-07-20")),
    recent_case_window_end = as.Date(c("2026-07-12", "2026-07-19", "2026-07-26")),
    zone_sante_notification = rep("Ensemble de la zone affectée", 3),
    case_lower = c(10, 15, 20),
    case_upper = c(20, 25, 30),
    death_lower = c(5, 8, 10),
    death_upper = c(15, 18, 20),
    Alert_case_threshold = c(15, 20, 25),
    Alert_death_threshold = c(10, 13, 15),
    case_alerts = c(12, 22, 28),
    death_alerts = c(8, 14, 18),
    total_alerts = c(20, 36, 46)
  )

  # Compute adequacy
  result <- mock_trend_data |>
    dplyr::mutate(
      case_threshold_mid = (case_lower + case_upper) / 2,
      death_threshold_mid = (death_lower + death_upper) / 2,
      case_adequacy = case_alerts / case_threshold_mid,
      death_adequacy = death_alerts / death_threshold_mid,
      aai = (case_adequacy + death_adequacy) / 2,
      adequacy_category = dplyr::case_when(
        aai < 0.75 ~ "Under-alerting",
        aai > 2.0 ~ "Over-alerting",
        .default = "Adequate"
      ),
      case_alerts_3w = zoo::rollmean(case_alerts, k = 3, fill = NA, align = "right"),
      Alert_case_threshold_lower = case_lower,
      Alert_case_threshold_upper = case_upper,
      Alert_death_threshold_lower = death_lower,
      Alert_death_threshold_upper = death_upper
    )

  expect_equal(result$case_adequacy[1], 12 / 15)
  expect_equal(result$death_adequacy[1], 8 / 10)
  expect_equal(result$aai[1], 0.8)
  expect_equal(result$adequacy_category[1], "Adequate")
  expect_equal(result$case_alerts_3w[3], mean(c(12, 22, 28)))

  # Verify plot functions handle the ensemble without error
  p_case <- plot_alert_trends(
    data = result,
    hz = "Ensemble de la zone affectée",
    metric = "case",
    start_date = NULL
  )
  expect_s3_class(p_case, "ggplot")

  p_death <- plot_alert_trends(
    data = result,
    hz = "Ensemble de la zone affectée",
    metric = "death",
    start_date = NULL
  )
  expect_s3_class(p_death, "ggplot")

  p_adeq <- plot_adequacy_stacked(
    data = result,
    hz = "Ensemble de la zone affectée",
    start_date = NULL
  )
  expect_s3_class(p_adeq, "ggplot")
})
