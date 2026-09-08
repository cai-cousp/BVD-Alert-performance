library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/combine_detection_estimates_by_hz.R"))

test_that("combine_detection_estimates_by_hz uses recent cases for true cases", {
  hz_detection_backcalc <- tibble::tibble(
    zone_sante_notification = c("A", "B", "C"),
    n_detected = c(100, 20, 10),
    n_observed_deaths = c(10, 2, 1),
    cfr_used = c(0.5, 0.5, 0.5),
    r_used = c(0.1, 0.1, 0.1),
    detection_rate_adj = c(0.4, 0.5, NA_real_),
    under_detection_rate = c(0.6, 0.5, NA_real_),
    estimated_true_cases = c(250, 40, NA_real_),
    estimated_true_cases_low = c(200, 30, NA_real_),
    estimated_true_cases_high = c(300, 50, NA_real_),
    detection_rate_low = c(0.3, 0.4, NA_real_),
    detection_rate_high = c(0.5, 0.6, NA_real_),
    detection_backcalc_status = c("estimated", "estimated", "missing_cfr")
  )

  hz_detection_epilink <- tibble::tibble(
    zone_sante_notification = c("A", "B", "C"),
    n_conf_valid_epilink = c(80, 10, 8),
    n_epilink = c(48, 5, 0),
    detection_rate_adj = c(0.6, 0.5, NA_real_),
    under_detection_rate = c(0.4, 0.5, NA_real_)
  )

  recent_cases <- tibble::tibble(
    zone_sante_notification = c("A", "B", "C"),
    n_recent_confirmed = c(4L, 0L, 3L),
    recent_case_window_start = as.Date("2026-06-15"),
    recent_case_window_end = as.Date("2026-06-28")
  )

  result <- combine_detection_estimates_by_hz(
    hz_detection_backcalc = hz_detection_backcalc,
    hz_detection_epilink = hz_detection_epilink,
    recent_cases = recent_cases
  )

  result_a <- result |>
    dplyr::filter(zone_sante_notification == "A")
  result_b <- result |>
    dplyr::filter(zone_sante_notification == "B")
  result_c <- result |>
    dplyr::filter(zone_sante_notification == "C")

  expect_equal(result_a$n_detected, 100)
  expect_equal(result_a$n_detected_recent, 4)
  expect_equal(result_a$detection_rate_adj, 0.5)
  expect_equal(result_a$estimated_true_cases_recent, 8)
  expect_equal(result_a$estimated_true_cases_basis, "recent_detected_cases")
  expect_equal(result_a$recent_case_window_start, as.Date("2026-06-15"))

  expect_equal(result_b$estimated_true_cases_recent, 0)
  expect_true(is.na(result_c$estimated_true_cases_recent))
})

test_that("combine_detection_estimates_by_hz keeps cumulative behavior without recent cases", {
  hz_detection_backcalc <- tibble::tibble(
    zone_sante_notification = "A",
    n_detected = 10,
    n_observed_deaths = 1,
    cfr_used = 0.5,
    r_used = 0.1,
    detection_rate_adj = 0.5,
    under_detection_rate = 0.5,
    estimated_true_cases = 20,
    estimated_true_cases_low = 10,
    estimated_true_cases_high = 30,
    detection_rate_low = 0.4,
    detection_rate_high = 0.6,
    detection_backcalc_status = "estimated"
  )

  hz_detection_epilink <- tibble::tibble(
    zone_sante_notification = "A",
    n_conf_valid_epilink = 10,
    n_epilink = 5,
    detection_rate_adj = 0.5,
    under_detection_rate = 0.5
  )

  result <- combine_detection_estimates_by_hz(
    hz_detection_backcalc = hz_detection_backcalc,
    hz_detection_epilink = hz_detection_epilink
  )

  expect_true(all(c(
    "n_observed_deaths_nowcast",
    "n_observed_deaths_nowcast_low",
    "n_observed_deaths_nowcast_high"
  ) %in% names(result)))
  expect_equal(result$estimated_true_cases_recent, 20)
  expect_equal(result$estimated_true_cases_basis, "cumulative_detected_cases")
})

test_that("combine_detection_estimates_by_hz supports rolling recent windows", {
  hz_detection_backcalc <- tibble::tibble(
    zone_sante_notification = "A",
    n_detected = 100,
    n_observed_deaths = 10,
    cfr_used = 0.5,
    r_used = 0.1,
    detection_rate_adj = 0.4,
    under_detection_rate = 0.6,
    estimated_true_cases = 250,
    estimated_true_cases_low = 200,
    estimated_true_cases_high = 300,
    detection_rate_low = 0.3,
    detection_rate_high = 0.5,
    detection_backcalc_status = "estimated"
  )

  hz_detection_epilink <- tibble::tibble(
    zone_sante_notification = "A",
    n_conf_valid_epilink = 80,
    n_epilink = 48,
    detection_rate_adj = 0.6,
    under_detection_rate = 0.4
  )

  recent_cases <- tibble::tibble(
    threshold_time_key = c("2026-07-08", "2026-07-15"),
    threshold_window_id = c("2026-06-17__2026-06-30", "2026-06-24__2026-07-07"),
    zone_sante_notification = "A",
    n_recent_confirmed = c(4L, 10L),
    recent_case_window_start = as.Date(c("2026-06-17", "2026-06-24")),
    recent_case_window_end = as.Date(c("2026-06-30", "2026-07-07"))
  )

  result <- combine_detection_estimates_by_hz(
    hz_detection_backcalc = hz_detection_backcalc,
    hz_detection_epilink = hz_detection_epilink,
    recent_cases = recent_cases
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$threshold_time_key, c("2026-07-08", "2026-07-15"))
  expect_equal(result$detection_rate_adj, c(0.5, 0.5))
  expect_equal(result$estimated_true_cases_recent, c(8, 20))
  expect_equal(
    result$estimated_true_cases_basis,
    c("recent_detected_cases", "recent_detected_cases")
  )
})

test_that("combine_detection_estimates_by_hz joins all-windowed inputs by key", {
  window_meta <- tibble::tibble(
    threshold_time_key = c("w1", "w2"),
    threshold_window_id = c("win1", "win2"),
    threshold_window_index = c(1L, 2L),
    zone_sante_notification = "A"
  )

  hz_detection_backcalc <- window_meta |>
    dplyr::mutate(
      n_detected = c(100L, 110L),
      n_observed_deaths = c(10L, 11L),
      cfr_used = 0.5,
      r_used = 0.1,
      detection_rate_adj = c(0.4, 0.5),
      under_detection_rate = c(0.6, 0.5),
      estimated_true_cases = c(250, 220),
      estimated_true_cases_low = c(200, 200),
      estimated_true_cases_high = c(300, 250),
      detection_rate_low = c(0.3, 0.4),
      detection_rate_high = c(0.5, 0.6),
      detection_backcalc_status = c("estimated", "estimated")
    )

  hz_detection_epilink <- window_meta |>
    dplyr::mutate(
      n_conf_valid_epilink = c(80L, 90L),
      n_epilink = c(48L, 63L),
      detection_rate_adj = c(0.6, 0.7),
      under_detection_rate = c(0.4, 0.3)
    )

  recent_cases <- window_meta |>
    dplyr::mutate(
      n_recent_confirmed = c(4L, 10L),
      recent_case_window_start = as.Date("2026-06-15"),
      recent_case_window_end = as.Date("2026-06-28")
    )

  result <- combine_detection_estimates_by_hz(
    hz_detection_backcalc = hz_detection_backcalc,
    hz_detection_epilink = hz_detection_epilink,
    recent_cases = recent_cases
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$threshold_time_key, c("w1", "w2"))
  expect_equal(result$detection_rate_adj, c(0.5, 0.6))
  expect_equal(result$n_detected_recent, c(4L, 10L))
  expect_equal(result$estimated_true_cases_recent, c(8, 10 / 0.6))
})
