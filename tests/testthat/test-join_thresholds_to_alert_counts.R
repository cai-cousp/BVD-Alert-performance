library(testthat)

source(here::here("R/alert_helpers.R"))

test_that("join_thresholds_to_alert_counts joins longitudinal thresholds by time", {
  # Mock line list
  evd_val <- tibble::tibble(
    date_heure_notification_alerte = as.Date(c(
      "2026-07-06", # Falls in first window
      "2026-07-07", # Falls in first window
      "2026-07-13", # Falls in second window
      "2026-07-15"  # Falls in second window
    )),
    zone_sante_notification = "A",
    nature_alerte = c("Vivant", "Décédé", "Vivant", "Vivant")
  )

  thresholds <- tibble::tibble(
    threshold_time_key = c("2026-07-06", "2026-07-13"),
    recent_case_window_start = as.Date(c("2026-07-06", "2026-07-13")),
    recent_case_window_end = as.Date(c("2026-07-12", "2026-07-19")),
    zone_sante_notification = "A",
    Alert_case_lower = c(2, 4),
    Alert_case_upper = c(6, 8),
    Alert_death_lower = c(1, 1),
    Alert_death_upper = c(3, 5)
  )

  result <- join_thresholds_to_alert_counts(evd_val, thresholds)

  expect_equal(nrow(result), 2)
  expect_equal(result$case_alerts, c(1, 2))
  expect_equal(result$death_alerts, c(1, 0))
  expect_equal(result$total_alerts, c(2, 2))
  expect_equal(result$week_start, as.Date(c("2026-07-06", "2026-07-13")))
  expect_equal(result$case_lower, c(2, 4))
  expect_equal(result$case_upper, c(6, 8))
})

test_that("join_thresholds_to_alert_counts rejects duplicate time keys", {
  evd_val <- tibble::tibble(
    date_heure_notification_alerte = as.Date("2026-07-06"),
    zone_sante_notification = "A",
    nature_alerte = "Vivant"
  )

  thresholds <- tibble::tibble(
    threshold_time_key = c("2026-07-06", "2026-07-06"),
    zone_sante_notification = "A",
    case_lower = c(2, 3),
    case_upper = c(6, 7),
    death_lower = c(1, 1),
    death_upper = c(3, 3)
  )

  expect_error(
    join_thresholds_to_alert_counts(evd_val, thresholds),
    "Thresholds must be unique"
  )
})

test_that("join_thresholds_to_alert_counts aggregates alerts across all HZs for Ensemble de la zone affectée", {
  evd_val <- tibble::tibble(
    date_heure_notification_alerte = as.Date(c(
      "2026-07-06", # HZ A, Case
      "2026-07-07", # HZ A, Death
      "2026-07-08", # HZ B, Case
      "2026-07-10"  # HZ B, Death
    )),
    zone_sante_notification = c("A", "A", "B", "B"),
    nature_alerte = c("Vivant", "Décédé", "Vivant", "Décédé")
  )

  thresholds <- tibble::tibble(
    threshold_time_key = c("2026-07-06", "2026-07-06", "2026-07-06"),
    recent_case_window_start = as.Date(rep("2026-07-06", 3)),
    recent_case_window_end = as.Date(rep("2026-07-12", 3)),
    zone_sante_notification = c("A", "B", "Ensemble de la zone affectée"),
    Alert_case_lower = c(2, 3, 5),
    Alert_case_upper = c(6, 7, 13),
    Alert_death_lower = c(1, 2, 3),
    Alert_death_upper = c(3, 4, 7)
  )

  result <- join_thresholds_to_alert_counts(evd_val, thresholds)

  expect_equal(nrow(result), 3)

  # Check HZ A
  res_a <- result |> dplyr::filter(zone_sante_notification == "A")
  expect_equal(res_a$case_alerts, 1)
  expect_equal(res_a$death_alerts, 1)
  expect_equal(res_a$total_alerts, 2)

  # Check HZ B
  res_b <- result |> dplyr::filter(zone_sante_notification == "B")
  expect_equal(res_b$case_alerts, 1)
  expect_equal(res_b$death_alerts, 1)
  expect_equal(res_b$total_alerts, 2)

  # Check Ensemble
  res_ens <- result |> dplyr::filter(zone_sante_notification == "Ensemble de la zone affectée")
  expect_equal(res_ens$case_alerts, 2)
  expect_equal(res_ens$death_alerts, 2)
  expect_equal(res_ens$total_alerts, 4)
})

