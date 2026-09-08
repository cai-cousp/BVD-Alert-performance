library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/window_utils.R"))
source(here::here("alert_helpers/compute_delays_by_hz.R"))

test_that("compute_delays_by_hz computes correct delays on dummy data", {
  evd <- tibble::tibble(
    zone_sante_notification = c("Zone A", "Zone A", "Zone A", "Zone B", "Zone B"),
    classification_finale = rep("Cas confirmé", 5),
    lab_resultat_final = rep("Positif", 5),
    nature_alerte = c("Vivant", "Vivant", "Décédé", "Vivant", "Décédé"),
    s6_statut_final_patient = c("Vivant", "Vivant", "Décédé", "Vivant", "Décédé"),
    alert_date_debut_symptoms = as.Date("2026-05-01") + c(0, 2, 4, 1, 3),
    s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
    date_heure_notification_alerte = as.Date("2026-05-01") + c(1, 3, 7, 6, 8),
    s6_date_deces = as.Date(c(NA, NA, "2026-05-06", NA, "2026-05-07")),
    date_de_deces = s6_date_deces
  )

  # Single overall estimate
  res <- compute_delays_by_hz(evd, min_cases_hz = 2L, min_deaths_hz = 1L)

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("zone_sante_notification", "n_cases_with_delay", "median_delay_used", "prop_notif_48h_used") %in% names(res)))
  
  # Zone A delays: 1, 1, 3 => median = 1
  res_a <- res |> dplyr::filter(zone_sante_notification == "Zone A")
  expect_equal(res_a$n_cases_with_delay, 3L)
  expect_equal(res_a$median_delay_onset_to_notif, 1)
  expect_equal(res_a$prop_notif_24h, 2/3)
  expect_equal(res_a$prop_notif_48h, 2/3)

  # Zone A death delay: notif (2026-05-08) - death (2026-05-06) = 2
  expect_equal(res_a$median_delay_death_to_notif, 2)
})

test_that("compute_delays_by_hz works across time windows", {
  evd <- tibble::tibble(
    zone_sante_notification = c(rep("Zone A", 4), rep("Zone B", 3)),
    classification_finale = rep("Cas confirmé", 7),
    lab_resultat_final = rep("Positif", 7),
    nature_alerte = rep("Vivant", 7),
    s6_statut_final_patient = rep("Vivant", 7),
    alert_date_debut_symptoms = as.Date("2026-05-01") + c(0, 2, 7, 9, 1, 8, 14),
    s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
    date_heure_notification_alerte = alert_date_debut_symptoms + 2L,
    s6_date_deces = as.Date(rep(NA, 7)),
    date_de_deces = as.Date(rep(NA, 7))
  )

  windows <- tibble::tibble(
    threshold_time_key = c("2026-05-01", "2026-05-08"),
    threshold_window_id = c("W1", "W2"),
    threshold_window_index = 1:2,
    threshold_valid_from = as.Date(c("2026-05-01", "2026-05-08")),
    threshold_valid_to = as.Date(c("2026-05-07", "2026-05-14")),
    recent_case_window_start = threshold_valid_from,
    recent_case_window_end = threshold_valid_to,
    recent_case_window_days = 7L,
    recent_case_anchor_date = threshold_valid_from
  )

  res_windowed <- compute_delays_by_hz(evd, windows = windows, lookback_days = 7L)

  expect_s3_class(res_windowed, "tbl_df")
  expect_equal(nrow(res_windowed), 4L) # 2 zones x 2 windows
  expect_true(all(c("threshold_time_key", "zone_sante_notification", "median_delay_used") %in% names(res_windowed)))
})
