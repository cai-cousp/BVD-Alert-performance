library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/window_utils.R"))
source(here::here("alert_helpers/compute_alert_multipliers.R"))

test_that("compute_alert_multipliers supports lag_to_notification = TRUE", {
  # Case with onset on Jan 5, notified on Jan 12 (1 week later)
  evd <- tibble::tibble(
    zone_sante_notification = rep("Zone A", 4),
    date_heure_notification_alerte = as.Date("2026-01-12") + c(0, 0, 7, 7),
    alert_date_debut_symptoms = as.Date("2026-01-05") + c(0, 0, 7, 7),
    s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
    classification_finale = rep("Cas confirmé", 4),
    lab_resultat_final = rep("Positif", 4)
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = rep("Zone A", 8),
    date_heure_notification_alerte = as.Date("2026-01-12") + c(0, 0, 0, 0, 7, 7, 7, 7),
    nature_alerte = c(rep("Vivant", 6), rep("Décédé", 2))
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "Zone A",
    estimated_true_cases_recent = 10
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "Zone A",
    cfr_used = 0.5
  )

  # Model with lag_to_notification = TRUE aligns with notification dates
  res_lagged <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    lag_to_notification = TRUE,
    min_model_weeks = 2L
  )

  expect_s3_class(res_lagged, "tbl_df")
  expect_true(is.finite(res_lagged$beta_c))
  expect_true(is.finite(res_lagged$beta_d))
})
