library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/compute_detection_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/nowcast_epinow2.R"))
source(here::here("alert_helpers/nowcast_by_zone.R"))

test_that("negative growth rates are treated as zero in delay adjustment", {
  data <- tibble::tibble(
    zone_sante_notification = c("A", "A", "A", "A"),
    classification_finale = c(
      "Cas confirmé",
      "Cas confirmé",
      NA_character_,
      NA_character_
    ),
    lab_resultat_final = c(
      "Positif",
      "Positif",
      NA_character_,
      NA_character_
    ),
    nature_alerte = c("Vivant", "Vivant", "Décédé", "Décédé"),
    s6_statut_final_patient = c("Vivant", "Vivant", "Décédé", "Décédé")
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5,
    cfr_low_used = 0.4,
    cfr_high_used = 0.6
  )

  hz_growth <- tibble::tibble(
    zone_sante_notification = "A",
    r_used = -0.2,
    r_low_used = -0.4,
    r_high_used = -0.1
  )

  result <- compute_detection_cfr_backcalc_by_hz(
    data = data,
    hz_cfr = hz_cfr,
    hz_growth = hz_growth
  )

  expect_equal(result$delay_adjustment, 1)
  expect_equal(result$delay_adjustment_low, 1)
  expect_equal(result$delay_adjustment_high, 1)
  expect_equal(result$estimated_true_cases, 4)
  expect_equal(result$detection_backcalc_status, "estimated")
})
