library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/window_utils.R"))
source(here::here("alert_helpers/compute_recent_confirmed_windows_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_by_hz.R"))
source(here::here("alert_helpers/compute_detection_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/compute_growth_rate_by_hz.R"))
source(here::here("alert_helpers/compute_detection_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/combine_detection_estimates_by_hz.R"))
source(here::here("alert_helpers/compute_contacts_per_case_by_hz.R"))
source(here::here("alert_helpers/compute_alert_multipliers.R"))
source(here::here("alert_helpers/compute_rt_hz.R"))

test_that("CFR with strict 7-day lookback isolates weekly mortality without cumulative accumulation", {
  # Zone A has 5 cases & 5 deaths in window 1, then 6 cases & 0 deaths in window 2
  w1_dates <- seq.Date(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day")
  w2_dates <- seq.Date(as.Date("2026-06-08"), as.Date("2026-06-13"), by = "day")

  evd <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif",
    alert_date_debut_symptoms = c(w1_dates, w2_dates),
    s2_date_debut_signes_symptomes = alert_date_debut_symptoms,
    nature_alerte = c(rep("Décédé", length(w1_dates)), rep("Vivant", length(w2_dates))),
    s6_statut_final_patient = nature_alerte,
    alert_lien_epidemiologic = "Oui"
  )

  windows <- tibble::tibble(
    threshold_valid_from = as.Date(c("2026-06-01", "2026-06-08")),
    threshold_valid_to = as.Date(c("2026-06-07", "2026-06-14")),
    threshold_time_key = format(threshold_valid_from),
    recent_case_window_start = threshold_valid_from,
    recent_case_window_end = threshold_valid_to,
    recent_case_window_days = 7L,
    recent_case_anchor_date = threshold_valid_from,
    threshold_window_id = paste(format(threshold_valid_from), format(threshold_valid_to), sep = "__"),
    threshold_window_index = c(1L, 2L)
  )

  cfr_7d <- compute_cfr_by_hz(evd, windows = windows, lookback_days = 7L)

  expect_equal(nrow(cfr_7d), 2L)
  w1 <- cfr_7d |> dplyr::filter(threshold_time_key == "2026-06-01")
  w2 <- cfr_7d |> dplyr::filter(threshold_time_key == "2026-06-08")

  # Window 1: 5 cases, 5 deaths -> 100% CFR
  expect_equal(w1$n_conf, 5L)
  expect_equal(w1$n_deaths, 5L)
  expect_equal(w1$cfr_used, 1.0)

  # Window 2: 6 cases, 0 deaths -> 0% CFR (NOT cumulative 5/11)
  expect_equal(w2$n_conf, 6L)
  expect_equal(w2$n_deaths, 0L)
  expect_equal(w2$cfr_used, 0.0)
})

test_that("combine_detection_estimates_by_hz sets estimated_true_cases_recent to 0 when recent cases = 0", {
  # Zone A has 0 cases in window 2, while Zone B has 5 cases
  recent_cases <- tibble::tibble(
    threshold_time_key = c("2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"),
    threshold_window_id = c("w1", "w1", "w2", "w2"),
    threshold_window_index = c(1L, 1L, 2L, 2L),
    threshold_valid_from = as.Date(c("2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08")),
    threshold_valid_to = as.Date(c("2026-06-07", "2026-06-07", "2026-06-14", "2026-06-14")),
    recent_case_window_start = threshold_valid_from,
    recent_case_window_end = threshold_valid_to,
    recent_case_window_days = 7L,
    recent_case_anchor_date = threshold_valid_from,
    zone_sante_notification = c("Zone_A", "Zone_B", "Zone_A", "Zone_B"),
    n_recent_confirmed = c(10L, 5L, 0L, 5L)
  )

  hz_backcalc <- tibble::tibble(
    threshold_time_key = c("2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"),
    zone_sante_notification = c("Zone_A", "Zone_B", "Zone_A", "Zone_B"),
    n_detected = c(10L, 5L, 0L, 5L),
    n_observed_deaths = c(5L, 2L, 0L, 2L),
    cfr_used = c(0.5, 0.4, 0.5, 0.4),
    r_used = c(0.0, 0.0, 0.0, 0.0),
    detection_rate_adj = c(0.8, 0.7, 0.8, 0.7),
    under_detection_rate = 1 - detection_rate_adj,
    estimated_true_cases = c(12.5, 7.1, 0, 7.1),
    estimated_true_cases_low = estimated_true_cases * 0.8,
    estimated_true_cases_high = estimated_true_cases * 1.2,
    detection_rate_low = detection_rate_adj * 0.8,
    detection_rate_high = detection_rate_adj * 1.2,
    detection_backcalc_status = "estimated"
  )

  hz_epilink <- tibble::tibble(
    threshold_time_key = c("2026-06-01", "2026-06-01", "2026-06-08", "2026-06-08"),
    zone_sante_notification = c("Zone_A", "Zone_B", "Zone_A", "Zone_B"),
    n_conf_valid_epilink = c(10L, 5L, 0L, 5L),
    n_epilink = c(8L, 4L, 0L, 4L),
    detection_rate_adj = c(0.8, 0.8, NA_real_, 0.8),
    under_detection_rate = c(0.2, 0.2, NA_real_, 0.2)
  )

  combined <- combine_detection_estimates_by_hz(
    hz_backcalc,
    hz_epilink,
    recent_cases = recent_cases
  )

  zone_a_w2 <- combined |>
    dplyr::filter(zone_sante_notification == "Zone_A", threshold_time_key == "2026-06-08")

  # Zero detected cases in window 2 -> exactly 0 true cases
  expect_equal(zone_a_w2$estimated_true_cases_recent, 0)
})

test_that("compute_contacts_per_case_by_hz computes windowed contact ratios", {
  w1_dates <- seq.Date(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day")
  w2_dates <- seq.Date(as.Date("2026-06-08"), as.Date("2026-06-12"), by = "day")

  evd <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif",
    alert_date_debut_symptoms = c(w1_dates, w2_dates),
    s2_date_debut_signes_symptomes = alert_date_debut_symptoms
  )

  contacts <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    enrolement_date = c(
      rep(as.Date("2026-06-02"), 15), # 15 contacts in w1 (3 per case)
      rep(as.Date("2026-06-09"), 25)  # 25 contacts in w2 (5 per case)
    ),
    date_debut_suivi = enrolement_date
  )

  windows <- tibble::tibble(
    threshold_valid_from = as.Date(c("2026-06-01", "2026-06-08")),
    threshold_valid_to = as.Date(c("2026-06-07", "2026-06-14")),
    threshold_time_key = format(threshold_valid_from),
    recent_case_window_start = threshold_valid_from,
    recent_case_window_end = threshold_valid_to,
    recent_case_window_days = 7L,
    recent_case_anchor_date = threshold_valid_from,
    threshold_window_id = paste(format(threshold_valid_from), format(threshold_valid_to), sep = "__"),
    threshold_window_index = c(1L, 2L)
  )

  result <- compute_contacts_per_case_by_hz(
    contacts,
    evd,
    windows = windows,
    lookback_days = 7L
  )

  expect_equal(nrow(result), 2L)
  w1 <- result |> dplyr::filter(threshold_time_key == "2026-06-01")
  w2 <- result |> dplyr::filter(threshold_time_key == "2026-06-08")

  expect_equal(w1$contacts_per_case_used, 3.0)
  expect_equal(w2$contacts_per_case_used, 5.0)
})
