library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/nowcast_epinow2.R"))
source(here::here("alert_helpers/nowcast_by_zone.R"))
source(here::here("alert_helpers/compute_rt_hz.R"))
source(here::here("alert_helpers/compute_recent_confirmed_windows_by_hz.R"))
source(here::here("alert_helpers/compute_growth_rate_by_hz.R"))
source(here::here("alert_helpers/compute_last_window_projection.R"))
source(here::here("alert_helpers/compute_detection_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_backcalc_by_hz.R"))
source(here::here("alert_helpers/compute_cfr_by_hz.R"))
source(here::here("alert_helpers/compute_contacts_per_case_by_hz.R"))
source(here::here("alert_helpers/compute_detection_by_hz.R"))
source(here::here("alert_helpers/combine_detection_estimates_by_hz.R"))
source(here::here("alert_helpers/window_utils.R"))
source(here::here("alert_helpers/compute_alert_multipliers.R"))

make_evd <- function(zone, onsets) {
  tibble::tibble(
    zone_sante_notification = rep(zone, length(onsets)),
    classification_finale = rep("Cas confirmé", length(onsets)),
    lab_resultat_final = rep("Positif", length(onsets)),
    alert_date_debut_symptoms = as.Date(onsets),
    s2_date_debut_signes_symptomes = as.Date(onsets),
    lab_date_analyse = as.Date(onsets) + 3L,
    nature_alerte = rep("Vivant", length(onsets)),
    s6_statut_final_patient = rep("Vivant", length(onsets)),
    alert_lien_epidemiologic = rep("Oui", length(onsets))
  )
}

make_nowcasts <- function(zone, dates, observed, median) {
  tibble::tibble(
    zone_sante_notification = rep(zone, length(dates)),
    date = as.Date(dates),
    observed = as.integer(observed),
    nowcast_median = as.double(median),
    nowcast_lower_90 = as.double(median) * 0.8,
    nowcast_upper_90 = as.double(median) * 1.2,
    method = "epinow2",
    status = "fit_ok"
  )
}

test_that("compute_rt_hz splices nowcast tail before estimating Rt", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(15, 4)
  )

  rt_observed <- compute_rt_hz(evd, window_days = 14L, ref_date = ref)
  rt_nowcast <- compute_rt_hz(
    evd,
    window_days = 14L,
    ref_date = ref,
    nowcast = nowcasts
  )

  expect_true(is.finite(rt_observed))
  expect_true(is.finite(rt_nowcast))
  expect_false(identical(rt_observed, rt_nowcast))
})

test_that("compute_growth_rate_by_hz keeps nowcast windows uncopied", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(15, 4)
  )

  out <- compute_growth_rate_by_hz(
    evd,
    window_days = 7L,
    n_copy_last = 1L,
    nowcast = nowcasts
  )

  last <- out |>
    dplyr::filter(
      .data$threshold_window_index == max(.data$threshold_window_index)
    )
  expect_equal(last$growth_count_source, "nowcast")
  expect_equal(last$growth_status, "estimated")
  expect_false(is.na(last$growth_n_cases_nowcast))
  expect_gt(last$growth_n_cases_nowcast, last$growth_n_cases)

  # Without the nowcast the same window is copied from the preceding one.
  out_raw <- compute_growth_rate_by_hz(
    evd,
    window_days = 7L,
    n_copy_last = 1L
  )
  last_raw <- out_raw |>
    dplyr::filter(
      .data$threshold_window_index == max(.data$threshold_window_index)
    )
  expect_equal(last_raw$growth_status, "copied_from_preceding")
})

test_that("compute_recent_confirmed_windows_by_hz adds nowcast counts", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(15, 4)
  )

  out <- compute_recent_confirmed_windows_by_hz(
    evd,
    window_days = 7L,
    nowcast = nowcasts
  )

  latest <- out |>
    dplyr::filter(
      .data$recent_case_window_end == max(.data$recent_case_window_end)
    )
  expect_equal(latest$recent_count_source, "nowcast")
  expect_gt(latest$n_recent_confirmed_nowcast, latest$n_recent_confirmed)

  older <- out |>
    dplyr::filter(
      .data$recent_case_window_end <= ref - 21L
    )
  expect_true(all(is.na(older$n_recent_confirmed_nowcast)))
})

test_that("compute_last_window_projection prefers nowcast counts", {
  recent <- tibble::tibble(
    threshold_time_key = c("w1", "w2"),
    zone_sante_notification = "Zone_A",
    n_recent_confirmed = c(5L, 3L),
    n_recent_confirmed_nowcast = c(NA_integer_, 30L)
  )
  growth <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    r_used = 0.1,
    predicted_last_count = 20
  )

  out <- compute_last_window_projection(recent, growth)

  expect_equal(out$n_recent_confirmed, c(5L, 30L))
  expect_equal(out$projection_method, c("observed", "nowcast"))
})

test_that("cumulative helpers add nowcast-adjusted confirmed counts", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  evd$nature_alerte[25:28] <- "Décédé"
  evd$s6_statut_final_patient[25:28] <- "Décédé"
  nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(10, 4)
  )

  growth <- compute_growth_rate_by_hz(evd, n_copy_last = 0L) |>
    dplyr::filter(
      .data$threshold_window_index == max(.data$threshold_window_index)
    )

  cfr_bk <- compute_cfr_backcalc_by_hz(evd, nowcast = nowcasts)
  expect_equal(cfr_bk$n_confirmed_nowcast, 30L + 36L)

  detection_bk <- compute_detection_cfr_backcalc_by_hz(
    evd, cfr_bk, growth, nowcast = nowcasts
  )
  expect_equal(detection_bk$n_detected_nowcast, 30L + 36L)
  expect_true(is.finite(detection_bk$detection_rate_cfr_backcalc_nowcast))

  contacts <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    enrolement_date = as.Date(dates[1:5]),
    date_debut_suivi = as.Date(dates[1:5])
  )
  contacts_out <- compute_contacts_per_case_by_hz(
    contacts, evd, nowcast = nowcasts
  )
  expect_equal(contacts_out$n_confirmed_cases_nowcast, 30L + 36L)
  expect_true(is.finite(contacts_out$contacts_per_case_nowcast))
})

test_that("detection back-calc uses the confirmed-death nowcast when supplied", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  evd$nature_alerte[25:28] <- "Décédé"
  evd$s6_statut_final_patient[25:28] <- "Décédé"

  death_nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(10, 4)
  )
  attr(death_nowcasts, "ref_date") <- ref

  cfr_bk <- compute_cfr_backcalc_by_hz(evd)
  growth <- compute_growth_rate_by_hz(evd, n_copy_last = 0L) |>
    dplyr::filter(
      .data$threshold_window_index == max(.data$threshold_window_index)
    )

  detection_bk <- compute_detection_cfr_backcalc_by_hz(
    evd,
    cfr_bk,
    growth,
    nowcast_deaths = death_nowcasts
  )

  expect_equal(detection_bk$n_observed_deaths, 4L)
  expect_equal(detection_bk$n_observed_deaths_nowcast, 40L)
  expect_true(all(c(
    "n_observed_deaths_nowcast_low",
    "n_observed_deaths_nowcast_high"
  ) %in% names(detection_bk)))
  expect_equal(
    detection_bk$estimated_true_cases,
    detection_bk$n_observed_deaths_nowcast *
      detection_bk$delay_adjustment /
      detection_bk$cfr_used
  )
})

test_that("CFR helpers expose confirmed-death nowcast columns", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  evd$nature_alerte[25:28] <- "Décédé"
  evd$s6_statut_final_patient[25:28] <- "Décédé"

  death_nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(10, 4)
  )
  attr(death_nowcasts, "ref_date") <- ref

  cfr <- compute_cfr_by_hz(evd, nowcast_deaths = death_nowcasts)
  expect_equal(cfr$n_deaths, 4L)
  expect_equal(cfr$n_deaths_nowcast, 40)
  expect_equal(cfr$cfr_hz_nowcast, 40 / 30)

  cfr_bk <- compute_cfr_backcalc_by_hz(
    evd,
    nowcast_deaths = death_nowcasts
  )
  expect_equal(cfr_bk$n_deaths_eligible, 4L)
  expect_equal(cfr_bk$n_deaths_eligible_nowcast, 40)
  expect_true(all(c(
    "cfr_used_nowcast",
    "cfr_low_used_nowcast",
    "cfr_high_used_nowcast"
  ) %in% names(cfr_bk)))
})

test_that("combine_detection_estimates_by_hz uses nowcast recent counts", {
  backcalc <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    n_detected = 100L,
    n_detected_nowcast = 130L,
    n_observed_deaths = 10L,
    cfr_used = 0.5,
    r_used = 0.1,
    detection_rate_adj = 0.8,
    under_detection_rate = 0.2,
    estimated_true_cases = 125,
    estimated_true_cases_low = 120,
    estimated_true_cases_high = 130,
    detection_rate_low = 0.7,
    detection_rate_high = 0.9,
    detection_backcalc_status = "estimated"
  )
  epilink <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    n_conf_valid_epilink = 100L,
    n_epilink = 80L,
    detection_rate_adj = 0.8,
    under_detection_rate = 0.2
  )
  recent <- tibble::tibble(
    threshold_time_key = "w1",
    threshold_window_index = 1L,
    zone_sante_notification = "Zone_A",
    n_recent_confirmed = 4L,
    n_recent_confirmed_nowcast = 25
  )

  out <- combine_detection_estimates_by_hz(
    backcalc,
    epilink,
    recent_cases = recent
  )

  expect_equal(out$cumulative_confirmed_cases_nowcast, 130)
  expect_equal(out$estimated_true_cases_recent, 25 / 0.8)
})

test_that("compute_alert_multipliers_by_window preserves nowcast estimates over n_copy_last", {
  ref <- Sys.Date()
  dates <- seq.Date(ref - 29L, ref, by = "day")
  evd <- make_evd("Zone_A", dates)
  val_alerts <- tibble::tibble(
    zone_sante_notification = rep("Zone_A", length(dates)),
    date_heure_notification_alerte = dates,
    nature_alerte = c(rep("Vivant", 25), rep("Décédé", 5))
  )

  true_cases <- tibble::tibble(
    threshold_time_key = c("w1", "w2"),
    threshold_window_id = c("w1_id", "w2_id"),
    zone_sante_notification = c("Zone_A", "Zone_A"),
    estimated_true_cases_recent = c(10, 15),
    recent_case_window_start = c(ref - 13L, ref - 6L),
    recent_case_window_end = c(ref - 7L, ref)
  )

  hz_cfr <- tibble::tibble(
    threshold_time_key = c("w1", "w2"),
    zone_sante_notification = c("Zone_A", "Zone_A"),
    cfr_used = c(0.5, 0.5)
  )

  nowcasts <- make_nowcasts(
    "Zone_A",
    dates[27:30],
    observed = rep(1, 4),
    median = rep(10, 4)
  )
  attr(nowcasts, "ref_date") <- ref

  out_nowcast <- compute_alert_multipliers_by_window(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    n_copy_last = 1L,
    nowcast = nowcasts,
    ref_date = ref
  )

  out_raw <- compute_alert_multipliers_by_window(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    n_copy_last = 1L,
    nowcast = NULL
  )

  # In out_raw, w2 beta_c equals w1 beta_c due to copying
  expect_equal(out_raw$beta_c[2], out_raw$beta_c[1])

  # In out_nowcast, w2 beta_c is estimated using nowcast data and not overwritten
  expect_false(identical(out_nowcast$beta_c[2], out_nowcast$beta_c[1]))
  expect_true(is.finite(out_nowcast$beta_c[2]))
})

