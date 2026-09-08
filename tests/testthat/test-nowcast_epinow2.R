library(testthat)

source(here::here("alert_helpers/nowcast_epinow2.R"))
source(here::here("alert_helpers/cfr_backcalc_utils.R"))

# Minimal line list builder: onset and lab dates, with a classification
# override so non-confirmed rows can be exercised.
make_evd <- function(onset, lab = onset, classification = "Cas confirmé") {
  tibble::tibble(
    classification_finale = classification,
    lab_resultat_final = rep("Positif", length(onset)),
    alert_date_debut_symptoms = as.Date(onset),
    lab_date_analyse = as.Date(lab)
  )
}

make_death_evd <- function(onset,
                           s6_date = as.Date(NA),
                           date_de = as.Date(NA),
                           notif = onset,
                           status = "Vivant") {
  tibble::tibble(
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif",
    alert_date_debut_symptoms = as.Date(onset),
    s2_date_debut_signes_symptomes = as.Date(onset),
    s6_statut_final_patient = "Vivant",
    s5_statut_patient_lors_prelev = "Vivant",
    nature_alerte = status,
    date_de_deces = as.Date(date_de),
    s6_date_deces = as.Date(s6_date),
    date_heure_notification_alerte = as.Date(notif)
  )
}

test_that("build_hz_full zero-fills, filters and caps at the reference date", {
  evd <- make_evd(
    c("2026-08-01", "2026-08-01", "2026-08-03", "2026-08-10"),
    classification = c(rep("Cas confirmé", 3), "Cas suspect")
  )

  out <- build_hz_full(
    evd,
    ref_date = as.Date("2026-08-05"),
    min_days = 3L
  )

  expect_equal(out$date, seq.Date(as.Date("2026-08-01"),
                                  as.Date("2026-08-05"),
                                  by = "day"))
  expect_equal(out$I, c(2L, 0L, 1L, 0L, 0L))
  # The suspect row and the onset after ref_date are both dropped.
  expect_equal(sum(out$I), 3L)
})

test_that("build_hz_full errors on missing columns and short series", {
  evd <- make_evd("2026-08-01")

  expect_error(
    build_hz_full(evd |> dplyr::select(-lab_resultat_final)),
    "Missing required columns"
  )

  expect_error(
    build_hz_full(evd, ref_date = as.Date("2026-08-02"), min_days = 3L),
    "at least 3 are required"
  )
})

test_that("build_reporting_delays keeps fully observable, non-negative delays", {
  evd <- make_evd(
    onset = c("2026-08-08", "2026-08-10", "2026-08-12", "2026-08-14"),
    lab = c("2026-08-12", "2026-08-15", "2026-08-12", "2026-08-16")
  )

  out <- build_reporting_delays(
    evd,
    ref_date = as.Date("2026-08-15"),
    max_delay = 5L,
    min_n = 1L
  )

  # Onset must be <= ref - max_delay = 2026-08-10 for full observability,
  # delay must be in [0, 5].
  expect_equal(out$delay, c(4L, 5L))
  expect_equal(out$onset, as.Date(c("2026-08-08", "2026-08-10")))
})

test_that("build_reporting_delays respects min_onset and min_n", {
  evd <- make_evd(
    onset = c("2026-07-01", "2026-07-20", "2026-07-25"),
    lab = c("2026-07-04", "2026-07-22", "2026-07-26")
  )

  out <- build_reporting_delays(
    evd,
    ref_date = as.Date("2026-08-15"),
    max_delay = 21L,
    min_onset = as.Date("2026-07-15"),
    min_n = 1L
  )

  expect_equal(out$onset, as.Date(c("2026-07-20", "2026-07-25")))

  expect_error(
    build_reporting_delays(evd, ref_date = as.Date("2026-08-15"),
                           max_delay = 21L, min_n = 100L),
    "at least 100 are required"
  )
})

test_that("build_death_reporting_delays uses s6_date_deces then date_de_deces then notification", {
  evd <- dplyr::bind_rows(
    make_death_evd("2026-08-08", s6_date = "2026-08-12"),
    make_death_evd("2026-08-10", date_de = "2026-08-15"),
    make_death_evd("2026-08-09", notif = "2026-08-14", status = "Décédé")
  )

  out <- build_death_reporting_delays(
    evd,
    ref_date = as.Date("2026-08-15"),
    max_delay = 5L,
    min_n = 1L
  )

  expect_setequal(out$delay, c(4L, 5L, 5L))
  expect_equal(nrow(out), 3L)
})

test_that("build_death_reporting_delays enforces min_n", {
  evd <- make_death_evd("2026-08-08", s6_date = "2026-08-12")

  expect_error(
    build_death_reporting_delays(
      evd,
      ref_date = as.Date("2026-08-15"),
      max_delay = 5L,
      min_n = 10L
    ),
    "at least 10 are required"
  )
})

test_that("summarise_reporting_delays returns expected statistics", {
  out <- summarise_reporting_delays(c(0L, 1L, 2L, 3L, 4L))

  expect_equal(out$n, 5L)
  expect_equal(out$median, 2)
  expect_equal(out$mean, 2)
  expect_equal(out$min, 0L)
  expect_equal(out$max, 4L)
})

test_that("reporting_delay_cdf builds an empirical CDF", {
  out <- reporting_delay_cdf(c(0L, 0L, 1L, 3L), max_delay = 3L)

  expect_equal(out$n, c(2L, 1L, 0L, 1L))
  expect_equal(out$prob, c(0.5, 0.25, 0, 0.25))
  expect_equal(out$cum_prob, c(0.5, 0.75, 0.75, 1))
})

test_that("tidy_nowcast joins observed counts and flags the tail", {
  fit <- list(
    summarised = tibble::tibble(
      date = as.Date(c("2026-08-10", "2026-08-11", "2026-08-12")),
      variable = "infections",
      type = "estimate",
      median = c(20, 30, 40),
      mean = c(20.5, 30.5, 40.5),
      sd = c(2, 3, 4),
      lower_90 = c(16, 24, 32),
      upper_90 = c(24, 36, 48),
      lower_50 = c(18.5, 28, 37),
      upper_50 = c(21.5, 32, 43)
    )
  )
  reported_cases <- tibble::tibble(
    date = as.Date(c("2026-08-10", "2026-08-11", "2026-08-12")),
    confirm = c(20L, 10L, 5L)
  )

  out <- tidy_nowcast(fit, reported_cases, tail_days = 2L)

  expect_equal(
    names(out),
    c(
      "date", "type",
      "nowcast_median", "nowcast_mean", "nowcast_sd",
      "nowcast_lower_90", "nowcast_upper_90",
      "nowcast_lower_50", "nowcast_upper_50",
      "observed", "tail", "correction"
    )
  )
  expect_equal(out$tail, c(FALSE, TRUE, TRUE))
  expect_equal(out$correction, c(NA_real_, 3, 8))
  expect_equal(out$nowcast_median, c(20, 30, 40))
})

test_that("tidy_nowcast errors when infections is absent", {
  fit <- list(
    summarised = tibble::tibble(
      date = as.Date("2026-08-10"),
      variable = "reported_cases",
      type = "estimate",
      median = 20
    )
  )

  expect_error(
    tidy_nowcast(fit, tibble::tibble(date = as.Date("2026-08-10"),
                                     confirm = 20L)),
    "No `infections` variable"
  )
})

test_that("extract_nowcast_samples filters and renames slim samples", {
  fit <- list(
    samples = tibble::tibble(
      .draw = c(1L, 1L, 2L),
      date = rep(as.Date("2026-08-12"), 3),
      variable = c("infections", "reported_cases", "R"),
      type = "estimate",
      value = c(30, 31, 1)
    )
  )

  out <- extract_nowcast_samples(fit)

  expect_equal(
    names(out),
    c(".draw", "date", "variable", "type", "value")
  )
  expect_equal(sort(unique(out$variable)), c("infections", "reported_cases"))
  expect_equal(nrow(out), 2L)
})

test_that("fit_nowcast_epinow2 defaults dist_samples to 2000L", {
  defaults <- formals(fit_nowcast_epinow2)
  expect_identical(defaults$dist_samples, 2000L)
})
