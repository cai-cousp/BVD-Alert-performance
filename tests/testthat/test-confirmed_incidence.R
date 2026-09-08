library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))

make_evd <- function(onset, zone = "Zone_A") {
  tibble::tibble(
    zone_sante_notification = rep(zone, length(onset)),
    classification_finale = rep("Cas confirmé", length(onset)),
    lab_resultat_final = rep("Positif", length(onset)),
    alert_date_debut_symptoms = as.Date(onset),
    s2_date_debut_signes_symptomes = as.Date(onset)
  )
}

make_evd_death <- function(onset, zone = "Zone_A", dead = TRUE) {
  tibble::tibble(
    zone_sante_notification = rep(zone, length(onset)),
    classification_finale = rep("Cas confirmé", length(onset)),
    lab_resultat_final = rep("Positif", length(onset)),
    alert_date_debut_symptoms = as.Date(onset),
    s2_date_debut_signes_symptomes = as.Date(onset),
    s6_statut_final_patient = if (dead) {
      rep("Décédé", length(onset))
    } else {
      rep("Vivant", length(onset))
    },
    s5_statut_patient_lors_prelev = rep("Vivant", length(onset)),
    nature_alerte = if (dead) {
      rep("Décédé", length(onset))
    } else {
      rep("Vivant", length(onset))
    },
    date_de_deces = if (dead) as.Date(NA) else as.Date(NA),
    s6_date_deces = if (dead) as.Date(onset) else as.Date(NA)
  )
}

test_that("build_confirmed_daily zero-fills through the reference date", {
  evd <- make_evd(c("2026-08-01", "2026-08-01", "2026-08-03"))

  out <- build_confirmed_daily(
    evd,
    ref_date = as.Date("2026-08-06"),
    min_days = 3L
  )

  expect_equal(
    out$date,
    seq.Date(as.Date("2026-08-01"), as.Date("2026-08-06"), by = "day")
  )
  expect_equal(out$I, c(2L, 0L, 1L, 0L, 0L, 0L))
})

test_that("build_confirmed_daily filters zone, dates and non-confirmed rows", {
  evd <- dplyr::bind_rows(
    make_evd(c("2026-08-01", "2026-08-02"), "Zone_A"),
    make_evd(c("2026-08-01", "2026-08-02"), "Zone_B"),
    make_evd("2026-08-10", "Zone_A")
  )
  evd$classification_finale[5] <- "Cas suspect"

  out <- build_confirmed_daily(
    evd,
    zone = "Zone_A",
    min_date = as.Date("2026-08-01"),
    ref_date = as.Date("2026-08-03"),
    min_days = 3L
  )

  expect_equal(out$I, c(1L, 1L, 0L))
  expect_equal(nrow(out), 3L)
})

test_that("build_confirmed_daily errors on empty series", {
  expect_error(
    build_confirmed_daily(
      make_evd("2026-08-01"),
      min_date = as.Date("2026-08-02"),
      ref_date = as.Date("2026-08-03"),
      min_days = 3L
    ),
    "No confirmed/positive cases"
  )
})

test_that("build_confirmed_death_daily counts only confirmed deaths", {
  evd <- dplyr::bind_rows(
    make_evd_death(c("2026-08-01", "2026-08-01", "2026-08-03")),
    make_evd_death(c("2026-08-02", "2026-08-04"), dead = FALSE)
  )

  out <- build_confirmed_death_daily(
    evd,
    ref_date = as.Date("2026-08-05"),
    min_days = 3L
  )

  expect_equal(out$I, c(2L, 0L, 1L, 0L, 0L))
})

test_that("build_confirmed_death_daily filters zone and future dates", {
  evd <- dplyr::bind_rows(
    make_evd_death(c("2026-08-01", "2026-08-10"), "Zone_A"),
    make_evd_death(c("2026-08-01"), "Zone_B")
  )

  out <- build_confirmed_death_daily(
    evd,
    zone = "Zone_A",
    ref_date = as.Date("2026-08-03"),
    min_days = 3L
  )

  expect_equal(out$I, c(1L, 0L, 0L))
})

test_that("build_confirmed_death_daily errors on an empty death series", {
  expect_error(
    build_confirmed_death_daily(
      make_evd_death("2026-08-01", dead = FALSE),
      ref_date = as.Date("2026-08-03"),
      min_days = 3L
    ),
    "No confirmed deaths"
  )
})

test_that("splice_nowcast_tail replaces only the tail", {
  counts <- tibble::tibble(
    date = seq.Date(as.Date("2026-08-10"), as.Date("2026-08-15"), by = "day"),
    I = c(10L, 11L, 12L, 3L, 1L, 0L)
  )
  nowcasts <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    date = as.Date(c("2026-08-13", "2026-08-14", "2026-08-15")),
    nowcast_median = c(20, 30, 40)
  )

  out <- splice_nowcast_tail(
    counts,
    nowcasts,
    zone = "Zone_A",
    ref_date = as.Date("2026-08-15"),
    max_delay = 3L
  )

  expect_equal(out$I, c(10L, 11L, 12L, 20L, 30L, 40L))
})

test_that("splice_nowcast_tail keeps observed values when absent", {
  counts <- tibble::tibble(
    date = seq.Date(as.Date("2026-08-13"), as.Date("2026-08-15"), by = "day"),
    I = c(3L, 1L, 0L)
  )
  nowcasts <- tibble::tibble(
    date = as.Date("2026-08-15"),
    nowcast_median = 40
  )

  out <- splice_nowcast_tail(
    counts,
    nowcasts,
    ref_date = as.Date("2026-08-15"),
    max_delay = 3L
  )

  expect_equal(out$I, c(3L, 1L, 40L))
})

test_that("nowcast_applies_to_data respects the nowcast horizon", {
  nowcasts <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    date = as.Date("2026-08-15"),
    observed = 1L,
    nowcast_median = 5,
    nowcast_lower_90 = 2,
    nowcast_upper_90 = 10
  )
  attr(nowcasts, "ref_date") <- as.Date("2026-08-15")

  current <- make_evd("2026-08-14")
  historical <- make_evd("2026-07-01")

  expect_true(nowcast_applies_to_data(nowcasts, current, max_delay = 21L))
  expect_false(nowcast_applies_to_data(nowcasts, historical, max_delay = 21L))
  expect_false(nowcast_applies_to_data(NULL, current, max_delay = 21L))
})
