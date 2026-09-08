library(testthat)

source(here::here("alert_helpers/compute_contacts_per_case_by_hz.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/nowcast_epinow2.R"))
source(here::here("alert_helpers/nowcast_by_zone.R"))

make_evd <- function(hz, onsets, classification = "Cas confirmé",
                     classification_col = "classification_finale") {
  out <- tibble::tibble(
    zone_sante_notification = hz,
    alert_date_debut_symptoms = as.Date(onsets),
    s2_date_debut_signes_symptomes = as.Date(onsets),
    lab_resultat_final = "Négatif"
  )
  out[[classification_col]] <- classification
  out
}

make_contacts <- function(hz, enrol, db = as.character(as.Date(enrol) + 1)) {
  tibble::tibble(
    zone_sante_notification = hz,
    enrolement_date = as.Date(enrol),
    date_debut_suivi = as.Date(db)
  )
}

test_that("HZ ratio is used when reliable; pooled ratio otherwise", {
  evd <- dplyr::bind_rows(
    make_evd("A", seq(as.Date("2026-06-01"), as.Date("2026-06-10"), by = "day")),
    make_evd("B", c("2026-06-02", "2026-06-03"))
  )
  contacts <- dplyr::bind_rows(
    make_contacts("A", rep("2026-06-02", 5)),
    make_contacts("B", c("2026-06-03", "2026-06-04", "2026-06-05", "2026-06-06"))
  )

  res <- compute_contacts_per_case_by_hz(contacts, evd)

  res_a <- res |> dplyr::filter(zone_sante_notification == "A")
  res_b <- res |> dplyr::filter(zone_sante_notification == "B")

  # Pooled = 9 followed / 12 confirmed = 0.75
  expect_equal(res_a$n_confirmed_cases, 10L)
  expect_equal(res_a$n_followed_contacts, 5L)
  expect_equal(res_a$contacts_per_case_hz, 0.5)
  expect_equal(res_a$contacts_per_case_used, 0.5)
  expect_equal(res_a$source, "hz")

  expect_equal(res_b$n_confirmed_cases, 2L)
  expect_equal(res_b$contacts_per_case_pooled, 0.75)
  expect_equal(res_b$contacts_per_case_used, 0.75)
  expect_equal(res_b$source, "pooled")
})

test_that("zero-contact HZ falls back to the pooled ratio", {
  evd <- make_evd(
    "C", seq(as.Date("2026-06-01"), as.Date("2026-06-10"), by = "day")
  )
  contacts <- make_contacts("A", rep("2026-06-02", 5))

  res <- compute_contacts_per_case_by_hz(contacts, evd)
  res_c <- res |> dplyr::filter(zone_sante_notification == "C")

  # Pooled = 5 followed / 10 confirmed = 0.5
  expect_equal(res_c$n_followed_contacts, 0L)
  expect_equal(res_c$contacts_per_case_used, 0.5)
  expect_equal(res_c$source, "pooled")
})

test_that("explicit window excludes cases and contacts outside it", {
  evd <- make_evd(
    "A",
    c("2026-05-01", "2026-06-05", "2026-06-06", "2026-06-07",
      "2026-06-08", "2026-06-09")
  )
  contacts <- dplyr::bind_rows(
    make_contacts("A", "2026-05-02"),                     # before window
    make_contacts("A", c("2026-06-06", "2026-06-07"))     # inside window
  )

  res <- compute_contacts_per_case_by_hz(
    contacts, evd,
    analysis_start_date = as.Date("2026-06-01"),
    analysis_end_date = as.Date("2026-06-30")
  )

  expect_equal(res$n_confirmed_cases, 5L)
  expect_equal(res$n_followed_contacts, 2L)
  expect_equal(res$contacts_per_case_hz, 0.4)
})

test_that("mis-parsed legacy follow-up dates are not counted as followed", {
  evd <- make_evd(
    "A", seq(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day")
  )
  contacts <- dplyr::bind_rows(
    make_contacts("A", "2026-06-02"),                     # db = enrol + 1 -> followed
    make_contacts("A", "2026-06-03", db = "2016-06-08")   # legacy parse error
  )

  res <- compute_contacts_per_case_by_hz(contacts, evd)

  expect_equal(res$n_contacts, 2L)
  expect_equal(res$n_followed_contacts, 1L)
  expect_equal(res$contacts_per_case_hz, 0.2)
})

test_that("aborts when no followed contacts are available", {
  evd <- make_evd(
    "A", seq(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day")
  )
  contacts <- make_contacts("A", "2026-06-02", db = "2016-06-08")

  expect_error(
    compute_contacts_per_case_by_hz(contacts, evd),
    "No followed contacts available"
  )
})

test_that("error_if_none = FALSE returns a zero-row tibble", {
  evd <- make_evd(
    "A", seq(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day")
  )
  contacts <- make_contacts("A", "2026-06-02", db = "2016-06-08")

  res <- compute_contacts_per_case_by_hz(contacts, evd, error_if_none = FALSE)

  expect_equal(nrow(res), 0L)
  expect_named(
    res,
    c("zone_sante_notification", "n_contacts", "n_followed_contacts",
      "n_confirmed_cases", "n_confirmed_cases_nowcast",
      "n_confirmed_cases_nowcast_low", "n_confirmed_cases_nowcast_high",
      "contacts_per_case_hz", "contacts_per_case_pooled",
      "contacts_per_case_nowcast", "contacts_per_case_used", "source")
  )
})

test_that("accepts the legacy classification_finale_cas column", {
  evd <- make_evd(
    "A", seq(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day"),
    classification_col = "classification_finale_cas"
  )
  contacts <- make_contacts("A", rep("2026-06-02", 2))

  res <- compute_contacts_per_case_by_hz(contacts, evd)

  expect_equal(res$n_confirmed_cases, 5L)
  expect_equal(res$contacts_per_case_hz, 0.4)
})

test_that("errors on missing required columns", {
  evd <- make_evd(
    "A", seq(as.Date("2026-06-01"), as.Date("2026-06-05"), by = "day")
  )
  contacts <- tibble::tibble(zone_sante_notification = "A")

  expect_error(
    compute_contacts_per_case_by_hz(contacts, evd),
    "Contact data is missing required columns"
  )
})
