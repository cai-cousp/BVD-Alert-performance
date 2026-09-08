library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))

test_that("alert_resolve_notification_date coalesces in priority order", {
  df <- tibble::tibble(
    date_heure_notification_alerte = as.Date(c("2026-05-10", NA, NA, NA)),
    date_investigation = as.Date(c("2026-05-11", "2026-05-12", NA, NA)),
    lab_date_reception = as.Date(c("2026-05-12", "2026-05-13", "2026-05-14", NA)),
    lab_date_prelevement = as.Date(c("2026-05-09", "2026-05-10", "2026-05-11", "2026-05-15"))
  )

  res <- alert_resolve_notification_date(df)
  expect_equal(res, as.Date(c("2026-05-10", "2026-05-12", "2026-05-14", "2026-05-15")))
})

test_that("alert_resolve_notification_date returns NA when all date fields are missing", {
  df <- tibble::tibble(
    date_heure_notification_alerte = as.Date(c(NA, NA)),
    date_investigation = as.Date(c(NA, NA))
  )

  res <- alert_resolve_notification_date(df)
  expect_equal(res, as.Date(c(NA, NA)))
})
