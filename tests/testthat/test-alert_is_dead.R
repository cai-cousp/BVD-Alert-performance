library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))

make_row <- function(...) {
  args <- list(
    zone_sante_notification = "Zone_A",
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif",
    s6_statut_final_patient = "Vivant",
    s5_statut_patient_lors_prelev = "Vivant",
    nature_alerte = "Vivant",
    date_de_deces = as.Date(NA),
    s6_date_deces = as.Date(NA)
  )
  args <- modifyList(args, list(...))
  tibble::as_tibble(args)
}

test_that("alert_is_dead recognises status and date death flags", {
  expect_true(alert_is_dead(make_row(s6_statut_final_patient = "Décédé")))
  expect_true(alert_is_dead(make_row(s5_statut_patient_lors_prelev = "Décédé")))
  expect_true(alert_is_dead(make_row(nature_alerte = "Décédé")))
  expect_true(alert_is_dead(make_row(s6_date_deces = as.Date("2026-08-01"))))
  expect_true(alert_is_dead(make_row(date_de_deces = as.Date("2026-08-01"))))
})

test_that("alert_is_dead requires a confirmed case and ignores notification-only rows", {
  expect_false(alert_is_dead(
    make_row(
      classification_finale = "Cas suspect",
      lab_resultat_final = NA_character_
    )
  ))
  expect_false(alert_is_dead(make_row(
    date_heure_notification_alerte = as.Date("2026-08-01")
  )))
})

test_that("alert_resolve_death_report_date follows s6 -> date_de -> notification", {
  data <- tibble::tibble(
    s6_date_deces = as.Date(c("2026-08-05", NA, NA, NA)),
    date_de_deces = as.Date(c("2026-08-04", "2026-08-04", NA, NA)),
    date_heure_notification_alerte = as.Date(c(
      "2026-08-06", "2026-08-06", "2026-08-06", NA
    ))
  )

  out <- alert_resolve_death_report_date(
    data,
    death_date_cols = c("s6_date_deces", "date_de_deces"),
    report_fallback_col = "date_heure_notification_alerte"
  )

  expect_equal(out, as.Date(c("2026-08-05", "2026-08-04", "2026-08-06", NA)))
})
