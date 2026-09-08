library(testthat)
library(dplyr)

source(here::here("R/alert_helpers.R"))

test_that("classify_notification_adequacy applies the project AAI rules", {
  expect_equal(
    as.character(classify_notification_adequacy(c(0, 0.749999, 0.75, 1, 2, 2.0001))),
    c("Under-alerting", "Under-alerting", "Adequate", "Adequate", "Adequate", "Over-alerting")
  )
})

test_that("classify_notification_adequacy returns NA for missing values", {
  out <- classify_notification_adequacy(c(NA_real_, NaN))
  expect_true(all(is.na(out)))
})

test_that("classify_notification_adequacy returns a fixed-level factor", {
  out <- classify_notification_adequacy(1)
  expect_s3_class(out, "factor")
  expect_equal(levels(out), notification_adequacy_levels)
})

test_that("classify_notification_adequacy validates inputs", {
  expect_error(classify_notification_adequacy("1"), "numeric")
  expect_error(classify_notification_adequacy(-1), "non-negative")
  expect_error(classify_notification_adequacy(Inf), "finite")
})

test_that("notification adequacy display constants are aligned", {
  expect_equal(names(notification_adequacy_palette), notification_adequacy_levels)
  expect_equal(names(notification_adequacy_labels_fr), notification_adequacy_levels)
  expect_equal(notification_adequacy_palette[["Under-alerting"]], "#D55E00")
  expect_equal(notification_adequacy_palette[["Adequate"]], "#009E73")
  expect_equal(notification_adequacy_palette[["Over-alerting"]], "#CC79A7")
})

test_that("resolve_notification_map_inputs falls back to the latest complete directory", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "2026_09_07"))
  dir.create(file.path(root, "2026_09_06"))
  file.create(file.path(root, "2026_09_06", "02_recent_adequacy.xlsx"))
  file.create(file.path(root, "2026_09_06", "02_trends_smooth.rds"))

  out <- resolve_notification_map_inputs(output_root = root)
  expect_equal(out$output_dir, file.path(root, "2026_09_06"))
  expect_equal(basename(out$adequacy_path), "02_recent_adequacy.xlsx")
  expect_equal(basename(out$trends_path), "02_trends_smooth.rds")
})

test_that("resolve_notification_map_inputs skips incomplete directories", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "2026_09_08"))
  file.create(file.path(root, "2026_09_08", "02_recent_adequacy.xlsx"))
  dir.create(file.path(root, "2026_09_07"))
  dir.create(file.path(root, "2026_09_06"))
  file.create(file.path(root, "2026_09_06", "02_recent_adequacy.xlsx"))
  file.create(file.path(root, "2026_09_06", "02_trends_smooth.rds"))

  out <- resolve_notification_map_inputs(output_root = root)
  expect_equal(out$output_dir, file.path(root, "2026_09_06"))
})

test_that("resolve_notification_map_inputs errors without complete inputs", {
  root <- withr::local_tempdir()
  expect_error(resolve_notification_map_inputs(output_root = root), "02_alert_trends")
})

test_that("resolve_notification_map_inputs supports explicit path overrides", {
  tmp <- withr::local_tempdir()
  adequacy <- file.path(tmp, "adequacy.xlsx")
  trends <- file.path(tmp, "trends.rds")
  file.create(c(adequacy, trends))

  out <- resolve_notification_map_inputs(
    adequacy_path = adequacy,
    trends_path = trends
  )
  expect_equal(out$adequacy_path, adequacy)
  expect_equal(out$trends_path, trends)
  expect_equal(out$output_dir, tmp)
})

test_that("prepare_notification_performance_data joins province and recomputes adequacy", {
  adequacy <- tibble::tibble(
    zone_sante_notification = c("Bunia", "Mambasa", "Ensemble de la zone affectée"),
    mean_aai = c(0.5, 1.5, 1),
    adequacy_category = c("Under-alerting", "Adequate", "Adequate"),
    mean_delay_days = c(3, 1, 2)
  )
  trends <- tibble::tibble(
    zone_sante_notification = c("Bunia", "Mambasa"),
    Province = c("Ituri", "Ituri"),
    threshold_valid_to = as.Date("2026-09-06")
  )

  out <- prepare_notification_performance_data(adequacy, trends)
  expect_equal(nrow(out$data), 2L)
  expect_equal(out$data$province_notification, c("Ituri", "Ituri"))
  expect_equal(
    as.character(out$data$adequacy_category_recomputed),
    c("Under-alerting", "Adequate")
  )
  expect_equal(out$audit$n_ensemble_rows_removed, 1L)
  expect_equal(out$audit$n_category_disagreements, 0L)
})

test_that("prepare_notification_performance_data audits category disagreements", {
  adequacy <- tibble::tibble(
    zone_sante_notification = "Bunia",
    mean_aai = 3,
    adequacy_category = "Adequate"
  )
  trends <- tibble::tibble(
    zone_sante_notification = "Bunia",
    Province = "Ituri"
  )

  out <- prepare_notification_performance_data(adequacy, trends)
  expect_equal(out$audit$n_category_disagreements, 1L)
  expect_equal(nrow(out$audit$category_disagreements), 1L)
})

test_that("prepare_notification_performance_data flags ambiguous province lookups", {
  adequacy <- tibble::tibble(
    zone_sante_notification = "Lubunga",
    mean_aai = 1,
    adequacy_category = "Adequate"
  )
  trends <- tibble::tibble(
    zone_sante_notification = c("Lubunga", "Lubunga"),
    Province = c("Tshopo", "Kasaï-Central")
  )

  out <- prepare_notification_performance_data(adequacy, trends)
  expect_equal(nrow(out$audit$province_lookup_problems), 1L)
  expect_true(is.na(out$data$province_notification))
})

test_that("prepare_notification_performance_data validates required columns", {
  expect_error(
    prepare_notification_performance_data(tibble::tibble(x = 1), tibble::tibble(y = 1)),
    "zone_sante_notification"
  )
})

test_that("notification_map_reference_date prefers attributes over directory names", {
  trends_a <- structure(
    list(threshold_valid_to = as.Date("2026-09-01")),
    evd_max_date = "2026-09-06"
  )
  expect_equal(
    notification_map_reference_date(trends_a, "output/2026_09_06"),
    as.Date("2026-09-06")
  )

  trends_b <- tibble::tibble(threshold_valid_to = as.Date("2026-09-03"))
  expect_equal(
    notification_map_reference_date(trends_b, "output/2026_09_01"),
    as.Date("2026-09-03")
  )

  expect_equal(
    notification_map_reference_date(tibble::tibble(), "output/2026_09_06"),
    as.Date("2026-09-06")
  )
})
