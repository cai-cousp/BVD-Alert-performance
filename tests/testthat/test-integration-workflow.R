# Integration test for the full alert threshold pipeline
# Verifies that the longitudinal synthesis is correctly produced

suppressPackageStartupMessages(library(tidyverse))
source(here::here("R/alert_helpers.R"))

get_latest_output_dir <- function() {
  f <- latest_output_file(here::here("output"), "01_thresholds_synthesis[.]rds")
  if (is.na(f)) return(NULL)
  dirname(f)
}

test_that("longitudinal synthesis has expected structure", {
  latest_dir <- get_latest_output_dir()
  skip_if(is.null(latest_dir), "No dated output directory found")
  synthesis_path <- file.path(latest_dir, "01_thresholds_synthesis.rds")
  skip_if(!file.exists(synthesis_path), "Synthesis file not found - run 01_alert_thresholds.R first")

  synthesis <- readRDS(synthesis_path)

  # Should be longitudinal (more rows than HZs)
  expect_gt(nrow(synthesis), 104)

  # Should have threshold time columns
  expect_true("threshold_time_key" %in% names(synthesis))
  expect_true("week_start" %in% names(synthesis))
  expect_true("threshold_valid_from" %in% names(synthesis) ||
                "threshold_valid_from.x" %in% names(synthesis))

  # Should have Alert_ prefixed columns (capital A)
  expect_true("Alert_case_lower" %in% names(synthesis))
  expect_true("Alert_case_upper" %in% names(synthesis))
  expect_true("Alert_death_lower" %in% names(synthesis))
  expect_true("Alert_death_upper" %in% names(synthesis))

  # Should have threshold C columns
  expect_true("alert_case_threshold_C" %in% names(synthesis))
  expect_true("alert_death_threshold_C" %in% names(synthesis))

  # Should have unique HZ-window combinations
  unique_keys <- synthesis |>
    dplyr::distinct(zone_sante_notification, threshold_time_key) |>
    nrow()
  expect_equal(unique_keys, nrow(synthesis))
})

test_that("normalize_threshold_columns maps Alert_ columns correctly", {
  latest_dir <- get_latest_output_dir()
  skip_if(is.null(latest_dir), "No dated output directory found")
  synthesis_path <- file.path(latest_dir, "01_thresholds_synthesis.rds")
  skip_if(!file.exists(synthesis_path), "Synthesis file not found")

  synthesis <- readRDS(synthesis_path)
  normalized <- normalize_threshold_columns(synthesis)

  # Should have lowercase column names after normalization
  expect_true("case_lower" %in% names(normalized))
  expect_true("case_upper" %in% names(normalized))
  expect_true("death_lower" %in% names(normalized))
  expect_true("death_upper" %in% names(normalized))
})

test_that("join_thresholds_to_alert_counts works with longitudinal thresholds", {
  latest_dir <- get_latest_output_dir()
  skip_if(is.null(latest_dir), "No dated output directory found")
  synthesis_path <- file.path(latest_dir, "01_thresholds_synthesis.rds")
  skip_if(!file.exists(synthesis_path), "Synthesis file not found")

  synthesis <- readRDS(synthesis_path)

  # Create minimal alert data
  alert_data <- tibble::tibble(
    zone_sante_notification = c("Bunia", "Bunia", "Beni"),
    date_heure_notification_alerte = as.Date(c("2026-06-02", "2026-06-09", "2026-06-02")),
    nature_alerte = c("Vivant", "Vivant", "Décédé")
  )

  result <- join_thresholds_to_alert_counts(alert_data, synthesis)

  # Should have joined threshold columns
  expect_true("case_lower" %in% names(result) || "Alert_case_lower" %in% names(result))
  expect_true("case_alerts" %in% names(result))
  expect_true("death_alerts" %in% names(result))
})

test_that("intermediate_params contains new diagnostics", {
  latest_dir <- get_latest_output_dir()
  skip_if(is.null(latest_dir), "No dated output directory found")
  params_path <- file.path(latest_dir, "01_intermediate_parameters.rds")
  skip_if(!file.exists(params_path), "Intermediate params file not found")

  params <- readRDS(params_path)

  # Should have new diagnostic elements
  expect_true("recent_cases" %in% names(params))
  expect_true("multipliers" %in% names(params))
  expect_true("detection_backcalc" %in% names(params))

  # recent_cases should have window metadata
  expect_true("threshold_time_key" %in% names(params$recent_cases))
  expect_true("recent_case_window_start" %in% names(params$recent_cases))
})
