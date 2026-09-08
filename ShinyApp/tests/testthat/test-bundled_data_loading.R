# =============================================================================
# Tests for BVD Alerts Dashboard — Bundled Data Loading Mode
# =============================================================================
# Verifies that the app can run 100% self-contained using ShinyApp/data/
# without requiring live output directories or external scripts (Shinylive/webR).
# =============================================================================

library(testthat)
library(dplyr)

resolve_bundled_dir <- function() {
  candidates <- c(
    normalizePath(file.path(getwd(), "data"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "ShinyApp", "data"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "data"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "..", "data"), mustWork = FALSE)
  )
  for (c in candidates) {
    if (dir.exists(c) && file.exists(file.path(c, "manifest.json"))) {
      return(c)
    }
  }
  NA_character_
}

bundled_dir <- resolve_bundled_dir()

test_that("bundled data directory and required files exist", {
  skip_if(is.na(bundled_dir), message = "Bundled data directory not found")

  expect_true(dir.exists(bundled_dir))
  expect_true(file.exists(file.path(bundled_dir, "01_thresholds_synthesis.rds")))
  expect_true(file.exists(file.path(bundled_dir, "01_intermediate_parameters.rds")))
  expect_true(file.exists(file.path(bundled_dir, "02_trends_smooth_adeq.rds")))
  expect_true(file.exists(file.path(bundled_dir, "02_recent_adequacy.xlsx")))
  expect_true(file.exists(file.path(bundled_dir, "notification_map_data.rds")))
  expect_true(file.exists(file.path(bundled_dir, "manifest.json")))
})

test_that("manifest.json contains valid metadata and payload size", {
  skip_if(is.na(bundled_dir), message = "Bundled data directory not found")

  manifest_file <- file.path(bundled_dir, "manifest.json")
  manifest <- jsonlite::fromJSON(manifest_file)

  expect_true("generated_at" %in% names(manifest))
  expect_true("source_output_dir" %in% names(manifest))
  expect_true("health_zones_count" %in% names(manifest))
  expect_true("total_payload_kb" %in% names(manifest))
  expect_gt(manifest$health_zones_count, 0)
  expect_lt(manifest$total_payload_kb, 2048) # Bundle must be under 2 MB
})

test_that("bundled data loading initializes global state cleanly", {
  skip_if(is.na(bundled_dir), message = "Bundled data directory not found")

  # Run in a clean isolated environment
  env <- new.env(parent = globalenv())
  env$app_dir <- dirname(bundled_dir)
  withr::with_options(list(bvd.use_bundled_data = TRUE), {
    sys.source(file.path(dirname(bundled_dir), "R", "global.R"), envir = env)
  })

  expect_true(exists("synthesis", envir = env))
  expect_true(exists("trends_smooth_adeq", envir = env))
  expect_true(exists("intermediate_params", envir = env))
  expect_true(exists("recent_adequacy", envir = env))
  expect_true(exists("notification_map_data", envir = env))

  expect_gt(nrow(env$synthesis), 0)
  expect_gt(nrow(env$trends_smooth_adeq), 0)
  expect_true(env$notification_map_available)
})

test_that("bundled notification map data has valid geometry and required columns", {
  skip_if(is.na(bundled_dir), message = "Bundled data directory not found")

  map_path <- file.path(bundled_dir, "notification_map_data.rds")
  map_data <- readRDS(map_path)

  expect_s3_class(map_data, "sf")
  expect_gt(nrow(map_data), 0)
  expect_equal(sf::st_crs(map_data)$epsg, 4326)

  required_cols <- c(
    "map_id", "zone_sante_notification", "total_alerts",
    "case_adequacy_recent", "death_adequacy_recent", "mean_aai_recent",
    "adequacy_category_recomputed"
  )
  for (col in required_cols) {
    expect_true(col %in% names(map_data), info = paste("Missing column:", col))
  }
  expect_false(anyDuplicated(map_data$map_id) > 0)
})

test_that("Table 1 and Table 2 generation work identically with bundled data", {
  skip_if(is.na(bundled_dir), message = "Bundled data directory not found")

  env <- new.env(parent = globalenv())
  env$app_dir <- dirname(bundled_dir)
  withr::with_options(list(bvd.use_bundled_data = TRUE), {
    sys.source(file.path(dirname(bundled_dir), "R", "global.R"), envir = env)
  })

  t1_ens <- env$get_table1_ensemble_df(env$trends_smooth_adeq)
  expect_s3_class(t1_ens, "data.frame")
  expect_gt(nrow(t1_ens), 0)

  t2_ens <- env$get_table2_ensemble_df(env$intermediate_params)
  expect_s3_class(t2_ens, "data.frame")
  expect_gt(nrow(t2_ens), 0)

  t1_hz <- env$get_table1_hz_df(env$trends_smooth_adeq, env$intermediate_params)
  expect_s3_class(t1_hz, "data.frame")
  expect_gt(nrow(t1_hz), 0)

  t2_hz <- env$get_table2_hz_df(env$intermediate_params, env$trends_smooth_adeq)
  expect_s3_class(t2_hz, "data.frame")
  expect_gt(nrow(t2_hz), 0)
})
