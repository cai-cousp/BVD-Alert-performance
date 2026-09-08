# =============================================================================
# Tests for BVD Alerts Dashboard — Data loading and basic sanity checks
# =============================================================================

library(testthat)
library(dplyr)

# Resolve the output directory relative to this test file
# Tests are in ShinyApp/tests/testthat/; output is in Alerts/output/
resolve_output_base <- function() {
  # Try multiple relative paths
  candidates <- c(
    normalizePath(file.path(getwd(), "..", "..", "..", "output"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "..", "output"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "output"), mustWork = FALSE)
  )
  for (c in candidates) {
    if (dir.exists(c)) return(c)
  }
  NA_character_
}

get_latest_dir <- function(output_base) {
  if (is.na(output_base) || !dir.exists(output_base)) return(NA_character_)
  output_dirs <- list.dirs(output_base, recursive = FALSE)
  if (length(output_dirs) == 0) return(NA_character_)
  valid_dirs <- output_dirs[file.exists(file.path(output_dirs, "01_thresholds_synthesis.rds"))]
  if (length(valid_dirs) == 0) return(NA_character_)
  sort(valid_dirs, decreasing = TRUE)[1]
}

output_base <- resolve_output_base()

test_that("data loading: output directory exists and contains files", {
  skip_if(is.na(output_base), message = "Output directory not found")
  output_dirs <- list.dirs(output_base, recursive = FALSE)
  expect_gt(length(output_dirs), 0, label = "Number of output directories")
})

test_that("data loading: synthesis has expected structure", {
  latest_dir <- get_latest_dir(output_base)
  skip_if(is.na(latest_dir), message = "No output directory available")

  synthesis_path <- file.path(latest_dir, "01_thresholds_synthesis.rds")
  skip_if(!file.exists(synthesis_path), message = "Synthesis file not found")

  synthesis <- readRDS(synthesis_path) |>
    tibble::as_tibble()

  expected_cols <- c(
    "Province", "zone_sante_notification", "Population",
    "expected_weekly_deaths_cmr",
    # Approach A
    "death_threshold_lower_A", "death_threshold_upper_A",
    # Approach B
    "alert_case_threshold_lower_B", "alert_case_threshold_upper_B",
    "alert_death_threshold_lower_B", "alert_death_threshold_upper_B",
    # Approach C
    "alert_case_threshold_C", "alert_case_threshold_lower_C", "alert_case_threshold_upper_C",
    "alert_death_threshold_C", "alert_death_threshold_lower_C", "alert_death_threshold_upper_C",
    # Consensus
    "Alert_case_lower", "Alert_case_upper", "Alert_case_threshold",
    "Alert_death_lower", "Alert_death_upper", "Alert_death_threshold",
    "week_start"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(synthesis),
                info = paste("Missing column:", col))
  }

  expect_gt(nrow(synthesis), 0, label = "Synthesis rows")
  expect_true(all(synthesis$Population > 0, na.rm = TRUE),
              label = "All populations positive")
  expect_true(all(synthesis$Alert_case_threshold >= 0, na.rm = TRUE),
              label = "Case thresholds non-negative")
})

test_that("data loading: trends_smooth has expected structure", {
  latest_dir <- get_latest_dir(output_base)
  skip_if(is.na(latest_dir), message = "No output directory available")

  trends_path <- file.path(latest_dir, "02_trends_smooth.rds")
  skip_if(!file.exists(trends_path), message = "Trends file not found")

  trends <- readRDS(trends_path) |>
    tibble::as_tibble()

  expected_cols <- c(
    "week_start", "zone_sante_notification",
    "case_alerts", "death_alerts", "total_alerts",
    "case_lower", "case_upper", "death_lower", "death_upper",
    "case_threshold_mid", "death_threshold_mid",
    "aai", "case_adequacy", "death_adequacy"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(trends),
                info = paste("Missing column:", col))
  }

  expect_gt(nrow(trends), 0, label = "Trends rows")
  expect_true(all(trends$case_alerts >= 0, na.rm = TRUE),
              label = "Case alerts non-negative")
  expect_true(all(trends$death_alerts >= 0, na.rm = TRUE),
              label = "Death alerts non-negative")

  aai_vals <- trends$aai[!is.na(trends$aai)]
  if (length(aai_vals) > 0) {
    expect_true(all(aai_vals >= 0 & aai_vals <= 10),
                label = "AAI values in plausible range [0, 10]")
  }
})

test_that("data loading: recent_adequacy has expected structure", {
  latest_dir <- get_latest_dir(output_base)
  skip_if(is.na(latest_dir), message = "No output directory available")

  adequacy_path <- file.path(latest_dir, "02_recent_adequacy.xlsx")
  skip_if(!file.exists(adequacy_path), message = "Adequacy file not found")

  adequacy <- readxl::read_excel(adequacy_path) |>
    tibble::as_tibble()

  expected_cols <- c(
    "zone_sante_notification", "mean_aai",
    "adequacy_category", "trend_direction"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(adequacy),
                info = paste("Missing column:", col))
  }

  expect_gt(nrow(adequacy), 0, label = "Adequacy rows")

  valid_cats <- c("Under-alerting", "Adequate", "Over-alerting")
  expect_true(
    all(adequacy$adequacy_category %in% valid_cats, na.rm = TRUE),
    label = "All adequacy categories valid"
  )

  valid_dirs <- c("Increasing", "Decreasing", "Stable", "Unknown")
  non_na_dirs <- adequacy$trend_direction[!is.na(adequacy$trend_direction)]
  expect_true(
    all(non_na_dirs %in% valid_dirs),
    label = "All non-NA trend directions valid"
  )
})

test_that("data loading: intermediate_params has expected structure", {
  latest_dir <- get_latest_dir(output_base)
  skip_if(is.na(latest_dir), message = "No output directory available")

  params_path <- file.path(latest_dir, "01_intermediate_parameters.rds")
  skip_if(!file.exists(params_path), message = "Parameters file not found")

  params <- readRDS(params_path)
  expect_type(params, "list")

  expected_elements <- c("cfr", "detection_rates", "rt_sar", "multipliers")

  for (elem in expected_elements) {
    expect_true(elem %in% names(params),
                info = paste("Missing list element:", elem))
  }

  for (elem in expected_elements) {
    if (is.data.frame(params[[elem]])) {
      expect_true("zone_sante_notification" %in% names(params[[elem]]),
                  info = paste(elem, "missing zone_sante_notification"))
    }
  }
})
