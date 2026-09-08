# =============================================================================
# Tests for Alert Pipeline Synchronization and Suffixed Data Loading
# =============================================================================

library(testthat)

resolve_root_and_output <- function() {
  # Find Alerts root
  candidates <- c(
    normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "..", ".."), mustWork = FALSE),
    normalizePath(getwd(), mustWork = FALSE)
  )
  for (c in candidates) {
    if (file.exists(file.path(c, "Alerts.Rproj")) || dir.exists(file.path(c, "ShinyApp"))) {
      return(list(
        root = c,
        output = file.path(c, "output"),
        data_folder = file.path(dirname(dirname(c)), "DataCleaning", "data", "Output")
      ))
    }
  }
  NULL
}

paths <- resolve_root_and_output()

test_that("extract_evd_date_stamp extracts timestamps from various filenames", {
  if (file.exists(file.path(paths$root, "alert_helpers", "nowcast_by_zone.R"))) {
    source(file.path(paths$root, "alert_helpers", "nowcast_by_zone.R"))
  }

  expect_equal(
    extract_evd_date_stamp("evd.clean_Int_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("01_thresholds_synthesis_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("01_intermediate_parameters_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("02_trends_smooth_adeq_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("05_nowcast_by_zone_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_null(extract_evd_date_stamp("01_thresholds_synthesis.rds"))
})

test_that("get_latest_evd_info and get_latest_exported_info return valid stamps", {
  skip_if(is.null(paths), message = "Paths not resolved")
  if (file.exists(file.path(paths$root, "ShinyApp", "R", "global.R"))) {
    suppressMessages(source(file.path(paths$root, "ShinyApp", "R", "global.R")))
  }

  evd_info <- get_latest_evd_info(paths$data_folder)
  if (!is.null(evd_info$path)) {
    expect_false(is.null(evd_info$date_stamp))
    expect_true(nchar(evd_info$date_stamp) >= 8)
  }

  exp_info <- get_latest_exported_info(paths$output)
  if (!is.null(exp_info$synthesis_path)) {
    expect_false(is.null(exp_info$date_stamp))
  }
})

test_that("sync_alert_data skips when data is already up-to-date", {
  skip_if(is.null(paths), message = "Paths not resolved")
  if (file.exists(file.path(paths$root, "ShinyApp", "R", "global.R"))) {
    suppressMessages(source(file.path(paths$root, "ShinyApp", "R", "global.R")))
  }

  # Check with force_rerun = FALSE when matching
  evd_info <- get_latest_evd_info(paths$data_folder)
  exp_info <- get_latest_exported_info(paths$output)

  if (!is.null(evd_info$date_stamp) && identical(evd_info$date_stamp, exp_info$date_stamp)) {
    ran <- sync_alert_data(paths$output, paths$root, paths$data_folder, force_rerun = FALSE)
    expect_false(ran)
  }
})

test_that("load_latest_data successfully loads suffixed analysis files", {
  skip_if(is.null(paths) || !dir.exists(paths$output), message = "Output not found")
  if (file.exists(file.path(paths$root, "ShinyApp", "R", "global.R"))) {
    suppressMessages(source(file.path(paths$root, "ShinyApp", "R", "global.R")))
  }

  data <- load_latest_data(paths$output)
  expect_type(data, "list")
  expect_true(is.data.frame(data$synthesis))
  expect_true(is.data.frame(data$trends_smooth_adeq))
  expect_true(nrow(data$synthesis) > 0)
  expect_true(nrow(data$trends_smooth_adeq) > 0)
})
