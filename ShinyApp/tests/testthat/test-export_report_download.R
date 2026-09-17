# =============================================================================
# Tests for Alert_performance_report.html download export in ShinyApp
# =============================================================================

library(testthat)

resolve_shiny_root <- function() {
  candidates <- c(
    normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."), mustWork = FALSE),
    normalizePath(getwd(), mustWork = FALSE)
  )
  for (cand in candidates) {
    if (file.exists(file.path(cand, "ui.R"))) {
      return(cand)
    }
  }
  NA_character_
}

shiny_root <- resolve_shiny_root()

if (!is.na(shiny_root)) {
  withr::with_dir(shiny_root, {
    if (!exists(".bvd_global_loaded") || !.bvd_global_loaded) {
      source("R/global.R")
    }
    source("R/mod_utils.R")
    source("R/mod_trends.R")
    if (!exists("notification_map_ui", mode = "function")) {
      source("R/map_data_helpers.R")
      source("R/mod_map.R")
    }
    source("R/mod_export.R")
  })
}

test_that("find_alert_report_html locates Alert_performance_report.html in docs/reports or shiny_root", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  expected_docs_file <- file.path(dirname(shiny_root), "docs", "reports", "Alert_performance_report.html")
  expected_app_file  <- file.path(shiny_root, "Alert_performance_report.html")

  skip_if(!file.exists(expected_docs_file) && !file.exists(expected_app_file),
          message = "Alert_performance_report.html not present in docs/reports or ShinyApp")

  # Act
  found_path <- find_alert_report_html(shiny_root)

  # Assert
  expect_false(is.null(found_path))
  expect_true(file.exists(found_path))
  expect_true(grepl("Alert_performance_report\\.html$", found_path))
})

test_that("find_report_template_html backwards compatibility alias works", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  found_path <- find_report_template_html(shiny_root)
  expect_false(is.null(found_path))
  expect_true(file.exists(found_path))
})

test_that("find_alert_report_html returns NULL when no template is found", {
  # Arrange: create an isolated empty directory
  temp_dir <- tempfile("test_empty_app_dir_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Act: with working directory set to empty directory
  result <- withr::with_dir(temp_dir, {
    find_alert_report_html(app_directory = temp_dir)
  })

  # Assert
  expect_null(result)
})

test_that("find_alert_methods_html locates methods_note_alert_thresholds_fr.html in docs/methods or shiny_root", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  found_path <- find_alert_methods_html(shiny_root)

  expect_false(is.null(found_path))
  expect_true(file.exists(found_path))
  expect_true(grepl("methods_note_alert_thresholds_fr\\.html$", found_path))
})

test_that("find_alert_methods_html returns NULL when no document is found", {
  temp_dir <- tempfile("test_empty_app_dir_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result <- withr::with_dir(temp_dir, {
    find_alert_methods_html(app_directory = temp_dir)
  })

  expect_null(result)
})

test_that("export_ui generates consult report and consult methods buttons with correct links and icons", {
  # Act
  ui_tags <- export_ui("export")
  ui_str <- as.character(htmltools::as.tags(ui_tags))

  # Assert
  expect_true(grepl("export-view_report", ui_str))
  expect_true(grepl("Consulter le rapport", ui_str))
  expect_true(grepl("Alert_performance_report\\.html", ui_str))

  expect_true(grepl("export-view_methods", ui_str))
  expect_true(grepl("Consulter la note m[ée]thodologique", ui_str))
  expect_true(grepl("methods_note_alert_thresholds_fr\\.html", ui_str))

  expect_true(grepl("export-open_browser_report", ui_str))
  expect_true(grepl("export-open_browser_methods", ui_str))

  # Obsolete download buttons are removed
  expect_false(grepl("export-download_report", ui_str))
  expect_false(grepl("export-download_data", ui_str))
})

test_that("export module server initialises without download_report or download_data handlers", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  dummy_filters <- list(
    date_range = function() c(as.Date("2026-01-01"), as.Date("2026-06-01")),
    selected_hzs = function() character(0),
    alert_level = function() "all",
    adequacy_filter = function() c("adequate", "inadequate")
  )

  shiny::testServer(export_server, args = list(filters = dummy_filters), {
    expect_error(output[["download_report"]], "hasn't been defined yet")
    expect_error(output[["download_data"]], "hasn't been defined yet")
  })
})

test_that("write_simple_html_report generates valid fallback HTML without missing variable errors", {
  temp_out <- tempfile(fileext = ".html")
  on.exit(unlink(temp_out), add = TRUE)

  dummy_filters <- list(
    date_range = function() c(as.Date("2026-01-01"), as.Date("2026-06-01")),
    selected_hzs = function() character(0),
    alert_level = function() "all",
    adequacy_filter = function() c("adequate", "inadequate")
  )

  dummy_trends <- tibble::tibble(
    zone_sante_notification = c("Zone A", "Zone B"),
    week_start = c(as.Date("2026-01-01"), as.Date("2026-01-08")),
    total_alerts = c(10L, 15L)
  )
  dummy_synth <- tibble::tibble(
    zone_sante_notification = c("Zone A", "Zone B"),
    adequacy_category = c("Adequate", "Under-alerting")
  )

  expect_no_error(write_simple_html_report(temp_out, dummy_trends, dummy_synth, dummy_filters))
  expect_true(file.exists(temp_out))
  content <- paste(readLines(temp_out, warn = FALSE), collapse = "\n")
  expect_true(grepl("BVD Alert Dashboard Report", content))
})
