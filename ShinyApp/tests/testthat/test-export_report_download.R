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

test_that("export_ui generates download_report button with arrow icon and correct label", {
  # Act
  ui_tags <- export_ui("export")
  ui_str <- as.character(htmltools::as.tags(ui_tags))

  # Assert
  expect_true(grepl("export-download_report", ui_str))
  expect_true(grepl("T[ée]l[ée]charger le rapport \\(HTML\\)", ui_str))
  expect_true(grepl("file-earmark-arrow-down|download", ui_str))
  expect_true(grepl("Alert_performance_report\\.html", ui_str))
})

test_that("export module downloadHandler serves Alert_performance_report.html with exact filename", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  # Arrange: Mock minimal filters
  dummy_filters <- list(
    date_range = function() c(as.Date("2026-01-01"), as.Date("2026-06-01")),
    selected_hzs = function() character(0),
    alert_level = function() "all",
    adequacy_filter = function() c("adequate", "inadequate")
  )

  # Act: run module server test with testServer
  shiny::testServer(export_server, args = list(filters = dummy_filters), {
    dl_path <- output[["download_report"]]

    # Assert: download handler executed and returned destination path
    expect_type(dl_path, "character")
    expect_equal(basename(dl_path), "Alert_performance_report.html")
    expect_true(file.exists(dl_path))
    expect_gt(file.size(dl_path), 1000L)

    # Check HTML signature
    first_line <- readLines(dl_path, n = 1, warn = FALSE)
    expect_true(grepl("<!DOCTYPE html>|<html", first_line, ignore.case = TRUE))
  })
})
