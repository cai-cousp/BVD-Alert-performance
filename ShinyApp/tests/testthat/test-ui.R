# =============================================================================
# Tests for UI layout and Frozen Centered Header at the Top of the App
# =============================================================================

library(testthat)

# Resolve ShinyApp root directory
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

# Source ui.R and server.R from ShinyApp root
if (!is.na(shiny_root)) {
  withr::with_dir(shiny_root, {
    if (!exists("ui")) source("ui.R")
    if (!exists("server")) source("server.R")
  })
}

test_that("UI contains the frozen top header with title and subtitle", {
  skip_if(!exists("ui"), message = "ui object not loaded")

  ui_str <- as.character(htmltools::as.tags(ui))

  # Header container exists and is centered
  expect_true(grepl("app-frozen-header", ui_str))

  # Title and description text are present
  expect_true(grepl("Analyse des tendances et performance des alertes de la MVE/B", ui_str))
  expect_true(grepl("Suivi longitudinal des alertes de cas et de d[ée]c[èe]s par rapport aux seuils attendus", ui_str))
  expect_true(grepl("avec [ée]valuation de la performance", ui_str))

  # Verify header is placed before app-content-body
  header_pos <- regexpr("app-frozen-header", ui_str)[1]
  content_pos <- regexpr("app-content-body", ui_str)[1]
  expect_gt(header_pos, 0)
  expect_gt(content_pos, 0)
  expect_lt(header_pos, content_pos)

  # Export section is at the bottom of the page
  export_pos <- regexpr("download_report", ui_str)[1]
  expect_gt(export_pos, content_pos)

  # Footer contains data source and author placeholders
  expect_true(grepl("Source des donn[ée]es", ui_str))
  expect_true(grepl("data_source_info", ui_str))
  expect_true(grepl("app_author_info", ui_str))
})

test_that("alert threshold documentation appears below report generation", {
  skip_if(!exists("ui"), message = "ui object not loaded")

  ui_str <- as.character(htmltools::as.tags(ui))
  report_pos <- regexpr("download_report", ui_str)[1]
  export_details_pos <- regexpr("D[ée]tails des formats d'exportation", ui_str)[1]
  documentation_pos <- regexpr("app-documentation", ui_str)[1]
  workflow_pos <- regexpr("Workflow des seuils d'alerte", ui_str)[1]

  expect_gt(report_pos, 0)
  expect_gt(export_details_pos, 0)
  expect_gt(documentation_pos, 0)
  expect_gt(workflow_pos, 0)
  expect_lt(report_pos, documentation_pos)
  expect_lt(export_details_pos, documentation_pos)
  expect_lt(documentation_pos, workflow_pos)
  expect_true(grepl("app-math", ui_str))

  # Documentation is user-facing methodology rather than implementation detail.
  documentation_start <- documentation_pos
  documentation_end <- workflow_pos + 5000L
  documentation_text <- substr(ui_str, documentation_start, documentation_end)
  expect_false(grepl("compute_|build_|_by_hz", documentation_text))
})

test_that("trends_ui does not contain duplicate header", {
  skip_if(!exists("trends_ui"), message = "trends_ui not loaded")
  t_ui <- trends_ui("test_trends")
  t_ui_str <- as.character(htmltools::as.tags(t_ui))

  # The main title should not appear inside trends_ui since it was moved to the top header
  expect_false(grepl("app-frozen-header", t_ui_str))
})

test_that("trend cards expose case and death tabs at both levels", {
  skip_if(!exists("trends_ui"), message = "trends_ui not loaded")
  t_ui <- trends_ui("test_trends")
  t_ui_str <- as.character(htmltools::as.tags(t_ui))

  expect_true(grepl("test_trends-ensemble_alert_metric", t_ui_str))
  expect_true(grepl("test_trends-ip_case_ensemble", t_ui_str))
  expect_true(grepl("test_trends-ip_death_ensemble", t_ui_str))

  expect_true(grepl("test_trends-hz_alert_metric", t_ui_str))
  expect_true(grepl("test_trends-per_hz_case_interactive", t_ui_str))
  expect_true(grepl("test_trends-per_hz_death_interactive", t_ui_str))

  expect_true(grepl("Cas", t_ui_str))
  expect_true(grepl("Décès", t_ui_str))
})

test_that("server renders footer with DHIS2-Tracker max window date (+6 days) and author IOA-CAI", {
  skip_if(!exists("server"), message = "server not loaded")
  skip_if(!exists("all_time_windows"), message = "all_time_windows not loaded")

  testServer(server, {
    max_w <- max(as.Date(all_time_windows), na.rm = TRUE)
    dhis2_date <- max_w + 6L
    expected_ds <- paste0("DHIS2-Tracker - ", format(dhis2_date, "%d-%m-%Y"))
    expected_author <- paste0("IOA-CAI © ", format(dhis2_date, "%Y"))

    expect_equal(output$data_source_info, expected_ds)
    expect_equal(output$app_author_info, expected_author)
  })
})

test_that("custom.css defines enclosed segmented switch button styles for alert tabs", {
  css_path <- file.path(shiny_root, "www", "custom.css")
  skip_if(!file.exists(css_path), message = "custom.css not found")

  css_content <- readLines(css_path, warn = FALSE)
  css_str <- paste(css_content, collapse = "\n")

  # Switch button container (enclosed)
  expect_true(grepl("\\.alert-trend-tabs \\.nav-pills", css_str))
  expect_true(grepl("border-radius:\\s*30px", css_str))
  expect_true(grepl("border:\\s*1px solid", css_str))

  # Unselected tab in black bold
  expect_true(grepl("\\.alert-trend-tabs \\.nav-link:not\\(\\.active\\)", css_str))
  expect_true(grepl("font-weight:\\s*700\\s*!important", css_str))
  expect_true(grepl("color:\\s*#000000\\s*!important", css_str))

  # Selected tab in white text on blue background
  expect_true(grepl("\\.alert-trend-tabs \\.nav-link\\.active", css_str))
  expect_true(grepl("color:\\s*#ffffff\\s*!important", css_str))
  expect_true(grepl("background-color:\\s*#0d6efd\\s*!important", css_str))
})
