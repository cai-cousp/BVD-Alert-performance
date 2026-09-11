# =============================================================================
# Tests for all plot, table, and data download handlers in ShinyApp
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
    source("R/alert_plots.R")
    source("R/map_data_helpers.R")
    source("R/mod_map.R")
    source("R/mod_trends.R")
    source("R/mod_export.R")
  })
}

# -----------------------------------------------------------------------------
# 1. Trends Plot Downloads (PNG)
# -----------------------------------------------------------------------------
test_that("trends_server serves valid PNG for download_plot_alert_ensemble", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    session$setInputs(ensemble_alert_metric = "case")
    png_path <- output[["download_plot_alert_ensemble"]]

    expect_type(png_path, "character")
    expect_true(file.exists(png_path))
    expect_gt(file.size(png_path), 5000L)
    expect_match(basename(png_path), "^BVD_Tendances_.*_Ensemble_.*\\.png$")
  })
})

test_that("trends_server serves valid PNG for download_plot_adeq_ensemble", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    session$setInputs(ensemble_alert_metric = "case")
    png_path <- output[["download_plot_adeq_ensemble"]]

    expect_type(png_path, "character")
    expect_true(file.exists(png_path))
    expect_gt(file.size(png_path), 5000L)
    expect_match(basename(png_path), "^BVD_Adequation_Ensemble_.*\\.png$")
  })
})

test_that("trends_server serves valid PNG for download_plot_alert_hz", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    session$setInputs(hz_alert_metric = "case", selected_hz = "Bunia")
    png_path <- output[["download_plot_alert_hz"]]

    expect_type(png_path, "character")
    expect_true(file.exists(png_path))
    expect_gt(file.size(png_path), 5000L)
    expect_match(basename(png_path), "^BVD_Tendances_.*_Bunia_.*\\.png$")
  })
})

test_that("trends_server serves valid PNG for download_plot_adeq_hz", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    session$setInputs(hz_alert_metric = "case", selected_hz = "Bunia")
    png_path <- output[["download_plot_adeq_hz"]]

    expect_type(png_path, "character")
    expect_true(file.exists(png_path))
    expect_gt(file.size(png_path), 5000L)
    expect_match(basename(png_path), "^BVD_Adequation_Bunia_.*\\.png$")
  })
})

# -----------------------------------------------------------------------------
# 2. Trends Table Downloads (XLSX)
# -----------------------------------------------------------------------------
test_that("trends_server serves valid XLSX for download_table1_ensemble", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    xlsx_path <- output[["download_table1_ensemble"]]

    expect_type(xlsx_path, "character")
    expect_true(file.exists(xlsx_path))
    expect_gt(file.size(xlsx_path), 1000L)
    expect_match(basename(xlsx_path), "^BVD_Tableau1_Ensemble_.*\\.xlsx$")
  })
})

test_that("trends_server serves valid XLSX for download_table2_ensemble", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    xlsx_path <- output[["download_table2_ensemble"]]

    expect_type(xlsx_path, "character")
    expect_true(file.exists(xlsx_path))
    expect_gt(file.size(xlsx_path), 1000L)
    expect_match(basename(xlsx_path), "^BVD_Tableau2_Parametres_Ensemble_.*\\.xlsx$")
  })
})

test_that("trends_server serves valid XLSX for download_table1_hz and download_table2_hz", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    t1_path <- output[["download_table1_hz"]]
    t2_path <- output[["download_table2_hz"]]

    expect_type(t1_path, "character")
    expect_true(file.exists(t1_path))
    expect_gt(file.size(t1_path), 1000L)

    expect_type(t2_path, "character")
    expect_true(file.exists(t2_path))
    expect_gt(file.size(t2_path), 1000L)
  })
})

test_that("trends_server serves valid XLSX for download_map_table1 and download_map_table2", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  shiny::testServer(trends_server, args = list(filters = NULL), {
    session$setInputs(selected_hz = "Bunia")
    m1_path <- output[["download_map_table1"]]
    m2_path <- output[["download_map_table2"]]

    expect_type(m1_path, "character")
    expect_true(file.exists(m1_path))
    expect_gt(file.size(m1_path), 1000L)

    expect_type(m2_path, "character")
    expect_true(file.exists(m2_path))
    expect_gt(file.size(m2_path), 1000L)
  })
})

# -----------------------------------------------------------------------------
# 3. Export Module Data & Report Downloads
# -----------------------------------------------------------------------------
test_that("export_server serves valid XLSX for download_data and CSV for download_csv", {
  skip_if(is.na(shiny_root), message = "shiny_root could not be resolved")

  dummy_filters <- list(
    date_range = function() c(as.Date("2026-05-01"), as.Date("2026-08-31")),
    selected_hzs = function() character(0),
    alert_level = function() "all",
    adequacy_filter = function() c("adequate", "inadequate")
  )

  shiny::testServer(export_server, args = list(filters = dummy_filters), {
    data_path <- output[["download_data"]]
    csv_path  <- output[["download_csv"]]

    expect_type(data_path, "character")
    expect_true(file.exists(data_path))
    expect_gt(file.size(data_path), 1000L)
    expect_match(basename(data_path), "^BVD_Alert_Data_.*\\.xlsx$")

    expect_type(csv_path, "character")
    expect_true(file.exists(csv_path))
    expect_gt(file.size(csv_path), 500L)
    expect_match(basename(csv_path), "^BVD_Alert_Trends_.*\\.csv$")
  })
})
