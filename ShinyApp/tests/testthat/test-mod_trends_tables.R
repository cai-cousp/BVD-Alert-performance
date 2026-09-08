# =============================================================================
# Tests for Trends tab tables (Ensemble and Per-HZ helpers)
# =============================================================================

library(testthat)
library(dplyr)

# Source global.R if running tests in a standalone process
if (!exists("trends_smooth_adeq") || !exists("get_table1_ensemble_df")) {
  candidates <- c(
    normalizePath(file.path(getwd(), "..", "..", "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "R", "global.R"), mustWork = FALSE)
  )
  for (cand in candidates) {
    if (file.exists(cand)) {
      source(cand)
      break
    }
  }
}

test_that("global data objects are loaded and valid", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")
  expect_true(exists("trends_smooth_adeq"))
  expect_true(exists("intermediate_params"))
  expect_true(is.data.frame(trends_smooth_adeq))
  expect_true(is.list(intermediate_params))
})

test_that("get_table1_ensemble_df produces expected structure", {
  t1_ens <- get_table1_ensemble_df(trends_smooth_adeq)
  expect_s3_class(t1_ens, "tbl_df")
  expect_gt(nrow(t1_ens), 0)
  
  expected_cols <- c(
    "threshold_time_key",
    "case_alerts", "death_alerts", "total_alerts",
    "Alert_case_threshold", "Alert_death_threshold",
    "case_adequacy", "death_adequacy", "aai"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(t1_ens), info = paste("Missing:", col))
  }
})

test_that("get_table2_ensemble_df produces expected structure", {
  t2_ens <- get_table2_ensemble_df(intermediate_params)
  expect_s3_class(t2_ens, "tbl_df")
  expect_gt(nrow(t2_ens), 0)

  expected_cols <- c(
    "threshold_time_key",
    "beta_c", "beta_d",
    "n_recent_confirmed", "n_recent_confirmed_nowcast",
    "detection_rate_adj", "estimated_true_cases_recent"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(t2_ens), info = paste("Missing:", col))
  }
})

test_that("get_table1_hz_df produces expected structure and filters by recent cases", {
  t1_hz <- get_table1_hz_df(trends_smooth_adeq, intermediate_params)
  expect_s3_class(t1_hz, "tbl_df")
  expect_gt(nrow(t1_hz), 0)

  expected_cols <- c(
    "zone_sante_notification", "Province",
    "case_alerts", "death_alerts", "total_alerts",
    "Alert_case_threshold", "Alert_death_threshold",
    "case_adequacy", "death_adequacy", "aai"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(t1_hz), info = paste("Missing:", col))
  }

  # Ensure Ensemble row is NOT present
  expect_false("Ensemble de la zone affectée" %in% t1_hz$zone_sante_notification)
})

test_that("get_table2_hz_df produces expected structure and filters by recent cases", {
  t2_hz <- get_table2_hz_df(intermediate_params, trends_smooth_adeq)
  expect_s3_class(t2_hz, "tbl_df")
  expect_gt(nrow(t2_hz), 0)

  expected_cols <- c(
    "zone_sante_notification", "Province",
    "beta_c", "beta_d",
    "n_recent_confirmed", "n_recent_confirmed_nowcast",
    "detection_rate_adj", "estimated_true_cases_recent"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(t2_hz), info = paste("Missing:", col))
  }

  # Ensure Ensemble row is NOT present
  expect_false("Ensemble de la zone affectée" %in% t2_hz$zone_sante_notification)

  # Ensure all individual health zones have n_recent_confirmed_nowcast > 0
  expect_true(all(t2_hz$n_recent_confirmed_nowcast > 0))

  # Ensure row alignment with table 1
  t1_hz <- get_table1_hz_df(trends_smooth_adeq, intermediate_params)
  expect_equal(t1_hz$zone_sante_notification, t2_hz$zone_sante_notification)
  expect_equal(nrow(t1_hz), nrow(t2_hz))
})

test_that("all_time_windows and time_window_choices are properly structured", {
  expect_true(exists("all_time_windows"))
  expect_true(exists("time_window_choices"))
  expect_type(all_time_windows, "character")
  expect_gt(length(all_time_windows), 0)
  expect_equal(all_time_windows, sort(unique(trends_smooth_adeq$threshold_time_key)))
  expect_equal(sort(unname(time_window_choices)), all_time_windows)
  expect_true(!is.null(names(time_window_choices)))
})

test_that("get_table1_hz_df and get_table2_hz_df work with specific time_key", {
  test_window <- all_time_windows[5] # 5th week

  t1_w <- get_table1_hz_df(trends_smooth_adeq, intermediate_params, time_key = test_window)
  t2_w <- get_table2_hz_df(intermediate_params, trends_smooth_adeq, time_key = test_window)

  expect_s3_class(t1_w, "tbl_df")
  expect_s3_class(t2_w, "tbl_df")
  expect_gt(nrow(t1_w), 0)
  expect_gt(nrow(t2_w), 0)
  expect_equal(t1_w$zone_sante_notification, t2_w$zone_sante_notification)
  expect_equal(nrow(t1_w), nrow(t2_w))
})

test_that("trends_server responds correctly to health zone and table_time_window inputs", {
  # Ensure trends_server is loaded
  if (!exists("trends_server")) {
    trends_mod_cand <- file.path(app_dir, "R", "mod_trends.R")
    if (file.exists(trends_mod_cand)) source(trends_mod_cand)
  }
  if (!exists("notification_map_server")) {
    map_mod_cand <- file.path(app_dir, "R", "mod_map.R")
    if (file.exists(map_mod_cand)) source(map_mod_cand)
  }

  shiny::testServer(trends_server, {
    session$setInputs(
      selected_hz = all_hz_individual[1],
      table_time_window = max(all_time_windows)
    )

    # Check dynamic table title outputs
    expect_true(grepl("Tableau 1", output$title_table1))
    expect_true(grepl("Tableau 2", output$title_table2))

    # Switch time window
    earlier_win <- all_time_windows[3]
    session$setInputs(table_time_window = earlier_win)
    expect_true(grepl(format(as.Date(earlier_win), "%d %b %Y"), output$title_table1))
  })
})

test_that("plot_adequacy_stacked_interactive renders legend with correct metric fill labels", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  # Ensemble interactive adequacy plot with show_legend = TRUE
  p_ens <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    show_legend = TRUE,
    show_header = FALSE,
    show_caption = FALSE
  )
  expect_s3_class(p_ens, "plotly")

  built_ens <- plotly::plotly_build(p_ens)
  expect_true(isTRUE(built_ens$x$layout$showlegend))

  # Extract trace names
  trace_names <- vapply(built_ens$x$data, function(tr) tr$name %||% "", character(1))
  expect_true("Performance des alertes vivants" %in% trace_names)
  expect_true("Performance des alertes décès" %in% trace_names)
  expect_true("Performance globale" %in% trace_names)

  # Per-HZ interactive adequacy plot with show_legend = TRUE
  test_hz <- all_hz_individual[1]
  p_hz <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = test_hz,
    show_legend = TRUE,
    show_header = FALSE,
    show_caption = FALSE
  )
  expect_s3_class(p_hz, "plotly")

  built_hz <- plotly::plotly_build(p_hz)
  expect_true(isTRUE(built_hz$x$layout$showlegend))

  trace_names_hz <- vapply(built_hz$x$data, function(tr) tr$name %||% "", character(1))
  expect_true("Performance des alertes vivants" %in% trace_names_hz)
  expect_true("Performance des alertes décès" %in% trace_names_hz)
  expect_true("Performance globale" %in% trace_names_hz)
})

test_that("interactive plots have plotly controls (displayModeBar) disabled when configured", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  # 1. Alert trends interactive with display_mode_bar = FALSE
  p_trends <- plot_alert_trends_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "case",
    colour_by_adequacy = TRUE,
    show_legend = FALSE,
    show_header = FALSE,
    show_caption = FALSE,
    display_mode_bar = FALSE
  )
  expect_s3_class(p_trends, "plotly")
  expect_false(isTRUE(p_trends$x$config$displayModeBar))

  # 2. Stacked adequacy interactive with display_mode_bar = FALSE
  p_adeq <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    show_legend = TRUE,
    show_header = FALSE,
    show_caption = FALSE,
    display_mode_bar = FALSE
  )
  expect_s3_class(p_adeq, "plotly")
  expect_false(isTRUE(p_adeq$x$config$displayModeBar))

  # 3. Via plotly::config(displayModeBar = FALSE) piping as used in ShinyApp
  p_piped <- plot_alert_trends_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "case"
  ) |>
    plotly::config(displayModeBar = FALSE)
  expect_false(isTRUE(p_piped$x$config$displayModeBar))
})

test_that("interactive plots have x-axis tickmode array aligned to time windows", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  # 1. Trends interactive plot
  p_trends <- plot_alert_trends_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "case"
  )
  built_trends <- plotly::plotly_build(p_trends)
  expect_equal(built_trends$x$layout$xaxis$tickmode, "array")
  expect_equal(built_trends$x$layout$xaxis$tickangle, 0)
  expect_gt(length(built_trends$x$layout$xaxis$tickvals), 0)
  expect_equal(
    length(built_trends$x$layout$xaxis$tickvals),
    length(built_trends$x$layout$xaxis$ticktext)
  )
  expect_true(all(grepl("^\\d{2}-\\d{2}(<br\\s*/?>|\\n)\\d{4}$", built_trends$x$layout$xaxis$ticktext)))

  # 2. Adequacy stacked interactive plot
  p_adeq <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée"
  )
  built_adeq <- plotly::plotly_build(p_adeq)
  expect_equal(built_adeq$x$layout$xaxis$tickmode, "array")
  expect_equal(built_adeq$x$layout$xaxis$tickangle, 0)
  expect_gt(length(built_adeq$x$layout$xaxis$tickvals), 0)
  expect_equal(
    length(built_adeq$x$layout$xaxis$tickvals),
    length(built_adeq$x$layout$xaxis$ticktext)
  )
  expect_true(all(grepl("^\\d{2}-\\d{2}(<br\\s*/?>|\\n)\\d{4}$", built_adeq$x$layout$xaxis$ticktext)))
})

test_that("adequacy stacked interactive bars have textposition none", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  p_adeq <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée"
  )
  built_adeq <- plotly::plotly_build(p_adeq)
  bar_traces <- Filter(function(tr) identical(tr$type, "bar"), built_adeq$x$data)
  expect_gt(length(bar_traces), 0)
  for (tr in bar_traces) {
    expect_true(all(tr$textposition == "none"))
    expect_true(all(tr$hoverinfo == "text"))
  }
})

test_that("rename_df_fr properly translates column names and generates valid xlsx", {
  # Source mod_trends.R if needed
  if (!exists("rename_df_fr")) {
    source(file.path(app_dir, "R", "mod_trends.R"))
  }

  t1_ens <- get_table1_ensemble_df(trends_smooth_adeq)
  t1_renamed <- rename_df_fr(t1_ens, table1_ensemble_labels_fr)
  expect_equal(nrow(t1_renamed), nrow(t1_ens))
  expect_true("Semaine de notification (début)" %in% names(t1_renamed))
  expect_true("Performance globale (AAI)" %in% names(t1_renamed))

  t2_ens <- get_table2_ensemble_df(intermediate_params)
  t2_renamed <- rename_df_fr(t2_ens, table2_ensemble_labels_fr)
  expect_true("Semaine de notification (début)" %in% names(t2_renamed))
  expect_true("Coefficient β : cas" %in% names(t2_renamed))
  expect_true("Cas vrais récents estimés" %in% names(t2_renamed))

  # Test writing to temporary xlsx
  tmp_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(t1_renamed, path = tmp_file)
  expect_true(file.exists(tmp_file))
  expect_gt(file.size(tmp_file), 0)
  unlink(tmp_file)
})

