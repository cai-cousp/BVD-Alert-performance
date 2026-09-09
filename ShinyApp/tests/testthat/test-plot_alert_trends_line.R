library(testthat)
library(ggplot2)
library(plotly)

# Ensure environment and data are loaded
if (!exists("trends_smooth_adeq") || !exists("plot_alert_trends")) {
  candidates <- c(
    normalizePath(file.path(getwd(), "..", "..", "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "ShinyApp", "R", "global.R"), mustWork = FALSE)
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      source(candidate)
      break
    }
  }
}

test_that("plot_alert_trends preserves a continuous line group when for_plotly = TRUE", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  # When for_plotly = TRUE, geom_line must group by health zone, not per-observation
  warnings_caught <- character()
  p <- withCallingHandlers(
    plot_alert_trends(
      data = trends_smooth_adeq,
      hz = "Bunia",
      metric = "case",
      approach = "Average",
      for_plotly = TRUE
    ),
    warning = function(w) {
      warnings_caught <<- c(warnings_caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # The group fragmentation warning must not occur
  expect_false(any(grepl("Each group consists of only one observation", warnings_caught)))

  gb <- ggplot2::ggplot_build(p)

  # Find geom_line layers (one midpoint line if present, one observed alerts line)
  line_layers <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_true(length(line_layers) >= 1L)

  # The alerts trend line is the one with colour == "black"
  alerts_line_idx <- NA_integer_
  for (idx in line_layers) {
    layer_data <- gb$data[[idx]]
    if (isTRUE(all(layer_data$colour == "black"))) {
      alerts_line_idx <- idx
      break
    }
  }

  expect_false(is.na(alerts_line_idx))
  trend_data <- gb$data[[alerts_line_idx]]

  expect_gt(nrow(trend_data), 1L)
  # Must form exactly 1 continuous group for a single health zone
  expect_equal(length(unique(trend_data$group)), 1L)
})

test_that("plot_alert_trends handles multiple health zones with distinct groups", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  hzs <- c("Bunia", "Komanda")
  p <- suppressWarnings(
    plot_alert_trends(
      data = trends_smooth_adeq,
      hz = hzs,
      facet = TRUE,
      metric = "case",
      approach = "Average",
      for_plotly = TRUE
    )
  )

  gb <- ggplot2::ggplot_build(p)
  line_layers <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))

  alerts_line_idx <- NA_integer_
  for (idx in line_layers) {
    layer_data <- gb$data[[idx]]
    if (isTRUE(all(layer_data$colour == "black"))) {
      alerts_line_idx <- idx
      break
    }
  }

  expect_false(is.na(alerts_line_idx))
  trend_data <- gb$data[[alerts_line_idx]]

  # Two health zones should yield exactly two distinct groups
  expect_equal(length(unique(trend_data$group)), 2L)
  expect_gt(nrow(trend_data), 2L)
})

test_that("plot_alert_trends_interactive includes a populated line trace", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  w <- plot_alert_trends_interactive(
    data = trends_smooth_adeq,
    hz = "Bunia",
    metric = "case",
    approach = "Average"
  )

  pb <- plotly::plotly_build(w)
  traces <- pb$x$data

  # Locate trace with mode == 'lines' for observed alerts
  line_traces <- Filter(function(tr) {
    isTRUE(tr$type == "scatter") &&
      grepl("lines", tr$mode) &&
      (identical(tr$name, "Alertes de cas") || identical(tr$name, "Cas"))
  }, traces)

  expect_gt(length(line_traces), 0L)
  trend_trace <- line_traces[[1]]
  expect_gt(length(trend_trace$x), 1L)
  expect_equal(length(trend_trace$x), length(trend_trace$y))
})

test_that("build_plotly_alert_trends orders ribbons before the black trend line trace", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")

  prep <- prepare_trend_data(
    data = trends_smooth_adeq,
    hz = "Bunia",
    metric = "case",
    approach = "Average"
  )

  p <- build_plotly_alert_trends(
    data = prep,
    metric = "case",
    approach = "Average"
  )

  pb <- plotly::plotly_build(p)
  traces <- pb$x$data

  ribbon_idx <- which(vapply(traces, function(tr) {
    grepl("Bande de seuil", tr$name)
  }, logical(1)))

  line_idx <- which(vapply(traces, function(tr) {
    grepl("lines", tr$mode) && isTRUE(tr$line$color == "black")
  }, logical(1)))

  expect_gt(length(ribbon_idx), 0L)
  expect_gt(length(line_idx), 0L)
  # Ribbon must be added before the black line for SVG z-order
  expect_lt(ribbon_idx[1], line_idx[1])
})
