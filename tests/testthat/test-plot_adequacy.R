library(testthat)

source(here::here("R/alert_helpers.R"))
source(here::here("R/alert_plots.R"))

test_that("prepare_adequacy_data validates required columns", {
  incomplete_df <- tibble::tibble(
    threshold_time_key = "2026-05-03",
    zone_sante_notification = "Bunia"
  )
  expect_error(
    prepare_adequacy_data(data = incomplete_df),
    "missing required columns"
  )
})

test_that("plot_adequacy_stacked produces valid ggplot with no NA in metric_label levels", {
  mock_data <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = c("Bunia", "Bunia"),
    case_adequacy = c(0.8, 1.2),
    death_adequacy = c(0.5, 0.9),
    aai = c(0.65, 1.05)
  )

  p <- plot_adequacy_stacked(data = mock_data, start_date = "2026-05-01")
  expect_s3_class(p, "ggplot")

  # Extract the pivoted data used in the plot
  plot_df <- ggplot2::ggplot_build(p)$plot$data
  expect_false(any(is.na(plot_df$metric_label)))
  expect_setequal(
    levels(plot_df$metric_label),
    c(
      "Performance des alertes vivants",
      "Performance des alertes décès",
      "Performance globale"
    )
  )
})

test_that("plot_adequacy_stacked returns list when one_per_hz is TRUE", {
  mock_data <- tibble::tibble(
    threshold_time_key = rep(c("2026-05-03", "2026-05-10"), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 2),
    case_adequacy = c(0.8, 1.2, 0.5, 0.7),
    death_adequacy = c(0.5, 0.9, 0.3, 0.6),
    aai = c(0.65, 1.05, 0.4, 0.65)
  )

  plots <- plot_adequacy_stacked(data = mock_data, one_per_hz = TRUE, start_date = "2026-05-01")
  expect_type(plots, "list")
  expect_named(plots, c("Bunia", "Beni"))
  expect_s3_class(plots$Bunia, "ggplot")
  expect_s3_class(plots$Beni, "ggplot")
})

test_that("plot_adequacy_footnote removes the source line from the caption", {
  mock_data <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = "Bunia",
    case_adequacy = c(0.8, 1.2),
    death_adequacy = c(0.5, 0.9),
    aai = c(0.65, 1.05)
  )

  p <- plot_adequacy_stacked(
    data = mock_data,
    hz = "Bunia",
    start_date = "2026-05-01",
    source_date = "2026-05-10"
  )
  footnote <- plot_adequacy_footnote(p)

  expect_false(grepl("^Source\\s*:", footnote))
  expect_match(footnote, "Performance of live alerts")
  expect_match(footnote, "Health zone: Bunia")
})

test_that("plot_adequacy_stacked defaults to scales = 'fixed' and respects custom scales", {
  mock_data <- tibble::tibble(
    threshold_time_key = rep(c("2026-05-03", "2026-05-10"), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 2),
    case_adequacy = c(0.8, 1.2, 0.5, 0.7),
    death_adequacy = c(0.5, 0.9, 0.3, 0.6),
    aai = c(0.65, 1.05, 0.4, 0.65)
  )

  # Default: scales = 'fixed' (free$y is FALSE)
  p_default <- plot_adequacy_stacked(data = mock_data, start_date = "2026-05-01")
  expect_false(p_default$facet$params$free$y)

  # Custom: scales = 'free_y' (free$y is TRUE)
  p_free <- plot_adequacy_stacked(data = mock_data, scales = "free_y", start_date = "2026-05-01")
  expect_true(p_free$facet$params$free$y)
})

test_that("plot_adequacy_stacked uses numeric date breaks as weeks", {
  mock_data <- tibble::tibble(
    threshold_time_key = seq(as.Date("2026-05-03"), by = "7 days", length.out = 10),
    zone_sante_notification = "Bunia",
    case_adequacy = 0.8,
    death_adequacy = 0.5,
    aai = 0.65
  )

  p <- plot_adequacy_stacked(
    data = mock_data,
    start_date = NULL,
    date_breaks = 3
  )
  breaks <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks
  date_breaks <- as.Date(stats::na.omit(breaks), origin = "1970-01-01")

  expect_true(length(date_breaks) > 1L)
  expect_true(all(as.numeric(diff(date_breaks)) == 21L))
})

test_that("plot_adequacy_stacked validates date breaks and propagates them per HZ", {
  mock_data <- tibble::tibble(
    threshold_time_key = rep(seq(as.Date("2026-05-03"), by = "7 days", length.out = 10), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 10),
    case_adequacy = 0.8,
    death_adequacy = 0.5,
    aai = 0.65
  )

  expect_error(
    plot_adequacy_stacked(
      data = mock_data,
      start_date = NULL,
      date_breaks = c("3 weeks", "4 weeks")
    ),
    "single character string"
  )
  expect_error(
    plot_adequacy_stacked(data = mock_data, start_date = NULL, date_breaks = 1.5),
    "single positive whole number"
  )

  plots <- plot_adequacy_stacked(
    data = mock_data,
    one_per_hz = TRUE,
    start_date = NULL,
    date_breaks = 3
  )
  breaks <- ggplot2::ggplot_build(plots$Bunia)$layout$panel_params[[1]]$x$breaks
  date_breaks <- as.Date(stats::na.omit(breaks), origin = "1970-01-01")

  expect_true(length(date_breaks) > 1L)
  expect_true(all(as.numeric(diff(date_breaks)) == 21L))
})

test_that("plot_alert_trends defaults to scales = 'fixed' and respects custom scales", {
  mock_trend_data <- tibble::tibble(
    threshold_time_key = rep(c("2026-05-03", "2026-05-10"), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 2),
    case_alerts = c(10, 15, 2, 4),
    Alert_case_threshold_lower = c(5, 5, 1, 1),
    Alert_case_threshold_upper = c(20, 20, 10, 10),
    Alert_case_threshold = c(12, 12, 5, 5),
    case_alerts_3w = c(10, 12.5, 2, 3),
    adequacy_category = rep("Adequate", 4)
  )

  # Default: scales = 'fixed' (free$y is FALSE)
  p_default <- plot_alert_trends(data = mock_trend_data, start_date = "2026-05-01")
  expect_false(p_default$facet$params$free$y)

  # Custom: scales = 'free_y' (free$y is TRUE)
  p_free <- plot_alert_trends(data = mock_trend_data, scales = "free_y", start_date = "2026-05-01")
  expect_true(p_free$facet$params$free$y)
})

test_that("plot_alert_trends supports metric = 'death'", {
  mock_death_data <- tibble::tibble(
    threshold_time_key = rep(c("2026-05-03", "2026-05-10"), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 2),
    death_alerts = c(3, 5, 0, 2),
    Alert_death_threshold_lower = c(2, 2, 1, 1),
    Alert_death_threshold_upper = c(8, 8, 4, 4),
    Alert_death_threshold = c(5, 5, 2.5, 2.5),
    death_alerts_3w = c(3, 4, 0, 1),
    adequacy_category = rep("Adequate", 4)
  )

  # Overview plot (faceted)
  p_death <- plot_alert_trends(
    data = mock_death_data,
    metric = "death",
    show_rolling = TRUE,
    colour_by_adequacy = TRUE,
    start_date = "2026-05-01"
  )
  expect_s3_class(p_death, "ggplot")
  expect_match(p_death$labels$title, "Tendances des alertes de décès")
  expect_match(p_death$labels$y, "Alertes de décès \\(nombre\\)")

  # Single HZ plot
  p_single <- plot_alert_trends(
    data = mock_death_data,
    hz = "Bunia",
    metric = "death",
    start_date = "2026-05-01"
  )
  expect_s3_class(p_single, "ggplot")
  expect_match(p_single$labels$subtitle, "Zone de santé : Bunia")

  # One per HZ list
  p_list <- plot_alert_trends(
    data = mock_death_data,
    metric = "death",
    one_per_hz = TRUE,
    start_date = "2026-05-01"
  )
  expect_type(p_list, "list")
  expect_named(p_list, c("Bunia", "Beni"))
  expect_s3_class(p_list$Bunia, "ggplot")
  expect_s3_class(p_list$Beni, "ggplot")
})

test_that("plot_alert_trends validates missing death columns", {
  missing_death_df <- tibble::tibble(
    threshold_time_key = "2026-05-03",
    zone_sante_notification = "Bunia",
    death_alerts = 2
    # Missing Alert_death_threshold_lower and Alert_death_threshold_upper
  )
  expect_error(
    plot_alert_trends(data = missing_death_df, metric = "death"),
    "missing required columns"
  )
})

test_that("plot_alert_trends_interactive supports metric = 'death'", {
  skip_if_not_installed("plotly")

  mock_death_data <- tibble::tibble(
    threshold_time_key = rep(c("2026-05-03", "2026-05-10"), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 2),
    death_alerts = c(3, 5, 0, 2),
    Alert_death_threshold_lower = c(2, 2, 1, 1),
    Alert_death_threshold_upper = c(8, 8, 4, 4),
    Alert_death_threshold = c(5, 5, 2.5, 2.5),
    death_alerts_3w = c(3, 4, 0, 1),
    adequacy_category = rep("Adequate", 4)
  )

  ip_death <- plot_alert_trends_interactive(
    data = mock_death_data,
    metric = "death",
    start_date = "2026-05-01"
  )
  expect_true(inherits(ip_death, "plotly") || inherits(ip_death, "htmlwidget"))

  # Interactive per-HZ list
  ip_list <- plot_alert_trends_interactive(
    data = mock_death_data,
    metric = "death",
    one_per_hz = TRUE,
    start_date = "2026-05-01"
  )
  expect_type(ip_list, "list")
  expect_named(ip_list, c("Bunia", "Beni"))
})

test_that("interactive trend plots show the threshold midpoint", {
  skip_if_not_installed("plotly")

  mock_case_data <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = "Bunia",
    case_alerts = c(4, 7),
    Alert_case_threshold_lower = c(2, 2),
    Alert_case_threshold_upper = c(8, 8),
    Alert_case_threshold = c(5, 6)
  )
  prepared <- prepare_trend_data(
    data = mock_case_data,
    metric = "case",
    start_date = "2026-05-01"
  )

  widget <- build_plotly_alert_trends(
    data = prepared,
    metric = "case",
    has_adequacy = FALSE
  )
  widget <- plotly::plotly_build(widget)

  trace_names <- vapply(
    widget$x$data,
    function(trace) if (is.null(trace$name)) "" else trace$name,
    character(1)
  )
  midpoint_trace <- widget$x$data[[which(trace_names == "Seuil médian")[1L]]]

  expect_equal(as.numeric(midpoint_trace$y), c(5, 6))
  expect_match(
    paste(widget$x$data[[length(widget$x$data)]]$text, collapse = "\n"),
    "Seuil médian"
  )

  interactive_widget <- plotly::plotly_build(
    plot_alert_trends_interactive(
      data = prepared,
      metric = "case",
      start_date = "2026-05-01"
    )
  )
  interactive_names <- vapply(
    interactive_widget$x$data,
    function(trace) if (is.null(trace$name)) "" else trace$name,
    character(1)
  )
  expect_true(any(interactive_names == "Seuil médian"))
})

test_that("interactive alert trend legend sits below the title and subtitle", {
  skip_if_not_installed("plotly")

  make_trend_data <- function(z) {
    tibble::tibble(
      threshold_time_key = c("2026-05-03", "2026-05-10"),
      zone_sante_notification = z,
      case_alerts = c(4, 7),
      Alert_case_threshold_lower = c(2, 2),
      Alert_case_threshold_upper = c(8, 8),
      Alert_case_threshold = c(5, 6),
      adequacy_category = "Adequate"
    )
  }

  single_widget <- plotly::plotly_build(
    plot_alert_trends_interactive(
      data = make_trend_data("Bunia"),
      metric = "case",
      start_date = "2026-05-01",
      colour_by_adequacy = TRUE
    )
  )

  find_annotation <- function(widget, pattern) {
    Filter(
      function(annotation) grepl(pattern, annotation$text),
      widget$x$layout$annotations
    )[[1L]]
  }

  expect_equal(single_widget$x$layout$legend$orientation, "h")
  expect_equal(single_widget$x$layout$legend$y, 1)
  expect_equal(single_widget$x$layout$legend$yanchor, "bottom")

  title <- find_annotation(single_widget, "^Tendances des ")
  subtitle <- find_annotation(single_widget, "observées \\(ligne/points\\)")
  zone_subtitle <- find_annotation(single_widget, "Zone de santé")
  caption <- find_annotation(single_widget, "^Source\\s*:")

  expect_gt(title$y, subtitle$y)
  expect_gt(subtitle$y, zone_subtitle$y)
  expect_gt(zone_subtitle$y, single_widget$x$layout$legend$y)
  expect_gt(single_widget$x$layout$legend$y, caption$y)

  faceted_widget <- plotly::plotly_build(
    plot_alert_trends_interactive(
      data = dplyr::bind_rows(
        make_trend_data("Bunia"),
        make_trend_data("Beni")
      ),
      metric = "case",
      start_date = "2026-05-01",
      colour_by_adequacy = TRUE
    )
  )

  faceted_title <- find_annotation(faceted_widget, "^Tendances des ")
  faceted_subtitle <- find_annotation(faceted_widget, "observées \\(ligne/points\\)")
  faceted_approach <- find_annotation(faceted_widget, "^Approche\\s*:")
  faceted_caption <- find_annotation(faceted_widget, "^Source\\s*:")
  panel_labels <- Filter(
    function(annotation) annotation$text %in% c("Bunia", "Beni"),
    faceted_widget$x$layout$annotations
  )

  expect_gt(faceted_title$y, faceted_subtitle$y)
  expect_gt(faceted_subtitle$y, faceted_approach$y)
  expect_gt(faceted_approach$y, faceted_widget$x$layout$legend$y)
  expect_true(all(vapply(
    panel_labels,
    function(annotation) annotation$y < faceted_widget$x$layout$legend$y,
    logical(1)
  )))
  expect_gt(faceted_widget$x$layout$legend$y, faceted_caption$y)
  expect_false(grepl("<sup>", faceted_title$text, fixed = TRUE))

  legend_names <- vapply(
    faceted_widget$x$data,
    function(trace) {
      if (isTRUE(trace$showlegend)) trace$name else NA_character_
    },
    character(1)
  )
  expect_equal(sum(legend_names == "Alertes de cas", na.rm = TRUE), 1)
})

test_that("ggplotly alert headers use the same annotation bands as native plots", {
  skip_if_not_installed("plotly")

  widget <- plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers") |>
    plotly::layout(
      title = list(text = "Tendances des alertes de cas vs seuils attendus"),
      annotations = list(
        list(
          text = "Alertes de cas observées (ligne/points) vs. seuils attendus (ruban)",
          xref = "paper",
          yref = "paper"
        ),
        list(
          text = "Source : DHIS2 Tracker - 16-08-2026",
          xref = "paper",
          yref = "paper"
        )
      )
    )

  positioned <- plotly::plotly_build(position_alert_plotly_header(widget))

  find_annotation <- function(pattern) {
    Filter(
      function(annotation) grepl(pattern, annotation$text),
      positioned$x$layout$annotations
    )[[1L]]
  }

  title <- find_annotation("^Tendances des ")
  subtitle <- find_annotation("observées \\(ligne/points\\)")
  caption <- find_annotation("^Source\\s*:")

  expect_true(
    is.null(positioned$x$layout$title) ||
      identical(positioned$x$layout$title$text, "")
  )
  expect_gt(title$y, subtitle$y)
  expect_gt(subtitle$y, positioned$x$layout$legend$y)
  expect_gt(positioned$x$layout$legend$y, caption$y)
})

test_that("interactive plots support show_legend = FALSE and compact frame fulfillment", {
  skip_if_not_installed("plotly")

  mock_data <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = "Bunia",
    case_alerts = c(4, 7),
    death_alerts = c(1, 2),
    Alert_case_threshold_lower = c(2, 2),
    Alert_case_threshold_upper = c(8, 8),
    Alert_case_threshold = c(5, 6),
    Alert_death_threshold_lower = c(1, 1),
    Alert_death_threshold_upper = c(4, 4),
    Alert_death_threshold = c(2.5, 2.5),
    case_adequacy = c(0.8, 1.1),
    death_adequacy = c(0.5, 0.8),
    aai = c(0.65, 0.95),
    adequacy_category = "Adequate"
  )

  # 1. Alert trend interactive without legend and header
  ip_trend <- plotly::plotly_build(
    plot_alert_trends_interactive(
      data = mock_data,
      hz = "Bunia",
      metric = "case",
      show_legend = FALSE,
      show_header = FALSE,
      show_caption = FALSE,
      start_date = "2026-05-01"
    )
  )

  expect_false(isTRUE(ip_trend$x$layout$showlegend))
  expect_equal(ip_trend$x$layout$margin$t, 15)
  expect_equal(ip_trend$x$layout$margin$b, 40)
  expect_length(ip_trend$x$layout$annotations, 0)

  # Check that traces have showlegend = FALSE
  for (tr in ip_trend$x$data) {
    expect_false(isTRUE(tr$showlegend))
  }

  # 2. Adequacy interactive without legend and header
  ip_adeq <- plotly::plotly_build(
    plot_adequacy_stacked_interactive(
      data = mock_data,
      hz = "Bunia",
      show_legend = FALSE,
      show_header = FALSE,
      show_caption = FALSE,
      start_date = "2026-05-01"
    )
  )

  expect_false(isTRUE(ip_adeq$x$layout$showlegend))
  expect_equal(ip_adeq$x$layout$margin$t, 15)
  expect_equal(ip_adeq$x$layout$margin$b, 40)
  expect_length(ip_adeq$x$layout$annotations, 0)

  for (tr in ip_adeq$x$data) {
    expect_false(isTRUE(tr$showlegend))
  }
})

test_that("plot_adequacy_stacked sets y-axis limits to c(0, 1) when all performances are under 100%", {
  mock_under_100 <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = c("Bunia", "Bunia"),
    case_adequacy = c(0.4, 0.8),
    death_adequacy = c(0.3, 0.6),
    aai = c(0.35, 0.70)
  )

  p <- plot_adequacy_stacked(data = mock_under_100, start_date = "2026-05-01")
  expect_equal(p$scales$get_scales("y")$limits, c(0, 1))
})

test_that("plot_adequacy_stacked has no y-axis limit when at least one performance is >= 100%", {
  mock_with_100 <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = c("Bunia", "Bunia"),
    case_adequacy = c(0.4, 1.0),
    death_adequacy = c(0.3, 0.6),
    aai = c(0.35, 0.80)
  )

  p_exact <- plot_adequacy_stacked(data = mock_with_100, start_date = "2026-05-01")
  expect_null(p_exact$scales$get_scales("y")$limits)

  mock_over_100 <- tibble::tibble(
    threshold_time_key = c("2026-05-03", "2026-05-10"),
    zone_sante_notification = c("Bunia", "Bunia"),
    case_adequacy = c(0.4, 1.35),
    death_adequacy = c(0.3, 0.6),
    aai = c(0.35, 0.975)
  )

  p_over <- plot_adequacy_stacked(data = mock_over_100, start_date = "2026-05-01")
  expect_null(p_over$scales$get_scales("y")$limits)
})

test_that("plot_adequacy_stacked one_per_hz applies y-axis limits independently per health zone", {
  mock_multi_hz <- tibble::tibble(
    threshold_time_key = rep(c("2026-05-03", "2026-05-10"), 2),
    zone_sante_notification = rep(c("Bunia", "Beni"), each = 2),
    case_adequacy = c(0.4, 0.8, 0.5, 1.25),
    death_adequacy = c(0.3, 0.6, 0.4, 0.9),
    aai = c(0.35, 0.70, 0.45, 1.075)
  )

  plots <- plot_adequacy_stacked(data = mock_multi_hz, one_per_hz = TRUE, start_date = "2026-05-01")
  # Bunia is all under 100% -> c(0, 1)
  expect_equal(plots$Bunia$scales$get_scales("y")$limits, c(0, 1))
  # Beni has >= 100% -> NULL
  expect_null(plots$Beni$scales$get_scales("y")$limits)
})
