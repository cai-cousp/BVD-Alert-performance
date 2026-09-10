library(testthat)
library(shiny)

if (!exists("trends_smooth_adeq") || !exists("trends_server")) {
  candidates <- c(
    normalizePath(file.path(getwd(), "..", "..", "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", "R", "global.R"), mustWork = FALSE),
    normalizePath(file.path(getwd(), "R", "global.R"), mustWork = FALSE)
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      source(candidate)
      break
    }
  }
}

if (exists("trends_smooth_adeq") && !exists("trends_server")) {
  source(file.path(dirname(candidate), "mod_trends.R"))
}

if (exists("trends_server") && !exists("notification_map_server")) {
  source(file.path(dirname(candidate), "mod_map.R"))
}

test_that("ensemble trend title, adequacy title and death plot respond to the alert metric tab", {
  skip_if(!exists("trends_server"), message = "trends module not loaded")

  testServer(trends_server, {
    # Default is case
    expect_match(output$title_ensemble_alert, "cas")
    expect_match(output$title_ensemble_adeq, "cas")
    expect_match(output$title_ensemble_adeq, "AAI")
    expect_false(is.null(output$ip_case_ensemble))
    expect_false(is.null(output$p_adeq_ensemble))

    # Switch to death
    session$setInputs(ensemble_alert_metric = "death")
    expect_match(output$title_ensemble_alert, "décès")
    expect_match(output$title_ensemble_adeq, "décès")
    expect_match(output$title_ensemble_adeq, "AAI")
    expect_false(is.null(output$ip_death_ensemble))
    expect_false(is.null(output$p_adeq_ensemble))
  })
})

test_that("health-zone trend title, adequacy title and death plot respond to the alert metric tab", {
  skip_if(!exists("trends_server"), message = "trends module not loaded")
  skip_if(!exists("all_hz_individual"), message = "health zones not loaded")

  testServer(trends_server, {
    # Default is case
    expect_match(output$title_hz_alert, "cas")
    expect_match(output$title_hz_adeq, "cas")
    expect_match(output$title_hz_adeq, "AAI")
    expect_false(is.null(output$per_hz_case_interactive))
    expect_false(is.null(output$p_adeq_hz))

    # Switch to death
    session$setInputs(
      hz_alert_metric = "death",
      selected_hz = all_hz_individual[[1]]
    )

    expect_match(output$title_hz_alert, "décès")
    expect_match(output$title_hz_adeq, "décès")
    expect_match(output$title_hz_adeq, "AAI")
    expect_false(is.null(output$per_hz_death_interactive))
    expect_false(is.null(output$p_adeq_hz))
  })
})

test_that("plot_adequacy_stacked always includes AAI for case, death and all", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")
  skip_if(!exists("plot_adequacy_stacked"), message = "plot_adequacy_stacked not loaded")

  p_case <- plot_adequacy_stacked(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "case"
  )
  expect_s3_class(p_case, "ggplot")
  expect_match(p_case$labels$title, "cas")
  expect_match(p_case$labels$title, "AAI")
  expect_setequal(
    levels(droplevels(p_case$data$metric_label)),
    c("Performance des alertes vivants", "Performance globale")
  )

  p_death <- plot_adequacy_stacked(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "death"
  )
  expect_s3_class(p_death, "ggplot")
  expect_match(p_death$labels$title, "décès")
  expect_match(p_death$labels$title, "AAI")
  expect_setequal(
    levels(droplevels(p_death$data$metric_label)),
    c("Performance des alertes décès", "Performance globale")
  )

  p_all <- plot_adequacy_stacked(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "all"
  )
  expect_s3_class(p_all, "ggplot")
  expect_setequal(
    levels(droplevels(p_all$data$metric_label)),
    c("Performance des alertes vivants", "Performance des alertes décès", "Performance globale")
  )
})

test_that("plot_adequacy_stacked_interactive supports metric argument with AAI", {
  skip_if(!exists("trends_smooth_adeq"), message = "trends_smooth_adeq not loaded")
  skip_if(!exists("plot_adequacy_stacked_interactive"), message = "plot_adequacy_stacked_interactive not loaded")

  w_case <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "case",
    show_legend = TRUE,
    show_header = FALSE
  )
  expect_s3_class(w_case, "plotly")

  w_death <- plot_adequacy_stacked_interactive(
    data = trends_smooth_adeq,
    hz = "Ensemble de la zone affectée",
    metric = "death",
    show_legend = TRUE,
    show_header = FALSE
  )
  expect_s3_class(w_death, "plotly")
})
