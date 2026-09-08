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

test_that("ensemble trend title and death plot respond to the alert metric tab", {
  skip_if(!exists("trends_server"), message = "trends module not loaded")

  testServer(trends_server, {
    session$setInputs(ensemble_alert_metric = "death")

    expect_match(output$title_ensemble_alert, "décès")
    expect_false(is.null(output$ip_death_ensemble))
  })
})

test_that("health-zone trend title and death plot respond to the alert metric tab", {
  skip_if(!exists("trends_server"), message = "trends module not loaded")
  skip_if(!exists("all_hz_individual"), message = "health zones not loaded")

  testServer(trends_server, {
    session$setInputs(
      hz_alert_metric = "death",
      selected_hz = all_hz_individual[[1]]
    )

    expect_match(output$title_hz_alert, "décès")
    expect_false(is.null(output$per_hz_death_interactive))
  })
})
