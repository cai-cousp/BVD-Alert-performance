library(testthat)

source(here::here("R/alert_helpers.R"))

test_that("bin_dates_by_grid assigns dates to Sunday-start grid windows", {
  # Boundary days: Sunday (2026-03-29) and the following Saturday
  # (2026-04-04) share window 2026-03-29; the next Sunday (2026-04-05)
  # starts the next window.
  grid <- tibble::tibble(
    threshold_time_key = c("2026-03-29", "2026-04-05"),
    recent_case_window_start = as.Date(c("2026-03-29", "2026-04-05")),
    recent_case_window_end = as.Date(c("2026-04-04", "2026-04-11"))
  )

  events <- tibble::tibble(
    event_date = as.Date(c(
      "2026-03-29",
      "2026-04-04",
      "2026-04-05",
      "2026-04-11",
      "2026-04-12" # outside the grid -> dropped
    ))
  )

  binned <- bin_dates_by_grid(events, grid, "event_date")

  expect_equal(
    binned$week_bin,
    as.Date(c("2026-03-29", "2026-03-29", "2026-04-05", "2026-04-05"))
  )
  expect_equal(
    binned$threshold_time_key,
    c("2026-03-29", "2026-03-29", "2026-04-05", "2026-04-05")
  )
  expect_equal(nrow(binned), 4)
})

test_that("bin_dates_by_grid supports per-HZ windows", {
  grid_hz <- tibble::tibble(
    zone_sante_notification = rep(c("A", "B"), each = 2),
    recent_case_window_start = rep(as.Date(c("2026-03-29", "2026-04-05")), 2),
    recent_case_window_end = rep(as.Date(c("2026-04-04", "2026-04-11")), 2)
  )

  events <- tibble::tibble(
    zone_sante_notification = c("A", "A", "B", "B"),
    event_date = as.Date(c("2026-03-30", "2026-04-06", "2026-03-31", "2026-04-05"))
  )

  binned <- bin_dates_by_grid(events, grid_hz, "event_date")

  expect_equal(
    binned$week_bin,
    as.Date(c("2026-03-29", "2026-04-05", "2026-03-29", "2026-04-05"))
  )
})

test_that("bin_dates_by_grid reproduces build_recent_case_windows boundaries", {
  # The grid bins must be exactly the backward-anchored 7-day non-overlapping
  # windows used by the threshold pipeline.
  threshold_dates <- as.Date(c("2026-03-29", "2026-04-05", "2026-04-12"))
  windows <- build_recent_case_windows(
    threshold_dates,
    window_days = 7L,
    threshold_step_days = 7L
  )

  expect_equal(
    as.integer(
      windows$recent_case_window_end - windows$recent_case_window_start
    ) + 1L,
    rep(7L, nrow(windows))
  )
  expect_equal(
    windows$recent_case_window_start[-1],
    windows$recent_case_window_end[-nrow(windows)] + 1L
  )

  events <- tibble::tibble(ev_date = windows$recent_case_window_start)
  binned <- bin_dates_by_grid(events, windows, "ev_date")

  expect_equal(binned$week_bin, windows$recent_case_window_start)
  expect_equal(binned$threshold_time_key, windows$threshold_time_key)
})

test_that("derive_window_grid extracts distinct windows from hz parameters", {
  hz_parameters <- tibble::tibble(
    zone_sante_notification = c("A", "A", "B", "B"),
    estimated_true_cases_recent = c(4, 4, 2, 2),
    threshold_time_key = c("2026-03-29", "2026-04-05", "2026-03-29", "2026-04-05"),
    recent_case_window_start = rep(as.Date(c("2026-03-29", "2026-04-05")), 2),
    recent_case_window_end = rep(as.Date(c("2026-04-04", "2026-04-11")), 2)
  )

  grid <- derive_window_grid(hz_parameters)

  expect_equal(nrow(grid), 4)
  expect_true(all(
    c(
      "zone_sante_notification",
      "recent_case_window_start",
      "recent_case_window_end",
      "threshold_time_key"
    ) %in% names(grid)
  ))

  # No window columns -> NULL (legacy fallback to Monday binning)
  expect_null(
    derive_window_grid(
      tibble::tibble(zone_sante_notification = "A", x = 1)
    )
  )
})
