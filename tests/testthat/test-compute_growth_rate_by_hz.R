library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))

source(here::here("alert_helpers/compute_recent_confirmed_windows_by_hz.R"))
source(here::here("alert_helpers/compute_growth_rate_by_hz.R"))

# Helper to build a minimal line list with confirmed cases on given dates
make_evd <- function(dates, hz = "Zone_A") {
  tibble::tibble(
    zone_sante_notification = rep(hz, length(dates)),
    classification_finale = rep("Cas confirmé", length(dates)),
    lab_resultat_final = rep("Positif", length(dates)),
    alert_date_debut_symptoms = as.Date(dates)
  )
}

# -- compute_growth_rate_hz (inner, per-zone) --------------------------------

test_that("compute_growth_rate_hz generates correct number of non-overlapping windows", {
  # 20 consecutive days of cases (Jan 1 to Jan 20)
  # max_date = Jan 20, window_days = 7.
  # Backward anchored windows:
  # Jan 14 - Jan 20
  # Jan  7 - Jan 13
  # Dec 31 - Jan  6 (extends before min_date)
  # -> 3 windows
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-20"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 1L
  )

  expect_s3_class(result, "tbl_df")
  # 3 non-overlapping windows
  expect_equal(nrow(result), 3)
  expect_equal(result$threshold_window_index, 1:3)
  # First window: [Dec 31, Jan 6]
  expect_equal(result$recent_case_window_start[1], as.Date("2025-12-31"))
  expect_equal(result$recent_case_window_end[1], as.Date("2026-01-06"))
  # Last window: [Jan 14, Jan 20]
  expect_equal(result$recent_case_window_start[3], as.Date("2026-01-14"))
  expect_equal(result$recent_case_window_end[3], as.Date("2026-01-20"))
})

test_that("last window is copied from preceding window", {
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-20"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 1L
  )

  last <- result[nrow(result), ]
  second_last <- result[nrow(result) - 1L, ]

  # Last window should be a copy

  expect_equal(last$growth_status, "copied_from_preceding")
  expect_equal(last$growth_rate_source, "copied")

  # Growth rate values should match preceding window

  expect_equal(last$r, second_last$r)
  expect_equal(last$r_low, second_last$r_low)
  expect_equal(last$r_high, second_last$r_high)
  expect_equal(last$doubling_time, second_last$doubling_time)

  # But window dates should reflect the actual last window
  expect_equal(last$recent_case_window_end, as.Date("2026-01-20"))
})

test_that("n_copy_last = 0 disables copying", {
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-20"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 0L
  )

  expect_equal(nrow(result), 3)
  # No copied rows
  expect_false("copied_from_preceding" %in% result$growth_status)
})

test_that("n_copy_last = 3 copies last 3 windows", {
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-20"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 3L
  )

  copied <- result[result$growth_status == "copied_from_preceding", ]
  expect_equal(nrow(copied), 2)
  # Copied windows are the last 2 (since there are only 3 windows total, max copy is 2)
  expect_equal(copied$threshold_window_index, 2:3)
})

test_that("returns empty result when no valid onset dates", {
  evd <- tibble::tibble(
    zone_sante_notification = "Zone_A",
    classification_finale = "Cas confirmé",
    lab_resultat_final = "Positif",
    alert_date_debut_symptoms = as.Date(NA_character_)
  )

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 1L
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$growth_status, "missing_onset_dates")
  expect_true(is.na(result$r))
})

test_that("returns empty result when date range shorter than window_days", {
  # Only 5 days of data — not enough for a 7-day window
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-05"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 1L
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$growth_status, "sparse_days")
})

test_that("n_copy_last is reduced when fewer windows than requested", {
  # 8 days → 2 windows; requesting n_copy_last = 5 should reduce to 1
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-08"), by = "day")
  evd <- make_evd(dates)

  expect_warning(
    result <- compute_growth_rate_hz(
      data = evd,
      zone_sante_notification = "Zone_A",
      window_days = 7L,
      min_cases = 3L,
      min_days = 3L,
      n_copy_last = 5L
    ),
    "reduced n_copy_last"
  )

  expect_equal(nrow(result), 2)
  copied <- result[result$growth_status == "copied_from_preceding", ]
  expect_equal(nrow(copied), 1)
})

test_that("single window is not copied even when n_copy_last > 0", {
  # Exactly 7 days → 1 window
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-07"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_hz(
    data = evd,
    zone_sante_notification = "Zone_A",
    window_days = 7L,
    min_cases = 3L,
    min_days = 3L,
    n_copy_last = 1L
  )

  expect_equal(nrow(result), 1)
  expect_false(result$growth_status == "copied_from_preceding")
})

# -- compute_growth_rate_by_hz (outer) ---------------------------------------

test_that("outer function returns one row per zone per window", {
  dates_a <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-15"), by = "day")
  dates_b <- seq.Date(as.Date("2026-01-03"), as.Date("2026-01-18"), by = "day")
  evd <- dplyr::bind_rows(make_evd(dates_a, "Zone_A"), make_evd(dates_b, "Zone_B"))

  result <- compute_growth_rate_by_hz(evd, n_copy_last = 1L)

  expect_s3_class(result, "tbl_df")
  # Zone_A: 15 days -> 3 non-overlapping windows (ends: 15, 8, 1)
  # Zone_B: 16 days -> 3 non-overlapping windows (ends: 18, 11, 4)
  n_a <- sum(result$zone_sante_notification == "Zone_A")
  n_b <- sum(result$zone_sante_notification == "Zone_B")
  expect_equal(n_a, 3)
  expect_equal(n_b, 3)

  # Each zone's windows are indexed 1..N
  expect_equal(
    result[result$zone_sante_notification == "Zone_A", ]$threshold_window_index,
    1:3
  )
  expect_equal(
    result[result$zone_sante_notification == "Zone_B", ]$threshold_window_index,
    1:3
  )
})

test_that("pooled fallback works with rolling windows", {
  # Zone_A has plenty of cases, Zone_B has very few
  dates_a <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-20"), by = "day")
  dates_b <- c("2026-01-05", "2026-01-15")

  evd <- dplyr::bind_rows(
    make_evd(dates_a, "Zone_A"),
    make_evd(dates_b, "Zone_B")
  )

  result <- compute_growth_rate_by_hz(evd, n_copy_last = 0L)

  # Zone_B should have sparse_cases for most windows (only 2 cases total)
  zone_b <- result[result$zone_sante_notification == "Zone_B", ]
  # Pooled fallback columns should be present
  expect_true("pooled_r" %in% names(result))
  expect_true("r_used" %in% names(result))

  # Where Zone_B's r is NA, r_used should fall back to pooled_r
  zone_b_na <- zone_b[!is.finite(zone_b$r), ]
  if (nrow(zone_b_na) > 0) {
    expect_true(
      all(zone_b_na$growth_fallback_reason %in%
        c("pooled_growth_rate", "missing_growth_rate"))
    )
  }
})

test_that("latest window per zone can be selected for backward compatibility", {
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-15"), by = "day")
  evd <- dplyr::bind_rows(
    make_evd(dates, "Zone_A"),
    make_evd(dates, "Zone_B")
  )

  result <- compute_growth_rate_by_hz(evd, n_copy_last = 1L)

  # Select latest window per zone (mimics downstream usage)
  latest <- result |>
    dplyr::group_by(zone_sante_notification) |>
    dplyr::filter(recent_case_window_end == max(recent_case_window_end, na.rm = TRUE)) |>
    dplyr::ungroup()

  expect_equal(nrow(latest), 2)
  expect_equal(latest$zone_sante_notification, c("Zone_A", "Zone_B"))
  # Each zone has exactly one row
  expect_true(all(latest$threshold_window_index == max(latest$threshold_window_index)))
})

test_that("output has all expected columns", {
  dates <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-15"), by = "day")
  evd <- make_evd(dates)

  result <- compute_growth_rate_by_hz(evd, n_copy_last = 1L)

  expected_cols <- c(
    "zone_sante_notification",
    "threshold_window_index",
    "recent_case_window_start",
    "recent_case_window_end",
    "growth_n_cases",
    "growth_n_days",
    "r", "r_low", "r_high",
    "doubling_time", "doubling_time_low", "doubling_time_high",
    "halving_time", "halving_time_low", "halving_time_high",
    "predicted_last_count",
    "growth_direction",
    "growth_rate_source",
    "growth_status",
    "pooled_r", "pooled_r_low", "pooled_r_high", "pooled_doubling_time",
    "growth_fallback_reason", "pooled_growth_used",
    "r_used", "r_low_used", "r_high_used", "doubling_time_used"
  )
  expect_true(all(expected_cols %in% names(result)))
})
