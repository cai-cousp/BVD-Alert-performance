library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/confirmed_incidence.R"))
source(here::here("alert_helpers/nowcast_epinow2.R"))
source(here::here("alert_helpers/nowcast_by_zone.R"))

fit_calls <- new.env(parent = emptyenv())
fit_calls$n <- 0L

make_evd <- function(zone, onsets, lab_offset = 3) {
  tibble::tibble(
    zone_sante_notification = rep(zone, length(onsets)),
    classification_finale = rep("Cas confirmé", length(onsets)),
    lab_resultat_final = rep("Positif", length(onsets)),
    alert_date_debut_symptoms = as.Date(onsets),
    s2_date_debut_signes_symptomes = as.Date(onsets),
    lab_date_analyse = as.Date(onsets) + lab_offset
  )
}

make_evd_death <- function(zone, onsets, lab_offset = 3) {
  tibble::tibble(
    zone_sante_notification = rep(zone, length(onsets)),
    classification_finale = rep("Cas confirmé", length(onsets)),
    lab_resultat_final = rep("Positif", length(onsets)),
    alert_date_debut_symptoms = as.Date(onsets),
    s2_date_debut_signes_symptomes = as.Date(onsets),
    lab_date_analyse = as.Date(onsets) + lab_offset,
    s6_statut_final_patient = rep("Décédé", length(onsets)),
    s5_statut_patient_lors_prelev = rep("Vivant", length(onsets)),
    nature_alerte = rep("Vivant", length(onsets)),
    date_de_deces = as.Date(NA),
    s6_date_deces = as.Date(onsets),
    date_heure_notification_alerte = as.Date(onsets) + lab_offset
  )
}

stub_fit <- function(reported_cases,
                     delay_values,
                     max_delay,
                     truncation_dist = NULL,
                     generation_time = NULL,
                     stan = NULL,
                     seed = NULL,
                     verbose = FALSE) {
  fit_calls$n <- fit_calls$n + 1L
  list(
    summarised = tibble::tibble(
      date = reported_cases$date,
      variable = "infections",
      type = "estimate",
      median = 50,
      mean = 50,
      sd = 5,
      lower_90 = 40,
      upper_90 = 60,
      lower_50 = 47,
      upper_50 = 53,
      lower_20 = 45,
      upper_20 = 55
    )
  )
}

test_that("empirical_delay_cdf is monotone and bounded", {
  out <- empirical_delay_cdf(
    c(0L, 1L, 1L, 2L, 5L),
    max_delay = 5L,
    n_boot = 200L,
    seed = 1L
  )

  expect_equal(nrow(out), 6L)
  expect_true(all(diff(out$f_median) >= 0))
  expect_true(all(out$f_median >= 0 & out$f_median <= 1))
  expect_equal(out$f_median[6], 1)
})

test_that("compute_nowcasts_by_zone fits eligible zones and falls back", {
  ref <- as.Date("2026-08-15")
  eligible <- seq.Date(ref - 30L, ref, by = "day")
  sparse <- c(ref - 30L, ref - 20L, ref - 8L)
  evd <- dplyr::bind_rows(
    make_evd("Zone_A", eligible),
    make_evd("Zone_B", sparse)
  )

  fit_calls$n <- 0L

  out <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    recent_window_days = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    seed = 1L
  )

  zone_a <- out |> dplyr::filter(.data$zone_sante_notification == "Zone_A")
  zone_b <- out |> dplyr::filter(.data$zone_sante_notification == "Zone_B")

  expect_equal(nrow(zone_a), 10L)
  expect_true(all(zone_a$method == "epinow2"))
  expect_true(all(zone_a$status == "fit_ok"))
  expect_equal(zone_a$nowcast_median, rep(50, 10))
  expect_equal(fit_calls$n, 1L)

  expect_true(all(zone_b$method == "empirical_delay_correction"))
  expect_equal(unique(zone_b$status), "below_min_cases")
  expect_equal(attr(out, "ref_date"), ref)
})

test_that("compute_nowcasts_by_zone fits the confirmed-death series", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd_death("Zone_A", seq.Date(ref - 30L, ref, by = "day"))
  capture <- new.env(parent = emptyenv())

  recording_fit <- function(reported_cases,
                            delay_values,
                            max_delay,
                            truncation_dist = NULL,
                            generation_time = NULL,
                            stan = NULL,
                            seed = NULL,
                            verbose = FALSE) {
    capture$reported_cases <- reported_cases
    capture$delay_values <- delay_values
    stub_fit(
      reported_cases,
      delay_values,
      max_delay,
      truncation_dist,
      generation_time,
      stan,
      seed,
      verbose
    )
  }

  out <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    recent_window_days = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = recording_fit,
    series = "confirmed_deaths",
    seed = 1L
  )

  expect_equal(attr(out, "series"), "confirmed_deaths")
  expect_equal(
    nrow(out |> dplyr::filter(zone_sante_notification == "Zone_A")),
    10L
  )
  expect_equal(sum(capture$reported_cases$confirm), 31L)
  expect_true(all(capture$delay_values == 0L))
})

test_that("compute_nowcasts_by_zone validates series and death columns", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd("Zone_A", seq.Date(ref - 30L, ref, by = "day"))

  expect_error(
    compute_nowcasts_by_zone(evd, ref_date = ref, series = "bad"),
    "must be one of"
  )

  expect_error(
    compute_nowcasts_by_zone(evd, ref_date = ref, series = "confirmed_deaths"),
    "requires columns"
  )
})

test_that("compute_nowcasts_by_zone returns both series as a named list", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd_death("Zone_A", seq.Date(ref - 30L, ref, by = "day"))
  fit_calls$n <- 0L

  out <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    recent_window_days = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    series = "both",
    seed = 1L
  )

  expect_named(out, c("confirmed_cases", "confirmed_deaths"))
  expect_equal(attr(out$confirmed_cases, "series"), "confirmed_cases")
  expect_equal(attr(out$confirmed_deaths, "series"), "confirmed_deaths")
  expect_equal(fit_calls$n, 2L)
})

test_that("compute_nowcasts_by_zone caches by snapshot key", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd("Zone_A", seq.Date(ref - 30L, ref, by = "day"))

  fit_calls$n <- 0L
  cache_file <- tempfile(fileext = ".rds")
  on.exit(unlink(cache_file), add = TRUE)

  first <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = cache_file,
    snapshot_key = "snap-1",
    seed = 1L
  )
  expect_equal(fit_calls$n, 1L)
  expect_true(file.exists(cache_file))

  second <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = cache_file,
    snapshot_key = "snap-1",
    seed = 1L
  )
  expect_equal(fit_calls$n, 1L)
  expect_equal(first$nowcast_median, second$nowcast_median)
  expect_equal(nrow(first), nrow(second))

  # A new snapshot key forces a refit.
  third <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = cache_file,
    snapshot_key = "snap-2",
    seed = 1L
  )
  expect_equal(fit_calls$n, 2L)
})

test_that("nowcast_total_delta aggregates tail corrections per zone", {
  nowcasts <- tibble::tibble(
    zone_sante_notification = c("A", "A", "B"),
    date = as.Date(c("2026-08-14", "2026-08-15", "2026-08-15")),
    observed = c(1L, 0L, 2L),
    nowcast_median = c(5, 8, 2),
    nowcast_lower_90 = c(3, 5, 2),
    nowcast_upper_90 = c(9, 12, 2)
  )

  out <- nowcast_total_delta(nowcasts)

  expect_equal(out$zone_sante_notification, c("A", "B"))
  expect_equal(out$delta_median, c(12, 0))
  expect_equal(out$delta_lower, c(7, 0))
  expect_equal(out$delta_upper, c(20, 0))
})

test_that("compute_nowcasts_by_zone suppresses and captures Stan WARN lines, then restores the logger", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd("Zone_A", seq.Date(ref - 30L, ref, by = "day"))
  logger <- "EpiNow2.epinow.estimate_infections.fit"

  futile.logger::flog.appender(
    futile.logger::appender.console(),
    name = logger
  )

  out <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    seed = 1L
  )

  expect_equal(attr(out, "stan_warning_count"), 0L)
  expect_equal(attr(out, "stan_warnings"), character())

  warn_output <- capture.output(
    futile.logger::flog.warn("should print again", name = logger),
    type = "output"
  )
  expect_true(any(grepl("should print again", warn_output)))
})

test_that("compute_nowcasts_by_zone uses stronger default sampler settings", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd("Zone_A", seq.Date(ref - 30L, ref, by = "day"))
  capture <- new.env(parent = emptyenv())
  capture$stan <- NULL

  recording_fit <- function(reported_cases,
                            delay_values,
                            max_delay,
                            truncation_dist = NULL,
                            generation_time = NULL,
                            stan = NULL,
                            seed = NULL,
                            verbose = FALSE) {
    capture$stan <- stan
    list(
      summarised = tibble::tibble(
        date = reported_cases$date,
        variable = "infections",
        type = "estimate",
        median = 50,
        mean = 50,
        sd = 5,
        lower_90 = 40,
        upper_90 = 60,
        lower_50 = 47,
        upper_50 = 53,
        lower_20 = 45,
        upper_20 = 55
      )
    )
  }

  compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = recording_fit,
    seed = 1L
  )

  expect_false(is.null(capture$stan))
  expect_equal(capture$stan$warmup, 1000L)
  expect_equal(capture$stan$iter, 1500L)
  expect_equal(capture$stan$control$adapt_delta, 0.95)
})

test_that("extract_evd_date_stamp extracts date correctly from various formats", {
  expect_equal(
    extract_evd_date_stamp("evd.clean_Int_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("evd.clean_Int_2026_08_23.rds"),
    "2026_08_23"
  )
  expect_equal(
    extract_evd_date_stamp("/path/to/Output/23_August/evd.cleaning/evd.clean_Int_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("05_nowcast_by_zone_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_equal(
    extract_evd_date_stamp("05_nowcast_by_zone_deaths_2026_08_23_1515.rds"),
    "2026_08_23_1515"
  )
  expect_null(
    extract_evd_date_stamp(NULL)
  )
})

test_that("compute_nowcasts_by_zone loads date-stamped cache and saves with date pattern", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd("Zone_A", seq.Date(ref - 30L, ref, by = "day"))

  tmp_base <- tempfile("test_nowcast_dir_")
  dir.create(file.path(tmp_base, "2026_08_15"), recursive = TRUE)
  dir.create(file.path(tmp_base, "2026_08_16"), recursive = TRUE)
  on.exit(unlink(tmp_base, recursive = TRUE), add = TRUE)

  fit_calls$n <- 0L

  # 1. Fit and save with date-stamped pattern
  saved_out <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = file.path(tmp_base, "2026_08_15", "05_nowcast_by_zone.rds"),
    snapshot_key = "evd.clean_Int_2026_08_15_1200.rds",
    seed = 1L
  )

  expect_equal(fit_calls$n, 1L)
  expected_saved_file <- file.path(
    tmp_base,
    "2026_08_15",
    "05_nowcast_by_zone_2026_08_15_1200.rds"
  )
  expect_true(file.exists(expected_saved_file))

  # 2. Call from next day directory (2026_08_16) referencing same snapshot key -> cache hit from 2026_08_15!
  cached_loaded <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = file.path(tmp_base, "2026_08_16", "05_nowcast_by_zone.rds"),
    snapshot_key = "evd.clean_Int_2026_08_15_1200.rds",
    seed = 1L
  )

  expect_equal(fit_calls$n, 1L) # No new fit call
  expect_equal(saved_out$nowcast_median, cached_loaded$nowcast_median)
})

test_that("compute_nowcasts_by_zone loads date-stamped death cache", {
  ref <- as.Date("2026-08-15")
  evd <- make_evd_death("Zone_A", seq.Date(ref - 30L, ref, by = "day"))

  tmp_base <- tempfile("test_nowcast_death_dir_")
  dir.create(file.path(tmp_base, "2026_08_15"), recursive = TRUE)
  on.exit(unlink(tmp_base, recursive = TRUE), add = TRUE)

  fit_calls$n <- 0L

  saved_out <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    series = "confirmed_deaths",
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = file.path(tmp_base, "2026_08_15", "05_nowcast_by_zone_deaths.rds"),
    snapshot_key = "evd.clean_Int_2026_08_15_1200.rds",
    seed = 1L
  )

  expect_equal(fit_calls$n, 1L)
  expected_saved_file <- file.path(
    tmp_base,
    "2026_08_15",
    "05_nowcast_by_zone_deaths_2026_08_15_1200.rds"
  )
  expect_true(file.exists(expected_saved_file))

  # Reloading finds the date-stamped deaths cache
  cached_loaded <- compute_nowcasts_by_zone(
    evd,
    ref_date = ref,
    series = "confirmed_deaths",
    max_delay = 10L,
    min_recent_cases = 5L,
    min_series_days = 14L,
    fit_fun = stub_fit,
    cache_path = file.path(tmp_base, "2026_08_15", "05_nowcast_by_zone_deaths.rds"),
    snapshot_key = "evd.clean_Int_2026_08_15_1200.rds",
    seed = 1L
  )

  expect_equal(fit_calls$n, 1L)
  expect_equal(saved_out$nowcast_median, cached_loaded$nowcast_median)
})

