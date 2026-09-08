library(testthat)

source(here::here("alert_helpers/cfr_backcalc_utils.R"))
source(here::here("alert_helpers/window_utils.R"))
source(here::here("alert_helpers/compute_alert_multipliers.R"))

test_that("compute_alert_multipliers fits HZ-week Poisson offset rates", {
  evd <- tibble::tibble(
    zone_sante_notification = c(rep("A", 6), rep("B", 5), "C"),
    date_heure_notification_alerte = as.Date("2026-01-05") +
      c(0, 0, 7, 7, 14, 14, 0, 0, 7, 7, 7, 0),
    alert_date_debut_symptoms = date_heure_notification_alerte,
    s2_date_debut_signes_symptomes = date_heure_notification_alerte,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = c(
      rep("A", 9),
      rep("B", 11),
      rep("C", 5)
    ),
    date_heure_notification_alerte = as.Date("2026-01-05") +
      c(
        0, 7, 7, 14, 14, 14, 0, 14, 14,
        rep(0, 6), rep(7, 4), 7,
        rep(0, 5)
      ),
    nature_alerte = c(
      rep("Vivant", 6),
      rep("Décédé", 3),
      rep("Vivant", 10),
      "Décédé",
      rep("Vivant", 4),
      "Décédé"
    )
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = c("A", "B", "C", "D"),
    estimated_true_cases_recent = c(12, 10, 8, 0)
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = c("A", "B", "C", "D"),
    cfr_used = c(0.25, 0.5, 0.25, 0.25)
  )

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    min_model_weeks = 2L
  )

  result_a <- result |>
    dplyr::filter(zone_sante_notification == "A")
  result_b <- result |>
    dplyr::filter(zone_sante_notification == "B")
  result_c <- result |>
    dplyr::filter(zone_sante_notification == "C")
  result_d <- result |>
    dplyr::filter(zone_sante_notification == "D")

  expect_equal(result_a$beta_c, 0.5, tolerance = 1e-8)
  expect_equal(result_a$beta_d, 1, tolerance = 1e-8)
  expect_equal(result_a$beta_c_model_status, "poisson_offset")
  expect_equal(result_a$beta_d_model_status, "poisson_offset")

  expect_equal(result_b$beta_c, 1, tolerance = 1e-8)
  expect_equal(result_b$beta_d, 0.2, tolerance = 1e-8)

  expect_equal(result_c$beta_c, 0.5, tolerance = 1e-8)
  expect_equal(result_c$beta_d, 0.5, tolerance = 1e-8)
  expect_equal(
    result_c$beta_c_model_status,
    "insufficient_weeks_ratio_fallback"
  )

  expect_true(is.na(result_d$beta_c))
  expect_equal(result_d$beta_c_model_status, "no_valid_exposure")
})

test_that("compute_alert_multipliers exposes weekly data and diagnostics", {
  evd <- tibble::tibble(
    zone_sante_notification = c("A", "A"),
    date_heure_notification_alerte = as.Date(c("2026-01-05", "2026-01-12")),
    alert_date_debut_symptoms = date_heure_notification_alerte,
    s2_date_debut_signes_symptomes = date_heure_notification_alerte,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = c("A", "A"),
    date_heure_notification_alerte = as.Date(c("2026-01-05", "2026-01-12")),
    nature_alerte = c("Vivant", "Décédé")
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 4
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd
  )

  weekly_data <- attr(result, "weekly_model_data")
  model_summary <- attr(result, "model_summary")

  expect_s3_class(weekly_data, "tbl_df")
  expect_s3_class(model_summary, "tbl_df")
  expect_named(
    result,
    c(
      "zone_sante_notification",
      "estimated_true_cases_recent",
      "cfr_used",
      "expected_deaths",
      "val_alive",
      "val_dead",
      "beta_c",
      "beta_c_low",
      "beta_c_high",
      "beta_c_model_status",
      "beta_c_n_model_weeks",
      "beta_d",
      "beta_d_low",
      "beta_d_high",
      "beta_d_model_status",
      "beta_d_n_model_weeks"
    )
  )
  expect_true(all(c("case", "death") %in% model_summary$model))
  expect_true(
    all(c("estimated_true_cases_week", "expected_deaths_week") %in%
      names(weekly_data))
  )
})

test_that("compute_alert_multipliers respects recent true-case windows", {
  evd <- tibble::tibble(
    zone_sante_notification = c("A", "A"),
    date_heure_notification_alerte = as.Date(c("2026-06-16", "2026-05-19")),
    alert_date_debut_symptoms = date_heure_notification_alerte,
    s2_date_debut_signes_symptomes = date_heure_notification_alerte,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = c(rep("A", 2), rep("A", 10)),
    date_heure_notification_alerte = as.Date(c(
      rep("2026-06-16", 2),
      rep("2026-05-19", 10)
    )),
    nature_alerte = "Vivant"
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 4,
    recent_case_window_start = as.Date("2026-06-15"),
    recent_case_window_end = as.Date("2026-06-28")
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd
  )

  weekly_data <- attr(result, "weekly_model_data")

  expect_equal(unique(weekly_data$week_bin), as.Date("2026-06-15"))
  expect_equal(result$val_alive, 2)
  expect_equal(result$beta_c, 0.5)
})

test_that("compute_alert_multipliers filters recent windows by exact dates", {
  evd <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = as.Date(c(
      "2026-06-16",
      "2026-06-17",
      "2026-06-30",
      "2026-07-01"
    )),
    alert_date_debut_symptoms = date_heure_notification_alerte,
    s2_date_debut_signes_symptomes = date_heure_notification_alerte,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = as.Date(c(
      "2026-06-16",
      "2026-06-17",
      "2026-06-30",
      "2026-07-01"
    )),
    nature_alerte = "Vivant"
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 4,
    recent_case_window_start = as.Date("2026-06-17"),
    recent_case_window_end = as.Date("2026-06-30")
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd
  )

  weekly_data <- attr(result, "weekly_model_data")

  # Harmonized grid path: the single derived grid window (2026-06-17..06-30)
  # is one bin; events outside it (06-16, 07-01) are excluded.
  expect_equal(unique(weekly_data$week_bin), as.Date("2026-06-17"))
  expect_equal(result$val_alive, 2)
  expect_equal(result$beta_c, 0.5)
})

test_that("compute_alert_multipliers_by_window keeps window keys", {
  evd <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = as.Date(c("2026-06-17", "2026-06-30")),
    alert_date_debut_symptoms = date_heure_notification_alerte,
    s2_date_debut_signes_symptomes = date_heure_notification_alerte,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = as.Date(c("2026-06-17", "2026-06-30")),
    nature_alerte = "Vivant"
  )

  true_cases <- tibble::tibble(
    threshold_time_key = c("2026-07-08", "2026-07-15"),
    threshold_window_id = c("2026-06-17__2026-06-30", "2026-06-24__2026-07-07"),
    zone_sante_notification = "A",
    estimated_true_cases_recent = c(4, 2),
    recent_case_window_start = as.Date(c("2026-06-17", "2026-06-24")),
    recent_case_window_end = as.Date(c("2026-06-30", "2026-07-07"))
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  result <- compute_alert_multipliers_by_window(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd
  )

  weekly_data <- attr(result, "weekly_model_data")
  model_summary <- attr(result, "model_summary")

  expect_equal(result$threshold_time_key, c("2026-07-08", "2026-07-15"))
  expect_true(all(c("threshold_time_key", "threshold_window_id") %in% names(weekly_data)))
  expect_true(all(c("threshold_time_key", "threshold_window_id") %in% names(model_summary)))
})

test_that("compute_alert_multipliers resolves case dates via s2 fallback when onset is missing", {
  # alert_date_debut_symptoms is NA; s2_date_debut_signes_symptomes carries the
  # symptom-onset date. date_heure_notification_alerte remains the alert-counting
  # date for validated alerts. The case-date resolution MUST use s2 (onset),
  # NOT the notification timestamp.
  onset_dates <- as.Date(c("2026-06-16", "2026-06-17", "2026-06-30", "2026-07-01"))

  evd <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = onset_dates,
    alert_date_debut_symptoms = as.Date(rep(NA, 4)),
    s2_date_debut_signes_symptomes = onset_dates,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = onset_dates,
    nature_alerte = "Vivant"
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 4,
    recent_case_window_start = as.Date("2026-06-17"),
    recent_case_window_end = as.Date("2026-06-30")
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd
  )

  weekly_data <- attr(result, "weekly_model_data")

  # Case dates resolved from s2 onset fall inside the true-case window
  # (single harmonized grid window 2026-06-17..06-30), matching the
  # equivalent test where alert_date_debut_symptoms is populated directly.
  # If the fallback were ignored, no case dates would resolve and beta_c
  # would be NA / 0.
  expect_equal(unique(weekly_data$week_bin), as.Date("2026-06-17"))
  expect_equal(result$val_alive, 2)
  expect_equal(result$beta_c, 0.5)
})

test_that("compute_alert_multipliers bins by grid windows (no Monday phase offset)", {
  # Regression test for the harmonized time axis: with a Sunday-anchored
  # grid, events are binned by grid window, NOT by Monday calendar weeks.
  # A Sunday and the following Saturday must share the same grid window
  # (2026-03-29); the next Sunday starts the next window (2026-04-05).
  grid <- tibble::tibble(
    threshold_time_key = c("2026-03-29", "2026-04-05"),
    recent_case_window_start = as.Date(c("2026-03-29", "2026-04-05")),
    recent_case_window_end = as.Date(c("2026-04-04", "2026-04-11"))
  )

  event_dates <- as.Date(c("2026-03-29", "2026-04-04", "2026-04-05"))

  evd <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = event_dates,
    alert_date_debut_symptoms = event_dates,
    s2_date_debut_signes_symptomes = event_dates,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = event_dates,
    nature_alerte = "Vivant"
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 3
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  result <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    windows = grid,
    use_full_window = TRUE,
    min_model_weeks = 2L
  )

  weekly_data <- attr(result, "weekly_model_data")

  # One bin per grid window, carrying the grid key; Sunday + Saturday events
  # land in the same Sunday-start window.
  expect_equal(
    weekly_data |>
      dplyr::arrange(week_bin) |>
      dplyr::select(week_bin, threshold_time_key, confirmed_cases_week, case_alerts_week),
    tibble::tibble(
      week_bin = as.Date(c("2026-03-29", "2026-04-05")),
      threshold_time_key = c("2026-03-29", "2026-04-05"),
      confirmed_cases_week = c(2L, 1L),
      case_alerts_week = c(2L, 1L)
    )
  )
  expect_equal(result$val_alive, 3)
  expect_equal(result$beta_c, 1, tolerance = 1e-8)
})

test_that("compute_alert_multipliers_by_window preserves nowcast estimates when n_copy_last > 0", {
  ref <- as.Date("2026-07-15")
  w1_start <- as.Date("2026-07-01")
  w1_end <- as.Date("2026-07-07")
  w2_start <- as.Date("2026-07-08")
  w2_end <- as.Date("2026-07-14")

  evd <- tibble::tibble(
    zone_sante_notification = c(rep("A", 4), rep("A", 2)),
    date_heure_notification_alerte = c(rep(w1_start, 4), rep(w2_start, 2)),
    alert_date_debut_symptoms = date_heure_notification_alerte,
    s2_date_debut_signes_symptomes = date_heure_notification_alerte,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = c(rep("A", 8), rep("A", 10)),
    date_heure_notification_alerte = c(rep(w1_start, 8), rep(w2_start, 10)),
    nature_alerte = c(rep("Vivant", 6), rep("Décédé", 2), rep("Vivant", 8), rep("Décédé", 2))
  )

  true_cases <- tibble::tibble(
    threshold_time_key = c("2026-07-08", "2026-07-15"),
    threshold_window_id = c("2026-07-01__2026-07-07", "2026-07-08__2026-07-14"),
    zone_sante_notification = c("A", "A"),
    estimated_true_cases_recent = c(8, 10),
    recent_case_window_start = c(w1_start, w2_start),
    recent_case_window_end = c(w1_end, w2_end)
  )

  hz_cfr <- tibble::tibble(
    threshold_time_key = c("2026-07-08", "2026-07-15"),
    zone_sante_notification = c("A", "A"),
    cfr_used = c(0.5, 0.5)
  )

  nowcasts <- tibble::tibble(
    zone_sante_notification = "A",
    date = w2_start,
    observed = 2L,
    nowcast_median = 6,
    nowcast_lower_90 = 4,
    nowcast_upper_90 = 8,
    method = "epinow2",
    status = "fit_ok"
  )
  attr(nowcasts, "ref_date") <- ref

  # With nowcast and n_copy_last = 1L, window 2 must NOT copy window 1's multipliers
  res_nowcast <- compute_alert_multipliers_by_window(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    n_copy_last = 1L,
    nowcast = nowcasts,
    ref_date = ref
  )

  # Without nowcast, window 2 MUST copy window 1's multipliers
  res_copied <- compute_alert_multipliers_by_window(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    n_copy_last = 1L,
    nowcast = NULL
  )

  # In res_copied, window 2 beta_c must equal window 1 beta_c
  expect_equal(res_copied$beta_c[2], res_copied$beta_c[1])

  # In res_nowcast, window 2 beta_c is computed from nowcast-adjusted data, not copied from window 1
  expect_false(identical(res_nowcast$beta_c[2], res_nowcast$beta_c[1]))
  expect_true(is.finite(res_nowcast$beta_c[2]))
})

test_that("fit_poisson_offset_rate falls back to negative binomial under overdispersion", {
  # Constant exposure (4) with overdispersed counts (0, 4, 4, 8) gives a
  # dispersion ratio above the default threshold (1.5) while remaining
  # estimable by glm.nb, triggering the negative-binomial refit.
  data <- tibble::tibble(
    zone_sante_notification = "A",
    week_bin = as.Date("2026-01-05") + c(0, 7, 14, 21),
    case_alerts_week = c(0, 4, 4, 8),
    estimated_true_cases_week = c(4, 4, 4, 4)
  )

  res <- fit_poisson_offset_rate(
    data = data,
    count_col = "case_alerts_week",
    exposure_col = "estimated_true_cases_week",
    model = "case",
    min_model_weeks = 2L,
    confidence_level = 0.95
  )

  expect_equal(res$model_status, "negative_binomial")
  expect_true(is.finite(res$theta))
  expect_gt(res$theta, 0)
  expect_true(is.finite(res$beta))
})

test_that("fit_poisson_offset_rate keeps Poisson when not overdispersed", {
  data <- tibble::tibble(
    zone_sante_notification = "A",
    week_bin = as.Date("2026-01-05") + c(0, 7, 14, 21),
    case_alerts_week = c(4, 4, 4, 4),
    estimated_true_cases_week = c(4, 4, 4, 4)
  )

  res <- fit_poisson_offset_rate(
    data = data,
    count_col = "case_alerts_week",
    exposure_col = "estimated_true_cases_week",
    model = "case",
    min_model_weeks = 2L,
    confidence_level = 0.95
  )

  expect_equal(res$model_status, "poisson_offset")
  expect_true(is.na(res$theta))
})

test_that("compute_alert_multipliers reports negative_binomial status when overdispersed", {
  weeks <- as.Date("2026-01-05") + c(0, 7, 14, 21)

  evd <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = weeks,
    alert_date_debut_symptoms = weeks,
    s2_date_debut_signes_symptomes = weeks,
    classification_finale = "Cas confirmé",
    lab_resultat_final = NA_character_
  )

  val_alerts <- tibble::tibble(
    zone_sante_notification = "A",
    date_heure_notification_alerte = rep(weeks, times = c(0, 4, 4, 8)),
    nature_alerte = "Vivant"
  )

  true_cases <- tibble::tibble(
    zone_sante_notification = "A",
    estimated_true_cases_recent = 16
  )

  hz_cfr <- tibble::tibble(
    zone_sante_notification = "A",
    cfr_used = 0.5
  )

  res <- compute_alert_multipliers(
    val_alerts = val_alerts,
    true_cases = true_cases,
    hz_cfr = hz_cfr,
    evd = evd,
    min_model_weeks = 2L
  )

  expect_equal(res$beta_c_model_status, "negative_binomial")
  expect_true(all(c("case", "death") %in% attr(res, "model_summary")$model))
  expect_true("theta" %in% names(attr(res, "model_summary")))
})

