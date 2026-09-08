library(testthat)

test_that("Approach C case thresholds are strictly proportional to estimated true cases without historical offset", {
  # Test dataset with two zones: Zone_Active (10 true cases) and Zone_Zero (0 true cases)
  test_data <- tibble::tibble(
    zone_sante_notification = c("Zone_Active", "Zone_Zero"),
    estimated_true_cases_recent = c(10, 0),
    cfr_used = c(0.5, 0.5),
    beta_c = c(2.0, 2.0),
    beta_c_low = c(1.5, 1.5),
    beta_c_high = c(2.5, 2.5),
    beta_d = c(1.2, 1.2),
    beta_d_low = c(0.8, 0.8),
    beta_d_high = c(1.6, 1.6),
    sar = c(0.1, 0.1),
    contacts_per_case_used = c(5, 5)
  )

  result <- test_data |>
    dplyr::mutate(
      expected_deaths = estimated_true_cases_recent * cfr_used,
      expected_deaths = dplyr::if_else(estimated_true_cases_recent == 0, 0, expected_deaths),
      expected_secondary = estimated_true_cases_recent * contacts_per_case_used * sar,
      alert_case_threshold_C = beta_c * estimated_true_cases_recent,
      alert_death_threshold_C = beta_d * expected_deaths,
      alert_case_threshold_lower_C = beta_c_low * estimated_true_cases_recent,
      alert_case_threshold_upper_C = beta_c_high * estimated_true_cases_recent,
      alert_death_threshold_lower_C = beta_d_low * expected_deaths,
      alert_death_threshold_upper_C = beta_d_high * expected_deaths
    )

  active <- result |> dplyr::filter(zone_sante_notification == "Zone_Active")
  zero <- result |> dplyr::filter(zone_sante_notification == "Zone_Zero")

  # Active zone thresholds
  expect_equal(active$alert_case_threshold_C, 20.0)
  expect_equal(active$alert_case_threshold_lower_C, 15.0)
  expect_equal(active$alert_case_threshold_upper_C, 25.0)
  expect_equal(active$expected_deaths, 5.0)
  expect_equal(active$alert_death_threshold_C, 6.0)
  expect_equal(active$alert_death_threshold_lower_C, 4.0)
  expect_equal(active$alert_death_threshold_upper_C, 8.0)

  # Zero-burden zone thresholds: strictly zero, not containing historical baseline offsets
  expect_equal(zero$alert_case_threshold_C, 0.0)
  expect_equal(zero$alert_case_threshold_lower_C, 0.0)
  expect_equal(zero$alert_case_threshold_upper_C, 0.0)
  expect_equal(zero$expected_deaths, 0.0)
  expect_equal(zero$alert_death_threshold_C, 0.0)
  expect_equal(zero$alert_death_threshold_lower_C, 0.0)
  expect_equal(zero$alert_death_threshold_upper_C, 0.0)
})

test_that("Synthesis properly blends Approach A, B, and pure Approach C", {
  # Approach A (demographic), Approach B (historical Beni), Approach C (pure case-derived)
  synthesis_input <- tibble::tibble(
    zone_sante_notification = c("Zone_Active", "Zone_Zero"),
    # Approach A
    death_threshold_lower_A = c(3, 3),
    death_threshold_upper_A = c(6, 6),
    # Approach B
    alert_case_threshold_lower_B = c(4, 4),
    alert_case_threshold_upper_B = c(8, 8),
    alert_death_threshold_lower_B = c(2, 2),
    alert_death_threshold_upper_B = c(4, 4),
    # Approach C (pure case-derived)
    alert_case_threshold_lower_C = c(10, 0),
    alert_case_threshold_upper_C = c(20, 0),
    alert_death_threshold_lower_C = c(5, 0),
    alert_death_threshold_upper_C = c(11, 0)
  )

  synthesis <- synthesis_input |>
    dplyr::mutate(
      Alert_case_lower = (alert_case_threshold_lower_B + tidyr::replace_na(alert_case_threshold_lower_C, 0)) / 2,
      Alert_case_upper = (alert_case_threshold_upper_B + tidyr::replace_na(alert_case_threshold_upper_C, 0)) / 2,
      Alert_death_lower = (death_threshold_lower_A + alert_death_threshold_lower_B + tidyr::replace_na(alert_death_threshold_lower_C, 0)) / 3,
      Alert_death_upper = (death_threshold_upper_A + alert_death_threshold_upper_B + tidyr::replace_na(alert_death_threshold_upper_C, 0)) / 3,
      Alert_case_threshold = (Alert_case_lower + Alert_case_upper) / 2,
      Alert_death_threshold = (Alert_death_lower + Alert_death_upper) / 2
    )

  active <- synthesis |> dplyr::filter(zone_sante_notification == "Zone_Active")
  zero <- synthesis |> dplyr::filter(zone_sante_notification == "Zone_Zero")

  # Active zone: blends B and C for cases; A, B, and C for deaths
  expect_equal(active$Alert_case_lower, (4 + 10) / 2) # 7
  expect_equal(active$Alert_case_upper, (8 + 20) / 2) # 14
  expect_equal(active$Alert_case_threshold, (7 + 14) / 2) # 10.5
  expect_equal(active$Alert_death_lower, (3 + 2 + 5) / 3) # 10/3
  expect_equal(active$Alert_death_upper, (6 + 4 + 11) / 3) # 7.0
  expect_equal(active$Alert_death_threshold, (10/3 + 7) / 2)

  # Zero zone: Approach C is 0, so synthesis only reflects A and B
  expect_equal(zero$Alert_case_lower, (4 + 0) / 2) # 2
  expect_equal(zero$Alert_case_upper, (8 + 0) / 2) # 4
  expect_equal(zero$Alert_death_lower, (3 + 2 + 0) / 3) # 5/3
  expect_equal(zero$Alert_death_upper, (6 + 4 + 0) / 3) # 10/3
})
