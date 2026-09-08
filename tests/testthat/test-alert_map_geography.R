library(testthat)
library(dplyr)
library(sf)

source(here::here("R/alert_helpers.R"))

make_grid3_fixture <- function() {
  # Two provinces, each with two zones. One zone name is duplicated across
  # provinces to exercise hierarchical disambiguation.
  st_sf(
    province = c(
      "Ituri", "Ituri", "Nord-Kivu", "Nord-Kivu", "Tshopo", "Tshopo"
    ),
    zonesante = c(
      "Bunia", "Lubunga", "Goma", "Lubunga", "Kisangani", "Bafwasende"
    ),
    geometry = st_sfc(
      st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))),
      st_polygon(list(cbind(c(1, 2, 2, 1, 1), c(0, 0, 1, 1, 0)))),
      st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(1, 1, 2, 2, 1)))),
      st_polygon(list(cbind(c(1, 2, 2, 1, 1), c(1, 1, 2, 2, 1)))),
      st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(2, 2, 3, 3, 2)))),
      st_polygon(list(cbind(c(1, 2, 2, 1, 1), c(2, 2, 3, 3, 2)))),
      crs = 4326
    )
  )
}

test_that("match_alert_health_zone_names returns exact canonical keys", {
  hz <- make_grid3_fixture()
  out <- match_alert_health_zone_names(
    provinces = c("Ituri", "Nord Kivu"),
    zones = c("Bunia", "Goma"),
    health_zones = hz
  )
  expect_equal(out$province_matched, c("Ituri", "Nord-Kivu"))
  expect_equal(out$zone_matched, c("Bunia", "Goma"))
})

test_that("match_alert_health_zone_names disambiguates duplicate zones by province", {
  hz <- make_grid3_fixture()
  out <- match_alert_health_zone_names(
    provinces = c("Nord Kivu", "Ituri"),
    zones = c("Lubunga", "Lubunga"),
    health_zones = hz
  )
  expect_equal(out$province_matched, c("Nord-Kivu", "Ituri"))
  expect_equal(out$zone_matched, c("Lubunga", "Lubunga"))
})

test_that("match_alert_health_zone_names infers a unique global zone province", {
  hz <- make_grid3_fixture()
  out <- match_alert_health_zone_names(
    provinces = NA_character_,
    zones = "Kisangani",
    health_zones = hz
  )
  expect_equal(out$province_matched, "Tshopo")
  expect_equal(out$zone_matched, "Kisangani")
})

test_that("match_alert_health_zone_names handles empty and unmatched inputs", {
  hz <- make_grid3_fixture()
  empty <- match_alert_health_zone_names(character(0), character(0), hz)
  expect_equal(nrow(empty), 0L)

  unmatched <- match_alert_health_zone_names(
    c("Ituri", "Ituri"),
    c(NA_character_, "Zone Inconnue Xyzzy"),
    hz
  )
  expect_true(is.na(unmatched$zone_matched[1]))
  expect_true(is.na(unmatched$zone_matched[2]))
})

test_that("match_alert_health_zone_names validates inputs", {
  hz <- make_grid3_fixture()
  expect_error(match_alert_health_zone_names("Ituri", "Bunia", hz[-1]), "province")
  expect_error(
    match_alert_health_zone_names("Ituri", "Bunia", hz, match_threshold = 101),
    "match_threshold"
  )
  expect_error(
    match_alert_health_zone_names("Ituri", c("Bunia", "Goma"), hz),
    "same length"
  )
})

test_that("read_alert_grid3_geography errors clearly for a missing directory", {
  expect_error(
    read_alert_grid3_geography(health_zone_dir = tempfile()),
    "health-zone"
  )
})

test_that("join_notification_performance_to_health_zones joins and audits matches", {
  hz <- make_grid3_fixture()
  performance <- tibble::tibble(
    province_notification = c("Ituri", "Nord Kivu", "Ituri"),
    zone_sante_notification = c("Bunia", "Goma", "Zone Inconnue Xyzzy"),
    mean_aai = c(0.5, 1.5, 2.5),
    adequacy_category_recomputed = factor(
      c("Under-alerting", "Adequate", "Over-alerting"),
      levels = notification_adequacy_levels
    )
  )

  out <- expect_warning(
    join_notification_performance_to_health_zones(performance, hz),
    "did not match"
  )
  expect_s3_class(out$data, "sf")
  expect_equal(out$audit$n_geometry_rows, 6L)
  expect_equal(out$audit$n_estimates, 2L)
  expect_equal(out$audit$n_performance_rows, 3L)
  expect_equal(out$audit$n_unmatched_rows, 1L)
  expect_equal(out$audit$unmatched_performance$zone_sante_notification, "Zone Inconnue Xyzzy")
})

test_that("join_notification_performance_to_health_zones warns on duplicate keys", {
  hz <- make_grid3_fixture()
  performance <- tibble::tibble(
    province_notification = c("Ituri", "Ituri"),
    zone_sante_notification = c("Bunia", "Bunia"),
    mean_aai = c(1, 2),
    adequacy_category_recomputed = factor(
      c("Adequate", "Adequate"),
      levels = notification_adequacy_levels
    )
  )

  expect_warning(
    join_notification_performance_to_health_zones(performance, hz),
    "duplicated geography key"
  )
})

test_that("join_notification_performance_to_health_zones handles empty performance data", {
  hz <- make_grid3_fixture()
  performance <- tibble::tibble(
    province_notification = character(0),
    zone_sante_notification = character(0),
    mean_aai = numeric(0),
    adequacy_category_recomputed = factor(levels = notification_adequacy_levels)
  )

  out <- join_notification_performance_to_health_zones(performance, hz)
  expect_equal(out$audit$n_estimates, 0L)
  expect_equal(out$audit$n_unmatched_rows, 0L)
})
