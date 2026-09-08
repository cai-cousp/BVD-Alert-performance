# =============================================================================
# Tests for the interactive notification map helpers
# =============================================================================

library(testthat)
library(dplyr)

global_candidates <- c(
  file.path(getwd(), "ShinyApp", "R", "global.R"),
  file.path(getwd(), "..", "R", "global.R"),
  file.path(getwd(), "..", "..", "R", "global.R"),
  file.path(getwd(), "R", "global.R")
)
global_candidate <- global_candidates[file.exists(global_candidates)][1]
if (!is.na(global_candidate)) {
  source(global_candidate, local = TRUE)
}

if (exists("app_dir") && file.exists(file.path(app_dir, "R", "mod_trends.R"))) {
  source(file.path(app_dir, "R", "mod_trends.R"), local = TRUE)
}
if (exists("app_dir") && file.exists(file.path(app_dir, "R", "mod_map.R"))) {
  source(file.path(app_dir, "R", "mod_map.R"), local = TRUE)
}

test_that("recent notification metrics use the last three completed windows", {
  skip_if(!exists("trends_smooth_adeq"), message = "global trends not loaded")

  metrics <- summarise_recent_notification_metrics(trends_smooth_adeq)
  last_end <- max(as.Date(trends_smooth_adeq$threshold_valid_to), na.rm = TRUE)
  first_end <- last_end - 14

  expected <- trends_smooth_adeq |>
    filter(
      as.Date(threshold_valid_to) >= first_end,
      as.Date(threshold_valid_to) <= last_end,
      zone_sante_notification != notification_ensemble_label
    ) |>
    filter(zone_sante_notification == first(metrics$zone_sante_notification))

  actual <- metrics |>
    filter(zone_sante_notification == first(expected$zone_sante_notification))

  expect_equal(actual$total_alerts[[1]], sum(expected$total_alerts, na.rm = TRUE))
  expect_equal(actual$n_recent_windows[[1]], 3L)
  expect_false(notification_ensemble_label %in% metrics$zone_sante_notification)
})

test_that("notification map data contains hover fields and stable layer IDs", {
  skip_if(!exists("notification_map_data"), message = "notification map not loaded")
  skip_if(is.null(notification_map_data), message = "map geography not loaded")

  expect_s3_class(notification_map_data, "sf")
  expect_true(nrow(notification_map_data) > 0)
  expect_setequal(
    c(
      "map_id", "zone_sante_notification", "total_alerts",
      "case_adequacy_recent", "death_adequacy_recent", "mean_aai_recent",
      "adequacy_category_recomputed"
    ),
    intersect(
      c(
        "map_id", "zone_sante_notification", "total_alerts",
        "case_adequacy_recent", "death_adequacy_recent", "mean_aai_recent",
        "adequacy_category_recomputed"
      ),
      names(notification_map_data)
    )
  )
  expect_false(anyDuplicated(notification_map_data$map_id) > 0)
  expect_equal(sf::st_crs(notification_map_data)$epsg, 4326)
})

test_that("hover labels contain every requested indicator", {
  skip_if(!exists("notification_map_data"), message = "notification map not loaded")
  skip_if(is.null(notification_map_data), message = "map geography not loaded")

  row <- notification_map_data |>
    filter(!is.na(zone_sante_notification)) |>
    slice(1)
  label <- as.character(build_notification_hover_label(row))

  expect_true(grepl("Total des alertes", label, fixed = TRUE))
  expect_true(grepl("alertes de cas", label, fixed = TRUE))
  expect_true(grepl("alertes de décès", label, fixed = TRUE))
  expect_true(grepl("AAI", label, fixed = TRUE))
  expect_true(grepl("Catégorie", label, fixed = TRUE))
})

test_that("selected-zone national-style tables have the expected structure", {
  skip_if(!exists("trends_smooth_adeq"), message = "global trends not loaded")

  hz <- all_hz_individual[[1]]
  t1 <- get_table1_selected_hz_df(trends_smooth_adeq, hz)
  t2 <- get_table2_selected_hz_df(intermediate_params, trends_smooth_adeq, hz)

  expect_s3_class(t1, "tbl_df")
  expect_true(nrow(t1) > 0)
  expect_setequal(
    names(get_table1_ensemble_df(trends_smooth_adeq)),
    names(t1)
  )
  expect_equal(sort(t1$threshold_time_key, decreasing = TRUE), t1$threshold_time_key)

  expect_s3_class(t2, "tbl_df")
  expect_true(nrow(t2) > 0)
  expect_setequal(
    names(get_table2_ensemble_df(intermediate_params)),
    names(t2)
  )
  expect_equal(sort(t2$threshold_time_key, decreasing = TRUE), t2$threshold_time_key)

  empty <- get_table1_selected_hz_df(trends_smooth_adeq, "Not a health zone")
  expect_equal(nrow(empty), 0L)
})

test_that("picker changes propagate to map-linked tables and titles", {
  skip_if(!exists("trends_smooth_adeq"), message = "global trends not loaded")

  skip_if(length(all_hz_individual) < 2L, message = "not enough health zones")
  hz <- all_hz_individual[[2]]
  shiny::testServer(trends_server, {
    session$setInputs(selected_hz = all_hz_individual[[1]])
    session$setInputs(selected_hz = hz)
    expect_true(grepl(hz, output$map_title_table1, fixed = TRUE))
    expect_true(grepl(hz, output$map_title_table2, fixed = TRUE))
  })
})

test_that("load_province_boundaries loads and simplifies DRC provinces", {
  skip_if(!exists("maps_base_dir") || !dir.exists(maps_base_dir), message = "Maps dir not available")

  prov <- load_province_boundaries(maps_base_dir)
  expect_s3_class(prov, "sf")
  expect_true(nrow(prov) >= 26L)
  expect_true("province_name" %in% names(prov))
  expect_equal(sf::st_crs(prov)$epsg, 4326)
  expect_true("Ituri" %in% prov$province_name)
  expect_true(any(grepl("Nord[- ]Kivu", prov$province_name)))

  # Filtering by province names (with normalized matching)
  filtered_prov <- load_province_boundaries(maps_base_dir, provinces = c("Bas Uele", "Ituri", "Tshopo"))
  expect_s3_class(filtered_prov, "sf")
  expect_equal(nrow(filtered_prov), 3L)

  # Graceful failure on invalid directory
  expect_warning(res <- load_province_boundaries("non_existent_directory_12345"))
  expect_null(res)
})

test_that("load_drc_boundary builds a valid full-country national boundary", {
  skip_if(!exists("maps_base_dir") || !dir.exists(maps_base_dir), message = "Maps dir not available")

  drc <- load_drc_boundary(maps_base_dir)
  expect_s3_class(drc, "sf")
  expect_equal(nrow(drc), 1L)
  expect_equal(sf::st_crs(drc)$epsg, 4326)

  bb <- sf::st_bbox(drc)
  # DRC spans roughly lng 12 to 31.5, lat -13.5 to 5.5
  expect_lt(bb[["xmin"]], 13)
  expect_gt(bb[["xmax"]], 31)
  expect_lt(bb[["ymin"]], -13)
  expect_gt(bb[["ymax"]], 5)
})

test_that("load_zsanddps_health_zones loads zones with spatial province assignment", {
  skip_if(!exists("maps_base_dir") || !dir.exists(maps_base_dir), message = "Maps dir not available")

  ituri_zones <- load_zsanddps_health_zones(maps_base_dir, provinces = "Ituri")
  expect_s3_class(ituri_zones, "sf")
  expect_equal(nrow(ituri_zones), 36L)
  expect_true("zonesante" %in% names(ituri_zones))
  expect_true("province" %in% names(ituri_zones))
  expect_true(all(ituri_zones$province == "Ituri"))
})

test_that("create_province_label_points extracts interior label coordinates", {
  skip_if(!exists("province_map_data") || is.null(province_map_data), message = "province_map_data not loaded")

  pts <- create_province_label_points(province_map_data)
  expect_s3_class(pts, "tbl_df")
  expect_named(pts, c("province_name", "lng", "lat"))
  expect_true(nrow(pts) > 0)
  expect_true(all(is.finite(pts$lng)))
  expect_true(all(is.finite(pts$lat)))

  # Coordinates should be within DRC bounding box roughly (lng 12 to 32, lat -14 to 6)
  expect_true(all(pts$lng >= 11 & pts$lng <= 33))
  expect_true(all(pts$lat >= -15 & pts$lat <= 6))

  # Handles empty or NULL input
  expect_equal(nrow(create_province_label_points(NULL)), 0L)
})


