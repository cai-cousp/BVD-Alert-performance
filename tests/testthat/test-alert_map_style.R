library(testthat)
library(dplyr)
library(sf)
library(ggplot2)

source(here::here("R/alert_helpers.R"))

make_style_fixture <- function() {
  st_sf(
    province = c("Ituri", "Ituri", "Nord-Kivu"),
    zonesante = c("Bunia", "Mambasa", "Goma"),
    adequacy_category_recomputed = factor(
      c("Under-alerting", "Adequate", "Over-alerting"),
      levels = notification_adequacy_levels
    ),
    geometry = st_sfc(
      st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))),
      st_polygon(list(cbind(c(1, 2, 2, 1, 1), c(0, 0, 1, 1, 0)))),
      st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(1, 1, 2, 2, 1)))),
      crs = 4326
    )
  )
}

test_that("create_alert_health_zone_labels labels classified polygons only", {
  map_data <- make_style_fixture()
  map_data$adequacy_category_recomputed[1] <- NA
  labels <- create_alert_health_zone_labels(map_data)
  expect_equal(labels$zone_label, c("Mambasa", "Goma"))
  expect_true(all(c("x", "y", "zone_label") %in% names(labels)))
  expect_false(inherits(labels, "sf"))
})

test_that("create_alert_admin1_labels filters labels to the viewport", {
  hz <- make_style_fixture()
  admin <- hz |> summarise(geometry = st_union(geometry), .by = province)
  labels <- create_alert_admin1_labels(
    admin,
    viewport = c(xmin = -0.5, ymin = -0.5, xmax = 2.5, ymax = 2.5)
  )
  expect_setequal(labels$province_label, c("Ituri", "Nord-Kivu"))
})

test_that("plot_health_zone_choropleth builds a Lab-style map", {
  map_data <- make_style_fixture()
  labels <- create_alert_health_zone_labels(map_data)
  admin <- map_data |> summarise(geometry = st_union(geometry), .by = province)

  p <- plot_health_zone_choropleth(
    map_data = map_data,
    labels = labels,
    admin_boundaries = admin,
    category_col = "adequacy_category_recomputed",
    palette = notification_adequacy_palette,
    category_labels = notification_adequacy_labels_fr,
    legend_title = "Performance de notification",
    plot_title = "Carte de test",
    plot_subtitle = "Sous-titre",
    caption = "Légende de test",
    include_reference_map = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Carte de test")
  expect_equal(p$labels$subtitle, "Sous-titre")
  expect_equal(p$labels$caption, "Légende de test")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(unname(fill_scale$palette(3)), unname(notification_adequacy_palette))
  expect_equal(fill_scale$breaks, notification_adequacy_levels)
})

test_that("plot_health_zone_choropleth errors without classified zones", {
  map_data <- make_style_fixture()
  map_data$adequacy_category_recomputed <- NA
  expect_error(
    plot_health_zone_choropleth(
      map_data = map_data,
      labels = tibble::tibble(),
      admin_boundaries = NULL,
      category_col = "adequacy_category_recomputed",
      palette = notification_adequacy_palette,
      category_labels = notification_adequacy_labels_fr,
      legend_title = "Performance",
      plot_title = "Titre",
      include_reference_map = FALSE
    ),
    "No classified"
  )
})

test_that("plot_health_zone_choropleth only legends categories present in data", {
  map_data <- make_style_fixture()
  map_data$adequacy_category_recomputed <- droplevels(
    map_data$adequacy_category_recomputed,
    exclude = "Over-alerting"
  )
  labels <- create_alert_health_zone_labels(map_data)
  admin <- map_data |> summarise(geometry = st_union(geometry), .by = province)

  p <- plot_health_zone_choropleth(
    map_data = map_data,
    labels = labels,
    admin_boundaries = admin,
    category_col = "adequacy_category_recomputed",
    palette = notification_adequacy_palette,
    category_labels = notification_adequacy_labels_fr,
    legend_title = "Performance de notification",
    plot_title = "Carte de test",
    include_reference_map = FALSE
  )

  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$breaks, c("Under-alerting", "Adequate"))
  expect_equal(
    unname(fill_scale$palette(2)),
    unname(notification_adequacy_palette[c("Under-alerting", "Adequate")])
  )
})
