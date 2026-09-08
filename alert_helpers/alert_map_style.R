# Lab-style visual helpers for health-zone choropleth maps
# -----------------------------------------------------------------------------
# Reproduces the visual conventions of Lab.Analysis/lab_map.R: grey national
# base polygons, colored classified zones, province outlines and labels,
# repelled health-zone labels, scale bar, north arrow and a DRC reference inset.

#' Create health-zone labels from classified polygons
#'
#' @param map_data An `sf` object containing the classification column.
#' @param category_col Column name holding the mapped category.
#' @param label_col Column name holding the canonical zone label.
#'
#' @return A non-spatial `tibble` with `x`, `y` and `zone_label` columns.
create_alert_health_zone_labels <- function(
  map_data,
  category_col = "adequacy_category_recomputed",
  label_col = "zonesante"
) {
  if (!inherits(map_data, "sf")) {
    rlang::abort("`map_data` must be an sf object.", call = rlang::caller_env())
  }
  if (!category_col %in% names(map_data) || !label_col %in% names(map_data)) {
    rlang::abort("`map_data` must contain the category and label columns.", call = rlang::caller_env())
  }

  label_source <- map_data |> dplyr::filter(!is.na(.data[[category_col]]))
  if (nrow(label_source) == 0L) {
    return(tibble::tibble())
  }

  label_points <- suppressWarnings(sf::st_centroid(label_source))
  coordinates <- sf::st_coordinates(label_points)

  label_points |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      x = unname(coordinates[, "X"]),
      y = unname(coordinates[, "Y"]),
      zone_label = .data[[label_col]]
    )
}

#' Create province labels filtered to the map viewport
#'
#' @param admin_boundaries Province `sf` boundaries.
#' @param viewport Optional named numeric bbox (`xmin`, `ymin`, `xmax`, `ymax`).
#' @param buffer_degrees Extra degrees around the viewport.
#'
#' @return A `tibble` with `province_label`, `x` and `y` columns.
create_alert_admin1_labels <- function(
  admin_boundaries,
  viewport = NULL,
  buffer_degrees = 0
) {
  if (!inherits(admin_boundaries, "sf")) {
    rlang::abort("`admin_boundaries` must be an sf object.", call = rlang::caller_env())
  }

  name_column <- intersect(c("adm1_name", "province", "adm1"), names(admin_boundaries))[1]
  if (is.na(name_column)) {
    return(tibble::tibble(
      province_label = character(),
      x = numeric(),
      y = numeric()
    ))
  }

  label_points <- suppressWarnings(sf::st_point_on_surface(admin_boundaries))
  coordinates <- sf::st_coordinates(label_points)

  labels <- admin_boundaries |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      province_label = .data[[name_column]],
      x = unname(coordinates[, "X"]),
      y = unname(coordinates[, "Y"])
    ) |>
    dplyr::filter(!is.na(.data$province_label))

  if (!is.null(viewport)) {
    labels <- labels[
      labels$x >= viewport[["xmin"]] - buffer_degrees &
        labels$x <= viewport[["xmax"]] + buffer_degrees &
        labels$y >= viewport[["ymin"]] - buffer_degrees &
        labels$y <= viewport[["ymax"]] + buffer_degrees,
      ,
      drop = FALSE
    ]
  }

  labels
}

#' Create the national DRC reference inset
#'
#' @param admin_boundaries Province `sf` boundaries used for national context.
#' @param focus_geometry `sf` geometry of the mapped focus area.
#'
#' @return A `ggplot` reference map.
create_alert_reference_map <- function(admin_boundaries, focus_geometry) {
  country_outline <- sf::st_union(admin_boundaries)
  country_box <- sf::st_bbox(admin_boundaries)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = admin_boundaries,
      fill = "#F2F2F2",
      colour = "#DADADA",
      linewidth = 0.1
    ) +
    ggplot2::geom_sf(
      data = country_outline,
      fill = NA,
      colour = "#6E6E6E",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = focus_geometry,
      fill = "#8f0000",
      colour = "#382222",
      linewidth = 0.1,
      alpha = 0.85
    ) +
    ggplot2::annotate(
      "text",
      x = mean(c(country_box[["xmin"]], country_box[["xmax"]])),
      y = mean(c(country_box[["ymin"]], country_box[["ymax"]])),
      label = "RDC | DRC",
      colour = "grey35",
      alpha = 0.5,
      size = 3,
      fontface = "bold"
    ) +
    ggplot2::labs(title = "") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void(base_size = 7) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 7, hjust = 0.5, colour = "grey25"
      ),
      plot.background = ggplot2::element_rect(
        fill = "#FFFFFFE6", colour = "grey65", linewidth = 0.2
      ),
      plot.margin = ggplot2::margin(2, 2, 2, 2, "pt")
    )
}

#' Plot a Lab-style health-zone choropleth
#'
#' @param map_data `sf` health-zone polygons including the category column.
#' @param labels Label `tibble` from [create_alert_health_zone_labels()].
#' @param admin_boundaries Optional province `sf` boundaries.
#' @param category_col Category column name in `map_data`.
#' @param palette Named character vector of category colors.
#' @param category_labels Named character vector of display labels.
#' @param legend_title Legend title.
#' @param plot_title,plot_subtitle,caption Map text.
#' @param na_value,na_label Fill color and legend label for missing values.
#' @param buffer_degrees Coordinate buffer around classified zones.
#' @param include_reference_map Whether to add the DRC reference inset.
#'
#' @details The fill legend only includes categories actually present in the
#'   mapped data, in canonical palette order; absent categories are omitted.
#' @return A `ggplot` (with inset when requested, a patchwork object).
plot_health_zone_choropleth <- function(
  map_data,
  labels,
  admin_boundaries = NULL,
  category_col,
  palette,
  category_labels = NULL,
  legend_title,
  plot_title,
  plot_subtitle = NULL,
  caption = NULL,
  na_value = "#F4F4F4",
  na_label = "Pas de données / hors analyse",
  buffer_degrees = 0.25,
  include_reference_map = TRUE
) {
  if (!inherits(map_data, "sf")) {
    rlang::abort("`map_data` must be an sf object.", call = rlang::caller_env())
  }
  if (!category_col %in% names(map_data)) {
    rlang::abort(
      c("`map_data` must contain the category column:", x = category_col),
      call = rlang::caller_env()
    )
  }
  if (!is.null(admin_boundaries) && !inherits(admin_boundaries, "sf")) {
    rlang::abort("`admin_boundaries` must be NULL or an sf object.", call = rlang::caller_env())
  }

  estimate_data <- map_data |> dplyr::filter(!is.na(.data[[category_col]]))
  if (nrow(estimate_data) == 0L) {
    rlang::abort("No classified health zones available to map.", call = rlang::caller_env())
  }

  present_levels <- intersect(
    names(palette),
    unique(as.character(estimate_data[[category_col]]))
  )
  if (length(present_levels) == 0L) {
    rlang::abort(
      "None of the mapped categories are present in the supplied palette.",
      call = rlang::caller_env()
    )
  }
  present_labels <- if (is.null(category_labels)) {
    present_levels
  } else {
    unname(category_labels[present_levels])
  }

  map_box <- sf::st_bbox(estimate_data)

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_data,
      fill = "#F4F4F4",
      colour = "#B4B4B4",
      linewidth = 0.05
    ) +
    ggplot2::geom_sf(
      data = estimate_data,
      mapping = ggplot2::aes(fill = .data[[category_col]]),
      colour = "#1a1a33",
      linewidth = 0.09
    ) +
    ggplot2::scale_fill_manual(
      values = palette[present_levels],
      breaks = present_levels,
      labels = present_labels,
      drop = TRUE,
      na.translate = TRUE,
      na.value = na_value,
      name = legend_title,
      guide = ggplot2::guide_legend(order = 1, nrow = 1)
    )

  if (!is.null(admin_boundaries)) {
    map <- map +
      ggplot2::geom_sf(
        data = admin_boundaries,
        fill = NA,
        colour = "grey30",
        linewidth = 0.75
      )
  }

  province_labels <- if (!is.null(admin_boundaries)) {
    create_alert_admin1_labels(admin_boundaries, map_box, buffer_degrees)
  } else {
    tibble::tibble()
  }

  if (nrow(province_labels) > 0L) {
    map <- map +
      ggrepel::geom_text_repel(
        data = province_labels,
        mapping = ggplot2::aes(x = x, y = y, label = province_label),
        colour = "grey35",
        alpha = 0.5,
        size = 4,
        fontface = "bold",
        min.segment.length = Inf,
        max.overlaps = Inf,
        seed = 2026
      )
  }

  if (!is.null(labels) && nrow(labels) > 0L) {
    map <- map +
      ggrepel::geom_label_repel(
        data = labels,
        mapping = ggplot2::aes(x = x, y = y, label = zone_label),
        colour = "black",
        fill = "#FFFFFFBF",
        size = 2.2,
        label.size = 0,
        label.padding = 0.1,
        label.r = grid::unit(0.05, "lines"),
        box.padding = 0.2,
        point.padding = 0.2,
        segment.size = 0.2,
        segment.colour = "grey50",
        min.segment.length = 0,
        max.overlaps = Inf,
        force = 1.5,
        seed = 2026
      )
  }

  map <- map +
    ggplot2::coord_sf(
      xlim = c(map_box[["xmin"]] - buffer_degrees, map_box[["xmax"]] + buffer_degrees),
      ylim = c(map_box[["ymin"]] - buffer_degrees, map_box[["ymax"]] + buffer_degrees),
      expand = FALSE
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = NULL,
      caption = caption
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA, colour = "grey"),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.125),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.5),
      axis.ticks.length = grid::unit(-0.5, "mm"),
      legend.position = "top",
      legend.background = ggplot2::element_rect(fill = grDevices::adjustcolor("white", alpha.f = 0.6)),
      legend.text = ggplot2::element_text(face = "bold", size = 8),
      legend.title = ggplot2::element_text(face = "bold", size = 8),
      legend.key.size = grid::unit(0.5, "cm"),
      plot.title = ggplot2::element_text(
        size = 13, face = "bold", colour = "#03084a", hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 8, hjust = 0.5),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold", size = 10)
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      style = "ticks",
      height = grid::unit(0.10, "cm"),
      pad_x = grid::unit(1, "cm"),
      pad_y = grid::unit(0.25, "cm"),
      text_pad = grid::unit(0.10, "cm"),
      text_face = "bold",
      text_cex = 0.6
    ) +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",
      height = grid::unit(0.5, "cm"),
      width = grid::unit(0.5, "cm"),
      pad_x = grid::unit(5.0, "cm"),
      pad_y = grid::unit(0.10, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(
        text_face = "bold",
        text_size = 4
      )
    )

  if (!is.null(admin_boundaries) && include_reference_map) {
    focus_geometry <- sf::st_union(estimate_data)
    reference_map <- create_alert_reference_map(admin_boundaries, focus_geometry)

    return(
      map +
        patchwork::inset_element(
          reference_map,
          left = 0.67,
          bottom = 0.02,
          right = 1.05,
          top = 0.24,
          on_top = FALSE,
          clip = TRUE,
          align_to = "panel"
        )
    )
  }

  map
}
