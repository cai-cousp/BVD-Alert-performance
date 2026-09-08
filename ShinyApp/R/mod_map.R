# =============================================================================
# BVD Alerts Dashboard — Leaflet notification performance map module
# =============================================================================

notification_map_ui <- function(id) {
  ns <- NS(id)

  card(
    class = "notification-map-card",
    card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      tags$span(
        tags$i(class = "bi bi-map me-2 text-success"),
        "Performance de notification des alertes par zone de santé"
      ),
      tags$small(
        class = "text-muted",
        "3 dernières semaines complètes"
      )
    ),
    card_body(
      class = "p-2",
      leafletOutput(ns("notification_map"), height = "560px"),
      tags$p(
        class = "text-muted small mt-2 mb-0",
        textOutput(ns("map_help"), inline = TRUE)
      )
    )
  )
}

notification_map_server <- function(
  id,
  map_data,
  selected_hz,
  province_data = NULL,
  province_labels = NULL
) {
  moduleServer(id, function(input, output, session) {
    valid_map_data <- if (inherits(map_data, "sf") && nrow(map_data) > 0L) {
      map_data
    } else {
      NULL
    }
    lookup <- notification_map_lookup(valid_map_data)

    active_data <- if (!is.null(valid_map_data)) {
      valid_map_data
    } else {
      NULL
    }

    # Resolve province boundaries: passed data > global env > dissolved fallback
    resolved_provinces <- if (!is.null(province_data) && inherits(province_data, "sf") && nrow(province_data) > 0L) {
      province_data
    } else if (exists("province_map_data", envir = .GlobalEnv) &&
               inherits(get("province_map_data", envir = .GlobalEnv), "sf") &&
               nrow(get("province_map_data", envir = .GlobalEnv)) > 0L) {
      get("province_map_data", envir = .GlobalEnv)
    } else if (!is.null(active_data)) {
      geometry_column <- attr(active_data, "sf_column") %||% "geometry"
      active_data |>
        dplyr::group_by(province_name = .data$province) |>
        dplyr::summarise(
          geometry = sf::st_union(.data[[geometry_column]])
        ) |>
        dplyr::ungroup()
    } else {
      NULL
    }

    # Resolve province labels
    resolved_labels <- if (!is.null(province_labels) && is.data.frame(province_labels) && nrow(province_labels) > 0L) {
      province_labels
    } else if (exists("province_label_data", envir = .GlobalEnv) &&
               is.data.frame(get("province_label_data", envir = .GlobalEnv)) &&
               nrow(get("province_label_data", envir = .GlobalEnv)) > 0L) {
      get("province_label_data", envir = .GlobalEnv)
    } else if (!is.null(resolved_provinces)) {
      create_province_label_points(resolved_provinces)
    } else {
      tibble::tibble(province_name = character(), lng = numeric(), lat = numeric())
    }

    output$map_help <- renderText({
      if (is.null(valid_map_data)) {
        "La carte interactive n'est pas disponible (géographie ou données manquantes)."
      } else {
        "Survolez une zone de santé pour afficher les indicateurs ; cliquez pour mettre à jour l'analyse."
      }
    })

    output$notification_map <- renderLeaflet({
      if (is.null(valid_map_data)) {
        leaflet() |>
          addTiles(options = tileOptions(opacity = 0.25)) |>
          setView(lng = 23.5, lat = -2.5, zoom = 5)
        return()
      }

      category_keys <- as.character(active_data$adequacy_category_recomputed)
      category_fill <- unname(notification_adequacy_palette[category_keys])
      category_fill[is.na(category_fill)] <- notification_na_fill

      map <- leaflet(
        data = valid_map_data,
        options = leafletOptions(
          preferCanvas = TRUE,
          zoomControl = TRUE,
          attributionControl = TRUE
        )
      ) |>
        addTiles(
          urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
          options = tileOptions(opacity = 0.45)
        ) |>
        # Health zone polygons (thin crisp white borders)
        addPolygons(
          data = active_data,
          group = "Zones de santé",
          layerId = active_data$map_id,
          fillColor = unname(category_fill),
          fillOpacity = 0.82,
          color = "#FFFFFF",
          weight = 0.8,
          opacity = 0.9,
          label = lapply(
            seq_len(nrow(active_data)),
            \(i) build_notification_hover_label(active_data[i, ])
          ),
          labelOptions = labelOptions(
            sticky = TRUE,
            direction = "auto",
            style = list(
              "background-color" = "rgba(255,255,255,0.97)",
              "border-color" = "#ADB5BD",
              "box-shadow" = "0 3px 12px rgba(0,0,0,0.12)",
              "font-size" = "12px"
            )
          ),
          highlightOptions = highlightOptions(
            color = "#0D6EFD",
            weight = 2.5,
            fillOpacity = 0.92,
            bringToFront = FALSE
          )
        )

      # Overlay distinct grey province boundaries on top of health zones
      if (!is.null(resolved_provinces) && inherits(resolved_provinces, "sf") && nrow(resolved_provinces) > 0L) {
        map <- map |>
          addPolylines(
            data = resolved_provinces,
            group = "Limites des provinces",
            color = "#6c757d",
            weight = 2.2,
            opacity = 0.9,
            options = pathOptions(interactive = FALSE, pointerEvents = "none")
          )
      }

      # Overlay bold grey province labels with first letter in capital
      if (!is.null(resolved_labels) && is.data.frame(resolved_labels) && nrow(resolved_labels) > 0L) {
        map <- map |>
          addLabelOnlyMarkers(
            data = resolved_labels,
            lng = ~lng,
            lat = ~lat,
            group = "Noms des provinces",
            label = ~province_name,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              style = list(
                "color" = "#6c757d",
                "font-weight" = "bold",
                "font-size" = "13px",
                "letter-spacing" = "0.5px",
                "text-shadow" = "1.5px 1.5px 3px #ffffff, -1.5px -1.5px 3px #ffffff, 1.5px -1.5px 3px #ffffff, -1.5px 1.5px 3px #ffffff",
                "pointer-events" = "none"
              )
            )
          )
      }

      map |>
        addLayersControl(
          overlayGroups = c("Zones de santé", "Limites des provinces", "Noms des provinces"),
          options = layersControlOptions(collapsed = TRUE)
        ) |>
        addLegend(
          position = "bottomleft",
          colors = unname(notification_adequacy_palette),
          labels = unname(notification_adequacy_labels_fr),
          opacity = 0.9,
          title = "Catégorie d'adéquation"
        ) |>
        fitBounds(
          lng1 = as.numeric(sf::st_bbox(active_data)[["xmin"]]),
          lat1 = as.numeric(sf::st_bbox(active_data)[["ymin"]]),
          lng2 = as.numeric(sf::st_bbox(active_data)[["xmax"]]),
          lat2 = as.numeric(sf::st_bbox(active_data)[["ymax"]])
        )
    })

    # The map is below the initial viewport on this long single-page layout.
    # Do not let Shiny defer it, and calculate it before lower-priority outputs.
    outputOptions(
      output,
      "notification_map",
      suspendWhenHidden = FALSE,
      priority = 10
    )
    outputOptions(
      output,
      "map_help",
      suspendWhenHidden = FALSE,
      priority = 10
    )

    clicked_hz <- eventReactive(
      input$notification_map_shape_click,
      {
        click <- input$notification_map_shape_click
        if (is.null(click) || length(click$id) == 0L || is.na(click$id)) {
          return(NULL)
        }

        matched <- lookup |>
          dplyr::filter(.data$map_id == as.character(click$id))
        if (nrow(matched) != 1L || is.na(matched$zone_sante_notification[[1L]])) {
          NULL
        } else {
          matched$zone_sante_notification[[1L]]
        }
      },
      ignoreInit = TRUE
    )

    # Highlight the selection when the picker or a previous map click changes
    # the health zone. Proxy updates preserve the user's zoom and pan.
    observeEvent(selected_hz(), {
      if (is.null(valid_map_data)) {
        return()
      }
      leafletProxy(session$ns("notification_map"), session = session) |>
        clearGroup("selected_zone")

      hz <- selected_hz()
      if (is.null(hz) || length(hz) != 1L || is.na(hz) || !nzchar(hz)) {
        return()
      }

      selected_row <- valid_map_data |>
        dplyr::filter(
          .data$zone_sante_notification == .env$hz,
          !is.na(.data$zone_sante_notification)
        )
      if (nrow(selected_row) == 0L) {
        return()
      }

      leafletProxy(session$ns("notification_map"), session = session) |>
        addPolygons(
          data = selected_row[1L, ],
          group = "selected_zone",
          fillColor = "transparent",
          fillOpacity = 0,
          color = "#0D6EFD",
          weight = 3,
          opacity = 0.95
        )
    }, ignoreInit = TRUE)

    clicked_hz
  })
}
