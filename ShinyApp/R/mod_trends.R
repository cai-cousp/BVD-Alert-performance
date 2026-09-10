# =============================================================================
# BVD Alerts App — Tab 2: Trend Analysis & Health Zone Monitoring
# =============================================================================
# Layout:
# 1) Ensemble Section:
#    - Row 1: case/death trend tabs (left) & p_adeq_ensemble (right) (with PNG downloads)
#    - Row 2: table1_ensemble_html (left) & table2_ensemble_html (right) (with XLSX downloads)
# 2) Health Zone Section:
#    - Filter: Health Zone picker
#    - Row 3: case/death trend tabs (left) & p_adeq_hz (right) (with PNG downloads)
#    - Row 4: table1_html (left) & table2_html (right) (with XLSX downloads)
# =============================================================================

# --- French column dictionaries ----------------------------------------------
table1_labels_fr <- c(
  zone_sante_notification = "Zone de santé",
  Province                = "Province",
  case_alerts             = "Alertes de cas",
  death_alerts            = "Alertes de décès",
  total_alerts            = "Total des alertes",
  Alert_case_threshold    = "Seuil médian : cas",
  Alert_death_threshold   = "Seuil médian : décès",
  case_adequacy           = "Performance : cas",
  death_adequacy          = "Performance : décès",
  aai                     = "Performance globale (AAI)"
)

table1_ensemble_labels_fr <- c(
  threshold_time_key    = "Semaine de notification (début)",
  case_alerts           = "Alertes de cas",
  death_alerts          = "Alertes de décès",
  total_alerts          = "Total des alertes",
  Alert_case_threshold  = "Seuil médian : cas",
  Alert_death_threshold = "Seuil médian : décès",
  case_adequacy         = "Performance : cas",
  death_adequacy        = "Performance : décès",
  aai                   = "Performance globale (AAI)"
)

table2_labels_fr <- c(
  zone_sante_notification    = "Zone de santé",
  Province                   = "Province",
  beta_c                     = "Coefficient β : cas",
  beta_d                     = "Coefficient β : décès",
  n_recent_confirmed         = "Cas confirmés récents",
  n_recent_confirmed_nowcast = "Cas confirmés récents (nowcast)",
  detection_rate_adj         = "Taux de détection combiné",
  estimated_true_cases_recent = "Cas vrais récents estimés"
)

table2_ensemble_labels_fr <- c(
  threshold_time_key         = "Semaine de notification (début)",
  beta_c                     = "Coefficient β : cas",
  beta_d                     = "Coefficient β : décès",
  n_recent_confirmed         = "Cas confirmés récents",
  n_recent_confirmed_nowcast = "Cas confirmés récents (nowcast)",
  detection_rate_adj         = "Taux de détection combiné",
  estimated_true_cases_recent = "Cas vrais récents estimés"
)

# Helper to rename data frame columns using a dictionary for Excel export
rename_df_fr <- function(df, dict) {
  cols_in_dict <- intersect(names(df), names(dict))
  new_names <- names(df)
  names_idx <- match(cols_in_dict, new_names)
  new_names[names_idx] <- dict[cols_in_dict]
  names(df) <- new_names
  df
}

alert_metric_label_fr <- function(metric) {
  switch(
    metric,
    case = "cas",
    death = "décès",
    stop("`metric` must be 'case' or 'death'.", call. = FALSE)
  )
}

alert_trend_title_fr <- function(metric, hz = NULL) {
  metric_label <- alert_metric_label_fr(metric)
  if (is.null(hz)) {
    paste0("Tendances des alertes de ", metric_label, " vs. seuils attendus")
  } else {
    paste0("Tendances des alertes de ", metric_label, " : ", hz)
  }
}

trends_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # =========================================================================
    # GROUPE 1 : Ensemble de la zone affectée
    # =========================================================================
    div(
      class = "mb-5",
      div(
        class = "d-flex align-items-center mb-3",
        tags$span(
          class = "badge bg-primary me-2 px-3 py-2 fs-6",
          "Niveau Global"
        ),
        h3(class = "mb-0", "Ensemble de la zone affectée")
      ),

      # --- 1) Deux graphiques côte à côte ------------------------------------
      layout_columns(
        col_widths = c(6, 6),
        gap = "16px",
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              tags$i(class = "bi bi-graph-up me-2 text-primary"),
              textOutput(ns("title_ensemble_alert"), inline = TRUE)
            ),
            downloadButton(
              ns("download_plot_alert_ensemble"),
              label = "PNG",
              class = "btn btn-sm btn-outline-primary py-0 px-2 fw-semibold",
              title = "Télécharger le graphique en PNG (300 DPI)"
            )
          ),
          card_body(
            class = "p-2",
            div(
              class = "alert-trend-tabs",
              navset_pill(
                id = ns("ensemble_alert_metric"),
                selected = "case",
                nav_panel(
                  title = "Cas",
                  value = "case",
                  plotlyOutput(
                    ns("ip_case_ensemble"),
                    height = "380px",
                    width = "100%"
                  )
                ),
                nav_panel(
                  title = "Décès",
                  value = "death",
                  plotlyOutput(
                    ns("ip_death_ensemble"),
                    height = "380px",
                    width = "100%"
                  )
                )
              )
            )
          )
        ),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              tags$i(class = "bi bi-bar-chart-fill me-2 text-primary"),
              textOutput(ns("title_ensemble_adeq"), inline = TRUE)
            ),
            downloadButton(
              ns("download_plot_adeq_ensemble"),
              label = "PNG",
              class = "btn btn-sm btn-outline-primary py-0 px-2 fw-semibold",
              title = "Télécharger le graphique en PNG (300 DPI)"
            )
          ),
          card_body(
            class = "p-2",
            plotlyOutput(ns("p_adeq_ensemble"), height = "380px", width = "100%"),
            tags$p(
              class = "plot-footnote text-muted mt-2 mb-0",
              textOutput(ns("p_adeq_ensemble_footnote"), inline = TRUE)
            )
          )
        )
      ),

      # --- 2) Deux tableaux sous les graphiques (scrollables) ----------------
      div(
        class = "mt-3",
        layout_columns(
          col_widths = c(6, 6),
          gap = "16px",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span(
                tags$i(class = "bi bi-table me-2 text-secondary"),
                "Tableau 1 — Alertes, seuils et performance (Ensemble)"
              ),
              downloadButton(
                ns("download_table1_ensemble"),
                label = "XLSX",
                class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
                title = "Télécharger le tableau au format Excel (.xlsx)"
              )
            ),
            card_body(
              DTOutput(ns("table1_ensemble_html"))
            )
          ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span(
                tags$i(class = "bi bi-calculator me-2 text-secondary"),
                "Tableau 2 — Paramètres du modèle (β, nowcast, détection)"
              ),
              downloadButton(
                ns("download_table2_ensemble"),
                label = "XLSX",
                class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
                title = "Télécharger le tableau au format Excel (.xlsx)"
              )
            ),
            card_body(
              DTOutput(ns("table2_ensemble_html"))
            )
          )
        )
      )
    ),

    tags$hr(class = "my-4"),

    # =========================================================================
    # GROUPE 2 : Analyse par zone de santé
    # =========================================================================
    div(
      class = "mb-4",
      div(
        class = "d-flex align-items-center justify-content-between flex-wrap gap-3 mb-4 pb-2 border-bottom",
        div(
          class = "d-flex align-items-center gap-2",
          tags$span(
            class = "badge bg-success px-3 py-2 fs-6",
            "Niveau Zone de santé"
          ),
          h3(class = "mb-0", "Analyse par zone de santé")
        ),
        div(
          class = "d-flex align-items-center gap-3",
          tags$label(
            `for` = ns("selected_hz"),
            class = "fw-bold text-dark mb-0 fs-6 text-nowrap",
            tags$i(class = "bi bi-geo-alt-fill text-success me-1"),
            "Zone de santé :"
          ),
          pickerInput(
            inputId = ns("selected_hz"),
            label = NULL,
            choices = split(all_hz_individual, trends_smooth_adeq$Province[match(all_hz_individual, trends_smooth_adeq$zone_sante_notification)]),
            selected = if ("Bunia" %in% all_hz_individual) "Bunia" else all_hz_individual[1],
            options = list(
              `live-search` = TRUE,
              `container` = "body",
              size = 12,
              `style` = "btn-light border fw-semibold shadow-sm",
              title = "Sélectionner une zone de santé..."
            ),
            width = "320px"
          )
        )
      ),

      # --- Interactive notification performance map -------------------------
      div(
        class = "mb-4",
        notification_map_ui(ns("map")),

        # Map-linked longitudinal tables mirror the national Table 1/Table 2
        # structure and respond directly to the selected health zone.
        layout_columns(
          col_widths = c(6, 6),
          gap = "16px",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span(
                tags$i(class = "bi bi-table me-2 text-secondary"),
                textOutput(ns("map_title_table1"), inline = TRUE)
              ),
              downloadButton(
                ns("download_map_table1"),
                label = "XLSX",
                class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
                title = "Télécharger le tableau au format Excel (.xlsx)"
              )
            ),
            card_body(
              DTOutput(ns("map_table1_html"))
            )
          ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span(
                tags$i(class = "bi bi-calculator me-2 text-secondary"),
                textOutput(ns("map_title_table2"), inline = TRUE)
              ),
              downloadButton(
                ns("download_map_table2"),
                label = "XLSX",
                class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
                title = "Télécharger le tableau au format Excel (.xlsx)"
              )
            ),
            card_body(
              DTOutput(ns("map_table2_html"))
            )
          )
        )
      ),

      # --- 3) Deux graphiques côte à côte pour la zone de santé sélectionnée -
      layout_columns(
        col_widths = c(6, 6),
        gap = "16px",
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              tags$i(class = "bi bi-graph-up me-2 text-success"),
              textOutput(ns("title_hz_alert"), inline = TRUE)
            ),
            downloadButton(
              ns("download_plot_alert_hz"),
              label = "PNG",
              class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
              title = "Télécharger le graphique en PNG (300 DPI)"
            )
          ),
          card_body(
            class = "p-2",
            div(
              class = "alert-trend-tabs",
              navset_pill(
                id = ns("hz_alert_metric"),
                selected = "case",
                nav_panel(
                  title = "Cas",
                  value = "case",
                  plotlyOutput(
                    ns("per_hz_case_interactive"),
                    height = "380px",
                    width = "100%"
                  )
                ),
                nav_panel(
                  title = "Décès",
                  value = "death",
                  plotlyOutput(
                    ns("per_hz_death_interactive"),
                    height = "380px",
                    width = "100%"
                  )
                )
              )
            )
          )
        ),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(
              tags$i(class = "bi bi-bar-chart-fill me-2 text-success"),
              textOutput(ns("title_hz_adeq"), inline = TRUE)
            ),
            downloadButton(
              ns("download_plot_adeq_hz"),
              label = "PNG",
              class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
              title = "Télécharger le graphique en PNG (300 DPI)"
            )
          ),
          card_body(
            class = "p-2",
            plotlyOutput(ns("p_adeq_hz"), height = "380px", width = "100%"),
            tags$p(
              class = "plot-footnote text-muted mt-2 mb-0",
              textOutput(ns("p_adeq_hz_footnote"), inline = TRUE)
            )
          )
        )
      ),

      # --- 4) Deux tableaux sous les graphiques de zones de santé ------------
      div(
        class = "mt-4 pt-2 border-top",
        div(
          class = "d-flex align-items-center justify-content-between flex-wrap gap-3 mb-3",
          div(
            class = "d-flex align-items-center gap-2",
            tags$span(
              class = "badge bg-secondary px-3 py-2 fs-6",
              "Tableaux comparatifs"
            ),
            h4(class = "mb-0", "Tableaux par zone de santé")
          ),
          div(
            class = "d-flex align-items-center gap-2",
            tags$label(
              `for` = ns("table_time_window"),
              class = "fw-bold text-dark mb-0 fs-6 text-nowrap",
              tags$i(class = "bi bi-calendar-event-fill text-secondary me-1"),
              "Fenêtre temporelle :"
            ),
            pickerInput(
              inputId = ns("table_time_window"),
              label = NULL,
              choices = time_window_choices,
              selected = max(all_time_windows),
              options = list(
                `live-search` = TRUE,
                `container` = "body",
                size = 10,
                `style` = "btn-light border fw-semibold shadow-sm",
                title = "Choisir une fenêtre..."
              ),
              width = "260px"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          gap = "16px",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span(
                tags$i(class = "bi bi-table me-2 text-secondary"),
                textOutput(ns("title_table1"), inline = TRUE)
              ),
              downloadButton(
                ns("download_table1_hz"),
                label = "XLSX",
                class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
                title = "Télécharger le tableau au format Excel (.xlsx)"
              )
            ),
            card_body(
              DTOutput(ns("table1_html"))
            )
          ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span(
                tags$i(class = "bi bi-calculator me-2 text-secondary"),
                textOutput(ns("title_table2"), inline = TRUE)
              ),
              downloadButton(
                ns("download_table2_hz"),
                label = "XLSX",
                class = "btn btn-sm btn-outline-success py-0 px-2 fw-semibold",
                title = "Télécharger le tableau au format Excel (.xlsx)"
              )
            ),
            card_body(
              DTOutput(ns("table2_html"))
            )
          )
        )
      )
    )
  )
}

trends_server <- function(id, filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # =========================================================================
    # GROUPE 1 : Ensemble de la zone affectée (Rendus)
    # =========================================================================

    ensemble_alert_metric <- reactive({
      match.arg(
        input$ensemble_alert_metric %||% "case",
        choices = c("case", "death")
      )
    })

    hz_alert_metric <- reactive({
      match.arg(
        input$hz_alert_metric %||% "case",
        choices = c("case", "death")
      )
    })

    # The map and picker both change the same selection state. Keeping the
    # reactive value in this module avoids a click/update feedback loop.
    default_selected_hz <- if ("Bunia" %in% all_hz_individual) {
      "Bunia"
    } else {
      all_hz_individual[1]
    }
    selected_hz_value <- reactiveVal(default_selected_hz)

    observeEvent(input$selected_hz, {
      hz <- input$selected_hz
      if (!is.null(hz) && length(hz) == 1L && !is.na(hz) &&
          hz %in% all_hz_individual && !identical(hz, selected_hz_value())) {
        selected_hz_value(hz)
      }
    }, ignoreInit = TRUE)

    map_selection <- notification_map_server(
      "map",
      map_data = notification_map_data,
      selected_hz = selected_hz_value,
      province_data = province_map_data,
      province_labels = province_label_data,
      drc_boundary = drc_boundary_data
    )

    observeEvent(map_selection(), {
      hz <- map_selection()
      if (is.null(hz) || length(hz) != 1L || is.na(hz) || !hz %in% all_hz_individual) {
        return()
      }
      if (!identical(hz, selected_hz_value())) {
        selected_hz_value(hz)
      }
      updatePickerInput(
        session,
        inputId = "selected_hz",
        selected = hz
      )
    }, ignoreInit = FALSE)

    output$title_ensemble_alert <- renderText({
      alert_trend_title_fr(ensemble_alert_metric())
    })

    output$title_ensemble_adeq <- renderText({
      switch(
        ensemble_alert_metric(),
        "case"  = "Performance des alertes de cas (adéquation vivants)",
        "death" = "Performance des alertes de décès (adéquation décès)",
        "all"   = "Performance globale des alertes (adéquation cas & décès)"
      )
    })

    # 1a. Graphique de gauche : tendances des alertes de cas
    output$ip_case_ensemble <- renderPlotly({
      plot_alert_trends_interactive(
        data = trends_smooth_adeq,
        hz = "Ensemble de la zone affectée",
        metric = "case",
        colour_by_adequacy = TRUE,
        show_legend = TRUE,
        show_header = FALSE,
        show_caption = FALSE
      ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # 1a'. Graphique de gauche : tendances des alertes de décès
    output$ip_death_ensemble <- renderPlotly({
      plot_alert_trends_interactive(
        data = trends_smooth_adeq,
        hz = "Ensemble de la zone affectée",
        metric = "death",
        colour_by_adequacy = TRUE,
        show_legend = TRUE,
        show_header = FALSE,
        show_caption = FALSE
      ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # 1b. Graphique de droite : p_adeq_ensemble (version interactive)
    output$p_adeq_ensemble <- renderPlotly({
      plot_adequacy_stacked_interactive(
        data = trends_smooth_adeq,
        hz = "Ensemble de la zone affectée",
        metric = ensemble_alert_metric(),
        show_legend = TRUE,
        show_header = FALSE,
        show_caption = FALSE
      ) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$p_adeq_ensemble_footnote <- renderText({
      plot_adequacy_stacked(
        data = trends_smooth_adeq,
        hz = "Ensemble de la zone affectée",
        metric = ensemble_alert_metric()
      ) |>
        plot_adequacy_footnote()
    })

    # 2a. Tableau 1 Ensemble (scrollable)
    output$table1_ensemble_html <- renderDT({
      df <- get_table1_ensemble_df(trends_smooth_adeq)
      render_scrollable_dt(
        df = df,
        col_names = unname(table1_ensemble_labels_fr[names(df)]),
        num_cols_2 = c("case_adequacy", "death_adequacy", "aai", "Alert_case_threshold", "Alert_death_threshold")
      )
    })

    # 2b. Tableau 2 Ensemble (scrollable)
    output$table2_ensemble_html <- renderDT({
      df <- get_table2_ensemble_df(intermediate_params)
      render_scrollable_dt(
        df = df,
        col_names = unname(table2_ensemble_labels_fr[names(df)]),
        num_cols_1 = c("estimated_true_cases_recent"),
        num_cols_3 = c("beta_c", "beta_d", "detection_rate_adj")
      )
    })

    # =========================================================================
    # GROUPE 2 : Analyse par zone de santé (Rendus)
    # =========================================================================

    # Titres dynamiques basés sur la zone choisie
    output$title_hz_alert <- renderText({
      hz <- selected_hz_value() %||% "Zone de santé"
      alert_trend_title_fr(hz_alert_metric(), hz = hz)
    })

    output$title_hz_adeq <- renderText({
      hz <- selected_hz_value() %||% "Zone de santé"
      metric_label <- switch(
        hz_alert_metric(),
        "case"  = "alertes de cas (adéquation vivants)",
        "death" = "alertes de décès (adéquation décès)",
        "all"   = "alertes"
      )
      paste0("Performance des ", metric_label, " : ", hz)
    })

    output$map_title_table1 <- renderText({
      paste0(
        "Tableau 1 — Alertes, seuils et performance : ",
        selected_hz_value() %||% "Zone de santé"
      )
    })

    output$map_title_table2 <- renderText({
      paste0(
        "Tableau 2 — Paramètres du modèle : ",
        selected_hz_value() %||% "Zone de santé"
      )
    })

    # 3a. Graphique de gauche : per_hz_case_interactive (série temporelle complète)
    output$per_hz_case_interactive <- renderPlotly({
      req(selected_hz_value())
      plot_alert_trends_interactive(
        data = trends_smooth_adeq,
        hz = selected_hz_value(),
        metric = "case",
        colour_by_adequacy = TRUE,
        show_legend = TRUE,
        show_header = FALSE,
        show_caption = FALSE
      ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # 3a'. Graphique de gauche : tendances des alertes de décès
    output$per_hz_death_interactive <- renderPlotly({
      req(selected_hz_value())
      plot_alert_trends_interactive(
        data = trends_smooth_adeq,
        hz = selected_hz_value(),
        metric = "death",
        colour_by_adequacy = TRUE,
        show_legend = TRUE,
        show_header = FALSE,
        show_caption = FALSE
      ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # 3b. Graphique de droite : p_adeq_hz (version interactive, série complète)
    output$p_adeq_hz <- renderPlotly({
      req(selected_hz_value())
      plot_adequacy_stacked_interactive(
        data = trends_smooth_adeq,
        hz = selected_hz_value(),
        metric = hz_alert_metric(),
        show_legend = TRUE,
        show_header = FALSE,
        show_caption = FALSE
      ) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$p_adeq_hz_footnote <- renderText({
      req(selected_hz_value())
      plot_adequacy_stacked(
        data = trends_smooth_adeq,
        hz = selected_hz_value(),
        metric = hz_alert_metric()
      ) |>
        plot_adequacy_footnote()
    })

    # Tableaux longitudinaux associés à la carte (structure nationale)
    output$map_table1_html <- renderDT({
      df <- get_table1_selected_hz_df(trends_smooth_adeq, selected_hz_value())
      render_scrollable_dt(
        df = df,
        col_names = unname(table1_ensemble_labels_fr[names(df)]),
        num_cols_2 = c(
          "case_adequacy", "death_adequacy", "aai",
          "Alert_case_threshold", "Alert_death_threshold"
        )
      )
    })

    output$map_table2_html <- renderDT({
      df <- get_table2_selected_hz_df(
        intermediate_params,
        trends_smooth_adeq,
        selected_hz_value()
      )
      render_scrollable_dt(
        df = df,
        col_names = unname(table2_ensemble_labels_fr[names(df)]),
        num_cols_1 = c("estimated_true_cases_recent"),
        num_cols_3 = c("beta_c", "beta_d", "detection_rate_adj")
      )
    })

    # Titres dynamiques des tableaux selon la fenêtre sélectionnée
    output$title_table1 <- renderText({
      win <- input$table_time_window %||% max(all_time_windows)
      win_label <- format(as.Date(win), "%d %b %Y")
      paste0("Tableau 1 — Alertes, seuils et performance (Fenêtre du ", win_label, ")")
    })

    output$title_table2 <- renderText({
      win <- input$table_time_window %||% max(all_time_windows)
      win_label <- format(as.Date(win), "%d %b %Y")
      paste0("Tableau 2 — Paramètres du modèle (Fenêtre du ", win_label, ")")
    })

    # 4a. Tableau 1 par zone de santé (filtré par la fenêtre sélectionnée)
    output$table1_html <- renderDT({
      win <- input$table_time_window %||% max(all_time_windows)
      df <- get_table1_hz_df(trends_smooth_adeq, intermediate_params, time_key = win)
      render_scrollable_dt(
        df = df,
        col_names = unname(table1_labels_fr[names(df)]),
        num_cols_2 = c("case_adequacy", "death_adequacy", "aai", "Alert_case_threshold", "Alert_death_threshold")
      )
    })

    # 4b. Tableau 2 par zone de santé (filtré par la fenêtre sélectionnée)
    output$table2_html <- renderDT({
      win <- input$table_time_window %||% max(all_time_windows)
      df <- get_table2_hz_df(intermediate_params, trends_smooth_adeq, time_key = win)
      render_scrollable_dt(
        df = df,
        col_names = unname(table2_labels_fr[names(df)]),
        num_cols_1 = c("estimated_true_cases_recent"),
        num_cols_3 = c("beta_c", "beta_d", "detection_rate_adj")
      )
    })

    # =========================================================================
    # Téléchargements : Graphiques et Tableaux
    # =========================================================================

    # 1. Graphique Tendances Ensemble (PNG)
    output$download_plot_alert_ensemble <- downloadHandler(
      filename = function() {
        metric_label <- tools::toTitleCase(alert_metric_label_fr(ensemble_alert_metric()))
        paste0(
          "BVD_Tendances_",
          metric_label,
          "_Ensemble_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        metric <- ensemble_alert_metric()
        p <- plot_alert_trends(
          data = trends_smooth_adeq,
          hz = "Ensemble de la zone affectée",
          metric = metric,
          colour_by_adequacy = TRUE
        )
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )

    # 2. Graphique Performance Ensemble (PNG)
    output$download_plot_adeq_ensemble <- downloadHandler(
      filename = function() {
        paste0("BVD_Adequation_Ensemble_", ensemble_alert_metric(), "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- plot_adequacy_stacked(
          data = trends_smooth_adeq,
          hz = "Ensemble de la zone affectée",
          metric = ensemble_alert_metric()
        )
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )

    # 3. Graphique Tendances Zone de santé (PNG)
    output$download_plot_alert_hz <- downloadHandler(
      filename = function() {
        hz <- selected_hz_value() %||% "Zone_sante"
        metric_label <- tools::toTitleCase(alert_metric_label_fr(hz_alert_metric()))
        paste0(
          "BVD_Tendances_",
          metric_label,
          "_",
          gsub("[^A-Za-z0-9_]+", "_", hz),
          "_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        hz <- selected_hz_value() %||% all_hz_individual[1]
        metric <- hz_alert_metric()
        p <- plot_alert_trends(
          data = trends_smooth_adeq,
          hz = hz,
          metric = metric,
          colour_by_adequacy = TRUE
        )
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )

    # 4. Graphique Performance Zone de santé (PNG)
    output$download_plot_adeq_hz <- downloadHandler(
      filename = function() {
        hz <- selected_hz_value() %||% "Zone_sante"
        paste0(
          "BVD_Adequation_",
          gsub("[^A-Za-z0-9_]+", "_", hz),
          "_",
          hz_alert_metric(),
          "_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        hz <- selected_hz_value() %||% all_hz_individual[1]
        p <- plot_adequacy_stacked(
          data = trends_smooth_adeq,
          hz = hz,
          metric = hz_alert_metric()
        )
        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )

    # 5. Tableau 1 Ensemble (XLSX)
    output$download_table1_ensemble <- downloadHandler(
      filename = function() {
        paste0("BVD_Tableau1_Ensemble_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- get_table1_ensemble_df(trends_smooth_adeq)
        df_export <- rename_df_fr(df, table1_ensemble_labels_fr)
        writexl::write_xlsx(df_export, path = file)
      }
    )

    # 6. Tableau 2 Ensemble (XLSX)
    output$download_table2_ensemble <- downloadHandler(
      filename = function() {
        paste0("BVD_Tableau2_Parametres_Ensemble_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- get_table2_ensemble_df(intermediate_params)
        df_export <- rename_df_fr(df, table2_ensemble_labels_fr)
        writexl::write_xlsx(df_export, path = file)
      }
    )

    # 7. Tableau 1 Zone de santé (XLSX)
    output$download_table1_hz <- downloadHandler(
      filename = function() {
        win <- input$table_time_window %||% max(all_time_windows)
        paste0("BVD_Tableau1_Zones_Fenetre_", win, ".xlsx")
      },
      content = function(file) {
        win <- input$table_time_window %||% max(all_time_windows)
        df <- get_table1_hz_df(trends_smooth_adeq, intermediate_params, time_key = win)
        df_export <- rename_df_fr(df, table1_labels_fr)
        writexl::write_xlsx(df_export, path = file)
      }
    )

    # 8. Tableau 2 Zone de santé (XLSX)
    output$download_table2_hz <- downloadHandler(
      filename = function() {
        win <- input$table_time_window %||% max(all_time_windows)
        paste0("BVD_Tableau2_Parametres_Zones_Fenetre_", win, ".xlsx")
      },
      content = function(file) {
        win <- input$table_time_window %||% max(all_time_windows)
        df <- get_table2_hz_df(intermediate_params, trends_smooth_adeq, time_key = win)
        df_export <- rename_df_fr(df, table2_labels_fr)
        writexl::write_xlsx(df_export, path = file)
      }
    )

    output$download_map_table1 <- downloadHandler(
      filename = function() {
        hz <- selected_hz_value() %||% "Zone_sante"
        paste0(
          "BVD_Tableau1_National_",
          gsub("[^A-Za-z0-9_]+", "_", hz),
          "_",
          Sys.Date(),
          ".xlsx"
        )
      },
      content = function(file) {
        df <- get_table1_selected_hz_df(trends_smooth_adeq, selected_hz_value())
        df_export <- rename_df_fr(df, table1_ensemble_labels_fr)
        writexl::write_xlsx(df_export, path = file)
      }
    )

    output$download_map_table2 <- downloadHandler(
      filename = function() {
        hz <- selected_hz_value() %||% "Zone_sante"
        paste0(
          "BVD_Tableau2_National_",
          gsub("[^A-Za-z0-9_]+", "_", hz),
          "_",
          Sys.Date(),
          ".xlsx"
        )
      },
      content = function(file) {
        df <- get_table2_selected_hz_df(
          intermediate_params,
          trends_smooth_adeq,
          selected_hz_value()
        )
        df_export <- rename_df_fr(df, table2_ensemble_labels_fr)
        writexl::write_xlsx(df_export, path = file)
      }
    )
  })
}
