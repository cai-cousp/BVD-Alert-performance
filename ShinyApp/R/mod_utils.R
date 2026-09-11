# =============================================================================
# BVD Alerts App — Shared utilities: theme, filter module, helpers
# =============================================================================



# --- Theme configuration -----------------------------------------------------
create_app_theme <- function() {
  bs_theme(
    version   = 5,
    bootswatch = "litera",
    primary   = "#0D6EFD",
    secondary = "#6C757D",
    success   = "#198754",
    danger    = "#DC3545",
    warning   = "#FFC107",
    info      = "#0DCAF0",
    base_font    = font_collection("Inter", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = font_collection("Inter", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    "font-size-base"     = "1rem",
    "card-border-radius" = "0.75rem",
    "card-border-width"  = "0",
    "card-cap-bg"        = "transparent",
    "navbar-bg"          = "#ffffff",
    "navbar-light-color" = "#495057",
    "navbar-light-active-color" = "#0D6EFD"
  )
}

# --- Filter module UI (horizontal, compact) ---------------------------------
# Designed to sit at the top of a tab as a card-based filter bar.
filter_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "filter-bar",
    layout_column_wrap(
      width = 1 / 4,
      heights_equal = "all",
      gap = "16px",
      fill = FALSE,

      # Date range
      pickerInput(
        inputId = ns("week_start"),
        label = "Start week",
        choices = setNames(as.character(all_weeks), format(all_weeks, "%d %b %Y")),
        selected = as.character(date_range_full[1]),
        options = list(`live-search` = TRUE, size = 10),
        width = "100%"
      ),

      pickerInput(
        inputId = ns("week_end"),
        label = "End week",
        choices = setNames(as.character(all_weeks), format(all_weeks, "%d %b %Y")),
        selected = as.character(date_range_full[2]),
        options = list(`live-search` = TRUE, size = 10),
        width = "100%"
      ),

      # Health zone multi-select grouped by province
      pickerInput(
        inputId = ns("selected_hzs"),
        label = "Health zones",
        choices = split(all_hz, hz_metadata$Province[match(all_hz, hz_metadata$zone_sante_notification)]),
        selected = all_hz,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        width = "100%"
      ),

      # Alert level + adequacy combined
      div(
        radioButtons(
          inputId = ns("alert_level"),
          label = "Alert type",
          choices = c(
            "All"   = "all",
            "Cases" = "case",
            "Deaths" = "death"
          ),
          selected = "all",
          inline = TRUE
        )
      )
    )
  )
}

# --- Filter module server ----------------------------------------------------
filter_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Validate date range
    observeEvent(input$week_start, {
      req(input$week_start, input$week_end)
      if (as.Date(input$week_start) > as.Date(input$week_end)) {
        updatePickerInput(session, "week_end",
                          selected = input$week_start)
      }
    })

    list(
      date_range = reactive({
        if (is.null(input$week_start) || is.null(input$week_end)) {
          return(date_range_full)
        }
        c(as.Date(input$week_start), as.Date(input$week_end))
      }),
      selected_hzs = reactive({
        if (is.null(input$selected_hzs)) {
          return(all_hz)
        }
        input$selected_hzs
      }),
      alert_level = reactive({
        input$alert_level %||% "all"
      }),
      adequacy_filter = reactive({
        c("Under-alerting", "Adequate", "Over-alerting")
      })
    )
  })
}

# --- Shared reactive: filtered trends ----------------------------------------
filtered_trends <- function(filters) {
  reactive({
    dr <- filters$date_range()
    hzs <- filters$selected_hzs()

    trends_smooth |>
      filter(
        .data$week_start >= dr[1],
        .data$week_start <= dr[2],
        .data$zone_sante_notification %in% hzs
      )
  })
}

# --- Shared reactive: filtered synthesis -------------------------------------
filtered_synthesis <- function(filters) {
  reactive({
    dr <- filters$date_range()
    hzs <- filters$selected_hzs()

    synthesis |>
      filter(
        .data$week_start >= dr[1],
        .data$week_start <= dr[2],
        .data$zone_sante_notification %in% hzs
      )
  })
}

# --- Formatting helpers ------------------------------------------------------
format_pct <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}

format_number <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", big.mark = ",", digits = digits)
}

title_case_hz <- function(x) {
  str_to_title(x)
}

# --- Color constants ---------------------------------------------------------
adequacy_colors <- c(
  "Under-alerting" = "#DC3545",
  "Adequate"       = "#198754",
  "Over-alerting"  = "#FFC107"
)

trend_icons <- c(
  "Increasing" = "arrow-up",
  "Decreasing" = "arrow-down",
  "Stable"     = "minus",
  "Unknown"    = "question"
)

approach_colors <- c(
  "Approach A (CMR)"            = "#0D6EFD",
  "Approach B (Beni)"           = "#FD7E14",
  "Approach C (Case-derived)"   = "#198754",
  "Consensus"                   = "#6C757D"
)

# --- DT datatable scrollable helper (publication-ready style) ----------------
render_scrollable_dt <- function(df, col_names = NULL, title = NULL,
                                 scrollY = "350px",
                                 num_cols_0 = NULL,
                                 num_cols_1 = NULL, num_cols_2 = NULL, num_cols_3 = NULL,
                                 adequacy_cols = NULL) {
  if (is.null(df) || nrow(df) == 0L) {
    return(DT::datatable(
      data.frame(Message = "Aucune donnée disponible"),
      rownames = FALSE,
      options = list(dom = "t")
    ))
  }

  df_display <- df

  # Format threshold_time_key date into standard French publication date format (DD/MM/YYYY)
  if ("threshold_time_key" %in% names(df_display)) {
    raw_dates <- df_display$threshold_time_key
    formatted_dates <- tryCatch(
      format(as.Date(raw_dates), "%d/%m/%Y"),
      error = function(e) raw_dates
    )
    if (!any(is.na(formatted_dates))) {
      df_display$threshold_time_key <- formatted_dates
    }
  }

  # Build column alignment definitions
  col_classes <- vapply(df_display, function(x) {
    if (is.numeric(x)) "numeric"
    else if (inherits(x, "Date") || inherits(x, "POSIXt")) "date"
    else "text"
  }, character(1))

  # Treat threshold_time_key as date column
  if ("threshold_time_key" %in% names(df_display)) {
    col_classes["threshold_time_key"] <- "date"
  }

  date_targets <- which(col_classes == "date") - 1L
  num_targets  <- which(col_classes == "numeric") - 1L
  text_targets <- which(col_classes == "text") - 1L

  column_defs <- list()
  if (length(date_targets) > 0L) {
    column_defs[[length(column_defs) + 1L]] <- list(
      className = "dt-center text-center",
      targets = as.list(date_targets)
    )
  }
  if (length(num_targets) > 0L) {
    column_defs[[length(column_defs) + 1L]] <- list(
      className = "dt-right text-end font-monospace-numbers",
      targets = as.list(num_targets)
    )
  }
  if (length(text_targets) > 0L) {
    column_defs[[length(column_defs) + 1L]] <- list(
      className = "dt-left text-start",
      targets = as.list(text_targets)
    )
  }

  dt <- DT::datatable(
    df_display,
    rownames = FALSE,
    caption = title,
    colnames = if (!is.null(col_names)) col_names else names(df_display),
    options = list(
      paging = FALSE,
      scrollY = scrollY,
      scrollX = TRUE,
      scrollCollapse = TRUE,
      dom = "ti",
      autoWidth = FALSE,
      columnDefs = column_defs
    ),
    class = "publication-table compact stripe hover",
    style = "bootstrap4"
  )

  # Integer formatting with thousand separator
  if (!is.null(num_cols_0)) {
    valid_cols_0 <- intersect(num_cols_0, names(df_display))
    if (length(valid_cols_0) > 0L) {
      dt <- DT::formatRound(dt, valid_cols_0, digits = 0, interval = 3, mark = " ")
    }
  }

  # 1 decimal formatting with thousand separator
  if (!is.null(num_cols_1)) {
    valid_cols_1 <- intersect(num_cols_1, names(df_display))
    if (length(valid_cols_1) > 0L) {
      dt <- DT::formatRound(dt, valid_cols_1, digits = 1, interval = 3, mark = " ")
    }
  }

  # 2 decimals formatting
  if (!is.null(num_cols_2)) {
    valid_cols_2 <- intersect(num_cols_2, names(df_display))
    if (length(valid_cols_2) > 0L) {
      dt <- DT::formatRound(dt, valid_cols_2, digits = 2)
    }
  }

  # 3 decimals formatting
  if (!is.null(num_cols_3)) {
    valid_cols_3 <- intersect(num_cols_3, names(df_display))
    if (length(valid_cols_3) > 0L) {
      dt <- DT::formatRound(dt, valid_cols_3, digits = 3)
    }
  }

  # Adequacy performance color highlight (Under-alerting / Adequate / Over-alerting)
  if (is.null(adequacy_cols)) {
    adequacy_cols <- intersect(c("case_adequacy", "death_adequacy", "aai"), names(df_display))
  } else {
    adequacy_cols <- intersect(adequacy_cols, names(df_display))
  }

  if (length(adequacy_cols) > 0L) {
    dt <- DT::formatStyle(
      dt,
      columns = adequacy_cols,
      backgroundColor = DT::styleInterval(
        c(0.75, 1.25),
        c("rgba(220, 53, 69, 0.12)", "rgba(25, 135, 84, 0.12)", "rgba(255, 193, 7, 0.20)")
      ),
      color = DT::styleInterval(
        c(0.75, 1.25),
        c("#991b1b", "#166534", "#854d0e")
      ),
      fontWeight = "600"
    )
  }

  # Emphasize first column (stub) and global AAI
  first_col <- names(df_display)[1]
  if (!is.null(first_col) && first_col %in% c("threshold_time_key", "zone_sante_notification")) {
    dt <- DT::formatStyle(dt, columns = first_col, fontWeight = "600")
  }
  if ("aai" %in% names(df_display)) {
    dt <- DT::formatStyle(dt, columns = "aai", fontWeight = "700")
  }

  dt
}

