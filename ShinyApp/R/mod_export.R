# =============================================================================
# BVD Alerts Dashboard — Export module: report download, data export
# =============================================================================

export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Methodology & Reports documentation sits at bottom of main page
    tags$section(
      id = ns("documentation"),
      class = "app-documentation mb-5",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-3 mb-4",
        div(
          class = "d-flex align-items-center",
          tags$span(
            class = "badge bg-primary me-2 px-3 py-2 fs-6",
            "Documentation"
          ),
          h3(class = "mb-0", "Méthodologie & Rapports")
        ),
        div(
          class = "d-flex gap-2 flex-wrap",
          tags$a(
            id = ns("view_report"),
            href = "Alert_performance_report.html",
            target = "_blank",
            class = "btn btn-outline-primary",
            tags$span(
              class = "btn-content",
              bs_icon("box-arrow-up-right", class = "me-1"),
              "Consulter le rapport"
            )
          ),
          tags$a(
            id = ns("view_methods"),
            href = "methods_note_alert_thresholds_fr.html",
            target = "_blank",
            class = "btn btn-outline-primary",
            tags$span(
              class = "btn-content",
              bs_icon("file-earmark-medical", class = "me-1"),
              "Consulter la note méthodologique"
            )
          ),
          actionButton(
            ns("open_browser_report"),
            label = tags$span(
              bs_icon("window-stack", class = "me-1"),
              "Ouvrir le rapport (navigateur)"
            ),
            class = "btn btn-sm btn-outline-secondary",
            title = "Ouvre le rapport directement dans votre navigateur par défaut (Google Chrome / Safari)"
          ),
          actionButton(
            ns("open_browser_methods"),
            label = tags$span(
              bs_icon("window-stack", class = "me-1"),
              "Ouvrir la méthodologie (navigateur)"
            ),
            class = "btn btn-sm btn-outline-secondary",
            title = "Ouvre la note méthodologique directement dans votre navigateur par défaut (Google Chrome / Safari)"
          )
        )
      ),
      tags$script(HTML("
        (function() {
          function getStaticUrl(filename) {
            var isIframeApp = window.location.pathname.indexOf('/app_') !== -1;
            return isIframeApp ? '../' + filename : filename;
          }

          function syncDocElements() {
            var reportUrl = getStaticUrl('Alert_performance_report.html');
            var methodsUrl = getStaticUrl('methods_note_alert_thresholds_fr.html');
            var viewReportBtn = document.getElementById('export-view_report');
            if (viewReportBtn) viewReportBtn.setAttribute('href', reportUrl);
            var viewMethodsBtn = document.getElementById('export-view_methods');
            if (viewMethodsBtn) viewMethodsBtn.setAttribute('href', methodsUrl);
          }

          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', syncDocElements);
          } else {
            syncDocElements();
          }
          setTimeout(syncDocElements, 500);
          setTimeout(syncDocElements, 2000);
        })();
      "))
    )
  )
}

export_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {

    filtered_trends_data <- filtered_trends(filters)
    filtered_synth_data <- filtered_synthesis(filters)

    # --- External Browser Observers -------------------------------------------
    observeEvent(input$open_browser_report, {
      target_dir <- if (exists("app_dir", inherits = TRUE)) app_dir else NULL
      report_html <- find_alert_report_html(target_dir)

      if (!is.null(report_html) && file.exists(report_html)) {
        utils::browseURL(normalizePath(report_html))
        showNotification(
          "Rapport ouvert dans votre navigateur par défaut.",
          type = "message",
          duration = 4
        )
      } else {
        showNotification(
          "Rapport non trouvé sur le disque.",
          type = "warning",
          duration = 4
        )
      }
    })

    observeEvent(input$open_browser_methods, {
      target_dir <- if (exists("app_dir", inherits = TRUE)) app_dir else NULL
      methods_html <- find_alert_methods_html(target_dir)

      if (!is.null(methods_html) && file.exists(methods_html)) {
        utils::browseURL(normalizePath(methods_html))
        showNotification(
          "Note méthodologique ouverte dans votre navigateur par défaut.",
          type = "message",
          duration = 4
        )
      } else {
        showNotification(
          "Note méthodologique non trouvée sur le disque.",
          type = "warning",
          duration = 4
        )
      }
    })
  })
}

# --- Helper: locate pre-rendered Alert_performance_report.html ----------------
find_alert_report_html <- function(app_directory = NULL) {
  explicit_app_dir <- !is.null(app_directory)
  if (!explicit_app_dir) {
    if (exists("app_dir", envir = parent.frame())) {
      app_directory <- get("app_dir", envir = parent.frame())
    } else if (exists("app_dir", envir = .GlobalEnv)) {
      app_directory <- get("app_dir", envir = .GlobalEnv)
    }
  }

  detected_root <- NULL
  if (!explicit_app_dir) {
    if (exists("root_dir", envir = parent.frame())) {
      detected_root <- get("root_dir", envir = parent.frame())
    } else if (exists("root_dir", envir = .GlobalEnv)) {
      detected_root <- get("root_dir", envir = .GlobalEnv)
    }
  }
  if (is.null(detected_root) && !is.null(app_directory)) {
    detected_root <- dirname(app_directory)
  }

  # Primary candidates: Alert_performance_report.html
  primary_candidates <- unique(c(
    if (!is.null(detected_root)) file.path(detected_root, "docs", "reports", "Alert_performance_report.html"),
    if (!is.null(app_directory)) file.path(dirname(app_directory), "docs", "reports", "Alert_performance_report.html"),
    file.path(getwd(), "docs", "reports", "Alert_performance_report.html"),
    file.path(getwd(), "..", "docs", "reports", "Alert_performance_report.html"),
    file.path("..", "docs", "reports", "Alert_performance_report.html"),
    file.path("docs", "reports", "Alert_performance_report.html"),
    if (!is.null(app_directory)) file.path(app_directory, "Alert_performance_report.html"),
    if (!is.null(app_directory)) file.path(app_directory, "www", "Alert_performance_report.html"),
    file.path(getwd(), "ShinyApp", "Alert_performance_report.html"),
    file.path(getwd(), "Alert_performance_report.html"),
    file.path(getwd(), "www", "Alert_performance_report.html")
  ))

  existing_primary <- primary_candidates[!is.na(primary_candidates) & file.exists(primary_candidates)]
  if (length(existing_primary) > 0L) {
    valid_primary <- existing_primary[file.info(existing_primary)$size > 1000000L]
    if (length(valid_primary) > 0L) {
      existing_primary <- valid_primary
    }
    if (length(existing_primary) > 1L) {
      mtimes <- file.info(existing_primary)$mtime
      best_idx <- which.max(mtimes)
      return(normalizePath(existing_primary[[best_idx]], mustWork = FALSE))
    }
    return(normalizePath(existing_primary[[1L]], mustWork = FALSE))
  }

  # Fallback candidates: legacy report_template.html
  fallback_candidates <- unique(c(
    if (!is.null(detected_root)) file.path(detected_root, "docs", "reports", "report_template.html"),
    if (!is.null(app_directory)) file.path(app_directory, "report_template.html"),
    if (!is.null(app_directory)) file.path(app_directory, "www", "report_template.html"),
    if (!is.null(app_directory)) file.path(dirname(app_directory), "docs", "reports", "report_template.html"),
    file.path(getwd(), "docs", "reports", "report_template.html"),
    file.path(getwd(), "ShinyApp", "report_template.html"),
    file.path(getwd(), "report_template.html"),
    file.path(getwd(), "www", "report_template.html"),
    file.path("..", "docs", "reports", "report_template.html"),
    file.path("docs", "reports", "report_template.html")
  ))

  existing_fallback <- fallback_candidates[!is.na(fallback_candidates) & file.exists(fallback_candidates)]
  if (length(existing_fallback) > 0L) {
    if (length(existing_fallback) > 1L) {
      mtimes <- file.info(existing_fallback)$mtime
      best_idx <- which.max(mtimes)
      return(normalizePath(existing_fallback[[best_idx]], mustWork = FALSE))
    }
    return(normalizePath(existing_fallback[[1L]], mustWork = FALSE))
  }

  NULL
}

# Alias for backward compatibility
find_report_template_html <- find_alert_report_html

# --- Helper: locate pre-rendered methods_note_alert_thresholds_fr.html --------
find_alert_methods_html <- function(app_directory = NULL) {
  explicit_app_dir <- !is.null(app_directory)
  if (!explicit_app_dir) {
    if (exists("app_dir", envir = parent.frame())) {
      app_directory <- get("app_dir", envir = parent.frame())
    } else if (exists("app_dir", envir = .GlobalEnv)) {
      app_directory <- get("app_dir", envir = .GlobalEnv)
    }
  }

  detected_root <- NULL
  if (!explicit_app_dir) {
    if (exists("root_dir", envir = parent.frame())) {
      detected_root <- get("root_dir", envir = parent.frame())
    } else if (exists("root_dir", envir = .GlobalEnv)) {
      detected_root <- get("root_dir", envir = .GlobalEnv)
    }
  }
  if (is.null(detected_root) && !is.null(app_directory)) {
    detected_root <- dirname(app_directory)
  }

  candidates <- unique(c(
    if (!is.null(detected_root)) file.path(detected_root, "docs", "methods", "methods_note_alert_thresholds_fr.html"),
    if (!is.null(app_directory)) file.path(dirname(app_directory), "docs", "methods", "methods_note_alert_thresholds_fr.html"),
    file.path(getwd(), "docs", "methods", "methods_note_alert_thresholds_fr.html"),
    file.path(getwd(), "..", "docs", "methods", "methods_note_alert_thresholds_fr.html"),
    file.path("..", "docs", "methods", "methods_note_alert_thresholds_fr.html"),
    file.path("docs", "methods", "methods_note_alert_thresholds_fr.html"),
    if (!is.null(app_directory)) file.path(app_directory, "methods_note_alert_thresholds_fr.html"),
    if (!is.null(app_directory)) file.path(app_directory, "www", "methods_note_alert_thresholds_fr.html"),
    file.path(getwd(), "ShinyApp", "methods_note_alert_thresholds_fr.html"),
    file.path(getwd(), "methods_note_alert_thresholds_fr.html"),
    file.path(getwd(), "www", "methods_note_alert_thresholds_fr.html")
  ))

  existing <- candidates[!is.na(candidates) & file.exists(candidates)]
  if (length(existing) > 0L) {
    if (length(existing) > 1L) {
      mtimes <- file.info(existing)$mtime
      best_idx <- which.max(mtimes)
      return(normalizePath(existing[[best_idx]], mustWork = FALSE))
    }
    return(normalizePath(existing[[1L]], mustWork = FALSE))
  }

  NULL
}

# --- Fallback simple HTML report generator -----------------------------------
write_simple_html_report <- function(file, trends_dat, synth_dat, filters) {
  report_date <- Sys.Date()
  n_hz <- if (!is.null(trends_dat) && "zone_sante_notification" %in% names(trends_dat)) {
    n_distinct(trends_dat$zone_sante_notification)
  } else 0L
  total_alerts <- if (!is.null(trends_dat) && "total_alerts" %in% names(trends_dat)) {
    sum(trends_dat$total_alerts, na.rm = TRUE)
  } else 0L
  n_weeks <- if (!is.null(trends_dat) && "week_start" %in% names(trends_dat)) {
    n_distinct(trends_dat$week_start)
  } else 0L
  date_start <- as.character(filters$date_range()[1])
  date_end   <- as.character(filters$date_range()[2])

  adeq_df <- if (exists("recent_adequacy", envir = .GlobalEnv)) {
    get("recent_adequacy", envir = .GlobalEnv)
  } else if (!is.null(synth_dat) && "adequacy_category" %in% names(synth_dat)) {
    synth_dat
  } else {
    data.frame(adequacy_category = character(0))
  }

  data_src <- if (exists("data_folder", envir = .GlobalEnv)) {
    get("data_folder", envir = .GlobalEnv)
  } else if (exists("output_base", envir = .GlobalEnv)) {
    get("output_base", envir = .GlobalEnv)
  } else {
    "Operational pipeline"
  }

  adequacy_rows <- paste0(
    sapply(c("Under-alerting", "Adequate", "Over-alerting"), function(cat) {
      n <- if ("adequacy_category" %in% names(adeq_df)) sum(adeq_df$adequacy_category == cat, na.rm = TRUE) else 0L
      paste0("<tr><td>", cat, "</td><td>", n, "</td></tr>")
    }),
    collapse = "\n"
  )

  html_content <- paste0(
    "<!DOCTYPE html>
<html lang='en'>
<head>
<meta charset='UTF-8'>
<title>BVD Alert Report</title>
<style>
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
       max-width: 800px; margin: 40px auto; padding: 0 20px; color: #333; }
h1 { color: #0D6EFD; border-bottom: 2px solid #0D6EFD; padding-bottom: 10px; }
h2 { color: #495057; margin-top: 30px; }
table { border-collapse: collapse; width: 100%; margin: 15px 0; }
th, td { border: 1px solid #dee2e6; padding: 8px 12px; text-align: left; }
th { background-color: #f8f9fa; }
.summary-box { background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 15px 0; }
.footer { margin-top: 40px; font-size: 0.85em; color: #6c757d; }
</style>
</head>
<body>
<h1>BVD Alert Dashboard Report</h1>
<p>Generated: ", report_date, "</p>

<div class='summary-box'>
<h2>Filter Summary</h2>
<ul>
<li><strong>Date range:</strong> ", date_start, " to ", date_end, "</li>
<li><strong>Health zones:</strong> ", n_hz, " selected</li>
<li><strong>Alert level:</strong> ", filters$alert_level(), "</li>
<li><strong>Adequacy filter:</strong> ", paste(filters$adequacy_filter(), collapse = ", "), "</li>
</ul>
</div>

<h2>Key Statistics</h2>
<ul>
<li>Total validated alerts (all HZs, all weeks): <strong>",
formatC(total_alerts, big.mark = ","), "</strong></li>
<li>Number of weeks in selection: <strong>",
n_weeks, "</strong></li>
<li>Number of health zones: <strong>", n_hz, "</strong></li>
</ul>

<h2>Recent Adequacy Summary</h2>
<table>
<tr><th>Adequacy Category</th><th>Number of HZs</th></tr>
", adequacy_rows, "
</table>

<div class='footer'>
<p>This report was generated by the BVD Alerts Dashboard Shiny application.</p>
<p>Data source: ", data_src, "</p>
<p>Methodology: Alert thresholds are computed using three approaches: (A) CMR-based expected deaths, ",
"(B) Beni historical benchmark from EVD10, and (C) case-derived expectations via detection rates, ",
"Rt, and SAR. The consensus threshold averages the relevant approaches.</p>
</div>
</body>
</html>"
  )

  writeLines(html_content, file)
}
