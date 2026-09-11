# =============================================================================
# BVD Alerts App — UI Definition
# =============================================================================
# Single-page layout with frozen centered header at the top, trends analysis,
# health zone monitoring, and export options at the bottom.
# =============================================================================

# --- Source dependencies (guarded against double-loading) --------------------
if (!exists(".bvd_global_loaded") || !.bvd_global_loaded) {
  source("R/global.R")
}
source("R/mod_utils.R")
source("R/mod_trends.R")
if (!exists("notification_map_ui", mode = "function")) {
  source("R/map_data_helpers.R")
  source("R/mod_map.R")
}
source("R/mod_export.R")

# --- UI ----------------------------------------------------------------------
ui <- tagList(
  # Custom CSS & inline full-width enforcement
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    # Plotly and DataTables dependencies for Shinylive / htmlwidgets
    tags$script(src = "https://cdn.plot.ly/plotly-2.35.2.min.js", charset = "utf-8"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdn.datatables.net/1.13.6/css/dataTables.bootstrap5.min.css"
    ),
    tags$script(
      type = "text/javascript",
      src = "https://cdn.datatables.net/1.13.6/js/jquery.dataTables.min.js"
    ),
    tags$script(
      type = "text/javascript",
      src = "https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap5.min.js"
    ),
    tags$style(HTML("
      html, body {
        width: 100% !important;
        max-width: 100% !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      .container-fluid,
      .container-xxl,
      .container-xl,
      .container-lg,
      .container,
      main {
        width: 100% !important;
        max-width: 100% !important;
        margin-left: 0 !important;
        margin-right: 0 !important;
        padding-left: 0 !important;
        padding-right: 0 !important;
      }
      .app-content-body {
        padding: 1.5rem 2rem !important;
      }

      /* Frozen Header: sticky right at top of page, centered */
      .app-frozen-header {
        position: sticky !important;
        top: 0 !important;
        z-index: 1030 !important;
        background-color: #ffffff !important;
        border-bottom: 1px solid #e9ecef !important;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05) !important;
        padding: 1.15rem 2rem !important;
        width: 100% !important;
        text-align: center !important;
      }

      .app-frozen-title {
        font-size: 1.45rem !important;
        font-weight: 700 !important;
        letter-spacing: -0.01em !important;
        color: #212529 !important;
        margin-top: 0 !important;
        margin-bottom: 0.35rem !important;
        text-align: center !important;
      }

      .app-frozen-subtitle {
        font-size: 0.92rem !important;
        color: #6c757d !important;
        line-height: 1.45 !important;
        margin-bottom: 0 !important;
        max-width: 900px !important;
        margin-left: auto !important;
        margin-right: auto !important;
        text-align: center !important;
      }

      @media (max-width: 768px) {
        .app-frozen-header {
          padding: 0.85rem 1rem !important;
        }
        .app-frozen-title {
          font-size: 1.2rem !important;
        }
        .app-frozen-subtitle {
          font-size: 0.82rem !important;
        }
        .app-content-body {
          padding: 1rem !important;
        }
      }

      /* Card Title Block & Icons */
      .card-header-title-block {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        flex: 1 1 auto;
      }
      .card-icon-box {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 38px;
        height: 38px;
        border-radius: 8px;
        font-size: 1.15rem;
        flex-shrink: 0;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
      }
      .card-icon-box.icon-blue {
        background-color: #e0f2fe;
        color: #0284c7;
        border: 1px solid #bae6fd;
      }
      .card-icon-box.icon-green {
        background-color: #dcfce7;
        color: #16a34a;
        border: 1px solid #bbf7d0;
      }
      .card-icon-box.icon-purple {
        background-color: #f3e8ff;
        color: #9333ea;
        border: 1px solid #e9d5ff;
      }
      .card-icon-box.icon-slate {
        background-color: #f1f5f9;
        color: #475569;
        border: 1px solid #e2e8f0;
      }
      .card-title-text-group {
        display: flex;
        flex-direction: column;
        gap: 0.12rem;
      }
      .card-title-main {
        font-size: 0.95rem;
        font-weight: 700;
        color: #0f172a;
        letter-spacing: -0.01em;
        line-height: 1.3;
      }
      .card-title-sub {
        font-size: 0.78rem;
        color: #64748b;
        line-height: 1.35;
        font-weight: 400;
      }
      .card-header-action-group {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        flex-shrink: 0;
      }
      .badge-context {
        font-size: 0.75rem;
        font-weight: 600;
        padding: 0.25rem 0.6rem;
        border-radius: 6px;
        letter-spacing: 0.01em;
      }

      /* Publication Tables */
      .dataTables_wrapper {
        font-size: 0.84rem;
        font-family: inherit;
        margin-top: 0.25rem;
      }
      table.dataTable.publication-table,
      table.publication-table {
        border-collapse: collapse !important;
        width: 100% !important;
        border-top: 2px solid #cbd5e1 !important;
        border-bottom: 2px solid #cbd5e1 !important;
        margin: 0.5rem 0 !important;
      }
      table.dataTable.publication-table thead th,
      table.publication-table thead th {
        font-weight: 700 !important;
        font-size: 0.78rem !important;
        text-transform: uppercase !important;
        letter-spacing: 0.04em !important;
        color: #334155 !important;
        background-color: #f8fafc !important;
        border-top: none !important;
        border-bottom: 2px solid #cbd5e1 !important;
        border-left: none !important;
        border-right: none !important;
        padding: 0.65rem 0.75rem !important;
        vertical-align: bottom !important;
        white-space: nowrap !important;
      }
      table.dataTable.publication-table tbody td,
      table.publication-table tbody td {
        padding: 0.5rem 0.75rem !important;
        border-top: none !important;
        border-bottom: 1px solid #e2e8f0 !important;
        border-left: none !important;
        border-right: none !important;
        vertical-align: middle !important;
        color: #1e293b !important;
        font-size: 0.83rem !important;
      }
      table.dataTable.publication-table,
      table.dataTable.publication-table th,
      table.dataTable.publication-table td,
      table.dataTable.row-border tbody th,
      table.dataTable.row-border tbody td {
        border-left: none !important;
        border-right: none !important;
      }
      .dataTables_scrollHead,
      .dataTables_scrollHeadInner,
      .dataTables_scrollHead table.dataTable,
      .dataTables_scrollHead table.publication-table {
        border-top: none !important;
      }
      .dataTables_scrollHead {
        border-top: 2px solid #cbd5e1 !important;
        border-bottom: 2px solid #cbd5e1 !important;
      }
      .dataTables_scrollBody {
        border-bottom: 2px solid #cbd5e1 !important;
      }
      .dataTables_wrapper.no-footer .dataTables_scrollBody,
      table.dataTable.no-footer {
        border-bottom: 2px solid #cbd5e1 !important;
      }
      table.dataTable.publication-table.stripe tbody tr.odd,
      table.publication-table.stripe tbody tr.odd {
        background-color: #ffffff !important;
      }
      table.dataTable.publication-table.stripe tbody tr.even,
      table.publication-table.stripe tbody tr.even {
        background-color: #f8fafc !important;
      }
      table.dataTable.publication-table tbody tr:hover,
      table.publication-table tbody tr:hover {
        background-color: #f1f5f9 !important;
      }
      .font-monospace-numbers,
      table.dataTable.publication-table td.dt-right,
      table.dataTable.publication-table td.text-end {
        font-variant-numeric: tabular-nums !important;
        font-feature-settings: 'tnum' 1 !important;
      }
      table.dataTable.publication-table th.dt-right,
      table.dataTable.publication-table td.dt-right {
        text-align: right !important;
      }
      table.dataTable.publication-table th.dt-center,
      table.dataTable.publication-table td.dt-center {
        text-align: center !important;
      }
      table.dataTable.publication-table th.dt-left,
      table.dataTable.publication-table td.dt-left {
        text-align: left !important;
      }
      .table-publication-note {
        font-size: 0.76rem;
        color: #64748b;
        line-height: 1.45;
        margin-top: 0.65rem;
        margin-bottom: 0.15rem;
        padding: 0.45rem 0.75rem;
        background-color: #f8fafc;
        border-left: 3px solid #94a3b8;
        border-radius: 0 0.35rem 0.35rem 0;
      }
      .table-publication-note strong {
        color: #334155;
        font-weight: 650;
      }
    "))
  ),

  page_fluid(
    theme = create_app_theme(),
    title = "Analyse des tendances et performance des alertes de la MVE/B",

    # Frozen header centered at top of page
    div(
      class = "app-frozen-header",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        div(class = "header-spacer d-none d-lg-block", style = "width: 140px;"),
        div(
          class = "text-center flex-grow-1",
          h1(class = "app-frozen-title", "Analyse des tendances et performance des alertes de la MVE/B"),
          p(
            class = "app-frozen-subtitle text-muted mb-0",
            "Suivi longitudinal des alertes de cas et de décès par rapport aux seuils attendus, ",
            "avec évaluation de la performance (adéquation) au niveau global et par zone de santé."
          )
        ),
        div(
          class = "header-actions text-end",
          actionButton(
            "header_open_browser",
            label = tags$span(
              tags$i(class = "bi bi-box-arrow-up-right me-1"),
              "Navigateur externe"
            ),
            class = "btn btn-sm btn-outline-secondary py-1 px-2 fw-semibold",
            title = "Ouvrir l'application dans votre navigateur par défaut (Chrome/Safari) pour les téléchargements directs"
          )
        )
      )
    ),

    # Main page content
    div(
      class = "app-content-body",
      trends_ui("trends"),

      tags$hr(class = "my-5"),

      # Export section at bottom of main page
      export_ui("export")
    ),

    # Page footer
    div(
      class = "app-footer text-center mt-5 mb-4",
      tags$p(
        class = "text-muted small mb-1",
        "Source des données : ", textOutput("data_source_info", inline = TRUE)
      ),
      tags$p(
        class = "text-muted small mb-0",
        textOutput("app_author_info", inline = TRUE)
      )
    )
  )
)
