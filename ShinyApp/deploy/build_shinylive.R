# =============================================================================
# BVD Alerts Dashboard — Shinylive Build & Local Preview Tool
# =============================================================================
# Prepares bundled data, stages only essential application runtime files,
# exports the Shiny application to a static Shinylive bundle (powered by webR),
# bundles WebAssembly packages ahead-of-time, and synchronizes the pre-rendered
# HTML report directly into the static output root (bypassing app.json).
#
# Usage:
#   Rscript ShinyApp/deploy/build_shinylive.R [options]
#
# Options:
#   --dest, -d <path>         Destination directory for static site (default: "site")
#   --serve, -s               Launch a local web server to preview in browser
#   --port, -p <number>       Port for local server (default: 8080)
#   --no-prep                 Skip re-running prepare_bundled_data.R
#   --no-wasm-packages        Skip pre-downloading WASM packages (defaults to TRUE)
#   --help, -h                Show this help message
# =============================================================================

suppressPackageStartupMessages({
  library(shinylive)
})

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)

show_help <- function() {
  cat("BVD Alerts Dashboard — Shinylive Build & Preview\n\n")
  cat("Usage: Rscript ShinyApp/deploy/build_shinylive.R [options]\n\n")
  cat("Options:\n")
  cat("  --dest, -d <path>         Destination directory for export (default: 'site')\n")
  cat("  --serve, -s               Start local server and preview in browser\n")
  cat("  --port, -p <number>       Port for local server (default: 8080)\n")
  cat("  --no-prep                 Skip re-running data preparation\n")
  cat("  --no-wasm-packages        Skip pre-downloading WASM packages into bundle\n")
  cat("  --help, -h                Show this help message\n\n")
  quit(status = 0)
}

if ("--help" %in% args || "-h" %in% args) {
  show_help()
}

dest_dir <- "site"
serve_preview <- FALSE
port <- 8080L
run_prep <- TRUE
wasm_packages <- TRUE

i <- 1L
while (i <= length(args)) {
  arg <- args[i]
  if (arg %in% c("--dest", "-d") && i < length(args)) {
    dest_dir <- args[i + 1L]
    i <- i + 2L
  } else if (arg %in% c("--serve", "-s")) {
    serve_preview <- TRUE
    i <- i + 1L
  } else if (arg %in% c("--port", "-p") && i < length(args)) {
    port <- as.integer(args[i + 1L])
    i <- i + 2L
  } else if (arg == "--no-prep") {
    run_prep <- FALSE
    i <- i + 1L
  } else if (arg == "--no-wasm-packages") {
    wasm_packages <- FALSE
    i <- i + 1L
  } else {
    i <- i + 1L
  }
}

# Find project directories
find_alerts_root <- function() {
  for (cand in c(".", "..", "../..", "../../..")) {
    p <- normalizePath(file.path(getwd(), cand), mustWork = FALSE)
    if (file.exists(file.path(p, "Alerts.Rproj")) ||
        (dir.exists(file.path(p, "ShinyApp")) && dir.exists(file.path(p, "output")))) {
      return(p)
    }
  }
  normalizePath(getwd(), mustWork = FALSE)
}

root_dir <- find_alerts_root()
app_dir <- file.path(root_dir, "ShinyApp")
dest_path <- if (grepl("^/", dest_dir)) dest_dir else file.path(root_dir, dest_dir)

message("=================================================================")
message(" BVD Alerts Dashboard — Shinylive Export")
message("=================================================================")
message("App directory:  ", app_dir)
message("Output static:  ", dest_path)
message("WASM packages:  ", wasm_packages)
message("Serve preview:  ", serve_preview)

# 1. Prepare bundled data
if (run_prep) {
  prep_script <- file.path(app_dir, "deploy", "prepare_bundled_data.R")
  if (file.exists(prep_script)) {
    message("\n[Step 1/3] Preparing bundled datasets and lightweight maps...")
    source(prep_script)
  }
} else {
  message("\n[Step 1/3] Skipping data preparation (--no-prep).")
}

# 2. Stage clean runtime files (strictly avoiding tests/, deploy/, and large HTML reports in app.json)
message("\n[Step 2/3] Staging clean application files for Shinylive export...")

# Clean up any accidental reports left in ShinyApp/ to keep working tree clean
for (stray in c("Alert_performance_report.html", "report_template.html",
                "www/Alert_performance_report.html", "www/report_template.html")) {
  stray_p <- file.path(app_dir, stray)
  if (file.exists(stray_p)) {
    unlink(stray_p)
  }
}

staging_dir <- tempfile("shinylive_stage_")
dir.create(staging_dir, recursive = TRUE)
dir.create(file.path(staging_dir, "R"), recursive = TRUE)
dir.create(file.path(staging_dir, "data"), recursive = TRUE)
dir.create(file.path(staging_dir, "www"), recursive = TRUE)
dir.create(file.path(staging_dir, "www", "js"), recursive = TRUE)

# Core app files
file.copy(file.path(app_dir, "app.R"), staging_dir)
file.copy(file.path(app_dir, "ui.R"), staging_dir)
file.copy(file.path(app_dir, "server.R"), staging_dir)

# R modules
for (f in list.files(file.path(app_dir, "R"), full.names = TRUE)) {
  file.copy(f, file.path(staging_dir, "R"))
}

# Data files (only required canonical data, avoiding legacy un-prefixed duplicates)
data_files <- list.files(file.path(app_dir, "data"), full.names = TRUE)
canonical_data <- c(
  "01_thresholds_synthesis.rds",
  "01_intermediate_parameters.rds",
  "02_trends_smooth_adeq.rds",
  "02_recent_adequacy.rds",
  "02_recent_adequacy.xlsx",
  "notification_map_data.rds",
  "province_map_data.rds",
  "drc_boundary_data.rds",
  "province_label_data.rds",
  "manifest.json"
)
for (f in data_files) {
  if (basename(f) %in% canonical_data) {
    file.copy(f, file.path(staging_dir, "data"))
  }
}

# Assets (custom.css and js libraries, EXCLUDING any .html reports)
if (file.exists(file.path(app_dir, "www", "custom.css"))) {
  file.copy(file.path(app_dir, "www", "custom.css"), file.path(staging_dir, "www"))
}
for (f in list.files(file.path(app_dir, "www", "js"), full.names = TRUE)) {
  file.copy(f, file.path(staging_dir, "www", "js"))
}

stage_files <- list.files(staging_dir, recursive = TRUE, full.names = TRUE)
stage_bytes <- sum(file.info(stage_files)$size)
message("Staging directory ready: ", length(stage_files), " files (total payload: ",
        round(stage_bytes / 1024, 1), " KB / ", round(stage_bytes / (1024 * 1024), 2), " MB).")

# Export using Shinylive
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

tryCatch({
  message("Running shinylive::export on staged application (wasm_packages = ", wasm_packages, ")...")
  shinylive::export(
    appdir = staging_dir,
    destdir = dest_path,
    wasm_packages = wasm_packages,
    package_cache = TRUE,
    quiet = FALSE
  )
  unlink(staging_dir, recursive = TRUE)
  message("\nExport completed successfully to: ", dest_path)

  # Check generated app.json size
  app_json_p <- file.path(dest_path, "app.json")
  if (file.exists(app_json_p)) {
    app_json_size <- file.info(app_json_p)$size
    message("Generated app.json size: ", round(app_json_size / 1024, 1), " KB (",
            round(app_json_size / (1024 * 1024), 2), " MB).")
  }

  # Copy static pre-rendered report directly into export directory for fast HTTP access
  report_src <- file.path(root_dir, "docs", "reports", "Alert_performance_report.html")
  if (!file.exists(report_src)) {
    report_src <- file.path(root_dir, "docs", "reports", "report_template.html")
  }
  if (file.exists(report_src)) {
    file.copy(report_src, file.path(dest_path, "Alert_performance_report.html"), overwrite = TRUE)
    file.copy(report_src, file.path(dest_path, "report_template.html"), overwrite = TRUE)
    message("Copied static Alert_performance_report.html directly into ", dest_path, " (",
            round(file.size(report_src) / (1024 * 1024), 2), " MB, not in app.json).")
  }

  # Inject custom loading screen, Plotly, and DataTables into index.html
  index_file <- file.path(dest_path, "index.html")
  if (file.exists(index_file)) {
    index_html <- paste(readLines(index_file, warn = FALSE), collapse = "\n")

    # 1. Plotly & DataTables CDN libraries
    if (!grepl("plotly-2.35.2.min.js", index_html, fixed = TRUE)) {
      head_inject <- paste0(
        "    <!-- Plotly & DataTables CDN libraries for webR Shinylive -->\n",
        "    <script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\" charset=\"utf-8\"></script>\n",
        "    <link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.datatables.net/1.13.6/css/dataTables.bootstrap5.min.css\"/>\n",
        "    <script type=\"text/javascript\" src=\"https://code.jquery.com/jquery-3.7.1.min.js\"></script>\n",
        "    <script type=\"text/javascript\" src=\"https://cdn.datatables.net/1.13.6/js/jquery.dataTables.min.js\"></script>\n",
        "    <script type=\"text/javascript\" src=\"https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap5.min.js\"></script>\n",
        "  </head>"
      )
      index_html <- sub("</head>", head_inject, index_html, fixed = TRUE)
    }

    # 2. Modern loading screen overlay
    if (!grepl("app-loading-screen", index_html, fixed = TRUE)) {
      loading_screen_html <- paste0(
        "\n    <!-- BVD Alerts Modern Loading Screen -->\n",
        "    <div id=\"app-loading-screen\">\n",
        "      <div class=\"loading-card\">\n",
        "        <div class=\"loading-badge\">Surveillance \u00c9pid\u00e9miologique BVD</div>\n",
        "        <h2 class=\"loading-title\">Performance des Alertes</h2>\n",
        "        <p class=\"loading-subtitle\">Nord-Kivu &amp; Ituri &bull; Analyse de d\u00e9tection et tendances</p>\n",
        "        <div class=\"progress-container\">\n",
        "          <div class=\"progress-bar-animated\"></div>\n",
        "        </div>\n",
        "        <div class=\"status-text\" id=\"loading-status-text\">\n",
        "          <span>Initialisation du moteur d'analyse WebAssembly...</span>\n",
        "        </div>\n",
        "        <div class=\"security-notice\">\n",
        "          Ex\u00e9cution locale s\u00e9curis\u00e9e directement dans votre navigateur via webR. Aucune donn\u00e9e confidentielle n'est envoy\u00e9e \u00e0 un serveur distant.\n",
        "        </div>\n",
        "      </div>\n",
        "    </div>\n",
        "    <style>\n",
        "      #app-loading-screen {\n",
        "        position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;\n",
        "        background: linear-gradient(135deg, #0b1329 0%, #1e293b 100%);\n",
        "        color: #f8fafc; display: flex; flex-direction: column;\n",
        "        justify-content: center; align-items: center; z-index: 99999;\n",
        "        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;\n",
        "        transition: opacity 0.6s cubic-bezier(0.16, 1, 0.3, 1), visibility 0.6s cubic-bezier(0.16, 1, 0.3, 1);\n",
        "      }\n",
        "      #app-loading-screen.fade-out { opacity: 0; visibility: hidden; pointer-events: none; }\n",
        "      .loading-card {\n",
        "        background: rgba(15, 23, 42, 0.85);\n",
        "        border: 1px solid rgba(255, 255, 255, 0.12);\n",
        "        backdrop-filter: blur(16px); -webkit-backdrop-filter: blur(16px);\n",
        "        border-radius: 20px; padding: 2.75rem 2.25rem;\n",
        "        max-width: 540px; width: 92%;\n",
        "        box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5), 0 0 0 1px rgba(255, 255, 255, 0.05);\n",
        "        text-align: center;\n",
        "      }\n",
        "      .loading-badge {\n",
        "        display: inline-block; padding: 0.35rem 0.9rem;\n",
        "        background: rgba(16, 185, 129, 0.15);\n",
        "        border: 1px solid rgba(16, 185, 129, 0.35);\n",
        "        color: #34d399; border-radius: 9999px;\n",
        "        font-size: 0.8rem; font-weight: 600;\n",
        "        margin-bottom: 1.25rem; letter-spacing: 0.03em;\n",
        "      }\n",
        "      .loading-title {\n",
        "        font-size: 1.5rem; font-weight: 700;\n",
        "        margin-bottom: 0.4rem; color: #ffffff; letter-spacing: -0.01em;\n",
        "      }\n",
        "      .loading-subtitle {\n",
        "        font-size: 0.925rem; color: #94a3b8;\n",
        "        margin-bottom: 1.75rem; line-height: 1.5;\n",
        "      }\n",
        "      .progress-container {\n",
        "        width: 100%; height: 8px;\n",
        "        background: rgba(255, 255, 255, 0.08);\n",
        "        border-radius: 9999px; overflow: hidden;\n",
        "        margin-bottom: 1.25rem; position: relative;\n",
        "      }\n",
        "      .progress-bar-animated {\n",
        "        height: 100%; width: 40%;\n",
        "        background: linear-gradient(90deg, #3b82f6, #10b981);\n",
        "        border-radius: 9999px; position: absolute;\n",
        "        animation: progress-indeterminate 2.2s infinite ease-in-out;\n",
        "      }\n",
        "      @keyframes progress-indeterminate {\n",
        "        0% { left: -40%; width: 40%; }\n",
        "        50% { left: 25%; width: 50%; }\n",
        "        100% { left: 100%; width: 40%; }\n",
        "      }\n",
        "      .status-text {\n",
        "        font-size: 0.875rem; color: #cbd5e1;\n",
        "        margin-bottom: 1.5rem;\n",
        "      }\n",
        "      .security-notice {\n",
        "        font-size: 0.775rem; color: #64748b;\n",
        "        border-top: 1px solid rgba(255, 255, 255, 0.08);\n",
        "        padding-top: 1.25rem; line-height: 1.45;\n",
        "      }\n",
        "    </style>\n",
        "    <script>\n",
        "      (function() {\n",
        "        var statusMessages = [\n",
        "          'Initialisation du moteur WebAssembly (webR)...',\n",
        "          'Chargement des modules statistiques et graphiques...',\n",
        "          'Chargement des donn\u00e9es \u00e9pid\u00e9miologiques et cartographiques...',\n",
        "          'Affichage de l\\'interface interactive...'\n",
        "        ];\n",
        "        var msgIndex = 0;\n",
        "        var statusElem = document.getElementById('loading-status-text');\n",
        "        var msgInterval = setInterval(function() {\n",
        "          msgIndex = (msgIndex + 1) % statusMessages.length;\n",
        "          if (statusElem) {\n",
        "            statusElem.innerHTML = '<span>' + statusMessages[msgIndex] + '</span>';\n",
        "          }\n",
        "        }, 3500);\n",
        "\n",
        "        function hideLoading() {\n",
        "          clearInterval(msgInterval);\n",
        "          var overlay = document.getElementById('app-loading-screen');\n",
        "          if (overlay && !overlay.classList.contains('fade-out')) {\n",
        "            overlay.classList.add('fade-out');\n",
        "            setTimeout(function() {\n",
        "              if (overlay.parentNode) overlay.parentNode.removeChild(overlay);\n",
        "            }, 650);\n",
        "          }\n",
        "        }\n",
        "\n",
        "        window.addEventListener('message', function(event) {\n",
        "          if (event.data && event.data.type === 'shiny:app_ready') {\n",
        "            hideLoading();\n",
        "          }\n",
        "        });\n",
        "\n",
        "        function pollAppReady() {\n",
        "          var root = document.getElementById('root');\n",
        "          if (root) {\n",
        "            var iframe = root.querySelector('iframe');\n",
        "            if (iframe) {\n",
        "              try {\n",
        "                var idoc = iframe.contentDocument || (iframe.contentWindow && iframe.contentWindow.document);\n",
        "                if (idoc && idoc.querySelector('.container-fluid, .bslib-page-fill, main, form')) {\n",
        "                  hideLoading();\n",
        "                  return;\n",
        "                }\n",
        "              } catch(e) {}\n",
        "            }\n",
        "            var hasApp = root.querySelector('.container-fluid, .bslib-page-fill, main, form');\n",
        "            if (hasApp && hasApp.offsetHeight > 50) {\n",
        "              hideLoading();\n",
        "              return;\n",
        "            }\n",
        "          }\n",
        "          setTimeout(pollAppReady, 300);\n",
        "        }\n",
        "        setTimeout(pollAppReady, 600);\n",
        "        setTimeout(hideLoading, 90000);\n",
        "      })();\n",
        "    </script>\n",
        "  </body>"
      )
      index_html <- sub("</body>", loading_screen_html, index_html, fixed = TRUE)
    }

    writeLines(index_html, index_file)
    message("Injected custom loading screen and libraries into ", index_file)
  }
}, error = function(e) {
  unlink(staging_dir, recursive = TRUE)
  stop("Failed to export with shinylive: ", conditionMessage(e))
})

# 3. Optional local server preview
if (serve_preview) {
  message("\n[Step 3/3] Launching local preview server...")
  message("Serving static site at: http://127.0.0.1:", port)
  message("Press Ctrl+C (or Esc) in console to stop server.")

  if (requireNamespace("httpuv", quietly = TRUE)) {
    httpuv::runStaticServer(dir = dest_path, port = port, browse = TRUE)
  } else {
    message("httpuv is not installed. You can serve the site with Python:")
    message("  cd ", dest_path, " && python3 -m http.server ", port)
  }
} else {
  message("\n[Step 3/3] Done! To preview locally:")
  message("  Rscript ShinyApp/deploy/build_shinylive.R --serve --port ", port)
  message("Or with python:")
  message("  python3 -m http.server ", port, " --directory ", dest_path)
}
