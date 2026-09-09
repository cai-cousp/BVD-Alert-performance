# =============================================================================
# BVD Alerts Dashboard — Shinylive Build & Local Preview Tool
# =============================================================================
# Prepares bundled data, exports the Shiny application to a static Shinylive
# bundle (powered by webR), and optionally serves it for local testing.
#
# Usage:
#   Rscript ShinyApp/deploy/build_shinylive.R [options]
#
# Options:
#   --dest, -d <path>   Destination directory for static site (default: "site")
#   --serve, -s         Launch a local web server to preview in browser
#   --port, -p <number> Port for local server (default: 8080)
#   --no-prep           Skip re-running prepare_bundled_data.R
#   --help, -h          Show this help message
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
  cat("  --dest, -d <path>    Destination directory for export (default: 'site')\n")
  cat("  --serve, -s          Start local server and preview in browser\n")
  cat("  --port, -p <number>  Port for local server (default: 8080)\n")
  cat("  --no-prep            Skip re-running data preparation\n")
  cat("  --help, -h           Show this help message\n\n")
  quit(status = 0)
}

if ("--help" %in% args || "-h" %in% args) {
  show_help()
}

dest_dir <- "site"
serve_preview <- FALSE
port <- 8080L
run_prep <- TRUE

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

# 2. Ensure assets and export
message("\n[Step 2/3] Exporting ShinyApp with Shinylive (webR)...")
if (!dir.exists(dest_path)) {
  dir.create(dest_path, recursive = TRUE)
}

# Ensure report_template.html is synchronized into ShinyApp/ before Shinylive export
report_src <- file.path(root_dir, "docs", "reports", "report_template.html")
report_dest_app <- file.path(app_dir, "report_template.html")
if (file.exists(report_src)) {
  if (!file.exists(report_dest_app) || file.info(report_src)$mtime > file.info(report_dest_app)$mtime) {
    file.copy(report_src, report_dest_app, overwrite = TRUE)
    message("Synchronized report_template.html into ShinyApp/ (", round(file.size(report_src) / (1024 * 1024), 2), " MB).")
  }
}

tryCatch({
  shinylive::export(
    appdir = app_dir,
    destdir = dest_path,
    wasm_packages = FALSE,
    quiet = FALSE
  )
  message("\nExport completed successfully to: ", dest_path)

  # Copy static report_template.html to export directory for direct static access as well
  if (file.exists(report_dest_app)) {
    file.copy(report_dest_app, file.path(dest_path, "report_template.html"), overwrite = TRUE)
    message("Copied static report_template.html directly into ", dest_path)
  }

  # Inject Plotly and DataTables into index.html so webR/htmlwidgets find them immediately
  index_file <- file.path(dest_path, "index.html")
  if (file.exists(index_file)) {
    index_html <- paste(readLines(index_file, warn = FALSE), collapse = "\n")
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
      writeLines(index_html, index_file)
      message("Injected Plotly and DataTables scripts into ", index_file)
    }
  }
}, error = function(e) {
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
