# =============================================================================
# BVD Alerts Dashboard — Bundled Data Preparation Script
# =============================================================================
# Prepares self-contained, lightweight datasets in ShinyApp/data/ for
# zero-server deployment (Shinylive / webR / GitHub Pages).
#
# Usage from project root:
#   Rscript ShinyApp/deploy/prepare_bundled_data.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(readxl)
  library(writexl)
})

# Locate paths robustly
script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)

find_alerts_root <- function() {
  for (cand in c(".", "..", "../..", "../../..")) {
    p <- normalizePath(file.path(getwd(), cand), mustWork = FALSE)
    if (file.exists(file.path(p, "Alerts.Rproj")) || (dir.exists(file.path(p, "ShinyApp")) && dir.exists(file.path(p, "output")))) {
      return(p)
    }
  }
  # Fallback relative to script dir
  normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
}

root_dir <- find_alerts_root()
output_base <- file.path(root_dir, "output")
app_dir <- file.path(root_dir, "ShinyApp")
data_dest <- file.path(app_dir, "data")
maps_base_dir <- file.path(dirname(dirname(root_dir)), "Maps")

message("Root directory:   ", root_dir)
message("Output base:      ", output_base)
message("App directory:    ", app_dir)
message("Data destination: ", data_dest)

if (!dir.exists(data_dest)) {
  dir.create(data_dest, recursive = TRUE)
}

# --- Find latest valid output directory ---------------------------------------
output_dirs <- list.dirs(output_base, recursive = FALSE, full.names = TRUE)
if (length(output_dirs) == 0L) {
  stop("No output directories found in: ", output_base)
}

valid_dirs <- output_dirs[
  (lengths(lapply(output_dirs, function(d) list.files(d, pattern = "^01_thresholds_synthesis.*[.]rds$"))) > 0L) &
  (lengths(lapply(output_dirs, function(d) list.files(d, pattern = "^02_trends_smooth.*[.]rds$"))) > 0L)
]

if (length(valid_dirs) == 0L) {
  stop("No valid output directory containing synthesis and trends found.")
}

latest_dir <- sort(valid_dirs, decreasing = TRUE)[[1L]]
message("Extracting from latest analysis output: ", basename(latest_dir))

# --- Copy / save core analysis files ------------------------------------------

# 1. Thresholds synthesis
synth_file <- sort(list.files(latest_dir, pattern = "^01_thresholds_synthesis.*[.]rds$", full.names = TRUE), decreasing = TRUE)[[1L]]
synth_data <- readRDS(synth_file)
saveRDS(synth_data, file.path(data_dest, "01_thresholds_synthesis.rds"), compress = "xz")
saveRDS(synth_data, file.path(data_dest, "synthesis.rds"), compress = "xz")
message("  -> 01_thresholds_synthesis.rds (", round(file.size(file.path(data_dest, "synthesis.rds")) / 1024, 1), " KB)")

# 2. Intermediate parameters
param_files <- sort(list.files(latest_dir, pattern = "^01_intermediate_parameters.*[.]rds$", full.names = TRUE), decreasing = TRUE)
if (length(param_files) > 0L) {
  param_data <- readRDS(param_files[[1L]])
  saveRDS(param_data, file.path(data_dest, "01_intermediate_parameters.rds"), compress = "xz")
  saveRDS(param_data, file.path(data_dest, "intermediate_params.rds"), compress = "xz")
  message("  -> 01_intermediate_parameters.rds (", round(file.size(file.path(data_dest, "intermediate_params.rds")) / 1024, 1), " KB)")
}

# 3. Trends smooth adeq
trend_files <- sort(list.files(latest_dir, pattern = "^02_trends_smooth_adeq.*[.]rds$", full.names = TRUE), decreasing = TRUE)
if (length(trend_files) == 0L) {
  trend_files <- sort(list.files(latest_dir, pattern = "^02_trends_smooth.*[.]rds$", full.names = TRUE), decreasing = TRUE)
}
trend_data <- readRDS(trend_files[[1L]])
saveRDS(trend_data, file.path(data_dest, "02_trends_smooth_adeq.rds"), compress = "xz")
saveRDS(trend_data, file.path(data_dest, "trends_smooth_adeq.rds"), compress = "xz")
message("  -> 02_trends_smooth_adeq.rds (", round(file.size(file.path(data_dest, "trends_smooth_adeq.rds")) / 1024, 1), " KB)")

# 4. Recent adequacy
adeq_files <- sort(list.files(latest_dir, pattern = "^02_recent_adequacy.*[.]xlsx$", full.names = TRUE), decreasing = TRUE)
if (length(adeq_files) > 0L) {
  file.copy(adeq_files[[1L]], file.path(data_dest, "02_recent_adequacy.xlsx"), overwrite = TRUE)
  file.copy(adeq_files[[1L]], file.path(data_dest, "recent_adequacy.xlsx"), overwrite = TRUE)
  message("  -> 02_recent_adequacy.xlsx (", round(file.size(file.path(data_dest, "recent_adequacy.xlsx")) / 1024, 1), " KB)")
}

# --- Prepare lightweight, optimized map layers --------------------------------
message("Preparing lightweight map layers...")

source(file.path(app_dir, "R", "map_data_helpers.R"))

# Check for precomputed notification performance map data in latest_dir
map_data_file <- sort(list.files(latest_dir, pattern = "^03b_alert_notification_performance_map_data.*[.]rds$", full.names = TRUE), decreasing = TRUE)

if (length(map_data_file) > 0L) {
  raw_map <- readRDS(map_data_file[[1L]])
  
  # Ensure geom column is set properly
  geom_col <- attr(raw_map, "sf_column")
  if (is.null(geom_col) || !geom_col %in% names(raw_map)) {
    if ("geom" %in% names(raw_map)) {
      sf::st_geometry(raw_map) <- "geom"
    } else if ("geometry" %in% names(raw_map)) {
      sf::st_geometry(raw_map) <- "geometry"
    }
  }

  # Filter to affected provinces: Ituri, Nord-Kivu, Sud-Kivu
  prov_keep <- c("ituri", "nord-kivu", "sud-kivu")
  map_subset <- raw_map[tolower(as.character(raw_map$province)) %in% prov_keep, ]
  
  # Attach hover indicators and metrics
  recent_metrics <- summarise_recent_notification_metrics(trend_data)
  map_subset <- map_subset |>
    dplyr::select(-dplyr::any_of(c(
      "total_alerts", "case_adequacy_recent", "death_adequacy_recent",
      "mean_aai_recent", "n_recent_windows"
    ))) |>
    dplyr::left_join(recent_metrics, by = "zone_sante_notification")

  if (!"adequacy_category_recomputed" %in% names(map_subset)) {
    if ("adequacy_category" %in% names(map_subset)) {
      map_subset$adequacy_category_recomputed <- map_subset$adequacy_category
    }
  }

  # Simplify in projected CRS EPSG:3379 for fast rendering
  map_simplified <- sf::st_transform(map_subset, 3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 500) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  if (!"map_id" %in% names(map_simplified)) {
    map_simplified$map_id <- sprintf("notification_hz_%04d", seq_len(nrow(map_simplified)))
  }

  saveRDS(map_simplified, file.path(data_dest, "notification_map_data.rds"), compress = "xz")
  message("  -> notification_map_data.rds (", round(file.size(file.path(data_dest, "notification_map_data.rds")) / 1024, 1), " KB)")
} else {
  message("  Notice: No 03b map data file found. Generating from geography helper if available.")
}

# Province boundary outlines
if (dir.exists(maps_base_dir)) {
  prov_sf <- tryCatch(
    load_province_boundaries(maps_base_dir),
    error = function(e) NULL
  )
  if (!is.null(prov_sf) && inherits(prov_sf, "sf") && nrow(prov_sf) > 0L) {
    # Keep only the 3 affected provinces
    prov_keep_names <- c("Ituri", "Nord-Kivu", "Sud-Kivu")
    prov_sub <- prov_sf[tolower(as.character(prov_sf$province_name)) %in% tolower(prov_keep_names), ]
    if (nrow(prov_sub) == 0L) prov_sub <- prov_sf
    
    saveRDS(prov_sub, file.path(data_dest, "province_map_data.rds"), compress = "xz")
    message("  -> province_map_data.rds (", round(file.size(file.path(data_dest, "province_map_data.rds")) / 1024, 1), " KB)")

    prov_labels <- create_province_label_points(prov_sub)
    saveRDS(prov_labels, file.path(data_dest, "province_label_data.rds"), compress = "xz")
    message("  -> province_label_data.rds (", round(file.size(file.path(data_dest, "province_label_data.rds")) / 1024, 1), " KB)")
  }
}

# --- Write manifest metadata --------------------------------------------------
all_files <- list.files(data_dest, full.names = TRUE)
file_summary <- lapply(all_files, function(f) {
  list(
    filename = basename(f),
    size_kb  = round(file.size(f) / 1024, 2)
  )
})

total_kb <- round(sum(file.size(all_files)) / 1024, 2)

manifest <- list(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  source_output_dir = basename(latest_dir),
  date_stamp = if (exists("extract_evd_date_stamp", mode = "function")) {
    extract_evd_date_stamp(basename(synth_file))
  } else NULL,
  health_zones_count = length(unique(trend_data$zone_sante_notification)),
  weeks_count = length(unique(trend_data$threshold_time_key)),
  total_payload_kb = total_kb,
  files = file_summary
)

json_manifest <- jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE)
writeLines(json_manifest, file.path(data_dest, "manifest.json"))
message("Manifest generated: total bundled payload is ", total_kb, " KB (< ", round(total_kb / 1024, 2), " MB).")
message("Done preparing bundled data!")
