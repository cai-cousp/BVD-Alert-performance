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

# Source health zones and provinces from ZSandDPSshapefiles
zone_shp <- file.path(
  maps_base_dir,
  "ZSandDPSshapefiles",
  "osm_rdc_sante_zones_211212",
  "OSM_RDC_sante_zones_211212.shp"
)
prov_shp <- file.path(
  maps_base_dir,
  "ZSandDPSshapefiles",
  "osm_rdc_sante_provinces_211212",
  "OSM_RDC_sante_provinces_211212.shp"
)

# Extract affected provinces dynamically from trend data
affected_provs_trends <- setdiff(unique(trend_data$Province), c("Ensemble", "Ensemble de la zone affectée"))
norm_affected <- tolower(gsub("[- ]", "", affected_provs_trends))
message("Affected provinces detected in trends: ", paste(affected_provs_trends, collapse = ", "))

# 1. Health zones layer
if (file.exists(zone_shp) && file.exists(prov_shp)) {
  zs_raw <- sf::read_sf(zone_shp, quiet = TRUE)
  dps_raw <- sf::read_sf(prov_shp, quiet = TRUE)

  if (is.na(sf::st_crs(zs_raw))) sf::st_crs(zs_raw) <- 4326
  if (is.na(sf::st_crs(dps_raw))) sf::st_crs(dps_raw) <- 4326

  # National boundary outline of the complete DRC (dissolved all provinces)
  drc_geom <- sf::st_union(dps_raw)
  drc_sf <- sf::st_sf(country = "RDC", geometry = drc_geom) |>
    sf::st_transform(3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 1000) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  saveRDS(drc_sf, file.path(data_dest, "drc_boundary_data.rds"), compress = "xz")
  message("  -> drc_boundary_data.rds (", round(file.size(file.path(data_dest, "drc_boundary_data.rds")) / 1024, 1), " KB)")

  # Spatially attribute province name to health zones
  dps_clean <- dps_raw |> dplyr::select(province = "name")
  zs_pts <- suppressWarnings(sf::st_point_on_surface(zs_raw))
  joined_pts <- suppressWarnings(sf::st_join(zs_pts, dps_clean, join = sf::st_intersects))

  zs_raw$province <- as.character(joined_pts$province)
  zs_raw$zonesante <- as.character(zs_raw$name)

  # Filter to health zones and provinces of the affected areas
  zs_sub <- zs_raw[tolower(gsub("[- ]", "", zs_raw$province)) %in% norm_affected, ]
  prov_sub <- dps_clean[tolower(gsub("[- ]", "", dps_clean$province)) %in% norm_affected, ]

  # Join metrics and adequacy by normalized name (handles hyphen/space differences like Boma-Mangbetu)
  recent_metrics <- summarise_recent_notification_metrics(trend_data)
  recent_adeq_data <- if (exists("recent_adequacy") && is.data.frame(recent_adequacy) && nrow(recent_adequacy) > 0L) {
    recent_adequacy
  } else if (file.exists(file.path(data_dest, "02_recent_adequacy.xlsx"))) {
    readxl::read_excel(file.path(data_dest, "02_recent_adequacy.xlsx"))
  } else {
    tibble::tibble()
  }

  zs_sub$norm_name <- tolower(gsub("[- ]", "", zs_sub$name))
  recent_metrics$norm_name <- tolower(gsub("[- ]", "", recent_metrics$zone_sante_notification))

  if (nrow(recent_adeq_data) > 0L && "zone_sante_notification" %in% names(recent_adeq_data)) {
    recent_adeq_data$norm_name <- tolower(gsub("[- ]", "", recent_adeq_data$zone_sante_notification))
    zs_joined <- zs_sub |>
      dplyr::left_join(
        recent_adeq_data |> dplyr::select("norm_name", "zone_sante_notification", "adequacy_category", "mean_aai"),
        by = "norm_name"
      ) |>
      dplyr::left_join(recent_metrics |> dplyr::select(-dplyr::any_of("zone_sante_notification")), by = "norm_name") |>
      dplyr::mutate(
        adequacy_category_recomputed = .data$adequacy_category,
        mean_aai_recent = dplyr::coalesce(.data$mean_aai_recent, .data$mean_aai),
        province_notification = .data$province
      ) |>
      dplyr::select(-"norm_name")
  } else {
    zs_joined <- zs_sub |>
      dplyr::left_join(recent_metrics, by = "norm_name") |>
      dplyr::mutate(
        province_notification = .data$province,
        adequacy_category_recomputed = NA_character_
      ) |>
      dplyr::select(-"norm_name")
  }

  # Simplify health zone geometries in projected CRS EPSG:3379 for fast rendering
  map_simplified <- sf::st_transform(zs_joined, 3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 500) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()

  map_simplified$map_id <- sprintf("notification_hz_%04d", seq_len(nrow(map_simplified)))

  saveRDS(map_simplified, file.path(data_dest, "notification_map_data.rds"), compress = "xz")
  message("  -> notification_map_data.rds (", round(file.size(file.path(data_dest, "notification_map_data.rds")) / 1024, 1), " KB, ", nrow(map_simplified), " zones, ", sum(!is.na(map_simplified$zone_sante_notification)), " affected)")

  # 2. Province boundary outlines and label centroids
  prov_simplified <- sf::st_transform(prov_sub, 3379) |>
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 500) |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    dplyr::mutate(province_name = stringr::str_to_title(as.character(.data$province)))

  saveRDS(prov_simplified, file.path(data_dest, "province_map_data.rds"), compress = "xz")
  message("  -> province_map_data.rds (", round(file.size(file.path(data_dest, "province_map_data.rds")) / 1024, 1), " KB, ", nrow(prov_simplified), " provinces)")

  prov_labels <- create_province_label_points(prov_simplified)
  saveRDS(prov_labels, file.path(data_dest, "province_label_data.rds"), compress = "xz")
  message("  -> province_label_data.rds (", round(file.size(file.path(data_dest, "province_label_data.rds")) / 1024, 1), " KB)")
} else {
  warning("ZSandDPSshapefiles directory not found. Skipping map bundling.")
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

# --- Synchronize report_template.html -----------------------------------------
report_src <- file.path(root_dir, "docs", "reports", "report_template.html")
if (file.exists(report_src)) {
  file.copy(report_src, file.path(app_dir, "report_template.html"), overwrite = TRUE)
  message("Synchronized report_template.html to ShinyApp/ (", round(file.size(report_src) / (1024 * 1024), 2), " MB).")
}

message("Done preparing bundled data!")
