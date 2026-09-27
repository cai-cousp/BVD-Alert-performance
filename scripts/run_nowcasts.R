#!/usr/bin/env Rscript

# Canonical status-specific nowcast stage.
#
# This stage owns EpiNow2 fitting. Threshold and trend scripts consume its
# versioned bundle; they do not launch their own nowcast fits.

suppressPackageStartupMessages({
  library(tidyverse)
  library(EpiNow2)
})

root <- here::here()
evd17_root <- normalizePath(file.path(root, ".."), mustWork = TRUE)
data_folder <- file.path(evd17_root, "DataCleaning", "data", "Output")

source(file.path(root, "R", "alert_helpers.R"))
source(file.path(
  evd17_root,
  "DataAnalysis",
  "helpers",
  "LoadLatestData.R"
))

output_dir <- create_output_dir(file.path(root, "output"))

message("Loading the latest cleaned EVD line list...")
evd <- load_latest_evd(data_folder)
latest_evd <- latest_evd_file(data_folder)
snapshot_key <- basename(latest_evd)
evd_file_date <- extract_evd_date_stamp(snapshot_key)
ref_date <- evd_notification_ref_date(evd)
line_list_fingerprint <- rlang::hash(evd)
nowcast_max_delay <- 21L

confirmed_zones <- evd |>
  dplyr::filter(
    alert_is_confirmed_case(evd),
    !is.na(.data$zone_sante_notification)
  ) |>
  dplyr::distinct(.data$zone_sante_notification) |>
  dplyr::pull(.data$zone_sante_notification)

message(sprintf(
  "Loaded %d rows; %d affected health zones; reference date %s.",
  nrow(evd),
  length(confirmed_zones),
  format(ref_date)
))

nowcast_window_days <- 42L
nowcast_min_date <- ref_date - nowcast_window_days
notification_dates <- alert_resolve_notification_date(evd)
onset_dates <- alert_resolve_onset_date(evd)

in_nowcast_window <- (
  (!is.na(notification_dates) &
     notification_dates >= nowcast_min_date &
     notification_dates <= ref_date) |
    (!is.na(onset_dates) &
       onset_dates >= nowcast_min_date &
       onset_dates <= ref_date)
)

evd_nowcast <- evd |>
  dplyr::filter(
    .data$zone_sante_notification %in% .env$confirmed_zones,
    .env$in_nowcast_window
  )

if (nrow(evd_nowcast) == 0L) {
  rlang::abort("No rows remain in the 42-day nowcast input window.")
}

message(sprintf(
  "Nowcast input: %d rows across %d zones (%s to %s).",
  nrow(evd_nowcast),
  dplyr::n_distinct(evd_nowcast$zone_sante_notification),
  format(nowcast_min_date),
  format(ref_date)
))

cache_path <- file.path(
  output_dir,
  sprintf("05_nowcast_by_zone_all_%s.rds", evd_file_date)
)

message("Fitting all-case, alive, and death nowcasts...")
nowcast_bundle <- compute_status_nowcasts(
  evd = evd_nowcast,
  ref_date = ref_date,
  min_date = nowcast_min_date,
  onset_col = "alert_date_debut_symptoms",
  report_col = "lab_date_analyse",
  max_delay = nowcast_max_delay,
  cache_path = cache_path,
  snapshot_key = snapshot_key,
  line_list_fingerprint = line_list_fingerprint,
  verbose = TRUE
)

coherence <- attr(nowcast_bundle, "coherence")
message("Status-nowcast coherence validated.")

cat("\nWorst coherence discrepancies:\n")
print(utils::head(coherence, 10))

cat("\nNowcast output:\n")
for (variant in names(nowcast_bundle$data)) {
  series <- get_nowcast_variant(nowcast_bundle, variant)
  cat(sprintf(
    "  %-18s %d zones × %d rows\n",
    variant,
    dplyr::n_distinct(series$zone_sante_notification),
    nrow(series)
  ))
}

message("Writing compatibility exports...")
saveRDS(
  get_nowcast_variant(nowcast_bundle, "confirmed_cases"),
  file.path(output_dir, "06_nowcast_cases.rds"),
  compress = TRUE
)
saveRDS(
  get_nowcast_variant(nowcast_bundle, "confirmed_alive"),
  file.path(output_dir, "06_nowcast_alive.rds"),
  compress = TRUE
)
saveRDS(
  get_nowcast_variant(nowcast_bundle, "confirmed_deaths"),
  file.path(output_dir, "06_nowcast_deaths.rds"),
  compress = TRUE
)

message("Nowcast bundle saved to: ", cache_path)
