#!/usr/bin/env Rscript
# Run the full nowcast pipeline — mirrors R/01_alert_thresholds.R:150–195.
# Sources the nowcast helpers directly (no dependency on 04_nowcast.R).

suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(EpiNow2)
})

root <- here::here()
# Project is at .../MVE Ituri/EVD17/BVD-Alert-performance
evd17_root <- normalizePath(file.path(root, ".."), mustWork = TRUE)
data_folder <- file.path(evd17_root, "DataCleaning", "data", "Output")

# Source helpers ----------------------------------------------------------
source(file.path(root, "R", "alert_helpers.R"))
# LoadLatestData must be sourced before the ShinyApp binding shadows it
source(file.path(evd17_root, "DataAnalysis", "helpers", "LoadLatestData.R"))
source(file.path(root, "alert_helpers", "nowcast_by_zone.R"))
source(file.path(root, "alert_helpers", "nowcast_epinow2.R"))
source(file.path(root, "alert_helpers", "cfr_backcalc_utils.R"))
source(file.path(root, "alert_helpers", "confirmed_incidence.R"))
source(file.path(root, "alert_helpers", "window_utils.R"))
source(file.path(root, "alert_helpers", "nowcast_coherence.R"))

output_dir <- file.path(root, "output", "2026_09_20")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Load data ---------------------------------------------------------------
message("Loading EVD ...")
evd <- load_latest_data(
  base_dir = data_folder,
  folder_name = "evd.cleaning",
  format = "rds"
)
message(sprintf("  %d rows; onset %s → %s",
                nrow(evd),
                format(min(evd$alert_date_debut_symptoms, na.rm = TRUE)),
                format(max(evd$alert_date_debut_symptoms, na.rm = TRUE))))

ref_date <- evd_notification_ref_date(evd)
onset_col <- "alert_date_debut_symptoms"
report_col <- "lab_date_analyse"
max_delay <- 21L
nowcast_window_days <- 42L
nowcast_min_date <- ref_date - nowcast_window_days

message(sprintf("Reference date: %s  window: %s → %s",
                format(ref_date), format(nowcast_min_date), format(ref_date)))

# Restrict to affected HZs and the recent 42-day window ------------------
notif_dates <- alert_resolve_notification_date(evd)
onset_dates <- alert_resolve_case_date(
  evd,
  case_date_col = onset_col,
  fallback_date_col = "s2_date_debut_signes_symptomes"
)

in_nowcast_window <- (
  (!is.na(notif_dates) & notif_dates >= nowcast_min_date & notif_dates <= ref_date) |
    (!is.na(onset_dates) & onset_dates >= nowcast_min_date & onset_dates <= ref_date)
)

conf.zs <- unique(evd$zone_sante_notification)
evd_nowcast <- evd[evd$zone_sante_notification %in% conf.zs & in_nowcast_window, ]

message(sprintf("Nowcast input: %d rows across %d zones (%s → %s)",
                nrow(evd_nowcast),
                length(conf.zs),
                format(min(evd_nowcast[[onset_col]], na.rm = TRUE)),
                format(max(evd_nowcast[[onset_col]], na.rm = TRUE))))

# Fit per-HZ nowcasts -----------------------------------------------------
message("Fitting per-HZ EpiNow2 nowcasts ...")
cache_path <- file.path(output_dir, "05_nowcast_by_zone_all.rds")
snapshot_key <- "evd.clean_Int_2026_09_20_0804.rds"

nowcasts_all <- compute_nowcasts_by_zone(
  evd = evd_nowcast,
  ref_date = ref_date,
  min_date = nowcast_min_date,
  onset_col = onset_col,
  report_col = report_col,
  max_delay = max_delay,
  series = "all",
  cache_path = cache_path,
  snapshot_key = snapshot_key,
  verbose = TRUE
)

# Validate coherence ------------------------------------------------------
message("Validating nowcast coherence ...")
coherence <- validate_status_nowcasts(
  nowcasts_all_cases = nowcasts_all$confirmed_cases,
  nowcasts_alive = nowcasts_all$confirmed_alive,
  nowcasts_deaths = nowcasts_all$confirmed_deaths,
  warn_above = 0.10,
  error_above = 0.25
)
cat("\nCoherence summary (worst zones):\n")
print(attr(coherence, "coherence") |> head(10))

# Summary stats -----------------------------------------------------------
message("\nNowcast output:")
for (nm in names(nowcasts_all)) {
  s <- nowcasts_all[[nm]]
  cat(sprintf("  %-15s  %d zones × %d dates\n", nm,
              length(unique(s$zone_sante_notification)),
              nrow(s)))
}

# Write per-series rds for downstream consumers
message("\nWriting per-series outputs ...")
write_rds <- function(obj, path) {
  saveRDS(obj, path, compress = TRUE)
  message(sprintf("  %s (%d rows)", basename(path), nrow(obj)))
}

write_rds(nowcasts_all$confirmed_cases, file.path(output_dir, "06_nowcast_cases.rds"))
write_rds(nowcasts_all$confirmed_alive, file.path(output_dir, "06_nowcast_alive.rds"))
write_rds(nowcasts_all$confirmed_deaths, file.path(output_dir, "06_nowcast_deaths.rds"))

message("\nDone.")
