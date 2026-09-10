# Script 3b: Lab-style health-zone notification performance map
# -----------------------------------------------------------------------------
# Maps recent alert notification adequacy (sous-notification, notification
# adéquate, surnotification) from the latest complete trends output, using the
# visual conventions of Lab.Analysis/lab_map.R and canonical GRID3 v8 geography.

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(ggrepel)
  library(ggspatial)
  library(patchwork)
  library(readxl)
})

source(here::here("R/alert_helpers.R"))

# 1. Resolve the latest complete inputs (never assumes today's directory) ----
map_inputs <- resolve_notification_map_inputs(output_root = here::here("output"))
message("Notification map inputs: ", map_inputs$output_dir)

adequacy_raw <- readxl::read_excel(map_inputs$adequacy_path)
trends_raw <- readRDS(map_inputs$trends_path)

# unique(adequacy_raw$adequacy_category)
# adequacy_raw |> arrange(desc(mean_aai)) |> View()


reference_date <- notification_map_reference_date(trends_raw, map_inputs$output_dir)
evd_file_date <- attr(trends_raw, "evd_file_date")
stamp <- if (!is.null(evd_file_date)) evd_file_date else format(reference_date, "%Y_%m_%d")

# 2. Prepare, classify and audit the notification performance data ----------
prepared <- prepare_notification_performance_data(adequacy_raw, trends_raw)
message(
  "Prepared ", prepared$audit$n_health_zone_rows, " health-zone rows (",
  prepared$audit$n_ensemble_rows_removed, " ensemble row(s) removed)."
)

# 3. Load canonical GRID3 geography and derive province boundaries ----------
message("Loading GRID3 v8 health-zone geography (all provinces)...")
health_zones <- read_alert_grid3_geography()
grid3_geom_col <- attr(health_zones, "sf_column")
admin_boundaries <- health_zones |>
  summarise(across(dplyr::all_of(grid3_geom_col), st_union), .by = province)

# 4. Join performance estimates to polygons with matching audit -------------
joined <- join_notification_performance_to_health_zones(
  prepared$data,
  health_zones
)
message(
  "Matched ", joined$audit$n_matched_rows, " / ", joined$audit$n_performance_rows,
  " notification rows; ", joined$audit$n_unmatched_rows, " unmatched."
)

# 5. Build the Lab-style choropleth -----------------------------------------
labels <- create_alert_health_zone_labels(joined$data)

caption_text <- sprintf(
  paste0(
    "Source : DHIS2 Tracker, données au %s. ",
    "Catégories : AAI < 0,75 ; 0,75–2,00 ; > 2,00. ",
    "Zones grises : pas de données ou hors périmètre d'analyse."
  ),
  format(reference_date, "%d-%m-%Y")
)

performance_map <- plot_health_zone_choropleth(
  map_data = joined$data,
  labels = labels,
  admin_boundaries = admin_boundaries,
  category_col = "adequacy_category_recomputed",
  palette = notification_adequacy_palette,
  category_labels = notification_adequacy_labels_fr,
  legend_title = "",
  plot_title = "Performance de notification des alertes par zone de santé",
  plot_subtitle = "Moyenne de l'indice de performance des alertes (AAI), 3 dernières semaines",
  caption = caption_text,
  na_value = notification_na_fill,
  na_label = notification_na_label_fr,
  buffer_degrees = 0.25,
  include_reference_map = TRUE
)

# 6. Save map outputs next to the source data -------------------------------
base_names <- c(
  generic = "03b_alert_notification_performance_map",
  stamped = paste0("03b_alert_notification_performance_map_", stamp)
)
output_names <- unique(base_names)

for (output_name in output_names) {
  ggsave(
    filename = file.path(map_inputs$output_dir, paste0(output_name, ".png")),
    plot = performance_map,
    width = 9,
    height = 10,
    dpi = 300,
    bg = "white",
    device = "png"
  )
  ggsave(
    filename = file.path(map_inputs$output_dir, paste0(output_name, ".pdf")),
    plot = performance_map,
    width = 9,
    height = 10,
    device = cairo_pdf
  )
}

# 7. Save map data and audit ------------------------------------------------
audit <- list(
  input_directory = map_inputs$output_dir,
  evd_file_date = stamp,
  reference_date = reference_date,
  preparation = prepared$audit,
  geography_join = joined$audit
)

saveRDS(joined$data, file.path(map_inputs$output_dir, paste0(base_names[["generic"]], "_data.rds")))
saveRDS(audit, file.path(map_inputs$output_dir, paste0(base_names[["generic"]], "_audit.rds")))

if (requireNamespace("writexl", quietly = TRUE)) {
  category_counts <- joined$data |>
    st_drop_geometry() |>
    filter(!is.na(adequacy_category_recomputed)) |>
    count(adequacy_category_recomputed) |>
    mutate(
      categorie = unname(notification_adequacy_labels_fr[as.character(adequacy_category_recomputed)])
    )

  summary_sheet <- tibble(
    Indicateur = c(
      "Répertoire d'entrée",
      "Date du fichier source",
      "Date de référence",
      "Lignes en entrée",
      "Lignes ensemble supprimées",
      "Zones de santé analysées",
      "AAI manquants",
      "Zones cartographiées",
      "Lignes appariées",
      "Lignes non appariées",
      "Clés géographiques dupliquées",
      "Désaccords de catégorie (source vs recalcul)"
    ),
    Valeur = c(
      map_inputs$output_dir,
      stamp,
      as.character(reference_date),
      prepared$audit$n_input_rows,
      prepared$audit$n_ensemble_rows_removed,
      prepared$audit$n_health_zone_rows,
      prepared$audit$n_missing_aai,
      joined$audit$n_estimates,
      joined$audit$n_matched_rows,
      joined$audit$n_unmatched_rows,
      nrow(joined$audit$duplicate_keys),
      prepared$audit$n_category_disagreements
    )
  )

  mapped_zones <- joined$data |>
    st_drop_geometry() |>
    filter(!is.na(adequacy_category_recomputed)) |>
    transmute(
      `Province (GRID3)` = province,
      `Zone de santé (GRID3)` = zonesante,
      `Zone notifiée (source)` = zone_sante_notification,
      `AAI moyen` = mean_aai,
      `Catégorie` = unname(
        notification_adequacy_labels_fr[as.character(adequacy_category_recomputed)]
      ),
      `Délai moyen (jours)` = mean_delay_days,
      `Tendance` = trend_direction,
      `Performance (volume + délai)` = performance_category
    )

  workbook_sheets <- list(
    "Résumé" = summary_sheet,
    "Zones cartographiées" = mapped_zones,
    "Zones non appariées" = joined$audit$unmatched_performance
  )
  if (nrow(prepared$audit$category_disagreements) > 0L) {
    workbook_sheets[["Désaccords de catégorie"]] <- prepared$audit$category_disagreements
  }

  writexl::write_xlsx(
    workbook_sheets,
    file.path(map_inputs$output_dir, paste0(base_names[["generic"]], "_audit.xlsx"))
  )
}

message(
  "Notification performance map complete. Outputs saved to ",
  map_inputs$output_dir
)
