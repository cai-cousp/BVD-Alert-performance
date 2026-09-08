# Geography helpers for the Lab-style alert performance map
# -----------------------------------------------------------------------------
# Loads GRID3 v8 health-zone geography and matches notification province /
# health-zone names to canonical polygon keys using the shared orthographic
# correction helper (hierarchical, province-first fuzzy matching).

#' Source the shared orthographic correction helper when needed
#'
#' @keywords internal
#' @return Invisible path of the sourced helper (or `NULL` when already loaded).
source_alert_ortho_helper <- function() {
  if (exists("auto_correct_typos", mode = "function", inherits = TRUE)) {
    return(invisible(NULL))
  }

  ortho_candidates <- c(
    file.path(here::here(), "..", "helpers", "OrthoCorrect.R"),
    file.path(evd17_root, "DataAnalysis", "helpers", "OrthoCorrect.R")
  )
  ortho_path <- ortho_candidates[file.exists(ortho_candidates)][1]
  if (is.na(ortho_path)) {
    rlang::abort(
      c(
        "Could not locate the shared orthographic correction helper.",
        x = paste(ortho_candidates, collapse = " | ")
      ),
      call = rlang::caller_env()
    )
  }
  source(ortho_path, local = FALSE)
  invisible(ortho_path)
}

#' Read GRID3 v8 health-zone geography
#'
#' Reads every provincial `GRID3_COD_*_health_zones_v8_0.gpkg` file and returns
#' a single valid `sf` object with canonical `province`, `zonesante` and
#' `geometry` columns. Loading all provinces by default supplies the national
#' grey base, province outlines and the DRC reference inset used by the
#' Lab-style map.
#'
#' @param health_zone_dir Directory containing the GRID3 provincial GPKGs.
#' @param provinces Optional character vector of GRID3 province names used to
#'   filter zones. `NULL` (default) keeps the whole country.
#'
#' @return An `sf` object with columns `province`, `zonesante`, `geometry`.
read_alert_grid3_geography <- function(
  health_zone_dir = file.path(evd17_root, "Maps", "health_zones"),
  provinces = NULL
) {
  if (!is.null(provinces) && !is.character(provinces)) {
    rlang::abort("`provinces` must be NULL or a character vector.", call = rlang::caller_env())
  }
  if (!dir.exists(health_zone_dir)) {
    rlang::abort(
      c("The GRID3 health-zone map directory does not exist.", x = health_zone_dir),
      call = rlang::caller_env()
    )
  }

  gpkg_files <- list.files(
    health_zone_dir,
    pattern = "^GRID3_COD_.*_health_zones_v8_0\\.gpkg$",
    full.names = TRUE
  )
  if (length(gpkg_files) == 0L) {
    rlang::abort(
      c("No GRID3 v8 health-zone GPKG files found in:", x = health_zone_dir),
      call = rlang::caller_env()
    )
  }

  layers <- lapply(gpkg_files, sf::read_sf, quiet = TRUE)
  combined <- lapply(layers, function(layer) {
    required <- c("province", "zonesante")
    missing <- setdiff(required, names(layer))
    if (length(missing) > 0L) {
      rlang::abort(
        c("A GRID3 layer is missing required field(s):", x = paste(missing, collapse = ", ")),
        call = rlang::caller_env()
      )
    }
    geom_column <- utils::tail(attr(layer, "sf_column"), 1)
    layer |> dplyr::select(dplyr::all_of(required), dplyr::all_of(geom_column))
  })
  health_zones <- sf::st_make_valid(dplyr::bind_rows(combined))

  if (is.na(sf::st_crs(health_zones))) {
    rlang::abort("GRID3 health-zone geography must have a defined CRS.", call = rlang::caller_env())
  }
  if (!is.null(provinces)) {
    health_zones <- health_zones |> dplyr::filter(.data$province %in% .env$provinces)
    if (nrow(health_zones) == 0L) {
      rlang::abort("No GRID3 zones matched the requested province filter.", call = rlang::caller_env())
    }
  }

  duplicate_keys <- health_zones |>
    sf::st_drop_geometry() |>
    dplyr::count(province, zonesante) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicate_keys) > 0L) {
    rlang::abort(
      c(
        "Duplicate province/health-zone keys found in GRID3 geography.",
        i = paste(nrow(duplicate_keys), "duplicated key(s).")
      ),
      call = rlang::caller_env()
    )
  }

  health_zones
}

#' Match notification geography names to canonical GRID3 keys
#'
#' Corrects notification province and health-zone names against reference
#' polygons using fuzzy string matching. Zones are matched within the
#' recognized province first (disambiguating duplicate zone names across
#' provinces), with a global fallback and province inference for zones that are
#' nationally unique.
#'
#' @param provinces Character vector of notification province names.
#' @param zones Character vector of notification health-zone names.
#' @param health_zones An `sf` object with `province` and `zonesante` columns.
#' @param match_threshold Similarity percentage threshold (0-100). Default 80.
#'
#' @return A `tibble` with `province_matched` and `zone_matched` columns.
match_alert_health_zone_names <- function(
  provinces,
  zones,
  health_zones,
  match_threshold = 80
) {
  if (!is.character(provinces)) {
    rlang::abort("`provinces` must be a character vector.", call = rlang::caller_env())
  }
  if (!is.character(zones)) {
    rlang::abort("`zones` must be a character vector.", call = rlang::caller_env())
  }
  if (length(provinces) != length(zones)) {
    rlang::abort("`provinces` and `zones` must have the same length.", call = rlang::caller_env())
  }
  if (!is.numeric(match_threshold) || length(match_threshold) != 1L ||
      match_threshold < 0 || match_threshold > 100) {
    rlang::abort("`match_threshold` must be a single numeric value between 0 and 100.", call = rlang::caller_env())
  }
  if (!inherits(health_zones, "sf") && !is.data.frame(health_zones)) {
    rlang::abort("`health_zones` must be an sf object or data frame.", call = rlang::caller_env())
  }

  required_cols <- c("province", "zonesante")
  missing_cols <- setdiff(required_cols, names(health_zones))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      c("Missing reference columns in `health_zones`:", x = paste(missing_cols, collapse = ", ")),
      call = rlang::caller_env()
    )
  }

  source_alert_ortho_helper()

  n <- length(zones)
  if (n == 0L) {
    return(tibble::tibble(
      province_matched = character(0),
      zone_matched = character(0)
    ))
  }

  ref_provinces <- unique(health_zones$province)
  ref_zones_all <- unique(health_zones$zonesante)

  # 1. Correct province names against canonical provinces.
  prov_corr <- auto_correct_typos(
    provinces,
    ref_provinces,
    match_threshold = match_threshold,
    nf = "target"
  )

  # 2. Hierarchical zone matching: intra-province first, global fallback.
  zone_corr <- character(n)
  for (i in seq_len(n)) {
    z <- zones[i]
    p <- prov_corr[i]

    if (is.na(z)) {
      zone_corr[i] <- NA_character_
      next
    }

    matched_z <- NA_character_
    if (!is.na(p) && p %in% ref_provinces) {
      prov_zones <- health_zones$zonesante[health_zones$province == p]
      matched_z <- auto_correct_typos(
        z,
        prov_zones,
        match_threshold = match_threshold,
        nf = "na"
      )
    }

    if (is.na(matched_z)) {
      matched_z <- auto_correct_typos(
        z,
        ref_zones_all,
        match_threshold = match_threshold,
        nf = "na"
      )

      if ((is.na(p) || !p %in% ref_provinces) && !is.na(matched_z)) {
        cand_p <- unique(health_zones$province[health_zones$zonesante == matched_z])
        if (length(cand_p) == 1L) {
          prov_corr[i] <- cand_p
        }
      }
    }

    zone_corr[i] <- matched_z
  }

  tibble::tibble(
    province_matched = prov_corr,
    zone_matched = zone_corr
  )
}
