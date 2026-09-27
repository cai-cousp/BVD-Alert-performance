# Canonical status-specific nowcast bundle.
#
# The three nowcast series have distinct surveillance interpretations:
# all confirmed cases for epidemic growth, confirmed alive cases for alive
# alerts, and confirmed deaths for death alerts. They are fitted separately but
# exchanged downstream as one versioned object so scripts cannot accidentally
# mix snapshots, reference dates, or variants.

#' Supported nowcast variants
#' @noRd
NOWCAST_VARIANTS <- c(
  "confirmed_cases",
  "confirmed_alive",
  "confirmed_deaths"
)

#' Current nowcast cache/schema version
#'
#' Bump this when nowcast construction, delay resolution, or the bundle schema
#' changes in a way that invalidates cached fits.
#' @export
STATUS_NOWCAST_CACHE_VERSION <- "2026-09-21"

#' Construct a validated status-specific nowcast bundle
#'
#' @param data Named list with `confirmed_cases`, `confirmed_alive`, and
#'   `confirmed_deaths` nowcast tibbles.
#' @param metadata Named list containing at least `snapshot_key`,
#'   `line_list_fingerprint`, and `max_delay`. Nowcast version, reference date,
#'   and generation time are completed when absent.
#' @return A `nowcast_bundle` object.
#' @export
new_nowcast_bundle <- function(data, metadata = list()) {
  if (!is.list(data)) {
    rlang::abort("`data` must be a list of nowcast tibbles.")
  }
  if (!is.list(metadata)) {
    rlang::abort("`metadata` must be a list.")
  }

  missing_variants <- setdiff(NOWCAST_VARIANTS, names(data))
  extra_variants <- setdiff(names(data), NOWCAST_VARIANTS)
  if (length(missing_variants) > 0L || length(extra_variants) > 0L) {
    rlang::abort(c(
      "`data` must contain exactly the supported nowcast variants.",
      x = paste0("Missing: ", paste(missing_variants, collapse = ", ")),
      x = paste0("Unsupported: ", paste(extra_variants, collapse = ", "))
    ))
  }

  if (is.null(metadata$snapshot_key) || !nzchar(metadata$snapshot_key)) {
    rlang::abort("`metadata$snapshot_key` is required.")
  }
  if (is.null(metadata$nowcast_version)) {
    metadata$nowcast_version <- STATUS_NOWCAST_CACHE_VERSION
  }
  if (is.null(metadata$ref_date)) {
    metadata$ref_date <- attr(data$confirmed_cases, "ref_date")
  }
  if (is.null(metadata$generated_at)) {
    metadata$generated_at <- Sys.time()
  }

  bundle <- structure(
    list(data = data, metadata = metadata),
    class = c("nowcast_bundle", "list")
  )
  validate_nowcast_bundle(bundle)
}

#' Validate a status-specific nowcast bundle
#'
#' @param bundle Object created by `new_nowcast_bundle()`.
#' @return `bundle`, invisibly.
#' @export
validate_nowcast_bundle <- function(bundle) {
  if (!inherits(bundle, "nowcast_bundle")) {
    rlang::abort("`bundle` must be a `nowcast_bundle`.")
  }
  if (!setequal(names(bundle$data), NOWCAST_VARIANTS)) {
    rlang::abort(c(
      "A nowcast bundle must contain exactly the supported variants.",
      i = paste(NOWCAST_VARIANTS, collapse = ", ")
    ))
  }
  if (is.null(bundle$metadata$snapshot_key)) {
    rlang::abort("A nowcast bundle requires `metadata$snapshot_key`.")
  }
  if (is.null(bundle$metadata$nowcast_version)) {
    rlang::abort("A nowcast bundle requires `metadata$nowcast_version`.")
  }
  if (is.null(bundle$metadata$ref_date)) {
    rlang::abort("A nowcast bundle requires `metadata$ref_date`.")
  }
  if (is.null(bundle$metadata$line_list_fingerprint)) {
    rlang::abort("A nowcast bundle requires `metadata$line_list_fingerprint`.")
  }
  if (is.null(bundle$metadata$max_delay)) {
    rlang::abort("A nowcast bundle requires `metadata$max_delay`.")
  }

  required_columns <- c(
    "province_notification",
    "zone_sante_notification",
    "date",
    "observed",
    "nowcast_median",
    "nowcast_lower_90",
    "nowcast_upper_90",
    "method",
    "status"
  )

  purrr::iwalk(bundle$data, \(variant, variant_name) {
    alert_required_columns(
      variant,
      required_columns,
      sprintf("nowcast bundle variant '%s'", variant_name)
    )
    if (!identical(attr(variant, "series"), variant_name)) {
      rlang::abort(sprintf(
        "The %s nowcast must have series attribute '%s'.",
        variant_name,
        variant_name
      ))
    }
    if (!inherits(variant$date, "Date")) {
      rlang::abort(sprintf("`date` in the %s nowcast must be a Date.", variant_name))
    }
    if (is.null(attr(variant, "ref_date"))) {
      rlang::abort(sprintf(
        "The %s nowcast must carry a reference-date attribute.",
        variant_name
      ))
    }
  })

  ref_dates <- purrr::map(bundle$data, ~ attr(.x, "ref_date"))
  if (!all(vapply(ref_dates, identical, logical(1L), y = ref_dates[[1L]]))) {
    rlang::abort("All nowcast variants must share the same reference date.")
  }
  if (!identical(bundle$metadata$ref_date, ref_dates[[1L]])) {
    rlang::abort("Bundle metadata and nowcast variants disagree on the reference date.")
  }

  invisible(bundle)
}

#' Extract one variant from a nowcast bundle
#'
#' @param bundle A `nowcast_bundle`.
#' @param variant One of `"confirmed_cases"`, `"confirmed_alive"`, or
#'   `"confirmed_deaths"`.
#' @return The selected nowcast tibble with its series attributes preserved.
#' @export
get_nowcast_variant <- function(bundle, variant) {
  validate_nowcast_bundle(bundle)
  variant <- rlang::arg_match(variant, NOWCAST_VARIANTS)
  bundle$data[[variant]]
}

#' Write a nowcast bundle atomically
#'
#' @param bundle A validated `nowcast_bundle`.
#' @param path RDS destination.
#' @return `path`, invisibly.
#' @export
write_nowcast_bundle <- function(bundle, path) {
  validate_nowcast_bundle(bundle)
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    rlang::abort("`path` must be a single output path.")
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary_path <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(temporary_path), add = TRUE)

  saveRDS(bundle, temporary_path, compress = TRUE)
  validate_nowcast_bundle(readRDS(temporary_path))
  if (!file.rename(temporary_path, path)) {
    rlang::abort("Could not finalize the nowcast bundle cache.")
  }

  invisible(path)
}

#' Read and validate a nowcast bundle cache
#'
#' @param path RDS path created by `write_nowcast_bundle()`.
#' @param expected_snapshot_key Optional snapshot key that must match the cache.
#' @param expected_nowcast_version Optional nowcast version that must match.
#' @param expected_max_delay Optional maximum reporting delay that must match.
#' @param expected_line_list_fingerprint Optional source-line-list hash that
#'   must match.
#' @return A validated `nowcast_bundle`.
#' @export
read_nowcast_bundle <- function(path,
                                expected_snapshot_key = NULL,
                                expected_nowcast_version = NULL,
                                expected_max_delay = NULL,
                                expected_line_list_fingerprint = NULL) {
  if (!file.exists(path)) {
    rlang::abort(sprintf("No nowcast bundle found at '%s'.", path))
  }

  bundle <- tryCatch(
    readRDS(path),
    error = function(e) {
      rlang::abort(
        sprintf("Could not read the nowcast bundle at '%s'.", path),
        parent = e
      )
    }
  )
  validate_nowcast_bundle(bundle)

  if (!is.null(expected_snapshot_key) &&
      !identical(bundle$metadata$snapshot_key, expected_snapshot_key)) {
    rlang::abort(c(
      "The nowcast bundle was produced from a different line-list snapshot.",
      x = sprintf("Expected: %s", expected_snapshot_key),
      x = sprintf("Found: %s", bundle$metadata$snapshot_key)
    ))
  }
  if (!is.null(expected_nowcast_version) &&
      !identical(bundle$metadata$nowcast_version, expected_nowcast_version)) {
    rlang::abort(c(
      "The nowcast bundle uses a different nowcast version.",
      x = sprintf("Expected: %s", expected_nowcast_version),
      x = sprintf("Found: %s", bundle$metadata$nowcast_version)
    ))
  }
  if (!is.null(expected_max_delay) &&
      !identical(bundle$metadata$max_delay, as.integer(expected_max_delay))) {
    rlang::abort(c(
      "The nowcast bundle uses a different max delay.",
      x = sprintf("Expected: %s", expected_max_delay),
      x = sprintf("Found: %s", bundle$metadata$max_delay)
    ))
  }
  if (!is.null(expected_line_list_fingerprint) &&
      !identical(
        bundle$metadata$line_list_fingerprint,
        expected_line_list_fingerprint
      )) {
    rlang::abort(c(
      "The nowcast bundle was produced from a different line-list fingerprint.",
      x = sprintf("Expected: %s", expected_line_list_fingerprint),
      x = sprintf("Found: %s", bundle$metadata$line_list_fingerprint)
    ))
  }

  bundle
}

#' Locate the latest valid nowcast bundle under an output directory
#'
#' If no valid bundle is found and `run_script` is not `NULL`, the nowcast stage
#' (`scripts/run_nowcasts.R`) is launched as a subprocess to (re)produce the
#' bundle, then the search is repeated. The subprocess is only started when a
#' suitable bundle is actually missing, so a current cache is never wasted.
#'
#' @param output_dir Directory searched recursively for bundle RDS files.
#' @param expected_snapshot_key Required line-list snapshot key.
#' @param expected_nowcast_version Required nowcast version.
#' @param run_script Path to the nowcast script to run when no valid bundle is
#'   found. Defaults to `scripts/run_nowcasts.R`, resolved from the project
#'   root, so a missing bundle is regenerated automatically. Pass `NULL` to
#'   skip the stage: the caller is then expected to run the script out-of-band
#'   and the search simply returns `NULL`.
#' @return Path to the latest matching bundle, or `NULL` when `run_script` is
#'   `NULL` and no bundle matches.
#' @export
find_nowcast_bundle <- function(output_dir,
                                expected_snapshot_key,
                                expected_nowcast_version = STATUS_NOWCAST_CACHE_VERSION,
                                expected_max_delay = NULL,
                                expected_line_list_fingerprint = NULL,
                                run_script = "scripts/run_nowcasts.R") {
  find_path <- function() {
    if (!dir.exists(output_dir)) {
      return(NULL)
    }
    candidates <- list.files(
      output_dir,
      pattern = "^05_nowcast_by_zone_all_.*[.]rds$",
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(candidates) == 0L) {
      return(NULL)
    }
    candidates <- candidates[order(file.info(candidates)$mtime, decreasing = TRUE)]

    for (path in candidates) {
      bundle <- tryCatch(
        read_nowcast_bundle(
          path,
          expected_snapshot_key = expected_snapshot_key,
          expected_nowcast_version = expected_nowcast_version,
          expected_max_delay = expected_max_delay,
          expected_line_list_fingerprint = expected_line_list_fingerprint
        ),
        error = function(e) NULL
      )
      if (!is.null(bundle)) {
        return(path)
      }
    }

    NULL
  }

  found <- find_path()
  if (!is.null(found)) {
    return(found)
  }

  if (!is.null(run_script)) {
    run_nowcast_stage(run_script)
    found <- find_path()
    if (!is.null(found)) {
      return(found)
    }
    rlang::abort(c(
      "The nowcast stage ran but did not produce a valid bundle.",
      i = sprintf("Expected snapshot: %s", expected_snapshot_key)
    ))
  }

  NULL
}

#' Run the nowcast stage as a subprocess
#'
#' Launches `Rscript <run_script>` (default: `scripts/run_nowcasts.R`) from the
#' project root. The script owns EpiNow2 fitting and writes a versioned bundle
#' under `output/`. Its stdout/stderr stream through the parent session so the
#' fit progress stays visible.
#'
#' @param run_script Path to the nowcast script to execute; `NULL` falls back
#'   to `scripts/run_nowcasts.R`.
#' @return Status code of the subprocess (invisibly), when available.
#' @noRd
run_nowcast_stage <- function(run_script) {
  if (interactive() && !nzchar(Sys.getenv("BVD_NOWCAST_SUBPROCESS", ""))) {
    # Fitting EpiNow2 nowcasts can take a long time. In an interactive R session
    # this is launched in a separate process so the user's session is not blocked
    # by the heavy fit. Set BVD_NOWCAST_SUBPROCESS=1 to run it inline instead.
    message("No valid nowcast bundle found. Running the nowcast stage in a subprocess...")
  } else {
    message("No valid nowcast bundle found. Running the nowcast stage: ", run_script)
  }

  default_script <- file.path(here::here(), "scripts", "run_nowcasts.R")
  script_path <- default_script
  if (!is.null(run_script) &&
      !identical(
        normalizePath(run_script, mustWork = FALSE),
        normalizePath(default_script, mustWork = FALSE)
      )) {
    script_path <- run_script
  }

  rscript <- Sys.which("Rscript")
  if (is.na(rscript)) {
    rlang::abort(c(
      "Could not find an Rscript executable to run the nowcast stage.",
      i = "Install R or add Rscript to your PATH."
    ))
  }

  status <- system2(rscript, args = shQuote(script_path), stdout = TRUE, stderr = TRUE)
  code <- attr(status, "status")
  if (!is.null(code) && code != 0) {
    rlang::abort(c(
      sprintf("The nowcast stage exited with code %s.", code),
      x = paste(status, collapse = "\n")
    ))
  }
  invisible(code)
}

#' Compute all status-specific nowcasts as one bundle
#'
#' This is the pipeline-level wrapper around `compute_nowcasts_by_zone()`. It
#' deliberately bypasses legacy single-series caches and returns one versioned
#' bundle containing all three variants.
#'
#' @inheritParams compute_nowcasts_by_zone
#' @param cache_path Optional path for the bundle RDS.
#' @param snapshot_key Required line-list snapshot identifier.
#' @param nowcast_version Cache/schema version to write and validate.
#' @param line_list_fingerprint Hash of the source line list. The default hashes
#'   `evd`; pipeline callers should pass a hash of the full source line list.
#' @param validate Logical; validate all-case/alive/death coherence before
#'   writing the cache.
#' @return A `nowcast_bundle`.
#' @export
compute_status_nowcasts <- function(evd,
                                    ...,
                                    cache_path = NULL,
                                    snapshot_key = NULL,
                                    nowcast_version = STATUS_NOWCAST_CACHE_VERSION,
                                    line_list_fingerprint = NULL,
                                    validate = TRUE) {
  if (is.null(snapshot_key)) {
    rlang::abort("compute_status_nowcasts() requires `snapshot_key`.")
  }

  args <- rlang::dots_list(..., .homonyms = "error")
  reserved <- c("series", "cache_path", "snapshot_key")
  if (any(reserved %in% names(args))) {
    rlang::abort(c(
      "compute_status_nowcasts() sets these arguments itself.",
      x = paste(intersect(reserved, names(args)), collapse = ", ")
    ))
  }
  max_delay <- args$max_delay %||% formals(compute_nowcasts_by_zone)$max_delay
  line_list_fingerprint <- line_list_fingerprint %||% rlang::hash(evd)

  if (!is.null(cache_path)) {
    cached <- tryCatch(
      read_nowcast_bundle(
        cache_path,
        expected_snapshot_key = snapshot_key,
        expected_nowcast_version = nowcast_version,
        expected_max_delay = max_delay,
        expected_line_list_fingerprint = line_list_fingerprint
      ),
      error = function(e) NULL
    )
    if (!is.null(cached)) {
      if (isTRUE(validate)) {
        status_nowcasts <- validate_status_nowcasts(
          nowcasts_all_cases = cached$data$confirmed_cases,
          nowcasts_alive = cached$data$confirmed_alive,
          nowcasts_deaths = cached$data$confirmed_deaths
        )
        attr(cached, "coherence") <- attr(status_nowcasts, "coherence")
      }
      return(cached)
    }
  }

  nowcasts <- rlang::exec(
    compute_nowcasts_by_zone,
    evd = evd,
    series = "all",
    !!!args,
    cache_path = NULL,
    snapshot_key = NULL
  )

  bundle <- new_nowcast_bundle(
    data = nowcasts,
    metadata = list(
      snapshot_key = snapshot_key,
      evd_file_date = extract_evd_date_stamp(snapshot_key),
      ref_date = attr(nowcasts$confirmed_cases, "ref_date"),
      nowcast_version = nowcast_version,
      generated_at = Sys.time(),
      max_delay = attr(nowcasts$confirmed_cases, "max_delay"),
      dist_samples = attr(nowcasts$confirmed_cases, "dist_samples"),
      line_list_fingerprint = line_list_fingerprint
    )
  )

  if (isTRUE(validate)) {
    status_nowcasts <- validate_status_nowcasts(
      nowcasts_all_cases = bundle$data$confirmed_cases,
      nowcasts_alive = bundle$data$confirmed_alive,
      nowcasts_deaths = bundle$data$confirmed_deaths
    )
    attr(bundle, "coherence") <- attr(status_nowcasts, "coherence")
  }

  if (!is.null(cache_path)) {
    write_nowcast_bundle(bundle, cache_path)
  }
  bundle
}
