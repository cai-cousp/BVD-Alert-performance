#!/usr/bin/env Rscript

# Run the complete BVD alert performance pipeline in one command.
#
# Each stage runs as an isolated `Rscript` subprocess, in the canonical order
# documented in README.md:
#
#   1. nowcast             scripts/run_nowcasts.R
#   2. thresholds          R/01_alert_thresholds.R
#   3. thresholds_windows  R/01b_alert_thresholds_windows.R
#   4. trends              R/02_alert_trends.R
#   5. mapping             R/03_alert_mapping_capacity.R
#   6. notification_map    R/03b_alert_notification_performance_map.R
#
# Optional stages (excluded from the default sequence, available by name or
# with `include_optional = TRUE`):
#
#   - trends_plots         R/02b_alert_trends_plots.R  (per-HZ plot exports)
#   - nowcast_legacy       R/04_nowcast.R              (exploratory nowcast)
#
# Usage (command line) -------------------------------------------------------
#   Rscript scripts/run_pipeline.R
#   Rscript scripts/run_pipeline.R --list-stages
#   Rscript scripts/run_pipeline.R --stages=nowcast,thresholds,trends
#   Rscript scripts/run_pipeline.R --skip=nowcast
#   Rscript scripts/run_pipeline.R --include-optional
#   Rscript scripts/run_pipeline.R --no-echo --continue-on-error
#   Rscript scripts/run_pipeline.R --dry-run
#
# Usage (interactive R session) ----------------------------------------------
#   source("scripts/run_pipeline.R")
#   run_pipeline()                                    # default README stages
#   run_pipeline(stages = c("nowcast", "trends"))     # subset (catalog order)
#   run_pipeline(stages = "all")                      # every catalog stage
#   run_pipeline(skip = "mapping")                    # default stages minus one
#   run_pipeline(include_optional = TRUE)             # also 02b and 04
#
# Outputs are written under `output/<YYYY_MM_DD>/` by the individual stages.

# --------------------------------------------------------------------------- #
# Stage catalog
# --------------------------------------------------------------------------- #

#' Ordered catalog of pipeline stages
#'
#' @param include_optional Logical. Include the optional plot-export and
#'   legacy-nowcast stages in the default sequence?
#' @return A data frame with one row per stage: `stage` (identifier),
#'   `script` (path relative to the project root), `description`,
#'   `default` (whether the stage runs by default), and `expected_outputs`
#'   (list of regular expressions matched against file names under
#'   `output/` for post-stage verification).
pipeline_stage_catalog <- function(include_optional = FALSE) {
  tibble::tibble(
    stage = c(
      "nowcast",
      "thresholds",
      "thresholds_windows",
      "trends",
      "mapping",
      "notification_map",
      "trends_plots",
      "nowcast_legacy"
    ),
    script = c(
      file.path("scripts", "run_nowcasts.R"),
      file.path("R", "01_alert_thresholds.R"),
      file.path("R", "01b_alert_thresholds_windows.R"),
      file.path("R", "02_alert_trends.R"),
      file.path("R", "03_alert_mapping_capacity.R"),
      file.path("R", "03b_alert_notification_performance_map.R"),
      file.path("R", "02b_alert_trends_plots.R"),
      file.path("R", "04_nowcast.R")
    ),
    description = c(
      "Canonical status-specific EpiNow2 nowcast bundle",
      "Weekly alert thresholds (three-approach synthesis)",
      "Longitudinal health-zone x time-window thresholds",
      "Weekly adequacy and trend analysis",
      "Spatial mapping against accessibility and capacity",
      "Lab-style notification performance choropleth",
      "Standalone trend plot exports (optional)",
      "Legacy exploratory nowcast of confirmed cases (optional)"
    ),
    default = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      isTRUE(include_optional),
      isTRUE(include_optional)
    ),
    expected_outputs = list(
      c("^05_nowcast_by_zone_all_.*\\.rds$", "^06_nowcast_cases\\.rds$"),
      c("^01_thresholds_synthesis(_.*)?\\.rds$", "^01_thresholds_ensemble(_.*)?\\.rds$"),
      c("^01b_thresholds_synthesis\\.rds$"),
      c("^02_trends_smooth(_.*)?\\.rds$", "^02_recent_adequacy(_.*)?\\.xlsx$"),
      c("^03_performance_dashboard\\.xlsx$"),
      c("^03b_alert_notification_performance_map.*\\.png$"),
      c("^alert_case_trends_thresholds\\.png$"),
      c("^04_nowcast_summary\\.rds$")
    )
  )
}

# --------------------------------------------------------------------------- #
# Internals
# --------------------------------------------------------------------------- #

#' Resolve the project root robustly
#'
#' Prefers the location of this script (works from any working directory),
#' then `here::here()`, then the current working directory.
pipeline_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    script_path <- sub("^--file=", "", file_arg[[length(file_arg)]])
    root <- normalizePath(
      file.path(dirname(script_path), ".."),
      mustWork = FALSE
    )
    if (dir.exists(file.path(root, "R")) &&
        dir.exists(file.path(root, "scripts"))) {
      return(root)
    }
  }

  if (requireNamespace("here", quietly = TRUE)) {
    root <- here::here()
    if (dir.exists(file.path(root, "R")) &&
        dir.exists(file.path(root, "scripts"))) {
      return(root)
    }
  }

  normalizePath(getwd(), mustWork = FALSE)
}

#' Full path to the current R installation's Rscript executable
pipeline_rscript <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  file.path(R.home("bin"), exe)
}

format_pipeline_duration <- function(seconds) {
  if (is.na(seconds)) {
    return(NA_character_)
  }
  if (seconds >= 60) {
    sprintf("%.1f min", seconds / 60)
  } else {
    sprintf("%.1f s", seconds)
  }
}

#' Identify expected-output patterns with no matching file under `output/`
find_missing_outputs <- function(patterns, output_root) {
  if (length(patterns) == 0L || !dir.exists(output_root)) {
    return(patterns)
  }
  produced <- list.files(output_root, recursive = TRUE)
  found <- vapply(
    patterns,
    function(pattern) any(grepl(pattern, basename(produced))),
    logical(1)
  )
  patterns[!found]
}

#' Run one pipeline stage as an isolated Rscript subprocess
#'
#' @param stage_row Single-row data frame from [pipeline_stage_catalog()].
#' @param echo Logical. Stream stage output live to the console.
#' @param verify_outputs Logical. Check expected outputs after the stage.
#' @param output_root Pipeline output root used for verification.
#' @return A named list describing the stage outcome.
run_pipeline_stage <- function(stage_row,
                               echo = TRUE,
                               verify_outputs = TRUE,
                               output_root) {
  script_path <- stage_row$script[[1]]
  if (!file.exists(script_path)) {
    rlang::abort(c(
      "Pipeline script does not exist:",
      x = script_path
    ))
  }

  started_at <- Sys.time()
  if (echo) {
    # Inherit the console so long-running stages (EpiNow2 fits) stream live.
    run <- system2(
      pipeline_rscript(),
      args = script_path,
      stdout = "",
      stderr = ""
    )
  } else {
    # Capture quietly; logs are surfaced below only when the stage fails.
    run <- suppressWarnings(system2(
      pipeline_rscript(),
      args = script_path,
      stdout = TRUE,
      stderr = TRUE
    ))
  }

  exit_code <- if (is.character(run)) {
    status <- attr(run, "status")
    if (is.null(status)) 0L else as.integer(status)
  } else {
    as.integer(run)
  }
  if (length(exit_code) != 1L || is.na(exit_code)) {
    exit_code <- 127L
  }

  elapsed_seconds <- as.numeric(
    difftime(Sys.time(), started_at, units = "secs")
  )

  if (exit_code != 0L && !echo && is.character(run) && length(run) > 0L) {
    cat("\n--- Captured output from failed stage ---\n")
    cat(paste(run, collapse = "\n"), "\n")
    cat("------------------------------------------\n")
  }

  missing_outputs <- character()
  if (exit_code == 0L && isTRUE(verify_outputs)) {
    missing_outputs <- find_missing_outputs(
      stage_row$expected_outputs[[1]],
      output_root
    )
    if (length(missing_outputs) > 0L) {
      rlang::warn(c(
        sprintf(
          "Stage '%s' finished but expected outputs were not found:",
          stage_row$stage[[1]]
        ),
        x = paste(missing_outputs, collapse = ", ")
      ))
    }
  }

  list(
    stage = stage_row$stage[[1]],
    script = script_path,
    status = if (exit_code == 0L) "success" else "failed",
    exit_code = exit_code,
    elapsed_seconds = elapsed_seconds,
    elapsed_label = format_pipeline_duration(elapsed_seconds),
    missing_outputs = missing_outputs
  )
}

# --------------------------------------------------------------------------- #
# Wrapper function
# --------------------------------------------------------------------------- #

#' Run the BVD alert performance pipeline
#'
#' Executes the pipeline stages as separate Rscript subprocesses so each stage
#' starts from a clean R session, matching the documented manual run order.
#' The pipeline stops at the first failed stage unless
#' `continue_on_error = TRUE`.
#'
#' @param stages Character vector of stage IDs (run in catalog order), `NULL`
#'   for the default sequence, or `"all"` for every catalog stage.
#' @param skip Character vector of stage IDs to exclude.
#' @param include_optional Logical. Add the optional `trends_plots` and
#'   `nowcast_legacy` stages to the default sequence.
#' @param echo Logical. Stream stage output live to the console. If `FALSE`,
#'   output is captured and printed only when a stage fails.
#' @param continue_on_error Logical. Keep running after a failed stage
#'   instead of aborting.
#' @param dry_run Logical. Print the stages and commands without running.
#' @param verify_outputs Logical. After each successful stage, check that the
#'   expected output files exist somewhere under `output/` (cached or fresh).
#'   Missing outputs raise a warning, not an error.
#' @param output_root Path to the pipeline output root
#'   (default: `<root>/output`).
#'
#' @return A tibble with one row per stage: `stage`, `script`, `status`
#'   (`"success"`, `"failed"`, `"skipped"`, or `"dry_run"`), `exit_code`,
#'   `elapsed_seconds`, `elapsed`, and `missing_outputs` (list column).
#' @examples
#' \dontrun{
#' run_pipeline()
#' run_pipeline(stages = c("nowcast", "thresholds", "trends"))
#' run_pipeline(skip = "mapping", echo = FALSE)
#' }
run_pipeline <- function(stages = NULL,
                         skip = character(),
                         include_optional = FALSE,
                         echo = TRUE,
                         continue_on_error = FALSE,
                         dry_run = FALSE,
                         verify_outputs = TRUE,
                         output_root = NULL) {
  if (!is.null(stages) && !is.character(stages)) {
    rlang::abort("`stages` must be NULL, a character vector, or \"all\".")
  }
  if (!is.character(skip)) {
    rlang::abort("`skip` must be a character vector of stage IDs.")
  }

  catalog <- pipeline_stage_catalog(include_optional = include_optional)

  unknown_stages <- setdiff(stages, c(catalog$stage, "all"))
  if (length(unknown_stages) > 0L) {
    rlang::abort(c(
      "Unknown pipeline stage(s):",
      x = paste(unknown_stages, collapse = ", "),
      i = paste("Available stages:", paste(catalog$stage, collapse = ", "))
    ))
  }
  unknown_skip <- setdiff(skip, catalog$stage)
  if (length(unknown_skip) > 0L) {
    rlang::abort(c(
      "Unknown stage(s) in `skip`:",
      x = paste(unknown_skip, collapse = ", "),
      i = paste("Available stages:", paste(catalog$stage, collapse = ", "))
    ))
  }

  selected <- if (is.null(stages)) {
    catalog$stage[catalog$default]
  } else if (identical(stages, "all")) {
    catalog$stage
  } else {
    catalog$stage[catalog$stage %in% stages]
  }
  selected <- setdiff(selected, skip)

  root <- pipeline_root()
  if (is.null(output_root)) {
    output_root <- file.path(root, "output")
  }

  run_label <- if (dry_run) "DRY RUN - " else ""
  message(sprintf(
    "%sPipeline: %d stage(s)%s | root: %s",
    run_label,
    length(selected),
    if (length(skip) > 0L) sprintf(" (%d skipped)", length(skip)) else "",
    root
  ))

  # Run stages from the project root so here::here() resolves identically
  # regardless of where the wrapper was invoked from.
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)

  results <- list()
  failed <- FALSE

  for (i in seq_along(selected)) {
    stage_id <- selected[[i]]
    stage_row <- catalog[catalog$stage == stage_id, ]

    message(sprintf(
      "[%d/%d] %s: %s",
      i,
      length(selected),
      stage_id,
      stage_row$description[[1]]
    ))
    message(sprintf("      Rscript %s", stage_row$script[[1]]))

    if (dry_run) {
      results[[stage_id]] <- list(
        stage = stage_id,
        script = stage_row$script[[1]],
        status = "dry_run",
        exit_code = NA_integer_,
        elapsed_seconds = NA_real_,
        elapsed_label = NA_character_,
        missing_outputs = list()
      )
      next
    }

    results[[stage_id]] <- run_pipeline_stage(
      stage_row,
      echo = echo,
      verify_outputs = verify_outputs,
      output_root = output_root
    )

    if (results[[stage_id]]$status == "failed") {
      failed <- TRUE
      if (!isTRUE(continue_on_error)) {
        break
      }
    }
  }

  if (length(skip) > 0L) {
    for (stage_id in skip) {
      stage_row <- catalog[catalog$stage == stage_id, ]
      results[[stage_id]] <- list(
        stage = stage_id,
        script = stage_row$script[[1]],
        status = "skipped",
        exit_code = NA_integer_,
        elapsed_seconds = NA_real_,
        elapsed_label = NA_character_,
        missing_outputs = list()
      )
    }
  }

  # Restore catalog order for reporting.
  results <- results[catalog$stage[catalog$stage %in% names(results)]]

  results_df <- tibble::tibble(
    stage = vapply(results, function(x) x$stage, character(1)),
    script = vapply(results, function(x) x$script, character(1)),
    status = vapply(results, function(x) x$status, character(1)),
    exit_code = vapply(results, function(x) x$exit_code, integer(1)),
    elapsed_seconds = vapply(
      results,
      function(x) x$elapsed_seconds,
      numeric(1)
    ),
    elapsed = vapply(results, function(x) x$elapsed_label, character(1)),
    missing_outputs = lapply(results, function(x) x$missing_outputs)
  )

  cat("\n================ Pipeline summary ================\n")
  print(
    as.data.frame(results_df[, c("stage", "status", "elapsed")]),
    row.names = FALSE
  )
  cat("==================================================\n")

  total_elapsed <- sum(results_df$elapsed_seconds, na.rm = TRUE)
  if (!dry_run) {
    message(sprintf(
      "Pipeline finished in %s (%d succeeded, %d failed, %d skipped).",
      format_pipeline_duration(total_elapsed),
      sum(results_df$status == "success"),
      sum(results_df$status == "failed"),
      sum(results_df$status == "skipped")
    ))
  }

  if (failed && !isTRUE(continue_on_error)) {
    failure <- results_df[results_df$status == "failed", ][1, ]
    rlang::abort(c(
      sprintf(
        "Pipeline stopped: stage '%s' failed (exit code %d).",
        failure$stage,
        failure$exit_code
      ),
      i = "Run that stage directly for the full error context, or use",
      i = "`continue_on_error = TRUE` to keep running after failures.",
      i = "The summary of stages run so far is in this error's `data` field."
    ), data = results_df)
  }

  if (failed) {
    rlang::warn(sprintf(
      "%d pipeline stage(s) failed; inspect the `status` column of the result.",
      sum(results_df$status == "failed")
    ))
  }

  results_df
}

# --------------------------------------------------------------------------- #
# Command-line interface
# --------------------------------------------------------------------------- #

parse_pipeline_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  flags <- list()
  for (arg in args) {
    if (!grepl("^--", arg)) {
      next
    }
    parts <- strsplit(sub("^--", "", arg), "=")[[1]]
    key <- gsub("-", "_", parts[[1]])
    value <- if (length(parts) > 1L) paste(parts[-1], collapse = "=") else TRUE

    if (identical(key, "no_echo")) {
      key <- "echo"
      value <- FALSE
    }
    if (identical(key, "no_verify")) {
      key <- "verify_outputs"
      value <- FALSE
    }
    if (is.character(value)) {
      if (key %in% c("stages", "skip")) {
        value <- trimws(strsplit(value, ",")[[1]])
        value <- value[nzchar(value)]
      } else if (tolower(value) %in% c("true", "false")) {
        value <- tolower(value) == "true"
      }
    }
    flags[[key]] <- value
  }
  flags
}

pipeline_usage <- function() {
  cat(
    "Usage: Rscript scripts/run_pipeline.R [options]",
    "",
    "Options:",
    "  --stages=ID,ID      Run only these stages (catalog order)",
    "  --skip=ID,ID        Exclude these stages from the run",
    "  --include-optional  Also run trends_plots and nowcast_legacy",
    "  --no-echo           Capture output; show it only on failure",
    "  --continue-on-error Keep running after a failed stage",
    "  --no-verify         Skip expected-output verification",
    "  --dry-run           Print the plan without running",
    "  --list-stages       List available stages and exit",
    "  --help              Show this help and exit",
    "",
    "Default stages: nowcast, thresholds, thresholds_windows, trends,",
    "mapping, notification_map",
    sep = "\n"
  )
}

if (sys.nframe() == 0L) {
  cli_args <- parse_pipeline_args()

  if (isTRUE(cli_args$help)) {
    pipeline_usage()
    quit(save = "no", status = 0L)
  }

  if (isTRUE(cli_args$list_stages)) {
    catalog <- pipeline_stage_catalog(include_optional = FALSE)
    print(
      as.data.frame(
        catalog[, c("stage", "script", "default", "description")]
      ),
      row.names = FALSE
    )
    quit(save = "no", status = 0L)
  }

  pipeline_results <- run_pipeline(
    stages = if (is.null(cli_args$stages)) NULL else cli_args$stages,
    skip = if (is.null(cli_args$skip)) character() else cli_args$skip,
    include_optional = isTRUE(cli_args$include_optional),
    echo = !identical(cli_args$echo, FALSE),
    continue_on_error = isTRUE(cli_args$continue_on_error),
    dry_run = isTRUE(cli_args$dry_run),
    verify_outputs = !identical(cli_args$verify_outputs, FALSE)
  )

  quit(
    save = "no",
    status = if (any(pipeline_results$status == "failed")) 1L else 0L
  )
}
