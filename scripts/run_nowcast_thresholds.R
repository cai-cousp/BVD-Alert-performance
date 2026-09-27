#!/usr/bin/env Rscript

# Convenience wrapper: run the nowcast and alert-threshold stages only.
#
# Usage:
#   Rscript scripts/run_nowcast_thresholds.R
#   source("scripts/run_nowcast_thresholds.R")   # in an interactive session
#
# Edit the `stages` vector below to run a different subset (see
# `pipeline_stage_catalog()` or `Rscript scripts/run_pipeline.R --list-stages`).

root <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()

source(file.path(root, "scripts", "run_pipeline.R"))

run_pipeline(stages = c("nowcast", "thresholds"))
