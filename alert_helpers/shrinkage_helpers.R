#' Precision-weighted analytical shrinkage for alert multipliers
#'
#' Applies partial pooling to zone-level beta estimates using inverse-variance
#' weighting. Sparse zones (high SE, few model weeks) shrink toward the pooled
#' estimate; data-rich zones retain their signal.
#'
#' @name shrinkage_helpers

# Local weighted median fallback (stats::weighted.median not always available)
weighted_median <- function(x, w) {
  if (length(x) == 0) return(NA_real_)
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w)
  half_w <- sum(w) / 2
  idx <- which(cw >= half_w)[1]
  if (is.na(idx)) return(mean(x, na.rm = TRUE))
  x[idx]
}

# ---------------------------------------------------------------------------
# Single-zone shrinkage
# ---------------------------------------------------------------------------

#' Shrink a single zone beta toward the pooled estimate
#'
#' @param beta_hz Numeric. Zone-specific beta estimate.
#' @param n_model_weeks Numeric. Number of weeks used in the zone-level fit.
#' @param beta_pooled Numeric. Group-level pooled beta estimate.
#' @param min_weeks_threshold Integer. Minimum weeks for full zone weight.
#' @return List with `beta_shrunk`, `shrinkage_weight`, `n_model_weeks`.
shrink_beta <- function(beta_hz, n_model_weeks, beta_pooled,
                        min_weeks_threshold = 3L) {
  # Edge case: zone estimate missing or non-finite -> return pooled
  if (!is.finite(beta_hz)) {
    return(list(
      beta_shrunk = beta_pooled,
      shrinkage_weight = 0,
      n_model_weeks = n_model_weeks
    ))
  }

  # Edge case: n_model_weeks missing -> keep zone estimate (cannot compute SE)
  if (!is.finite(n_model_weeks) || n_model_weeks <= 0) {
    return(list(
      beta_shrunk = beta_hz,
      shrinkage_weight = 1,
      n_model_weeks = n_model_weeks
    ))
  }

  # Edge case: pooled estimate non-positive or non-finite -> keep zone estimate
  if (!is.finite(beta_pooled) || beta_pooled <= 0) {
    return(list(
      beta_shrunk = beta_hz,
      shrinkage_weight = 1,
      n_model_weeks = n_model_weeks
    ))
  }

  # Variance approximation: SE ~ beta / sqrt(n) (Poisson-like)
  se_hz <- abs(beta_hz) / sqrt(n_model_weeks)
  # Pooled SE: rough reference
  se_pooled <- abs(beta_pooled) / sqrt(2)

  # Precision weights
  w_hz <- 1 / se_hz^2
  w_pooled <- 1 / se_pooled^2

  # Dampen zone weight when few weeks observed
  effective_w_hz <- w_hz * pmin(n_model_weeks / min_weeks_threshold, 1)

  # Weighted average
  beta_shrunk <- (beta_hz * effective_w_hz + beta_pooled * w_pooled) /
    (effective_w_hz + w_pooled)
  beta_shrunk <- round(beta_shrunk, 10)

  shrinkage_weight <- effective_w_hz / (effective_w_hz + w_pooled)

  list(
    beta_shrunk = beta_shrunk,
    shrinkage_weight = shrinkage_weight,
    n_model_weeks = n_model_weeks
  )
}

# ---------------------------------------------------------------------------
# Vectorised shrinkage
# ---------------------------------------------------------------------------

#' Vectorised precision-weighted shrinkage for multiple zones
#'
#' @param df Tibble with columns: `beta_hz`, `se_hz`, `beta_pooled`,
#'   `n_model_weeks`.
#' @param tau2 Numeric. Between-zone variance (for hierarchical extension).
#'   Default 0 gives pure precision-weighted shrinkage.
#' @param min_weeks_threshold Integer. Minimum weeks for full zone weight.
#' @return Tibble with `beta_shrunk` and `shrinkage_weight` columns.
shrink_beta_vec <- function(df, tau2 = 0, min_weeks_threshold = 3L) {
  n <- nrow(df)
  if (n == 0) {
    return(tibble::tibble(
      beta_shrunk = numeric(0),
      shrinkage_weight = numeric(0)
    ))
  }

  beta_hz <- df$beta_hz
  se_hz <- df$se_hz
  beta_pooled <- df$beta_pooled
  n_weeks <- df$n_model_weeks

  # Effective variance per zone (add tau2 for hierarchical extension)
  # Use .Machine$double.eps to avoid division by zero
  var_hz <- pmax(se_hz^2, .Machine$double.eps) + tau2
  w_hz <- 1 / var_hz

  # Pooled variance reference (use min epsilon to avoid div-by-zero)
  se_pooled <- pmax(abs(beta_pooled), .Machine$double.eps) / sqrt(2)
  w_pooled <- 1 / se_pooled^2

  # Dampen zone weight for sparse zones
  dampen <- pmin(n_weeks / min_weeks_threshold, 1)
  effective_w_hz <- w_hz * dampen

  # Shrinkage weight (proportion of total weight coming from the zone)
  shrinkage_weight <- effective_w_hz / (effective_w_hz + w_pooled)

  # Shrunk estimate: weighted average of zone and pooled
  beta_shrunk <- (beta_hz * effective_w_hz + beta_pooled * w_pooled) /
    (effective_w_hz + w_pooled)
  # Round to avoid floating-point underflow on exact equality checks
  beta_shrunk <- round(beta_shrunk, 10)

  tibble::tibble(
    beta_shrunk = beta_shrunk,
    shrinkage_weight = shrinkage_weight
  )
}

# ---------------------------------------------------------------------------
# Between-zone variance estimation
# ---------------------------------------------------------------------------

#' Estimate between-zone variance (tau^2) using MAD-LOO hybrid
#'
#' Robust to outliers: uses the median absolute deviation of the core cluster
#' rather than raw sample variance, which would be inflated by extreme sparse
#' zone estimates.
#'
#' @param betas Numeric vector of zone-level beta estimates.
#' @param n_weeks Numeric vector of model weeks per zone.
#' @return Single numeric: estimated tau^2 (>= 0).
estimate_tau2 <- function(betas, n_weeks) {
  # Drop non-finite
  valid <- is.finite(betas) & is.finite(n_weeks)
  betas <- betas[valid]
  n_weeks <- n_weeks[valid]

  if (length(betas) < 2) return(0)

  # Robust location: weighted median by inverse SE
  se_per_zone <- abs(betas) / sqrt(pmax(n_weeks, 1))
  weights <- 1 / pmax(se_per_zone^2, .Machine$double.eps)
  loc <- weighted_median(betas, weights)

  # MAD of deviations from robust center
  mad_val <- stats::mad(betas - loc, constant = 1)
  # Convert MAD to variance estimate (MAD ~ 0.6745 * sigma for normals)
  tau2 <- (mad_val / 0.6745)^2

  max(tau2, 0)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Apply precision-weighted shrinkage to a multipliers dataframe
#'
#' Computes per-window pooled beta estimates, then shrinks each zone's beta
#' toward the pooled value. Returns the input dataframe with added shrunk
#' columns.
#'
#' @param df Tibble with at least: `zone_sante_notification`,
#'   `threshold_time_key`, `beta_c`, `beta_d`, `beta_c_n_model_weeks`,
#'   `beta_d_n_model_weeks`, `estimated_true_alive_cases_recent`,
#'   `expected_deaths`, `val_alive`, `val_dead`.
#' @param use_bayesian Logical. Not used in precision-weighted mode; kept for
#'   API compatibility.
#' @return Tibble with added columns: `beta_c_shrunk`, `beta_d_shrunk`,
#'   `beta_c_shrinkage_weight`, `beta_d_shrinkage_weight`.
bayesian_shrink <- function(df, use_bayesian = FALSE) {
  if (nrow(df) == 0) return(df)

  # Pre-compute SE from beta and n_model_weeks (Poisson-like)
  df <- df |>
    dplyr::mutate(
      se_beta_c = dplyr::if_else(
        is.finite(beta_c) & beta_c_n_model_weeks > 0,
        abs(beta_c) / sqrt(beta_c_n_model_weeks),
        NA_real_
      ),
      se_beta_d = dplyr::if_else(
        is.finite(beta_d) & beta_d_n_model_weeks > 0,
        abs(beta_d) / sqrt(beta_d_n_model_weeks),
        NA_real_
      )
    )

  # Compute pooled beta per window
  window_pooled <- df |>
    dplyr::filter(is.finite(val_alive) & is.finite(estimated_true_alive_cases_recent) &
                    estimated_true_alive_cases_recent > 0) |>
    dplyr::summarise(
      pooled_beta_c = sum(val_alive, na.rm = TRUE) /
        sum(estimated_true_alive_cases_recent, na.rm = TRUE),
      .by = threshold_time_key
    )

  window_pooled_d <- df |>
    dplyr::filter(is.finite(val_dead) & is.finite(expected_deaths) &
                    expected_deaths > 0) |>
    dplyr::summarise(
      pooled_beta_d = sum(val_dead, na.rm = TRUE) /
        sum(expected_deaths, na.rm = TRUE),
      .by = threshold_time_key
    )

  df <- df |>
    dplyr::left_join(window_pooled, by = "threshold_time_key") |>
    dplyr::left_join(window_pooled_d, by = "threshold_time_key")

  # Apply shrinkage per row — build properly-named tibbles for shrink_beta_vec
  shrink_input_c <- tibble::tibble(
    beta_hz = df$beta_c,
    se_hz = df$se_beta_c,
    beta_pooled = df$pooled_beta_c,
    n_model_weeks = df$beta_c_n_model_weeks
  )
  shrink_input_d <- tibble::tibble(
    beta_hz = df$beta_d,
    se_hz = df$se_beta_d,
    beta_pooled = df$pooled_beta_d,
    n_model_weeks = df$beta_d_n_model_weeks
  )

  shrink_c <- shrink_beta_vec(shrink_input_c, tau2 = 0, min_weeks_threshold = 3L)
  shrink_d <- shrink_beta_vec(shrink_input_d, tau2 = 0, min_weeks_threshold = 3L)

  df <- df |>
    dplyr::mutate(
      beta_c_shrunk = shrink_c$beta_shrunk,
      beta_c_shrinkage_weight = shrink_c$shrinkage_weight,
      beta_d_shrunk = shrink_d$beta_shrunk,
      beta_d_shrinkage_weight = shrink_d$shrinkage_weight
    )

  # Remove helper columns
  df |>
    dplyr::select(-dplyr::any_of(c("se_beta_c", "se_beta_d",
                                    "pooled_beta_c", "pooled_beta_d")))
}
