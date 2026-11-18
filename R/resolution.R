# resolution.R — the resolution report: minimum detectable edge (MDE), selection_regret, the MCB
# simultaneous bound, and the R1/R2 refusal rules. Port of resolution.py; R's qt() replaces
# scipy.stats.t.ppf.

#' @keywords internal
.validate_one_sided <- function(alpha, power = NULL) {
  .validate_alpha(alpha)
  if (alpha >= 0.5) {
    stop(sprintf(
      paste0(
        "alpha must be < 0.5 for a one-sided bound to be meaningful (alpha=%s would make the ",
        "critical value non-positive, so the reported bound could come out TIGHTER than the ",
        "observed edge it is supposed to bound); got %s."
      ), alpha, alpha
    ), call. = FALSE)
  }
  if (!is.null(power)) {
    .validate_alpha(power, name = "power")
    if (power <= 0.5) {
      stop(sprintf(
        paste0(
          "power must be > 0.5 (a power target at or below a coin flip is not a real power ",
          "target, and makes the minimum detectable edge go negative); got %s."
        ), power
      ), call. = FALSE)
    }
  }
  invisible(NULL)
}

#' @keywords internal
.effective_n <- function(w) {
  w <- as.numeric(w)
  if (any(abs(w) > .MAX_ABS_LOSS)) {
    stop(sprintf(
      paste0(
        "weight magnitude above %.0e, far beyond any real observation-count or frequency weight ",
        "-- squaring it overflows double precision silently. Check for a units/scaling bug upstream."
      ), .MAX_ABS_LOSS
    ), call. = FALSE)
  }
  if (any(w != 0 & abs(w) < 1e-100)) {
    stop(paste0(
      "weight magnitude below 1e-100 (and nonzero) -- squaring it underflows double precision ",
      "silently, corrupting the effective-sample-size computation with a NaN that then ",
      "propagates through every resolution-report statistic. Check for a units/scaling bug upstream."
    ), call. = FALSE)
  }
  sum(w)^2 / sum(w^2)
}

#' @keywords internal
# Two assumptions, both disclosed in the user docs:
# 1. No autocorrelation: Var(weighted mean) = sigma^2 / n_eff, no HAC correction. Under positive
#    autocorrelation the SE is too small and "resolved" overstates confidence.
# 2. Variance does not depend on weight: var_ is the unweighted variance; only n_eff uses w. When
#    heavy weights sit on volatile periods the SE is understated (false-confidence rate among
#    "resolved" verdicts measured at 15-39% for weight/volatility correlation 0.2-0.8). Typical
#    case: weights are observation counts and the high-count periods are the volatile ones.
#    resolution_report() warns when it sees that pattern.
# A runtime lag-1 ACF check was rejected: at T=10-60 its null sampling SD (~1/sqrt(T)) would fire
# on many fine panels. report() prints a standing disclosure instead. The formula matches Python.
.weighted_se <- function(diff, w) {
  T <- length(diff)
  if (T < 2) stop(sprintf("need at least 2 periods to estimate a standard error; got T=%d.", T), call. = FALSE)
  n_eff <- .effective_n(w)
  var_ <- stats::var(diff)  # ddof = 1, as np.var(ddof=1)
  sqrt(var_) / sqrt(n_eff)
}

#' Closest-pooled-mean rival to the champion: the rival hardest to distinguish on the pooled mean,
#' which is what the power/MDE statistics need. A different question from `decision_breakdown()`'s
#' `binding_opponent` (fewest deletions to flip); the two differ on ~18-25% of panels. Ties go to
#' the first name in radix order.
#' @keywords internal
#' @noRd
.binding_rival <- function(L, w, champ) {
  means <- sapply(names(L), function(m) stats::weighted.mean(as.numeric(L[[m]]), w))
  rivals <- setdiff(names(L), champ)
  dists <- sapply(rivals, function(m) abs(means[m] - means[champ]))
  best_dist <- min(dists)
  tol <- .MARGIN_REL_FLOOR * (abs(means[rivals]) + abs(means[champ])) / 2.0
  tied <- rivals[(dists - best_dist) <= tol]
  sort(tied, method = "radix")[1]
}

#' @keywords internal
.edge_components <- function(L, w) {
  champ <- pooled_winner(L, w)
  rival <- .binding_rival(L, w, champ)
  la <- as.numeric(L[[champ]])
  lb <- as.numeric(L[[rival]])
  diff_ <- lb - la
  se <- .weighted_se(diff_, w)
  mean_champ <- stats::weighted.mean(la, w)
  mean_rival <- stats::weighted.mean(lb, w)
  list(champ = champ, rival = rival, diff = diff_, se = se, mean_champ = mean_champ, mean_rival = mean_rival)
}

#' @keywords internal
# Normalizes by the champion's mean alone, as Python's _scale(). When that mean is near zero
# (a skill score, a de-meaned metric, two near-tied models) the percentage can blow up and swing by
# orders of magnitude under tiny input changes. Disclosed in the Assumption sections.
.res_scale <- function(mean_champ) {
  if (abs(mean_champ) > 1e-12) abs(mean_champ) else 1.0
}

#' Minimum detectable edge against the binding rival
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param power Target power, strictly between 0.5 and 1.
#' @param alpha One-sided significance level (< 0.5).
#' @return The minimum detectable edge, as a fraction of the champion's loss level.
#' @section Assumption: the standard error assumes no autocorrelation in the per-period
#'   (rival - champion) loss differential; under positive autocorrelation it is too small. If that
#'   is a concern, prefer the block-bootstrap [mcs()]/[model_confidence_set()]. The result is a
#'   fraction of the champion's mean loss, so it is meaningless when that mean is near zero (a
#'   skill score, a de-meaned metric, two near-tied models): tiny input changes can swing it by
#'   orders of magnitude. In that regime R and Python can also differ (e.g. champion
#'   `c(1e-8, 3e-8, 3e-8)` vs rival `c(1e8, 1e8, 1e8)`: 1.04 vs 2.54) because their variance
#'   routines round the cancellation differently. Well-scaled losses agree to floating point.
#' @seealso [significance_boundary()], [mcb_bound()], [resolution_report()] (combines all three
#'   into the full R1/R2 refusal-gated workflow).
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' # With only 10 periods of noisy loss, the champion's edge over its binding
#' # rival would have to be at least this large (as a fraction of the champion's
#' # own loss level) for an 80%-power test to have a chance of detecting it.
#' minimum_detectable_edge(panel)
#' @export
minimum_detectable_edge <- function(L, w = NULL, power = 0.80, alpha = 0.05) {
  .validate_one_sided(alpha, power)
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  ec <- .edge_components(L, w)
  crit <- stats::qt(1 - alpha, df = T - 1) + stats::qt(power, df = T - 1)
  (crit * ec$se) / .res_scale(ec$mean_champ)
}

#' One-sided significance threshold against the binding rival
#'
#' @inheritParams minimum_detectable_edge
#' @return The significance threshold, as a fraction of the champion's loss level. The observed
#'   edge must clear this value to be called significant at the stated one-sided `alpha`.
#' @section Assumption: same as [minimum_detectable_edge()]: no autocorrelation in the loss
#'   differential, and a champion mean loss not near zero.
#' @seealso [minimum_detectable_edge()], [mcb_bound()], [resolution_report()].
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' # The champion's observed edge must clear this fraction to be called
#' # significant at the one-sided alpha = 0.05 level against its binding rival.
#' significance_boundary(panel)
#' @export
significance_boundary <- function(L, w = NULL, alpha = 0.05) {
  .validate_one_sided(alpha)
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  ec <- .edge_components(L, w)
  crit <- stats::qt(1 - alpha, df = T - 1)
  (crit * ec$se) / .res_scale(ec$mean_champ)
}

#' MCB-style simultaneous upper confidence bound on the champion's edge
#'
#' @inheritParams minimum_detectable_edge
#' @return The upper confidence bound on the champion's true edge over its binding rival, as a
#'   fraction of the champion's loss level, corrected for the post-hoc rival-selection problem
#'   (unlike `k*`, which is a descriptive fact about the observed data, not a corrected bound).
#' @section Assumption: same as [minimum_detectable_edge()]: no autocorrelation in the loss
#'   differential, and a champion mean loss not near zero.
#'   At extreme loss magnitudes (~1e12) with weight concentrated on one period, rounding can make
#'   the bound slightly smaller than `resolution_report()$observed_edge`; Python behaves the same.
#' @seealso [minimum_detectable_edge()], [significance_boundary()], [resolution_report()],
#'   [prop22_certifies()] (contrast: NOT multiple-comparisons-corrected, for a single
#'   pre-specified rival rather than an adaptively-searched one).
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' # This corrects for having picked the champion as the best of several rivals
#' # after the fact: read the result as "the champion's true edge may be as
#' # large as this fraction of its own loss level, at the stated confidence."
#' mcb_bound(panel)
#' @export
# `point` is weighted.mean(diff), not mean_rival - mean_champ, to match Python bit-for-bit. The two
# are equal algebraically but can round differently at extreme magnitudes (see @section Assumption).
mcb_bound <- function(L, w = NULL, alpha = 0.05) {
  .validate_one_sided(alpha)
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  models <- names(L)
  ec <- .edge_components(L, w)
  n_rivals <- length(models) - 1
  point <- stats::weighted.mean(ec$diff, w)
  crit <- stats::qt(1 - alpha / n_rivals, df = T - 1)
  bound <- point + crit * ec$se
  bound / .res_scale(ec$mean_champ)
}

#' Leave-one-period-out cost of the picking rule
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param weights Optional numeric vector of per-period weights (primary name).
#' @param w Optional alias for `weights` (keyword-only in the Python original; pass only one).
#' @return The weighted-average realised regret of the picking rule.
#' @section Limitation: needs positive weight on at least two periods -- a leave-one-period-out
#'   fold that holds out the only weighted period has nothing left to pick a champion from, so that
#'   case raises an error naming the period.
#' @seealso [resolution_report()], [decision_breakdown()].
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' # Re-picks the champion leaving one period out at a time, then measures how
#' # much worse that re-picked model does, on average, than the best model
#' # actually available in the left-out period -- the realised cost of trusting
#' # the selection rule rather than having picked the true best model each time.
#' selection_regret(panel)
#' @export
selection_regret <- function(L, weights = NULL, w = NULL) {
  if (!is.null(w)) {
    if (!is.null(weights)) {
      stop(paste0(
        "selection_regret() got both 'weights' and 'w' -- pass only one; they are the same ",
        "parameter (w is an alias for backward-compatible weights)."
      ), call. = FALSE)
    }
    weights <- w
  }
  up <- .unwrap_panel(L, weights); L <- up$L; weights <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, weights)
  wv <- if (is.null(weights)) rep(1, T) else as.numeric(weights)
  if (T < 3) stop(sprintf("need at least 3 periods for leave-one-period-out selection_regret; got T=%d.", T), call. = FALSE)
  if (sum(wv > 0) == 1) {
    stop(sprintf(paste0(
      "selection_regret(): all of the positive weight is on a single period (position %d), so ",
      "leaving that period out leaves a fold with no weight to pick a champion from. Spread the ",
      "weight across at least two periods, or drop the zero-weight periods before calling."
    ), which(wv > 0)), call. = FALSE)
  }
  models <- names(L)
  M <- stats::setNames(lapply(models, function(m) as.numeric(L[[m]])), models)
  regrets <- numeric(T)
  for (t in seq_len(T)) {
    keep <- seq_len(T) != t
    fold_L <- lapply(M, function(v) v[keep])
    fold_champ <- pooled_winner(fold_L, wv[keep])
    best_at_t <- min(sapply(models, function(m) M[[m]][t]))
    regrets[t] <- M[[fold_champ]][t] - best_at_t
  }
  stats::weighted.mean(regrets, wv)
}

#' R1/R2 resolution report with refusal rules
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param alpha One-sided significance level for R1 (< 0.5).
#' @param power Target power for the reported MDE (> 0.5).
#' @param mcs_alpha Significance level passed to `identified()` for R2.
#' @param mcs_seed RNG seed passed to `identified()` for R2 (see "Seed sensitivity" below).
#' @return A named list: `champion`, `resolved`, `observed_edge`, `mde`, `significance_boundary`,
#'   `binding_rival`, `identified` (`NULL` when the internal `identified()` call errors; see
#'   "Sentinel conventions" in `?selectionfragility`), `fragility_read`.
#' @seealso [minimum_detectable_edge()], [significance_boundary()], [mcb_bound()] for the
#'   individual statistics this report assembles; [report()] for the one-screen view that reuses
#'   this same R1/R2 logic.
#' @section Assumption: `resolved`, `mde` and `significance_boundary` assume no autocorrelation in
#'   the per-period loss differential, so "resolved" can overstate confidence under autocorrelated
#'   losses (the `identified` check uses the block bootstrap and does not share this assumption).
#'   They also assume per-period variance does not depend on `w`. That fails when weights are
#'   observation counts and the high-count periods are also the volatile ones: among "resolved"
#'   verdicts, a false-confidence rate of 15-39% was measured for weight/volatility correlations of
#'   0.2-0.8, and 8% on an ordinary seasonal reporting pattern. The function warns when `w` is
#'   non-uniform, positively correlated with the differential's volatility, and the read is
#'   "resolved". The warning is a screening heuristic that also fires on ~13-38% of panels where
#'   the assumption holds; treat it as a prompt to check, not proof. It checks the closest-mean
#'   rival only, so it can miss the case that matters: e.g. a grid operator weights winter days
#'   for peak planning, and a winter cold snap is exactly where `decision_breakdown()`'s
#'   `binding_opponent` fails. If your weights line up with when a known pivotal model misses,
#'   check that correlation yourself.
#' @section Seed sensitivity: the `identified` check is a bootstrap, so on a borderline panel the
#'   verdict can depend on `mcs_seed` (see `?mcs`, "Adversarial use"); try a few seeds if it
#'   matters.
#' @section Reading the two gates: R1 (`resolved`) and R2 (`identified`/`fragility_read`) can
#'   refuse at the same time for different reasons (e.g. the edge isn't powered AND non-uniform
#'   weights make the MCS call refuse). Inspect both fields, not just `fragility_read`, to see which
#'   gate failed. `identified = NULL` means the MCS could not be computed for this panel (a
#'   data-driven refusal, such as non-uniform weights); an invalid `mcs_alpha`/`mcs_seed` raises an
#'   error immediately rather than being reported as "undetermined".
#' @examples
#' # Only 4 periods, and nbeats narrowly beats ar1 on average -- but with this
#' # little data, that edge is NOT resolved: it falls short of the significance
#' # boundary (R1), and the champion isn't even uniquely identified by the MCS
#' # (R2). Expect the small-sample warning on a panel this tiny.
#' L <- list(
#'   ar1    = c(1.00, 0.95, 1.05, 0.90),
#'   nbeats = c(0.95, 0.97, 0.90, 0.99)
#' )
#' panel <- loss_panel(L)
#' r <- resolution_report(panel)
#' r$champion       # "nbeats" -- the pooled winner
#' r$resolved       # FALSE -- observed edge doesn't clear the significance boundary
#' r$fragility_read # "undetermined" -- too little data to certify this pick
#' @export
resolution_report <- function(L, w = NULL, alpha = 0.05, power = 0.80, mcs_alpha = 0.10, mcs_seed = 0) {
  # Validate before the tryCatch around identified(), which would otherwise turn a typo into
  # fragility_read = "undetermined". (Python's resolution_report() still does.)
  .validate_alpha(mcs_alpha, name = "mcs_alpha")
  .validate_seed(mcs_seed, "resolution_report(): mcs_seed")
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  ww <- if (is.null(w)) rep(1, T) else as.numeric(w)
  ec <- .edge_components(L, ww)
  observed_edge <- (ec$mean_rival - ec$mean_champ) / .res_scale(ec$mean_champ)

  mde <- minimum_detectable_edge(L, ww, power = power, alpha = alpha)
  sig_boundary <- significance_boundary(L, ww, alpha = alpha)
  resolved <- isTRUE(observed_edge >= sig_boundary)

  is_identified <- NULL
  fragility_read <- NULL
  id_result <- tryCatch(isTRUE(identified(L, alpha = mcs_alpha, w = ww, seed = mcs_seed)), error = function(e) e)
  if (inherits(id_result, "error")) {
    is_identified <- NULL
    fragility_read <- "undetermined"
  } else {
    is_identified <- id_result
    if (!is_identified) {
      fragility_read <- "undetermined"
    } else if (!resolved) {
      fragility_read <- "undetermined"
    } else {
      fragility_read <- "resolved"
    }
  }

  # Screening warning for the variance-homogeneity assumption (see .weighted_se): non-uniform
  # weights that correlate POSITIVELY with the differential's squared residuals. Negative
  # correlation makes the SE conservative, so it is not flagged. Gated on `resolved`, not
  # fragility_read, because non-uniform weights always make fragility_read "undetermined" while
  # report() still prints "resolved". Fires on noise at ~13-38% (T=40 down to T=6) when the
  # assumption holds. It checks the closest-mean rival only, so it can miss weight correlation
  # aimed at decision_breakdown()'s binding_opponent.
  if (isTRUE(resolved) && length(ww) >= 4 && !.weights_are_uniform(ww)) {
    cv_w <- stats::sd(ww) / mean(ww)
    if (is.finite(cv_w) && cv_w > 0.3) {
      # Unweighted mean as baseline: a weighted mean pulls toward the heavy periods and flips the sign.
      resid2 <- (ec$diff - mean(ec$diff))^2
      wv_cor <- suppressWarnings(stats::cor(ww, resid2))
      if (is.finite(wv_cor) && wv_cor > 0.2) {
        warning(sprintf(paste0(
          "resolution_report(): weights are non-uniform (CV=%.2f) and POSITIVELY correlate with ",
          "the per-period differential's volatility (r=%.2f) -- .weighted_se() uses the RAW, ",
          "UNWEIGHTED variance (only the effective-sample-size denominator uses the weights), ",
          "which can understate the true SE when high-weight periods are also the volatile ones. ",
          "This is a CHEAP SCREENING HEURISTIC on one realization of the data, not a confirmed ",
          "diagnosis -- it can and does fire on panels where the understatement isn't actually ",
          "severe, especially at small T. Treat it as a prompt to sanity-check this 'resolved' ",
          "read yourself, not as proof the read is wrong; see resolution_report()'s @section ",
          "Assumption for the confirmed scope of this issue."
        ), cv_w, wv_cor), call. = FALSE)
      }
    }
  }

  list(
    champion = ec$champ,
    resolved = resolved,
    observed_edge = observed_edge,
    mde = mde,
    significance_boundary = sig_boundary,
    binding_rival = ec$rival,
    identified = is_identified,
    fragility_read = fragility_read
  )
}
