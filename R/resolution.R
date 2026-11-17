# resolution.R — the resolution report: minimum detectable edge (MDE), selection_regret, the MCB
# simultaneous bound, and the R1/R2 refusal rules. Faithful port of resolution.py; uses R's `qt()`
# in place of scipy.stats.t.ppf (same Student-t quantile function).

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
    .validate_alpha(power)
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
# I.I.D. ASSUMPTION, NOT HAC-CORRECTED (ADDED 2026-11-17, 6-reviewer convergence pass): this
# estimates Var(weighted mean) = sigma^2/n_eff under an implicit assumption that successive
# per-period (rival - champion) differentials are uncorrelated. Forecast loss differentials are
# canonically autocorrelated in practice -- the same reason this package's own mcs()/
# model_confidence_set() runs a block bootstrap rather than an i.i.d. variance formula. Under
# positive autocorrelation, this SE understates the true uncertainty, so
# minimum_detectable_edge()/significance_boundary()/mcb_bound()/resolution_report() can report
# more confidence (a smaller MDE, a "resolved" read) than the data actually supports. No HAC/
# Newey-West correction is implemented here; prefer mcs()/model_confidence_set()'s
# block-bootstrap-based identification over this module's closed-form significance reads if
# autocorrelation is a live concern for your panel.
.weighted_se <- function(diff, w) {
  T <- length(diff)
  if (T < 2) stop(sprintf("need at least 2 periods to estimate a standard error; got T=%d.", T), call. = FALSE)
  n_eff <- .effective_n(w)
  var_ <- stats::var(diff)  # R's var() is ddof=1 (sample variance) by default, matching np.var(ddof=1)
  sqrt(var_) / sqrt(n_eff)
}

#' @keywords internal
.binding_rival <- function(L, w, champ) {
  means <- sapply(names(L), function(m) stats::weighted.mean(as.numeric(L[[m]]), w))
  rivals <- setdiff(names(L), champ)
  dists <- sapply(rivals, function(m) abs(means[m] - means[champ]))
  best_dist <- min(dists)
  tol <- .MARGIN_REL_FLOOR * (abs(means[rivals]) + abs(means[champ])) / 2.0
  tied <- rivals[(dists - best_dist) <= tol]
  sort(tied)[1]
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
.res_scale <- function(mean_champ) {
  if (abs(mean_champ) > 1e-12) abs(mean_champ) else 1.0
}

#' Minimum detectable edge against the binding rival
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param power Target power (> 0.5).
#' @param alpha One-sided significance level (< 0.5).
#' @return The minimum detectable edge, as a fraction of the champion's loss level.
#' @section Assumption: assumes no autocorrelation in the per-period (rival - champion) loss
#'   differential -- see `.weighted_se`'s comment in resolution.R for why this can matter and
#'   what to do instead (prefer `mcs()`/`model_confidence_set()`'s block bootstrap) when it does.
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
#' @section Assumption: assumes no autocorrelation in the per-period (rival - champion) loss
#'   differential -- see `.weighted_se`'s comment in resolution.R.
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
#' @section Assumption: assumes no autocorrelation in the per-period (rival - champion) loss
#'   differential -- see `.weighted_se`'s comment in resolution.R.
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
#'   `binding_rival`, `identified`, `fragility_read`.
#' @seealso [minimum_detectable_edge()], [significance_boundary()], [mcb_bound()] for the
#'   individual statistics this report assembles; [report()] for the one-screen view that reuses
#'   this same R1/R2 logic.
#' @section Assumption: `resolved`/`mde`/`significance_boundary` assume no autocorrelation in the
#'   per-period loss differential (see `.weighted_se`'s comment in resolution.R). `fragility_read`'s
#'   `identified` check does not share this assumption (it goes through the block-bootstrap MCS),
#'   but the R1 `resolved` gate does; a "resolved" read can overstate confidence under
#'   autocorrelated losses.
#' @section Seed sensitivity: `mcs_seed` (ADDED 2026-11-17, 6-reviewer adversarial pass):
#'   `fragility_read`'s `identified` check is a bootstrap-based MCS call, and on a genuinely
#'   borderline panel the verdict can be seed-sensitive (see `?mcs`'s "Adversarial use" section
#'   for a quantified example). Previously silently pinned to `seed=0`; pass a few different seeds
#'   yourself near a borderline alpha if this matters for your use case.
#' @examples
#' # Only 4 periods, and nbeats narrowly beats ar1 on average -- but with this
#' # little data, that edge is NOT resolved: it falls short of the significance
#' # boundary (R1), and the champion isn't even uniquely identified by the MCS
#' # (R2). The report's job is to say so honestly rather than overclaim.
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
