# resolution.R — the resolution report: minimum detectable edge (MDE), selection_regret, the MCB
# simultaneous bound, and the R1/R2 refusal rules. Faithful port of resolution.py; uses R's `qt()`
# in place of scipy.stats.t.ppf (same Student-t quantile function).

#' @keywords internal
# `power` reuses .validate_alpha()'s (0,1) range check under its own name, so an invalid power
# raises "power must be ...", not "alpha must be ...". (Python's _validate_one_sided() still
# reuses the alpha-worded message here; error TEXT is not part of cross-language numeric parity.)
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
# I.I.D. ASSUMPTION, NOT HAC-CORRECTED: this
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
#
# SECOND, SEPARATE ASSUMPTION -- VARIANCE HOMOGENEITY ACROSS WEIGHT CLASSES: `var_` is the RAW,
# UNWEIGHTED sample variance of `diff`; only `n_eff` (the Kish effective-sample-size denominator)
# actually uses `w`. This is only valid if per-period variance doesn't depend on weight. Weights
# are documented elsewhere (loss_panel()'s own docs) as typically "observation counts per period" --
# exactly the kind of weight that, in real panels, often correlates with how volatile a period's
# loss differential is (e.g. a few high-observation-count periods that are also the volatile ones,
# many low-observation-count periods that are flat). When that correlation holds, the many flat
# periods drag the pooled unweighted variance down even though the decision is actually being
# driven by the volatile, high-weight few -- understating this SE and letting
# minimum_detectable_edge()/significance_boundary()/mcb_bound() print a confident "resolved" edge
# that the weight-heterogeneous variance doesn't actually support. No weighted-variance correction
# is implemented here; this is a genuine, undisclosed-until-now sibling of the autocorrelation
# caveat above, not a coding bug.
#
# SEVERITY, RE-VERIFIED (not a rare corner case): a dedicated re-verification measured the
# false-confidence rate among "resolved" verdicts directly -- NOT just one hand-picked 18-vs-2
# split, but 1200 synthetic panels at graded, realistic weight/volatility correlations (Gaussian-
# copula rho in {0.1, 0.2, 0.5, 0.8}, plus an unremarkable seasonal-reporting weight pattern with
# only a mild ~2x volume/volatility bump). Conditional on the package already saying "resolved,"
# the false-confidence rate scaled smoothly from 15.3% at rho=0.2 to 38.7% at rho=0.8, and was
# still 26.9% at rho=0.1 (barely correlated) and 8.1% in the unremarkable seasonal case -- this is
# a graded, commonly-triggered risk at mild/realistic correlations, not an extreme-only failure.
# resolution_report() now warns (see below) when the same conditions that make this likely are
# present AND it is about to report "resolved" -- a guard clause, not a formula change, so it
# doesn't touch this shared, oracle-pinned function.
#
# WHY THE AUTOCORRELATION ASSUMPTION ITSELF HAS NO RUNTIME GUARD (2026-11-18, considered and
# rejected, not an oversight): a domain-expert real-world review round found that on realistic,
# UNIFORM-weight macro/financial panels (the common case), nothing at runtime signals a violation
# of this function's "no autocorrelation" assumption -- the weight/volatility guard above is
# gated on NON-uniform weights and structurally cannot fire here, and report()'s own
# VERDICT/RESOLUTION disagreement caveat only fires when that disagreement happens to manifest on
# the specific data draw, not reliably. A lightweight lag-1-ACF-threshold runtime check was
# considered as the obvious fix and rejected: at this package's typical T (~10-60), the sample
# lag-1 ACF's sampling SD under the TRUE null (no real autocorrelation) is ~1/sqrt(T) -- roughly
# 0.29 at T=12, 0.18 at T=30, 0.13 at T=60. Any threshold sensitive enough to flag a real rho~0.5
# case (the kind that actually matters) would also fire on a large fraction of genuinely-fine
# panels, and -- unlike the weight/volatility guard above, which only fires when weights are
# non-uniform (a real gating precondition that keeps its own 15-39% false-positive rate scoped to
# an unusual case) -- this would be exposed on EVERY uniform-weighted call, i.e. the common case,
# with no gate to narrow it. That crosses from "screening heuristic" into "noise on nearly every
# call." A statistically proper test (Ljung-Box with correct lag selection, HAC-consistent
# variance, small-T correction) is a much larger scope addition requiring its own calibration
# pass, and still would not fix the underlying assumption violation without redesigning this
# shared, oracle-pinned `.weighted_se()` formula itself. The chosen alternative is an
# always-printed, non-conditional disclosure in report()'s RESOLUTION section (see R/report.R)
# instead of a dynamic guard -- do not re-add a naive ACF check later without re-deriving why it
# was rejected here.
.weighted_se <- function(diff, w) {
  T <- length(diff)
  if (T < 2) stop(sprintf("need at least 2 periods to estimate a standard error; got T=%d.", T), call. = FALSE)
  n_eff <- .effective_n(w)
  var_ <- stats::var(diff)  # R's var() is ddof=1 (sample variance) by default, matching np.var(ddof=1)
  sqrt(var_) / sqrt(n_eff)
}

#' Closest-pooled-mean rival to the champion -- a DIFFERENT question from `decision_breakdown()`'s
#' `binding_opponent` (fewest-deletions-to-flip, k*-minimality), not a competing tie-break for the
#' same one. See the consolidated explanation in `decision_breakdown()`'s `@section Limitation`
#' (fragility.R) for the full comparison across all three tie-break/selection rules in this package.
#' This one feeds `minimum_detectable_edge()`/`significance_boundary()`/`mcb_bound()`/
#' `resolution_report()`'s own `binding_rival` field -- the rival that is hardest to statistically
#' distinguish from the champion on the pooled mean, which is what matters for a power/MDE
#' calculation, not which opponent is cheapest to flip to by deleting periods. It is not restricted
#' to `decision_breakdown()`'s k*-tied set at all, and ties here are broken by sorted name (same
#' alphabetical convention as `decision_breakdown()`, for the same reason: a stable, citable name,
#' not a data-invariance property anything downstream of THIS function depends on). Measured
#' directly: ~18-25% of random panels with a strict pooled winner name a DIFFERENT model here than
#' `decision_breakdown()`'s `binding_opponent` does -- common, not a contrived edge case. Both
#' answers are correct for their own question; `report()`'s RESOLUTION paragraph reflects this
#' function, its PIVOT paragraph reflects `decision_breakdown()`/`concentration_share()` -- see the
#' inline comment at report.R's PIVOT call site.
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
# KNOWN LIMITATION, NOT FIXED HERE: normalizing by the champion's own mean alone (rather than
# some combination of champion and rival scale) can produce a nonsensical, unbounded "percentage"
# when the champion and rival sit at very different loss scales (both individually valid,
# unflagged loss magnitudes -- e.g. champion mean ~1e-8, rival mean ~1e8 can print an "observed
# edge" over 9,999,999,650%, straight into report()'s RESOLUTION line with no guard).
#
# BROADER THAN INITIALLY DOCUMENTED (5-lens round, 2026-11-18): the trigger isn't "extreme
# cross-model scale mismatch" -- it's simply mean_champ landing close to the +-1e-12 floor below
# in ABSOLUTE terms, which happens on completely ordinary, SAME-scale, non-adversarial data
# whenever the champion's own weighted mean is small relative to its volatility (a skill score, a
# de-meaned/benchmark-relative metric, or -- this package's central use case -- two genuinely
# close/tied models). Confirmed: a sub-measurement-precision weight perturbation (1e-9 to 1e-11,
# far below any realistic measurement precision) can swing minimum_detectable_edge()/
# significance_boundary()/mcb_bound()/resolution_report()$observed_edge by 10+ ORDERS OF
# MAGNITUDE between two values that both look superficially plausible, because the 1e-12 floor
# is a hard, unsmoothed discontinuity rather than something that tapers the near-zero problem
# away. A 2000-trial frequency check on unremarkable, same-scale 8-period panels (no weights, no
# engineered scale gap) found ~0.1% already producing an observed-edge magnitude over 10,000%
# from ordinary sampling variation alone.
#
# This formula is shared, oracle-pinned behavior with the Python original's `_scale(mean_champ)`
# (identify.py's sibling module resolution.py) -- changing it here alone would create exactly the
# cross-language divergence this package's review discipline exists to prevent, and changing it
# in both languages is a statistical-methodology decision outside the scope of a CRAN-submission
# pass. Left as a documented caveat (see minimum_detectable_edge()/significance_boundary()/
# mcb_bound()'s @section Assumption) rather than silently redefining the statistic right before
# submission.
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
#' @section Assumption: assumes no autocorrelation in the per-period (rival - champion) loss
#'   differential -- see `.weighted_se`'s comment in resolution.R for why this can matter and
#'   what to do instead (prefer `mcs()`/`model_confidence_set()`'s block bootstrap) when it does. Also assumes the champion
#'   and its binding rival sit at a comparable loss scale -- an extreme scale mismatch between
#'   them can produce a nonsensical, unbounded percentage (normalization is by the champion's
#'   own mean only); sanity-check this fraction yourself if your models' loss magnitudes differ
#'   by many orders of magnitude. The trigger is actually broader than "extreme scale mismatch":
#'   normalization divides by the champion's mean with a hard floor at 1e-12, so the SAME-scale,
#'   non-adversarial case of a champion mean that's merely small relative to its volatility (a
#'   skill score, a de-meaned metric, or simply two close/tied models -- this package's central
#'   use case) can also land near that floor, where a sub-measurement-precision input change
#'   (e.g. one weight shifted by 1e-9) swings this value by 10+ orders of magnitude between two
#'   superficially plausible-looking answers. Both answers are meaningless near this floor;
#'   sanity-check whenever your champion's mean is close to zero, not just when scales visibly
#'   differ. In that same pathological regime this value can -- for specific
#'   inputs, not as a general rule of the scale mismatch alone -- also differ numerically from the
#'   Python original by a small constant factor: confirmed reproducible, e.g. champion losses
#'   `c(1e-8, 3e-8, 3e-8)` against a rival `c(1e8, 1e8, 1e8)` gives R 1.0378 vs Python 2.5421 (a
#'   factor of sqrt(6)), because the paired standard error is a `var()` of near-equal huge numbers
#'   and R's and NumPy's variance routines round that catastrophic cancellation differently bit for
#'   bit. Other extreme-scale panels have been observed to agree to floating-point instead -- this
#'   is a rare, input-dependent rounding artifact, not a threshold you can rely on crossing at a
#'   fixed number of orders of magnitude. Both answers are meaningless in this regime regardless;
#'   well-scaled losses agree to floating-point.
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
#'   differential -- see `.weighted_se`'s comment in resolution.R. Also assumes the champion and
#'   its binding rival sit at a comparable loss scale -- see [minimum_detectable_edge()]'s
#'   Assumption section for why an extreme scale mismatch can produce a nonsensical percentage.
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
#'   differential -- see `.weighted_se`'s comment in resolution.R. Also assumes the champion and
#'   its binding rival sit at a comparable loss scale -- see [minimum_detectable_edge()]'s
#'   Assumption section for why an extreme scale mismatch can produce a nonsensical percentage.
#'   The "never tighter than the observed edge" guarantee below additionally assumes `point` and
#'   `resolution_report()$observed_edge` agree to floating-point precision; see the inline comment
#'   above this function's body for a constructed combination (extreme loss magnitude + weight
#'   concentration + near-zero true variance) where that guarantee is confirmed to break.
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
# KNOWN LIMITATION, NOT FIXED HERE (exhaustive-adversarial round, 2026-11-18, `.edge_components()`
# self-consistency lens): `point` below is computed as `weighted.mean(ec$diff, w)` -- the per-period
# (rival - champion) differential, weighted-averaged -- while `resolution_report()$observed_edge` is
# computed from the SAME underlying data as `(ec$mean_rival - ec$mean_champ) / .res_scale(...)` --
# two SEPARATELY weighted-averaged means, then subtracted. Both are the exact same quantity in exact
# arithmetic (`weighted.mean(lb, w) - weighted.mean(la, w) == weighted.mean(lb - la, w)` algebraically),
# but NOT bit-identical in double precision: the two paths round differently when the champion/rival
# sit at large loss magnitude (both individually valid, within `.MAX_ABS_LOSS`) combined with weight
# concentration on one period, because `mean_rival - mean_champ` subtracts two already-rounded huge
# weighted sums (losing precision on the order of the champion's own magnitude's ulp), while
# `weighted.mean(diff, w)` is built from the per-period `diff = lb - la` computed BEFORE any
# weighted-averaging (an exact, Sterbenz-lemma subtraction when `la`/`lb` are close in magnitude).
# This is not merely cosmetic: the same regime can ALSO round the true per-period noise in `diff` away
# entirely (the noise is smaller than the ulp at that magnitude), making `.weighted_se()`'s `se`
# round to exactly 0 -- removing the one term (`crit * se`, both non-negative) this function's own
# docstring relies on to guarantee `bound` is "by construction never tighter than the observed
# point-estimate edge." Confirmed reproducible: champion losses `1e12 + rnorm(8)` (seed 1), rival =
# champion + 0.0007 + rnorm(8, 0, 1e-7), weight `c(1e8, rep(1, 7))` gives `se == 0` and `point ==
# 0.000732421875` against `mean_rival - mean_champ == 0.0008544921875` for the identical panel --
# `mcb_bound(L, w)` (7.3242e-16) comes out strictly SMALLER than `resolution_report(L, w)
# $observed_edge` (8.5449e-16) on the exact same data, directly violating the "never tighter" claim
# above. CONFIRMED SHARED, oracle-pinned behavior: the identical repro against the Python original
# (resolution.py's own `point = float(np.average(diff, weights=w))`, which its own comment says is
# "kept... not mean_rival - mean_champ, to preserve the exact prior floating-point computation path
# bit-for-bit") reproduces the exact same violation bit-for-bit in Python too -- this is a pre-existing
# property of the shared formula, not something introduced by the R port. Switching `point` to
# `ec$mean_rival - ec$mean_champ` here alone would fix THIS specific invariant at this specific input
# but silently diverge mcb_bound()'s ordinary-case numeric output from the Python oracle (the whole
# reason `point` is NOT already computed that way, per the Python comment above) -- exactly the
# cross-language divergence this package's review discipline exists to prevent. Left undisclosed-
# until-now rather than fixed; both `mcb_bound()` and `resolution_report()$observed_edge` are
# individually finite and well-defined in this regime, just not mutually consistent with each other
# at this specific combination of extreme magnitude, weight concentration, and near-zero true signal.
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
#'   `binding_rival`, `identified` (`NULL`, not `NA`, when the internal `identified()` call
#'   errors -- see `?selectionfragility`'s "Sentinel conventions" section, including why this
#'   field specifically mirrors the Python oracle's `None`), `fragility_read`.
#' @seealso [minimum_detectable_edge()], [significance_boundary()], [mcb_bound()] for the
#'   individual statistics this report assembles; [report()] for the one-screen view that reuses
#'   this same R1/R2 logic.
#' @section Assumption: `resolved`/`mde`/`significance_boundary` assume no autocorrelation in the
#'   per-period loss differential (see `.weighted_se`'s comment in resolution.R). `fragility_read`'s
#'   `identified` check does not share this assumption (it goes through the block-bootstrap MCS),
#'   but the R1 `resolved` gate does; a "resolved" read can overstate confidence under
#'   autocorrelated losses. The R1 gate also assumes per-period variance doesn't depend on `w`
#'   (`.weighted_se`'s comment again) -- re-verified NOT a rare edge case: among "resolved" verdicts,
#'   a false-confidence rate of 15-39% was measured across mild-to-strong, realistic weight/
#'   volatility correlations (as low as rho=0.1), and 8% even on an unremarkable seasonal
#'   observation-count pattern. This function now `warning()`s when `w` is non-uniform AND
#'   POSITIVELY correlates with the differential's volatility AND it is about to report "resolved".
#'   The warning is a cheap, single-realization screening heuristic, not a precise diagnosis -- it
#'   also fires on a non-trivial fraction of panels where the assumption genuinely holds (~13-38%
#'   depending on T, purely from sampling noise in the correlation estimate); treat it as a prompt
#'   to sanity-check the result yourself, not as proof the result is wrong.
#' @section Seed sensitivity: `mcs_seed` -- `fragility_read`'s `identified` check is a
#'   bootstrap-based MCS call, and on a genuinely
#'   borderline panel the verdict can be seed-sensitive (see `?mcs`'s "Adversarial use" section
#'   for a quantified example). Previously silently pinned to `seed=0`; pass a few different seeds
#'   yourself near a borderline alpha if this matters for your use case.
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
#' # (R2). The report's job is to say so honestly rather than overclaim. This T=4/K=2 panel is
#' # deliberately tiny -- small enough to trigger the internal MCS engine's own small-sample
#' # warning too, which is apt here, not noise: the whole point of this example is a panel too
#' # small to trust.
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
  # Validate the R2 arguments BEFORE the tryCatch around identified() below: that catch exists to
  # turn a data-driven MCS refusal (e.g. non-uniform weights) into fragility_read="undetermined",
  # and would otherwise also swallow a caller's own typo (mcs_alpha=2, mcs_seed=-5) into the same
  # honest-looking "undetermined" read. (Python's resolution_report() still swallows these.)
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

  # CONFIRMED GAP, NOT FIXED HERE (realistic-domain pass, 2026-11-18, utility short-term-load-
  # forecasting scenario -- see tests/testthat/test-realworld-energy.R): the warning below is
  # computed against `ec$rival` == `.binding_rival(L, w, champ)` (closest-pooled-mean rival), NOT
  # `decision_breakdown()`'s `binding_opponent` (fewest-deletions-to-flip rival) -- the two are
  # already documented above (`.binding_rival()`'s own comment) to disagree on ~18-25% of random
  # panels with a strict pooled winner. Concretely confirmed to matter for THIS warning, not just
  # for the headline statistic: on a simulated 365-day, 4-model load-forecasting panel where a
  # grid operator legitimately weights winter months more heavily (for winter peak-demand
  # planning) and a 9-day winter cold-snap event is exactly where the champion's binding_opponent
  # (by decision_breakdown()'s rule) has a concentrated tail-response weakness, the weight/
  # volatility correlation guard below never fires (it checks the champion against the closest-
  # pooled-mean rival instead, whose differential is NOT volatility-correlated with the weights),
  # even though the identical diagnostic computed against the champion/binding_opponent pair
  # clears the guard's own CV>0.3 and correlation>0.2 thresholds comfortably. So a realistic
  # winter-weighting choice can defeat this exact safety warning precisely when it would matter
  # most -- not a contrived input, just a different, equally-valid choice of "which rival" than
  # the one this guard happens to use. Left undocumented-until-now rather than fixed: switching
  # the warning to check against binding_opponent instead would itself need its own dedicated
  # review (binding_opponent is a k*-minimality rule, not a statistical-distinguishability rule,
  # so using it here changes what the guard actually measures) rather than a unilateral change
  # bundled into an unrelated pass. Caller guidance: if your weights are non-uniform AND
  # correlated with when a KNOWN fragile/pivotal model (per decision_breakdown()'s
  # binding_opponent, not this field's binding_rival) tends to miss, check that correlation
  # yourself -- don't rely on this warning to catch it.
  #
  # Low-risk mitigation for the weighted-SE assumption disclosed in .weighted_se()'s comment
  # (variance-homogeneity-across-weight-classes): a dedicated re-verification measured a 28.5%
  # OVERALL false-confidence rate among "resolved" verdicts, averaged across rho=0.1-0.8 weight/
  # volatility correlations combined -- and the rate is still double-digit (15.3%-38.7%, see the
  # per-rho breakdown and the 8.1% seasonal-pattern figure above) even at mild, non-adversarial
  # correlation -- not a rare edge case. Rather than touch the shared,
  # oracle-pinned .weighted_se() formula this close to submission, warn when the SAME conditions
  # that make the understatement likely are present on a panel this function is about to call
  # "resolved": non-uniform weights (CV(w) above a modest threshold) that correlate with the
  # per-period differential's squared residuals (a cheap, no-formula-change proxy for "the
  # volatile periods are also the heavily-weighted ones").
  # NOTE: gated on `resolved` (the R1-only boolean), NOT `fragility_read`. For any non-uniform-
  # weighted panel, MCS::MCSprocedure() already rejects the weights, forcing identified() to error
  # and fragility_read to "undetermined" regardless of this warning -- gating on fragility_read
  # here would make this guard unreachable dead code (the exact bug class already found and fixed
  # in report.R's Prop 2.2 branch). report()'s own RESOLUTION line prints "-- resolved" from
  # `resolved` directly (see report()'s RESOLUTION block), independent of fragility_read/VERDICT, which is
  # exactly the risk this guard is meant to catch.
  # STRICT-AUDIT FIX (2026-11-18): the guard originally fired on abs(wv_cor) > 0.2 -- but a NEGATIVE
  # correlation (high weight on the CALM periods) makes .weighted_se() OVERSTATE the true SE, which
  # is conservative, not the false-confidence risk this guard exists to catch. Only POSITIVE
  # correlation is the documented risk; fixed to wv_cor > 0.2 (one-sided), not abs(). A SEPARATE,
  # self-found bug in this same pass: the residual baseline was weighted.mean(diff, w), which
  # creates a mechanical sign artifact (wherever the weight concentrates, the weighted mean gets
  # pulled there, making THOSE points' own residual artificially small) -- on the test suite's own
  # real-risk panel this flipped the sign (cor=-0.9998 instead of the true +0.98), silently making
  # the guard NEVER fire on the exact case it's supposed to catch. Fixed to use the UNWEIGHTED mean
  # as the residual baseline. Separately, a dedicated re-verification found this single-realization
  # cor() proxy fires on pure sampling noise at a non-trivial rate even when the homogeneity
  # assumption genuinely holds (freshly re-measured against the corrected unweighted-mean proxy:
  # ~38% at T=6, ~28% at T=10, ~22% at T=20, ~13% at T=40, with NO real weight/volatility
  # relationship) -- it is a cheap screening heuristic, not a precise diagnosis, and the warning
  # text below says so explicitly rather than implying a confirmed problem.
  if (isTRUE(resolved) && length(ww) >= 4 && !.weights_are_uniform(ww)) {
    cv_w <- stats::sd(ww) / mean(ww)
    if (is.finite(cv_w) && cv_w > 0.3) {
      # SELF-FOUND FIX (same pass): the residual baseline MUST be the UNWEIGHTED mean, not
      # weighted.mean(diff, ww). Using the weighted mean creates a mechanical sign artifact --
      # wherever the weight concentrates, the weighted mean gets pulled there, making THOSE
      # points' own residual artificially small, which flips the correlation's sign relative to
      # the real relationship between weight and underlying volatility. Confirmed directly: on
      # the test suite's own real-risk panel (heavy weight on the 2 genuinely volatile periods),
      # the weighted-mean version gave cor=-0.9998 (wrongly negative, guard silently never fired)
      # while the unweighted-mean version correctly gives cor=+0.98.
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
