# prop22.R — Proposition 2.2, inverted into a k* certificate, plus the certified-tied subset.
# Faithful port of prop22.py.

#' @keywords internal
.prop22_threshold <- function(T, z = 1.96) {
  k <- 0
  while ((k + 1) < T && sqrt(T * (k + 1) / (T - (k + 1))) < z) k <- k + 1
  k
}

#' Zero-simulation certificate of non-significance for a champion-vs-rival k*
#'
#' Scope, not enforced here: this function takes a bare `k`/`T`, with no visibility into how `k`
#' was computed, so it CANNOT itself check two preconditions the caller must. (1) `k` must come
#' from an UNWEIGHTED `breakdown_number()` call -- see `certified_tied_subset()`'s own guard for
#' the bug this produces when violated (a genuinely significant gap silently certified
#' "non-significant"). (2) This certifies the SINGLE binding rival named by k*'s definition (min
#' over rivals), not an adaptively re-searched one -- if your workflow re-runs
#' `decision_breakdown()` with different weight vectors until a favorable rival turns up, that is
#' a multiple-comparisons problem this certificate does not correct for (contrast [mcb_bound()],
#' which DOES Bonferroni-correct for an analogous post-hoc rival-selection problem).
#'
#' @param k The pairwise decision breakdown point (k*).
#' @param T Number of periods.
#' @param z Two-sided critical value (default 1.96, two-sided 5%).
#' @return `TRUE`/`FALSE`, or `NA` when the certificate does not apply (`k <= 0` or `k >= T`).
#' @seealso [certified_tied_subset()] (applies this certificate across a whole panel),
#'   [breakdown_number()]/[decision_breakdown()] (where `k` comes from), [mcb_bound()] (the
#'   multiple-comparisons-corrected analogue for an adaptively-searched rival).
#' @export
#' @examples
#' prop22_certifies(2, T = 10)   # TRUE  -- k*=2 is certified non-significant at T=10
#' prop22_certifies(5, T = 10)   # FALSE -- k*=5 is too large for Prop 2.2 to certify
#' prop22_certifies(0, T = 10)   # NA -- sentinel: k <= 0 means no breakdown exists, out of scope
#' prop22_certifies(10, T = 10)  # NA -- sentinel: k >= T is also out of scope
prop22_certifies <- function(k, T, z = 1.96) {
  if (!(is.finite(k) && is.finite(T) && is.finite(z))) {
    stop(sprintf("prop22_certifies() requires finite k, T, z -- got k=%s, T=%s, z=%s", k, T, z), call. = FALSE)
  }
  if (z <= 0) stop(sprintf("prop22_certifies() requires z > 0 (a critical value) -- got z=%s", z), call. = FALSE)
  if (k <= 0 || k >= T) return(NA)
  isTRUE(k <= .prop22_threshold(T, z = z))
}

#' @keywords internal
# Tolerance matches numpy's `np.allclose(w, w.flat[0])` default (atol=1e-8, rtol=1e-5) exactly --
# a tighter, ad hoc tolerance here previously rejected float-noisy-but-uniform weight vectors
# (e.g. observation counts recomputed from a slightly perturbed source) that the Python oracle
# accepts, a real cross-language behavioral divergence, not just a decimal-precision difference.
# Shared by certified_tied_subset() (below) and report()'s PIVOT section (report.R) -- both call
# prop22_certifies() on a k* that is only valid when it came from UNWEIGHTED breakdown_number().
.weights_are_uniform <- function(w) {
  isTRUE(all(abs(w - w[1]) <= 1e-8 + 1e-5 * abs(w[1])))
}

#' No-bootstrap lower bound on the tied set's size
#'
#' The champion plus every rival whose pairwise k* is certified non-significant by Proposition 2.2.
#' NOT a confidence set and NOT nested in the Model Confidence Set -- see this file's header.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights; must be uniform (or `NULL`).
#' @param z Two-sided critical value (default 1.96, two-sided 5%).
#' @return A sorted character vector of model names in the certified tied subset.
#' @seealso [prop22_certifies()] (the underlying per-k* certificate), [mcs()]/[identified()]
#'   (bootstrap-based tie assessment, no uniform-weight restriction).
#' @export
#' @examples
#' # ar1 and nbeats are a genuine near-tie; ets is clearly worse.
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(1.01, 0.97, 1.03, 1.00, 0.98, 1.04, 0.96, 1.01, 1.01, 0.99),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' certified_tied_subset(panel)  # c("ar1", "nbeats") -- ets is not certified tied
certified_tied_subset <- function(L, w = NULL, z = 1.96) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  if (!.weights_are_uniform(w)) {
    stop(paste0(
      "certified_tied_subset() only supports uniform weights (or w=NULL) -- Proposition 2.2's ",
      "bound is proved for the unweighted i.i.d. population-sd statistic, and applying it to a ",
      "WEIGHTED breakdown point is unsound. No MCS engine in this package (mcs()/",
      "model_confidence_set(), or the independent mcs_size()/identified()) supports genuinely ",
      "non-uniform weighting either. For a weighted panel, use winner_stability() or fragility(), ",
      "which do accept arbitrary weights."
    ), call. = FALSE)
  }
  champ <- pooled_winner(L, w)
  la <- as.numeric(L[[champ]])
  subset <- champ
  for (m in sort(setdiff(names(L), champ))) {
    res <- breakdown_number(la, as.numeric(L[[m]]), w)
    k <- res$k
    cert <- prop22_certifies(k, T, z = z)
    if (k == 0 || isTRUE(cert)) subset <- c(subset, m)
  }
  sort(unique(subset))
}
