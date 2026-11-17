# mcs.R — the Model Confidence Set (Hansen, Lunde & Nason 2011), the identification companion to k*.
#
# Faithful port of mcs.py. The elimination/bootstrap arithmetic (studentized e_max rule, degenerate-se
# floors, Monte-Carlo p-value) is ported exactly; the bootstrap draws themselves use R's own RNG
# (via set.seed()), so results are NOT bit-for-bit identical to the Python package on the "same"
# seed -- only statistically equivalent. Deterministic edge cases (an exact tie, a model that
# dominates every period) are verified to agree exactly with the Python oracle since those paths
# do not depend on the bootstrap draw at all.
#
# `elimination = NULL` (the default) reads `getOption("selectionfragility.elimination", "studentized")`
# at call time -- the R analogue of Python's module-level `ELIMINATION` flag read at call time via
# `elimination=None`. Override globally with `options(selectionfragility.elimination = "raw")`.

.MIN_T_FOR_ZERO_SE <- 8
.SE_REL_FLOOR <- 1e-12
.MAX_BOOTSTRAP_B <- 100000

#' @keywords internal
.std0 <- function(x) sqrt(mean((x - mean(x))^2))  # population std (ddof=0), matching numpy's default

#' Model Confidence Set on a T x K loss matrix
#'
#' @param L A numeric matrix (T periods x K models; lower loss = better).
#' @param alpha Significance level (strictly between 0 and 1).
#' @param B Number of bootstrap resamples.
#' @param block Block length for the circular block bootstrap. The default (3) is tuned for
#'   roughly-i.i.d. losses; it does NOT adapt to serial dependence, and the EPA/MCS bootstrap test
#'   is over-sized (retains too few models) under persistent losses with a short block (e.g. at
#'   AR(1)=0.5 the complete-null retain-all rate falls to ~0.62 vs the nominal 0.90). If your loss
#'   differentials are autocorrelated, raise `block` and report the result across a block sweep
#'   rather than trusting the default. (This over-elimination is conservative for a tie claim --
#'   it shrinks the MCS -- but it inflates the apparent identification rate.)
#' @param seed RNG seed.
#' @param elimination `"studentized"` (Hansen-Lunde-Nason's e_max, default) or `"raw"` (legacy
#'   mean-based rule). `NULL` reads `getOption("selectionfragility.elimination", "studentized")`.
#' @return A list `list(surviving = surviving_column_indices, p = p_at_stop)` (1-based indices).
#'
#' @examples
#' # A 10-period, 3-model loss panel where models 1 and 2 are tied (identical losses) and
#' # model 3 is clearly worse -- the MCS should retain the tied pair. No set.seed() call is
#' # needed here: mcs() saves/restores the caller's RNG state internally, so reproducibility
#' # comes from the seed= argument below, not from the global RNG.
#' L <- cbind(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   ets    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' # B kept small here purely for example speed; use the B=2000 default for real analyses.
#' res <- mcs(L, B = 200, seed = 1)
#' res$surviving  # columns 1 and 2 (ar1, ets) survive; nbeats is eliminated
#'
#' @section Adversarial use / competition settings:
#' This diagnostic was built for a trusted analyst diagnosing their own model comparison, not for
#' a setting where the models being evaluated were chosen knowing the methodology and have an
#' incentive to beat it. Before using an MCS "co-champion" / "statistically tied" claim in a
#' competitive setting (a forecasting leaderboard, a benchmark with prizes), two gaming vectors are
#' real and confirmed, not hypothetical:
#' \itemize{
#'   \item \strong{Noise can hide genuine inferiority inside a tie claim.} A model with a real,
#'     substantial mean-loss disadvantage can survive MCS elimination at a high rate if its
#'     per-period losses carry high idiosyncratic variance, since noise suppresses the studentized
#'     elimination statistic below threshold. This is correct arithmetic, not a bug, but it is a
#'     deliberately-engineerable exploit: do not use MCS survival alone as a "co-champion"
#'     criterion in an adversarial setting -- pair it with an independent accuracy floor.
#'   \item \strong{Whoever controls the weight vector controls the result.} If `w` is
#'     participant-supplied rather than fixed by the competition operator in advance, it can flip
#'     both the pooled winner and k* entirely. Weights must always be operator-controlled and
#'     fixed before results are known in any setting with an adversarial party.
#'   \item \strong{Seed-shopping on a borderline panel is at least as exploitable as the two
#'     vectors above}: quantified on a genuinely
#'     borderline K=2, T=20 panel sitting near the alpha=0.10 boundary, sweeping seed=1..300 with
#'     every other argument at its default: the native engine (this function) called the comparison
#'     "identified" (singleton MCS) on 37.3% of seeds at B=2000 (42.0% at B=500) and "not identified"
#'     on the rest; the independent cross-check engine ([mcs_size()]/[identified()], which delegates
#'     to `MCS::MCSprocedure()`) called it "identified" on only 13.0% of seeds. A party who tries a
#'     handful of seeds and reports only the favorable one can flip the headline verdict roughly 1
#'     time in 3 (this engine) to 1 time in 8 (the cross-check engine) with NO data manipulation at
#'     all. [winner_stability()] is NOT exploitable this way (it stayed within 0.933-0.960 across the
#'     same seed sweep) -- only the MCS/identification layer is. [report()] has a `seed=` argument
#'     (default 0, matching its prior hardcoded behavior) precisely so a caller CAN check this --
#'     call `report(panel, seed=...)`/`mcs()`/`mcs_size()`/`identified()` across a few different
#'     seeds yourself near a borderline alpha if this matters for your use case, rather than
#'     trusting one fixed-seed call.
#' }
#' Strategic period-withholding (submitting only "easy" periods to inflate an apparent record) is
#' only PARTIALLY closed at the input layer: `loss_panel()` requires every model to have the SAME
#' period count, which closes the ASYMMETRIC case (one model's record quietly shorter than the
#' others'). It cannot and does not close the SYMMETRIC case -- an analyst handing every model the
#' same curated/cherry-picked subset of periods, which looks identical to honest data from inside
#' any loss-matrix-consuming tool. That is an inherent limitation of any diagnostic at this level,
#' not a gap specific to this package.
#' @seealso [model_confidence_set()] (list-return alias), [identified()]/[mcs_size()] (the
#'   independent `MCS::MCSprocedure()`-backed cross-check engine), [report()] (one-screen summary
#'   that calls this internally).
#' @export
mcs <- function(L, alpha = 0.10, B = 2000, block = 3, seed = 0, elimination = NULL) {
  if (is.null(elimination)) elimination <- getOption("selectionfragility.elimination", "studentized")
  if (!elimination %in% c("studentized", "raw")) {
    stop(sprintf("elimination must be 'studentized' or 'raw'; got '%s'.", elimination), call. = FALSE)
  }
  # Coerce the two integer-count tuning args up front. Without this, a fractional/character B or
  # block would (a) truncate silently in seq_len()/the resampler, and (b) crash the very %d-format
  # error/warning strings meant to describe them ("invalid format '%d'; use %f..."). Validate B as
  # a finite positive scalar with a worded message; floor block (a fractional block otherwise makes
  # .block_resample_idx pad its index vector with NA).
  B_in <- B
  B <- suppressWarnings(as.integer(B))
  if (length(B) != 1 || is.na(B)) {
    stop(sprintf("mcs(): B must be a single positive integer; got %s.", paste(format(B_in), collapse = ", ")), call. = FALSE)
  }
  block <- suppressWarnings(as.integer(block))
  if (length(block) != 1 || is.na(block) || block < 0) {
    stop(sprintf("mcs(): block must be a single non-negative integer; got %s.", paste(format(block), collapse = ", ")), call. = FALSE)
  }
  L <- as.matrix(L)
  storage.mode(L) <- "double"
  if (length(alpha) != 1 || !is.numeric(alpha) || !is.finite(alpha)) {
    stop(sprintf("mcs(): alpha must be a single finite number strictly between 0 and 1; got a %s of length %d.",
                  class(alpha)[1], length(alpha)), call. = FALSE)
  }
  if (!(alpha > 0.0 && alpha < 1.0)) {
    stop(sprintf(
      paste0(
        "mcs(): alpha must be strictly between 0 and 1 (it is a significance level, e.g. 0.05 or ",
        "0.10, not a percentage); got %s."
      ), alpha
    ), call. = FALSE)
  }
  if (length(dim(L)) != 2 || ncol(L) < 2) {
    stop(sprintf("mcs() needs a 2-D T x K loss matrix with K>=2 models; got dim %s.",
                  paste(dim(L), collapse = "x")), call. = FALSE)
  }
  if (!all(is.finite(L))) {
    stop(paste0(
      "mcs(): loss matrix has non-finite values (NA/NaN/Inf); drop or impute those periods first -- ",
      "a NA silently poisons a model's mean and can crown a spurious point-identified winner."
    ), call. = FALSE)
  }
  if (any(abs(L) > .MAX_ABS_LOSS)) {
    bad <- max(abs(L))
    stop(sprintf(
      paste0(
        "mcs(): loss magnitude %.3g exceeds %.0e; this is almost certainly a units/scale mistake, ",
        "not a real per-period loss. Rescale first."
      ), bad, .MAX_ABS_LOSS
    ), call. = FALSE)
  }
  if (B > .MAX_BOOTSTRAP_B) {
    stop(sprintf(
      paste0(
        "mcs(): B=%d exceeds the %d bootstrap-resample ceiling. Cost scales with T*B; if you have a ",
        "genuine reason to exceed it, call the bootstrap loop directly rather than through this guard."
      ), B, .MAX_BOOTSTRAP_B
    ), call. = FALSE)
  }
  if (B < 1) stop(sprintf("mcs(): B must be a positive integer; got %d.", B), call. = FALSE)

  T <- nrow(L); K <- ncol(L)
  if (K > .LARGE_K_WARN_THRESHOLD) {
    # The native engine is reached directly here (matrix input) without routing through
    # loss_panel()'s large-K warning, so warn at this entry too -- this is the steepest path
    # (super-cubic in K). Not an error; large sweeps are supported.
    warning(sprintf(
      paste0(
        "mcs(): K=%d models -- the native MCS engine's pairwise elimination is super-cubic in K ",
        "(O(K^3 * B)); expect this to be slow for large K. Not an error -- narrow to a candidate ",
        "shortlist if you don't need the full set."
      ), K
    ), call. = FALSE)
  }
  if (as.double(T) * B > 20000000) {  # as.double guards int32 overflow (NA -> if() crash) at large T
    warning(sprintf(
      paste0(
        "mcs(): T=%d periods x B=%d bootstrap resamples = %s -- boot-index construction cost scales ",
        "with T*B. This is not an error -- large T is supported -- but expect it to be slow and ",
        "memory-heavy; consider a smaller B if you don't need fine Monte-Carlo p-value resolution."
      ), T, B, format(as.double(T) * B, big.mark = ",")  # as.double here too -- plain T*B still overflows to NA
    ), call. = FALSE)
  }
  if (T <= block) {
    block_eff <- max(1, T %/% 2)
    warning(sprintf("mcs(): T=%d <= block=%d; shrinking block to %d so the bootstrap retains power.", T, block, block_eff), call. = FALSE)
    block <- block_eff
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  boot <- lapply(seq_len(B), function(i) .block_resample_idx(T, block, circular = TRUE))

  surv <- seq_len(K)
  p_stop <- 1.0
  while (length(surv) > 1) {
    sub <- L[, surv, drop = FALSE]
    k <- length(surv)
    dbar <- colMeans(sub)
    bmeans <- do.call(rbind, lapply(boot, function(idx) colMeans(sub[idx, , drop = FALSE])))  # B x k

    TR <- 0.0
    TRb <- rep(0.0, B)
    if (k >= 2) {
      for (a in 1:(k - 1)) for (b in (a + 1):k) {
        diff_ <- bmeans[, a] - bmeans[, b]
        se <- .std0(diff_)
        obs_diff <- dbar[a] - dbar[b]
        scale <- max(abs(dbar[a]), abs(dbar[b]), abs(obs_diff))
        if (se <= .SE_REL_FLOOR * scale) {
          if (abs(obs_diff) > 0 && T >= .MIN_T_FOR_ZERO_SE) TR <- Inf
          next
        }
        t_obs <- abs(obs_diff) / se
        TR <- max(TR, t_obs)
        TRb <- pmax(TRb, abs(diff_ - obs_diff) / se)
      }
    }
    p <- if (is.finite(TR)) (sum(TRb >= TR) + 1) / (B + 1) else 0.0
    if (p >= alpha) { p_stop <- p; break }

    if (elimination == "studentized") {
      d_dot <- dbar - mean(dbar)
      b_dot <- bmeans - matrix(rowMeans(bmeans), nrow = B, ncol = k)
      se_dot <- apply(b_dot, 2, .std0)
      scale <- max(max(abs(dbar)), 1e-300)
      safe <- se_dot > .SE_REL_FLOOR * scale
      t_dot <- ifelse(safe, d_dot / ifelse(safe, se_dot, 1.0), ifelse(d_dot > 0, Inf, -Inf))
      drop_pos <- which.max(t_dot)
    } else {
      drop_pos <- which.max(dbar)
    }
    surv <- surv[-drop_pos]
    p_stop <- p
  }
  list(surviving = surv, p = p_stop)
}

#' Model Confidence Set on a named list of per-period losses
#'
#' Convenience wrapper over `mcs()` that takes a named list `{model: vector}` (or a `loss_panel`)
#' and returns the surviving model NAMES rather than column indices.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @inheritParams mcs
#' @return A list `list(surviving = surviving_model_names, p = p_at_stop)`.
#'
#' @examples
#' # Same tied panel as mcs(), but as a named list -- the surviving set comes back as names.
#' # No set.seed() call is needed: reproducibility comes from the seed= argument below.
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   ets    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' # B kept small here purely for example speed; use the B=2000 default for real analyses.
#' res <- model_confidence_set(L, B = 200, seed = 1)
#' res$surviving  # c("ar1", "ets"); nbeats is eliminated
#' @seealso [mcs()] (column-index return, same engine), [identified()]/[mcs_size()].
#' @export
model_confidence_set <- function(L, alpha = 0.10, B = 2000, block = 3, seed = 0, elimination = NULL) {
  up <- .unwrap_panel(L, NULL); L <- up$L
  models <- names(L)
  if (length(models) < 2) {
    stop(sprintf("model_confidence_set needs >=2 models; got %d.", length(models)), call. = FALSE)
  }
  if (anyDuplicated(models)) {
    stop(sprintf(
      "model names must be unique -- got duplicate name(s): %s.",
      paste(unique(models[duplicated(models)]), collapse = ", ")
    ), call. = FALSE)
  }
  lengths <- unique(sapply(models, function(m) length(as.numeric(L[[m]]))))
  if (length(lengths) != 1) {
    stop(sprintf("all models must share the same number of periods; got lengths %s.",
                  paste(sort(lengths), collapse = ", ")), call. = FALSE)
  }
  M <- do.call(cbind, lapply(models, function(m) as.numeric(L[[m]])))
  res <- mcs(M, alpha = alpha, B = B, block = block, seed = seed, elimination = elimination)
  list(surviving = models[res$surviving], p = res$p)
}
