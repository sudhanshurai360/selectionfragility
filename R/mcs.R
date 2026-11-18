# mcs.R — the Model Confidence Set (Hansen, Lunde & Nason 2011), the identification companion to k*.
#
# Port of mcs.py. The elimination arithmetic is ported exactly; bootstrap draws use R's RNG, so
# results match Python statistically, not bit-for-bit. Deterministic cases (exact ties, a model that
# wins every period) agree exactly.
#
# `elimination = NULL` reads getOption("selectionfragility.elimination", "studentized") at call
# time, the analogue of Python's module-level ELIMINATION flag.

.MIN_T_FOR_ZERO_SE <- 8
.SE_REL_FLOOR <- 1e-12
.MAX_BOOTSTRAP_B <- 100000
.SMALL_T_RATIO_WARN <- 10

#' @keywords internal
.std0 <- function(x) sqrt(mean((x - mean(x))^2))  # population std (ddof=0), matching numpy's default

#' Model Confidence Set on a T x K loss matrix
#'
#' @param L A numeric matrix (T periods x K models; lower loss = better).
#' @param alpha Significance level (strictly between 0 and 1).
#' @param B Number of bootstrap resamples.
#' @param block Block length for the circular block bootstrap. The default (3) suits roughly
#'   i.i.d. losses and does not adapt to serial dependence: at AR(1)=0.5 the complete-null
#'   retain-all rate falls to ~0.62 (nominal 0.90). With autocorrelated loss differentials, raise
#'   `block` and report results across a block sweep.
#'
#'   Even with no autocorrelation the test is over-sized at ordinary panel sizes: at T=30, K=3 the
#'   false-elimination rate was ~18% at alpha=0.10 and ~9% at alpha=0.05, and ~41% at T=12, K=4
#'   (e.g. 12 years of annual actuarial reserving data comparing 4 methods). It converges to
#'   nominal only as T reaches the low hundreds; the function warns when T/K < 10.
#' @param seed RNG seed.
#' @param elimination `"studentized"` (Hansen-Lunde-Nason's e_max, default) or `"raw"` (legacy
#'   mean-based rule). `NULL` reads `getOption("selectionfragility.elimination", "studentized")`.
#'   The two rules agree on ~95-97% of panels at K=3-9, ~77% at K=10-15 and only ~50% at K=16-30,
#'   so at large K check both and always report `elimination` alongside `surviving`.
#' @return A list `list(surviving = surviving_column_indices, p = p_at_stop, elimination =
#'   elimination_rule_used)` (1-based indices). `elimination` records which rule produced the
#'   result, since a global option can silently change it between sessions.
#'
#' @examples
#' # A 30-period, 3-model panel (T/K=10, so no small-sample warning): models 1 and 2 are tied,
#' # model 3 is clearly worse, so the MCS should keep the tied pair.
#' ar1_ets <- rep(c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98), 3)
#' nbeats  <- rep(c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18), 3)
#' L <- cbind(ar1 = ar1_ets, ets = ar1_ets, nbeats = nbeats)
#' # B kept small for example speed; use the default 2000 in real analyses.
#' res <- mcs(L, B = 200, seed = 1)
#' res$surviving  # columns 1 and 2 (ar1, ets) survive; nbeats is eliminated
#'
#' @section Adversarial use / competition settings:
#' This diagnostic assumes an analyst checking their own comparison. In a competitive setting (a
#' leaderboard, a benchmark with prizes) it can be gamed:
#' \itemize{
#'   \item \strong{Noise can hide inferiority inside a tie claim.} A clearly worse model with high
#'     per-period variance often survives elimination. Pair MCS survival with an independent
#'     accuracy floor before calling anything a co-champion.
#'   \item \strong{Whoever controls the weights controls the result.} Participant-supplied `w` can
#'     flip both the pooled winner and k*; weights must be fixed by the operator in advance.
#'   \item \strong{Seed-shopping on a borderline panel.} On a borderline K=2, T=20 panel,
#'     sweeping seed=1..300 gave "identified" on 37% of seeds with this engine and 13% with
#'     [mcs_size()]/[identified()]; [winner_stability()] stayed within 0.93-0.96. Near a
#'     borderline alpha, check several seeds rather than trusting one.
#'   \item \strong{Exact MCS size is less reliable than the yes/no verdict.} With a genuine single
#'     winner the two engines' surviving counts match on ~97-100% of panels. Under a complete null,
#'     agreement on the exact size falls from ~90% at T/K=5 to ~10% at T/K=0.33, while both still
#'     correctly say "not identified". Do not read the size as a precise set width when T is small
#'     relative to K.
#' }
#' `loss_panel()` requires every model to share the same periods, which blocks one model's record
#' being quietly shorter. It cannot detect every model being given the same cherry-picked subset of
#' periods; no loss-matrix tool can.
#' @seealso [model_confidence_set()] (list-return alias), [identified()]/[mcs_size()] (the
#'   independent `MCS::MCSprocedure()`-backed cross-check engine), [report()] (one-screen summary
#'   that calls this internally).
#' @export
mcs <- function(L, alpha = 0.10, B = 2000, block = 3, seed = 0, elimination = NULL) {
  if (is.null(elimination)) elimination <- getOption("selectionfragility.elimination", "studentized")
  if (length(elimination) != 1 || !is.character(elimination) || is.na(elimination)) {
    stop(sprintf("elimination must be a single string, 'studentized' or 'raw'; got %s.",
                  paste(format(elimination), collapse = ", ")), call. = FALSE)
  }
  if (!elimination %in% c("studentized", "raw")) {
    stop(sprintf("elimination must be 'studentized' or 'raw'; got '%s'.", elimination), call. = FALSE)
  }
  # Coerce counts up front so fractional/character input gets a worded error, not a crash.
  B_in <- B
  B <- suppressWarnings(as.integer(B))
  if (length(B) != 1 || is.na(B)) {
    stop(sprintf("mcs(): B must be a single positive integer; got %s.", paste(format(B_in), collapse = ", ")), call. = FALSE)
  }
  block <- suppressWarnings(as.integer(block))
  if (length(block) != 1 || is.na(block) || block < 0) {
    stop(sprintf("mcs(): block must be a single non-negative integer; got %s.", paste(format(block), collapse = ", ")), call. = FALSE)
  }
  seed_in <- seed
  seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1 || is.na(seed) || seed < 0) {
    stop(sprintf("mcs(): seed must be a single non-negative integer; got %s.", paste(format(seed_in), collapse = ", ")), call. = FALSE)
  }
  if (is.data.frame(L)) {
    bad <- names(L)[!vapply(L, is.numeric, logical(1))]
    if (length(bad) > 0) {
      stop(sprintf("mcs(): data.frame column(s) %s are not numeric; mcs() needs a numeric matrix.",
                    paste(bad, collapse = ", ")), call. = FALSE)
    }
  }
  # A named list or loss_panel belongs in model_confidence_set(); say so instead of a coercion error.
  if (is.list(L) && !is.data.frame(L)) {
    stop(paste0(
      "mcs(): L is a list, not a numeric T x K matrix. mcs() takes a plain matrix of losses; a ",
      "named list `{model: vector}` (or a loss_panel) goes through model_confidence_set() ",
      "instead, which returns surviving model NAMES rather than column indices."
    ), call. = FALSE)
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
    # Matrix input skips loss_panel()'s large-K warning, so warn here too.
    warning(sprintf(
      paste0(
        "mcs(): K=%d models -- the native MCS engine's pairwise elimination is super-cubic in K ",
        "(O(K^3 * B)); expect this to be slow for large K. Not an error -- narrow to a candidate ",
        "shortlist if you don't need the full set."
      ), K
    ), call. = FALSE)
  }
  if (T / K < .SMALL_T_RATIO_WARN) {
    # Static warning, not data-dependent. T/K < 10 is anchored on the two measured points:
    # ~18% false elimination at T=30/K=3 and ~41% at T=12/K=4 (nominal 10%). T=12/K=4 is a normal
    # panel in practice, e.g. 12 years of annual actuarial reserving with 4 methods.
    warning(sprintf(
      paste0(
        "mcs(): T=%d periods for K=%d models (T/K=%.1f) is a small-sample regime where this ",
        "procedure's false-elimination rate runs well above the nominal alpha=%.2g -- under a TRUE ",
        "null with no real difference among models, at alpha=0.10 it was measured at ~18%% (not 10%%) ",
        "at T=30/K=3 and ~41%% at T=12/K=4. This is a blanket caveat tied to T alone, not a diagnosis of ",
        "THIS panel -- a genuinely well-separated panel at this T can still have a reliable VERDICT; ",
        "but if the models are close, treat 'not identified' as noisier than alpha=%.2g implies, and ",
        "an 'identified' singleton with the same suspicion. Not an error -- small-T panels are ",
        "supported -- but interpret VERDICT accordingly."
      ), T, K, T / K, alpha, alpha
    ), call. = FALSE)
  }
  if (as.double(T) * B > 20000000) {  # as.double: T * B can overflow integer
    warning(sprintf(
      paste0(
        "mcs(): T=%d periods x B=%d bootstrap resamples = %s -- boot-index construction cost scales ",
        "with T*B. This is not an error -- large T is supported -- but expect it to be slow and ",
        "memory-heavy; consider a smaller B if you don't need fine Monte-Carlo p-value resolution."
      ), T, B, format(as.double(T) * B, big.mark = ",")
    ), call. = FALSE)
  }
  if (T <= block) {
    block_eff <- max(1, T %/% 2)
    warning(sprintf("mcs(): T=%d <= block=%d; shrinking block to %d so the bootstrap retains power.", T, block, block_eff), call. = FALSE)
    block <- block_eff
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
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
  list(surviving = surv, p = p_stop, elimination = elimination)
}

#' Model Confidence Set on a named list of per-period losses
#'
#' Convenience wrapper over `mcs()` that takes a named list `{model: vector}` (or a `loss_panel`)
#' and returns the surviving model NAMES rather than column indices.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @inheritParams mcs
#' @return A list `list(surviving = surviving_model_names, p = p_at_stop, elimination =
#'   elimination_rule_used)`.
#'
#' @examples
#' # Same tied panel as mcs(), as a named list: the surviving set comes back as names.
#' ar1_ets <- rep(c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98), 3)
#' nbeats  <- rep(c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18), 3)
#' L <- list(ar1 = ar1_ets, ets = ar1_ets, nbeats = nbeats)
#' # B kept small for example speed; use the default 2000 in real analyses.
#' res <- model_confidence_set(L, B = 200, seed = 1)
#' res$surviving  # c("ar1", "ets"); nbeats is eliminated
#' @seealso [mcs()] (column-index return, same engine), [identified()]/[mcs_size()].
#' @export
model_confidence_set <- function(L, alpha = 0.10, B = 2000, block = 3, seed = 0, elimination = NULL) {
  up <- .unwrap_panel(L, NULL); L <- up$L
  # The engine is unweighted; refuse a weighted panel rather than silently ignore its weights.
  # (Python's model_confidence_set() still drops them.)
  if (!is.null(up$w) && !.weights_are_uniform(as.numeric(up$w))) {
    stop(paste0(
      "model_confidence_set(): this loss_panel has non-uniform weights, but the Model Confidence ",
      "Set engine is unweighted -- computing it here would silently ignore the weights the panel's ",
      "pooled champion is computed with. Pass panel$losses instead if you deliberately want an ",
      "unweighted MCS."
    ), call. = FALSE)
  }
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
  list(surviving = models[res$surviving], p = res$p, elimination = res$elimination)
}
