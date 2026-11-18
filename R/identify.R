# identify.R — `identified()` / `mcs_size()`, delegating to the CRAN `MCS` package
# (Hansen, Lunde & Nason 2011; Bernardi & Catania 2018, MCSprocedure()).
#
# The R analogue of identify.py's delegation to arch.bootstrap.MCS: an independent MCS engine kept
# separate from the native mcs() (mcs.R) as a cross-check. MCSprocedure() handles exact ties
# natively, so identify.py's jitter workaround for arch is not needed.
#
# Python's engine uses a circular block bootstrap; MCSprocedure() uses a moving-block bootstrap,
# so verdicts on the same panel can differ between the two ports.

#' @keywords internal
.validate_alpha <- function(alpha, name = "alpha") {
  # Shape check first so bad input gets a worded error, not an R internal one. `name` lets the
  # same (0,1) check serve power and mcs_alpha.
  if (length(alpha) != 1 || !is.numeric(alpha) || !is.finite(alpha)) {
    stop(sprintf(
      "%s must be a single finite number strictly between 0 and 1; got a %s of length %d.",
      name, class(alpha)[1], length(alpha)
    ), call. = FALSE)
  }
  if (!(alpha > 0.0 && alpha < 1.0)) {
    what <- if (name == "power") {
      "it is a target probability, e.g. 0.80"
    } else {
      "it is a significance level, e.g. 0.05 or 0.10"
    }
    stop(sprintf(
      "%s must be strictly between 0 and 1 (%s, not a percentage); got %s.",
      name, what, alpha
    ), call. = FALSE)
  }
}

#' @keywords internal
.validate_seed <- function(seed, what) {
  seed_int <- suppressWarnings(as.integer(seed))
  if (length(seed_int) != 1 || is.na(seed_int) || seed_int < 0) {
    stop(sprintf("%s must be a single non-negative integer; got %s.", what,
                  paste(format(seed), collapse = ", ")), call. = FALSE)
  }
  invisible(seed_int)
}

#' @keywords internal
.validate_mcs_weights2 <- function(w) {
  if (is.null(w)) return(invisible(NULL))
  wa <- as.numeric(w)
  # Same tolerance as certified_tied_subset() and Python's np.allclose.
  if (length(wa) > 0 && !.weights_are_uniform(wa)) {
    stop(paste0(
      "MCS::MCSprocedure() has no native support for non-uniform period weights, so the Model ",
      "Confidence Set cannot be silently computed unweighted while the panel's pooled champion is ",
      "computed WITH these weights. Pass uniform weights (or w=NULL) to compute an MCS, or drop ",
      "weighting for this call."
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' @keywords internal
.to_matrix_sorted <- function(L) {
  if (!is.list(L) || is.null(names(L)) || any(names(L) == "")) {
    stop(paste0(
      "L must be a named list {model: vector} (or a loss_panel's own $losses); got ",
      class(L)[1], "."
    ), call. = FALSE)
  }
  if (length(L) < 2) {
    stop(sprintf("need at least 2 models to compute a Model Confidence Set; got %d.", length(L)), call. = FALSE)
  }
  if (anyDuplicated(names(L))) {
    stop(sprintf(
      "model names must be unique -- got duplicate name(s): %s.",
      paste(unique(names(L)[duplicated(names(L))]), collapse = ", ")
    ), call. = FALSE)
  }
  models <- sort(names(L), method = "radix")
  T <- NULL
  cols <- list()
  for (m in models) {
    a <- as.numeric(L[[m]])
    if (length(a) == 0) stop(sprintf("model '%s' loss must be a non-empty 1-D vector.", m), call. = FALSE)
    if (is.null(T)) {
      T <- length(a)
      if (T < 2) stop(sprintf("need at least 2 periods to compute a Model Confidence Set; got %d.", T), call. = FALSE)
    } else if (length(a) != T) {
      stop(sprintf("all models must share the same number of periods; '%s' has %d, expected %d.", m, length(a), T), call. = FALSE)
    }
    if (!all(is.finite(a))) {
      stop(sprintf("model '%s' has NA/NaN/Inf loss values; drop or impute those periods before computing a Model Confidence Set.", m), call. = FALSE)
    }
    if (any(abs(a) > .MAX_ABS_LOSS)) {
      stop(sprintf(
        paste0(
          "model '%s' has a loss magnitude above %.0e, far beyond any real per-period loss ",
          "metric -- arithmetic at this scale silently overflows the bootstrap variance ",
          "calculation. Check for a units/scaling bug upstream."
        ), m, .MAX_ABS_LOSS
      ), call. = FALSE)
    }
    cols[[m]] <- a
  }
  M <- do.call(cbind, cols)
  colnames(M) <- models
  M
}

#' @keywords internal
.run_mcs_arch <- function(L, alpha = 0.10, reps = 500, block_size = 3, seed = 0, w = NULL) {
  .validate_alpha(alpha)
  .validate_mcs_weights2(w)
  reps_in <- reps; reps <- suppressWarnings(as.integer(reps))
  if (length(reps) != 1 || is.na(reps) || reps < 2) {
    stop(sprintf(
      paste0(
        "identified()/mcs_size(): reps must be a single integer >= 2 (MCS::MCSprocedure()'s own ",
        "bootstrap-replication floor); got %s."
      ), paste(format(reps_in), collapse = ", ")
    ), call. = FALSE)
  }
  block_size_in <- block_size; block_size <- suppressWarnings(as.integer(block_size))
  if (length(block_size) != 1 || is.na(block_size) || block_size < 0) {
    stop(sprintf("identified()/mcs_size(): block_size must be a single non-negative integer; got %s.", paste(format(block_size_in), collapse = ", ")), call. = FALSE)
  }
  seed_in <- seed; seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1 || is.na(seed) || seed < 0) {
    stop(sprintf("identified()/mcs_size(): seed must be a single non-negative integer; got %s.", paste(format(seed_in), collapse = ", ")), call. = FALSE)
  }
  M <- .to_matrix_sorted(L)
  models <- colnames(M)
  T_eff <- nrow(M)
  # Same large-K warning as mcs(), for callers who skip loss_panel().
  if (length(models) > .LARGE_K_WARN_THRESHOLD) {
    warning(sprintf(
      paste0(
        "identify: K=%d models -- the MCS::MCSprocedure()-backed identified()/mcs_size() engine's ",
        "cost grows with model count too (though more gently than the native mcs()/",
        "model_confidence_set() engine's measured super-cubic O(K^3 * B)); this is not an error -- ",
        "large model sweeps are supported -- but expect it to be slower and more memory-heavy. If ",
        "you don't need per-model MCS membership, consider narrowing to a candidate shortlist first."
      ), length(models)
    ), call. = FALSE)
  }
  if (T_eff / length(models) < .SMALL_T_RATIO_WARN) {
    # Same small-T warning as mcs(); see .SMALL_T_RATIO_WARN in mcs.R.
    warning(sprintf(
      paste0(
        "identified()/mcs_size(): T=%d periods for K=%d models (T/K=%.1f) is a small-sample ",
        "regime where this procedure's false-elimination rate runs well above the nominal ",
        "alpha=%.2g -- under a TRUE null with no real difference among models, at alpha=0.10 it was ",
        "measured at ~18%% (not 10%%) at T=30/K=3 and ~41%% at T=12/K=4. This is a blanket caveat tied to ",
        "T alone, not a diagnosis of THIS panel -- a genuinely well-separated panel at this T can ",
        "still have a reliable VERDICT; but if the models are close, treat 'not identified' as ",
        "noisier than alpha=%.2g implies, and an 'identified' singleton with the same suspicion. ",
        "Not an error -- small-T panels are supported -- but interpret the result accordingly."
      ), T_eff, length(models), T_eff / length(models), alpha, alpha
    ), call. = FALSE)
  }
  if (block_size > 0 && T_eff <= block_size) {
    block_eff <- max(1, T_eff %/% 2)
    warning(sprintf(
      paste0(
        "identify: T=%d <= block_size=%d; shrinking block to %d so the bootstrap retains power ",
        "(a block >= T only ever produces a rotation of the same T periods, which manufactures ",
        "spurious point-identification)."
      ), T_eff, block_size, block_eff
    ), call. = FALSE)
    block_size <- block_eff
  }
  # MCSprocedure() calls set.seed() internally, so save and restore the caller's .Random.seed.
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  # statistic = "TR" (range statistic) matches arch's default and the native mcs(); MCSprocedure()'s
  # own default "Tmax" is a different test (it changed the surviving set on 6% of simulated
  # K=5/T=25 panels).
  res <- MCS::MCSprocedure(M, alpha = alpha, B = as.integer(reps), statistic = "TR",
                            k = as.integer(block_size), verbose = FALSE, seed = as.integer(seed))
  # Fail loudly if a future MCS release renames Info$included, rather than returning NULL.
  if (!("included" %in% names(res@Info))) {
    stop(paste0(
      "identify(): MCS::MCSprocedure()'s returned Info list no longer has an 'included' field -- ",
      "this likely means an incompatible MCS package update changed its internal result ",
      "structure. Check the installed MCS version against the minimum this package requires."
    ), call. = FALSE)
  }
  res@Info$included
}

#' Size of the Model Confidence Set at level alpha (independent engine)
#'
#' Delegates to `MCS::MCSprocedure()` (Hansen, Lunde & Nason 2011; range statistic, moving-block
#' bootstrap). This is a second, independently implemented engine kept separate from this
#' package's own native [mcs()]/[model_confidence_set()] as a cross-check: the two agree closely
#' when `reps`/`B` are matched, but are not required to agree on unmatched defaults.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional per-period weights; must be uniform (or `NULL`). `MCS::MCSprocedure()` has no
#'   support for period weights, so non-uniform weights raise an error rather than being silently
#'   ignored.
#' @param alpha Significance level (strictly between 0 and 1).
#' @param reps Number of bootstrap replications (>= 2).
#' @param block_size Block length for the moving-block bootstrap (0 = iid resampling).
#' @param seed RNG seed.
#' @return Integer size of the surviving Model Confidence Set.
#' @seealso [mcs()]/[model_confidence_set()], the package's native MCS engine.
#'
#' @examples
#' # A 10-period, 3-model loss panel where ar1 and ets are tied and nbeats is clearly worse.
#' # Small for example speed; expect the small-sample warning.
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   ets    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' # reps kept small for example speed; use the default 500 in real analyses.
#' mcs_size(L, reps = 200, seed = 1)  # 2 -- ar1 and ets both survive
#' @export
mcs_size <- function(L, w = NULL, alpha = 0.10, reps = 500, block_size = 3, seed = 0) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  included <- .run_mcs_arch(L, alpha = alpha, reps = reps, block_size = block_size, seed = seed, w = w)
  length(included)
}

#' Is the Model Confidence Set at level alpha a singleton? (independent engine)
#'
#' A one-line convenience wrapper over [mcs_size()]: `isTRUE(mcs_size(...) == 1L)`. Takes the
#' same arguments and uses the same `MCS::MCSprocedure()`-backed engine; call `mcs_size()`
#' directly if you want the actual set size rather than just the yes/no identification question.
#'
#' @inheritParams mcs_size
#' @return `TRUE` iff the MCS contains exactly one model.
#' @section If this returns FALSE: it says only that the bootstrap did not narrow to one model,
#'   not why. [resolution_report()] (is the edge powered?), [decision_breakdown()]/[fragility()]
#'   (how many periods would flip it?) and [winner_stability()] answer the follow-ups; [report()]
#'   runs them all together.
#' @seealso [mcs_size()], which this wraps; [mcs()]/[model_confidence_set()] for this package's
#'   own independent native MCS engine (a different bootstrap, see their own documentation).
#'
#' @examples
#' # Small panels for example speed; expect the small-sample warning.
#' # Tied pair -> not identified (MCS size 2, not 1).
#' L_tied <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   ets    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' identified(L_tied, reps = 200, seed = 1)  # FALSE
#'
#' # Only one clearly-better model left -> identified (MCS size 1).
#' L_clear <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' identified(L_clear, reps = 200, seed = 1)  # TRUE
#' @export
identified <- function(L, w = NULL, alpha = 0.10, reps = 500, block_size = 3, seed = 0) {
  isTRUE(mcs_size(L, w = w, alpha = alpha, reps = reps, block_size = block_size, seed = seed) == 1L)
}
