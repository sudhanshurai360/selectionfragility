# fragility.R — the decision breakdown point (k*) and ranking-fragility diagnostics.
#
# Faithful port of fragility.py. Indexing convention: this port uses 1-based period indices
# throughout (R idiom), where the Python original uses 0-based -- `removed`/`responsible`
# period positions are shifted by +1 accordingly. All numeric results (margins, k*, ratios,
# winner names) match the Python oracle exactly on deterministic inputs; `winner_stability()`
# and `exchangeable_benchmark()` depend on a block-bootstrap RNG and are NOT bit-for-bit
# reproducible against the Python package (different RNG streams across languages) -- they are
# verified for statistical equivalence (same shape of distribution), not exact equality.

.MARGIN_REL_FLOOR <- 1e-12

#' @keywords internal
.unwrap_panel <- function(L, w) {
  if (inherits(L, "loss_panel")) {
    if (is.null(w)) w <- L$weights
    L <- L$losses
  }
  list(L = L, w = w)
}

#' @keywords internal
.as_loss_dict <- function(L) {
  if (is.data.frame(L)) {
    return(stats::setNames(lapply(colnames(L), function(c) as.numeric(L[[c]])), colnames(L)))
  }
  L
}

#' @keywords internal
.validate_losses <- function(L, w = NULL) {
  if (!is.list(L) || is.null(names(L)) || any(names(L) == "")) {
    stop(paste0(
      "L must be a named list {model: vector} (or a loss_panel's own $losses); got ",
      class(L)[1], ". Build a loss_panel first with loss_panel(...) if you have a data.frame/",
      "matrix, or pass panel$losses/panel$weights directly for this raw-vector entry point."
    ), call. = FALSE)
  }
  if (length(L) < 2) {
    stop(sprintf("need >=2 models to compare a selection decision; got %d.", length(L)), call. = FALSE)
  }
  T <- NULL
  for (m in names(L)) {
    v <- L[[m]]
    if (!is.null(dim(v))) {
      stop(sprintf("each model's loss must be a non-empty 1-D vector; '%s' has dim %s.", m,
                    paste(dim(v), collapse = "x")), call. = FALSE)
    }
    if (!is.numeric(v)) {
      # Coercion-introduced NAs (e.g. a character vector) would silently pass a bare is.finite()
      # check, so refuse non-numeric input outright rather than let a type mistake masquerade as
      # a data quality issue.
      stop(sprintf("model '%s's loss must be a vector of numeric values; got class %s.", m, class(v)[1]), call. = FALSE)
    }
    a <- as.numeric(v)
    if (length(a) == 0) stop(sprintf("each model's loss must be a non-empty 1-D vector; '%s' is empty.", m), call. = FALSE)
    if (is.null(T)) {
      T <- length(a)
    } else if (length(a) != T) {
      stop(sprintf("all models must share the same number of periods; '%s' has %d, expected %d.", m, length(a), T), call. = FALSE)
    }
    if (!all(is.finite(a))) {
      stop(sprintf(
        paste0(
          "model '%s' has non-finite loss values (NA/NaN/Inf). Drop or impute those periods before ",
          "calling -- a single NA silently poisons the pooled mean and the selected winner."
        ), m
      ), call. = FALSE)
    }
  }
  if (!is.null(w)) {
    # Coercion failures (e.g. non-numeric strings) must raise distinctly from a genuine
    # length/finiteness problem -- `as.numeric()` on unconvertible values silently produces NA
    # with only a warning, which `suppressWarnings()` alone would let fall through into the
    # generic finite-check below and report a misleading "got length N" message even though the
    # length was correct and the real problem was that the values weren't numeric at all.
    coercion_failed <- FALSE
    wa <- withCallingHandlers(
      as.numeric(w),
      warning = function(wcond) {
        if (grepl("NAs introduced by coercion", conditionMessage(wcond), fixed = TRUE)) {
          coercion_failed <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )
    if (coercion_failed) {
      stop(sprintf(
        "weights must be a numeric vector of %d values (one per period); got values that could not be converted to numeric.",
        T
      ), call. = FALSE)
    }
    if (length(wa) != T || !all(is.finite(wa))) {
      stop(sprintf("weights must be finite and of length %d; got length %d.", T, length(wa)), call. = FALSE)
    }
    if (any(wa < 0)) {
      stop("weights must not contain negative values -- weights are observation counts, not signed contributions.", call. = FALSE)
    }
    if (all(wa == 0)) {
      stop("weights are all zero; at least one period must carry positive weight.", call. = FALSE)
    }
  }
  T
}

#' Pooled (score) winner — model with the lowest weighted-mean loss
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @return The winning model's name.
#' @export
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' pooled_winner(L)  # "ar1" -- nbeats' one bad period (2.60) drags its mean above ar1's
pooled_winner <- function(L, w = NULL) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  means <- sapply(names(L), function(m) stats::weighted.mean(as.numeric(L[[m]]), w))
  best <- min(means)
  best_m <- names(sort(means))[1]
  scale <- (abs(means[best_m]) + means) / 2.0
  tied <- names(L)[(means - best) <= .MARGIN_REL_FLOOR * scale]
  sort(tied)[1]
}

#' Per-period (plurality) winner — the model winning the most periods
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param return_ties If TRUE, return `list(winner, tied)` instead of just the winner.
#' @return The winning model's name, or a list with the full tied set.
#' @export
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' per_period_winner(L)  # "nbeats" -- it wins 9 of 10 periods despite losing on pooled mean
#'
#' # return_ties = TRUE matters when two models tie for the per-period win count -- here ar1
#' # and ets each win 2 of 4 periods, so the plain winner would arbitrarily pick one:
#' L2 <- list(
#'   ar1 = c(1.0, 1.0, 1.2, 1.2),
#'   ets = c(1.1, 1.1, 1.0, 1.0)
#' )
#' per_period_winner(L2, return_ties = TRUE)  # $tied = c("ar1", "ets"); both won 2 of 4 periods
per_period_winner <- function(L, return_ties = FALSE) {
  up <- .unwrap_panel(L, NULL); L <- up$L
  L <- .as_loss_dict(L)
  .validate_losses(L)
  models <- sort(names(L))
  M <- do.call(rbind, lapply(models, function(m) as.numeric(L[[m]])))  # models x T
  per_period_min <- apply(M, 2, min)
  is_min <- sweep(M, 2, per_period_min, `==`)
  n_min <- colSums(is_min)
  is_unique_min <- is_min & matrix(n_min == 1, nrow = nrow(M), ncol = ncol(M), byrow = TRUE)
  wins <- rowSums(is_unique_min)
  top <- max(wins)
  tied <- models[wins == top]
  if (return_ties) list(winner = tied[1], tied = tied) else tied[1]
}

#' Condorcet winner — beats every other model in pairwise per-period majority
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @return The winning model's name, or `NULL` if none exists.
#' @export
#' @examples
#' # Each model is best in one period and worst in another -- a rock/paper/scissors cycle,
#' # so no model beats both others in pairwise per-period majority.
#' L <- list(
#'   ar1    = c(1, 3, 2),
#'   nbeats = c(2, 1, 3),
#'   ets    = c(3, 2, 1)
#' )
#' condorcet_winner(L)  # NULL -- no model beats every other model pairwise
condorcet_winner <- function(L) {
  up <- .unwrap_panel(L, NULL); L <- up$L
  L <- .as_loss_dict(L)
  .validate_losses(L)
  models <- names(L)
  M <- stats::setNames(lapply(models, function(m) as.numeric(L[[m]])), models)
  for (a in models) {
    ok <- TRUE
    for (b in models) {
      if (a == b) next
      if (!(sum(M[[a]] < M[[b]]) > sum(M[[b]] < M[[a]]))) { ok <- FALSE; break }
    }
    if (ok) return(a)
  }
  NULL
}

#' Classify the pairwise-majority tournament ('winner' | 'cycle' | 'tie')
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @return One of `"winner"` (a Condorcet winner exists), `"cycle"` (no winner, and the
#'   pairwise-majority relation is intransitive -- e.g. rock/paper/scissors), or `"tie"` (no
#'   winner, but no cycle either -- a direct pairwise tie).
#' @export
#' @examples
#' # Same rock/paper/scissors panel as condorcet_winner(): no model dominates, so the
#' # pairwise-majority tournament is a cycle rather than a clean winner or an outright tie.
#' L <- list(
#'   ar1    = c(1, 3, 2),
#'   nbeats = c(2, 1, 3),
#'   ets    = c(3, 2, 1)
#' )
#' condorcet_status(L)  # "cycle"
condorcet_status <- function(L) {
  up <- .unwrap_panel(L, NULL); L <- up$L
  L <- .as_loss_dict(L)
  .validate_losses(L)
  models <- names(L)
  M <- stats::setNames(lapply(models, function(m) as.numeric(L[[m]])), models)
  beats <- function(x, y) sum(M[[x]] < M[[y]]) > sum(M[[y]] < M[[x]])
  for (a in models) {
    if (all(sapply(models, function(b) a == b || beats(a, b)))) return("winner")
  }
  indeg <- stats::setNames(rep(0L, length(models)), models)
  adj <- stats::setNames(vector("list", length(models)), models)
  for (x in models) adj[[x]] <- character(0)
  for (x in models) for (y in models) {
    if (x != y && beats(x, y)) {
      adj[[x]] <- c(adj[[x]], y)
      indeg[y] <- indeg[y] + 1L
    }
  }
  q <- models[indeg == 0L]
  ordered <- 0L
  while (length(q) > 0) {
    u <- q[1]; q <- q[-1]
    ordered <- ordered + 1L
    for (v in adj[[u]]) {
      indeg[v] <- indeg[v] - 1L
      if (indeg[v] == 0L) q <- c(q, v)
    }
  }
  if (ordered < length(models)) "cycle" else "tie"
}

#' @keywords internal
.pair_scale <- function(la, lb, w, T) {
  s <- if (T > 0) (mean(abs(la)) + mean(abs(lb))) / 2.0 else 0.0
  wscale <- if (T > 0) mean(w) else 0.0
  s * wscale
}

#' @keywords internal
.is_degenerate_pair <- function(k, la, lb, w, T) {
  c_ <- w * (lb - la)
  M <- sum(c_)
  isTRUE(k == 0 || (M / T) <= .MARGIN_REL_FLOOR * .pair_scale(la, lb, w, T))
}

#' @keywords internal
.loss_scale <- function(L) {
  L <- .as_loss_dict(L)
  vals <- unlist(lapply(L, function(v) abs(as.numeric(v))))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) 0.0 else mean(vals)
}

#' Pairwise decision breakdown point
#'
#' k* for a pair: the minimum number of periods to delete to flip "a beats b" on the pooled
#' metric, where `la` is the current pooled winner's losses.
#'
#' @param la,lb Numeric vectors, per-period loss for the champion (`la`) and the opponent (`lb`).
#' @param w Numeric vector of per-period weights.
#' @return A list `list(k = k_star, removed = removed_period_indices)` (1-based indices).
#' @export
#' @examples
#' # nbeats is cheaper than ar1 in 4 of 5 periods, but one shock period (5.0) is bad enough
#' # that ar1 wins on pooled mean (1.0 vs 1.4) -- removing just that one period flips it back.
#' ar1    <- c(1.0, 1.0, 1.0, 1.0, 1.0)
#' nbeats <- c(0.5, 0.5, 0.5, 0.5, 5.0)
#' breakdown_number(la = ar1, lb = nbeats, w = rep(1, 5))  # k = 1, removed = 5
breakdown_number <- function(la, lb, w) {
  w <- .validate_weights(as.numeric(w), length(w))
  la <- as.numeric(la); lb <- as.numeric(lb); w <- as.numeric(w)
  if (!(all(is.finite(la)) && all(is.finite(lb)))) {
    stop(paste0(
      "breakdown_number requires finite (non-NA/NaN/Inf) losses for both models; a single NA ",
      "silently poisons the margin sum and can report a confident, meaningless k*."
    ), call. = FALSE)
  }
  c_ <- w * (lb - la)
  M <- sum(c_)
  T <- length(w)
  scale <- if (T > 0) (mean(abs(la)) + mean(abs(lb))) / 2.0 else 0.0
  wscale <- if (T > 0) mean(w) else 0.0
  if (M <= 0 || (T > 0 && (M / T) <= .MARGIN_REL_FLOOR * scale * wscale)) {
    return(list(k = 0L, removed = integer(0)))
  }
  order_idx <- order(-c_)  # R's order() is stable: ties keep earliest original index
  s <- M
  removed <- integer(0)
  for (i in order_idx) {
    if (c_[i] <= 0) break
    s <- s - c_[i]
    removed <- c(removed, i)
    if (s <= 0 || s <= .MARGIN_REL_FLOOR * scale * T * wscale) break
  }
  list(k = length(removed), removed = removed)
}

#' Decision-level k* — fewest periods whose deletion flips the pooled winner to ANY opponent
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param a Optional champion model name; defaults to `pooled_winner(L, w)`.
#' @param return_ties If TRUE, also return every opponent tied at k*.
#' @return A list `list(k, binding_opponent, removed, ties)`. `removed` is always a bare, 1-based
#'   POSITION into the loss vectors -- it is NOT resolved against `L`'s period labels even when
#'   `L` is a `loss_panel` built with `labels=`. Only [report()] resolves `removed` into actual
#'   period labels for display; call `panel$labels[removed]` yourself if you need them here.
#' @export
#' @examples
#' # Same shock-period panel as breakdown_number(): on the full panel, ar1 is the pooled
#' # winner (nbeats' one bad period pulls its mean above ar1's), but it only takes deleting
#' # that single period to flip the decision to nbeats.
#' L <- list(
#'   ar1    = c(1.0, 1.0, 1.0, 1.0, 1.0),
#'   nbeats = c(0.5, 0.5, 0.5, 0.5, 5.0)
#' )
#' decision_breakdown(L)  # k = 1, binding_opponent = "nbeats", removed = 5
decision_breakdown <- function(L, w = NULL, a = NULL, return_ties = FALSE) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  for (m in names(L)) {
    v <- as.numeric(L[[m]])
    if (length(v) > 0 && any(abs(v) > .MAX_ABS_LOSS)) {
      stop(sprintf(
        paste0(
          "loss magnitude for model '%s' exceeds %.0e; double-precision arithmetic overflows here ",
          "and would silently report k*=0. Check for a units/scaling bug upstream."
        ), m, .MAX_ABS_LOSS
      ), call. = FALSE)
    }
  }
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  if (is.null(a)) a <- pooled_winner(L, w)
  la <- as.numeric(L[[a]])
  best <- NULL  # list(k, opponent, removed)
  ties <- character(0)
  for (m in sort(setdiff(names(L), a))) {
    res <- breakdown_number(la, as.numeric(L[[m]]), w)
    k <- res$k
    if (is.null(best) || k < best$k) {
      best <- list(k = k, opponent = m, removed = res$removed)
      ties <- m
    } else if (k == best$k) {
      ties <- c(ties, m)
    }
  }
  if (is.null(best)) {
    return(list(k = 0L, binding_opponent = NULL, removed = integer(0), ties = character(0)))
  }
  list(k = best$k, binding_opponent = best$opponent, removed = best$removed, ties = ties)
}

#' @keywords internal
.block_resample_idx <- function(T, block, circular = TRUE) {
  if (block <= 1) return(sample.int(T, size = T, replace = TRUE))
  if (!circular) {
    out <- integer(0)
    while (length(out) < T) {
      start <- sample.int(max(1, T - block + 1), 1)
      out <- c(out, seq(start, min(start + block - 1, T)))
    }
    return(out[1:T])
  }
  n_starts <- ceiling(T / block)
  starts <- sample.int(T, size = n_starts, replace = TRUE)
  idx <- unlist(lapply(starts, function(s) ((s - 1 + 0:(block - 1)) %% T) + 1))
  idx[1:T]
}

#' Block-bootstrap winner stability
#'
#' Resample evaluation periods (circular block bootstrap) and recompute the pooled winner on
#' each resample. NOTE: uses R's own RNG, so results are not bit-for-bit identical to the
#' Python package's numpy-based bootstrap on the same seed -- only statistically equivalent.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param n_boot Number of bootstrap resamples.
#' @param block Block length; defaults to `round(T^(1/3))`.
#' @param seed RNG seed.
#' @param circular Use the circular (wrap-around) block bootstrap (default) or moving-block.
#' @return A list `list(stability, freq)`.
#' @export
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' ws <- winner_stability(L, seed = 1)
#' ws$stability  # fraction of bootstrap resamples where ar1 is still the pooled winner
#' # This number is NOT interpretable in isolation -- a mid-range value like this can be exactly
#' # what pure chance produces with only 3 models and 10 periods. Compare it to
#' # exchangeable_benchmark(K = 3, T = 10)'s p05: only below that is the win unusually fragile
#' # relative to chance, not just "looks uncertain."
winner_stability <- function(L, w = NULL, n_boot = 2000, block = NULL, seed = 0, circular = TRUE) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  if (is.null(block)) block <- max(1, round(T ^ (1 / 3)))
  if (block >= T) {
    stop(sprintf(
      paste0(
        "block=%d >= T=%d periods -- a circular block bootstrap with a block at least as long as ",
        "the sample only ever produces a rotation of the same %d periods, so the pooled winner ",
        "cannot change across resamples and stability would be trivially 1.0 regardless of the ",
        "data. Pass a smaller block (default is round(T^(1/3)))."
      ), block, T, T
    ), call. = FALSE)
  }
  if (n_boot <= 0) stop(sprintf("n_boot must be a positive integer; got %d.", n_boot), call. = FALSE)

  obs <- pooled_winner(L, w)
  db <- decision_breakdown(L, w, a = obs)
  la <- as.numeric(L[[obs]])
  lb <- if (!is.null(db$binding_opponent)) as.numeric(L[[db$binding_opponent]]) else la
  if (.is_degenerate_pair(db$k, la, lb, w, T)) {
    return(list(stability = NaN, freq = stats::setNames(rep(1 / length(L), length(L)), names(L))))
  }
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  counts <- stats::setNames(rep(0L, length(L)), names(L))
  for (b in seq_len(n_boot)) {
    idx <- .block_resample_idx(T, block, circular = circular)
    Lb <- lapply(L, function(v) as.numeric(v)[idx])
    winner <- pooled_winner(Lb, w[idx])
    counts[winner] <- counts[winner] + 1L
  }
  freq <- counts / n_boot
  list(stability = unname(freq[obs]), freq = freq)
}

#' Null distribution of winner_stability under an exchangeable (no-true-winner) panel
#'
#' @param K Number of models (all drawn from the same distribution, so no model is truly better).
#' @param T Number of periods.
#' @param n_rep Number of simulated panels.
#' @param n_boot Bootstrap resamples passed to `winner_stability()` for each simulated panel.
#' @param block Block length passed to `winner_stability()`.
#' @param seed RNG seed.
#' @param circular Use the circular (wrap-around) block bootstrap (default) or moving-block.
#' @return A list `list(median, p05, p95)`.
#' @export
#' @examples
#' # n_rep/n_boot are tiny here purely to keep the example fast; real usage should use the
#' # defaults (n_rep = 200, n_boot = 500) or larger for a stable null distribution.
#' eb <- exchangeable_benchmark(K = 3, T = 20, n_rep = 20, n_boot = 50, seed = 1)
#' eb$median  # typical winner_stability() under "no model is really better"
exchangeable_benchmark <- function(K, T, n_rep = 200, n_boot = 500, block = NULL, seed = 0, circular = TRUE) {
  w <- rep(1, T)
  out <- numeric(n_rep)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  for (r in seq_len(n_rep)) {
    set.seed(seed + r)
    L <- stats::setNames(lapply(seq_len(K), function(i) stats::rnorm(T, 1.0, 0.30)), paste0("m", seq_len(K) - 1L))
    boot_seed <- seed + 1000003 + r
    out[r] <- winner_stability(L, w, n_boot = n_boot, block = block, seed = boot_seed, circular = circular)$stability
  }
  out <- sort(out)
  list(median = stats::median(out), p05 = unname(stats::quantile(out, 0.05)), p95 = unname(stats::quantile(out, 0.95)))
}

#' Surprise concentration — how lopsided is irreducible task difficulty across periods?
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param labels Optional vector of period labels (used only to report `hardest`).
#' @return A list `list(sci, eff_surprising_periods, hardest, d_max_over_median)`.
#' @export
#' @examples
#' # Irreducible difficulty (the best ANY model can do each period) is flat except period 4,
#' # which is hard for every model at once -- the panel's difficulty is concentrated there,
#' # regardless of which model wins the overall decision.
#' L <- list(
#'   ar1    = c(1.0, 1.0, 1.0, 3.0, 1.0),
#'   nbeats = c(0.9, 0.9, 0.9, 2.8, 0.9),
#'   ets    = c(1.2, 1.2, 1.2, 3.2, 1.2)
#' )
#' surprise_concentration(L)  # sci = 1 -- all of the surprise is in one period (hardest = 3)
surprise_concentration <- function(L, labels = NULL) {
  up <- .unwrap_panel(L, NULL); L <- up$L
  L <- .as_loss_dict(L)
  .validate_losses(L)
  models <- names(L)
  M <- do.call(rbind, lapply(models, function(m) as.numeric(L[[m]])))
  d <- apply(M, 2, min)
  b <- stats::median(d)
  s <- pmax(0.0, d - b)
  tot <- sum(s)
  lab <- if (!is.null(labels)) labels else (seq_along(d) - 1L)
  d_max <- max(d)
  d_max_over_median <- if (b == 0.0 && d_max == 0.0) {
    1.0
  } else if (b == 0.0) {
    Inf
  } else if ((b < 0.0) != (d_max < 0.0)) {
    NaN
  } else {
    d_max / b
  }
  list(
    sci = if (tot > 0) max(s) / tot else 0.0,
    eff_surprising_periods = if (tot > 0) tot^2 / sum(s^2) else length(d),
    hardest = lab[[which.max(d)]],
    d_max_over_median = d_max_over_median
  )
}

#' Full ranking-fragility diagnostic for a selection decision
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param shock_periods Optional vector of known shock-period labels.
#' @param labels Optional vector of period labels.
#' @param n_boot,seed Bootstrap settings for `winner_stability()`.
#' @param stability_block Block length override for the stability bootstrap.
#' @param benchmark Optional `exchangeable_benchmark()` result (or its p05) to calibrate `fragile`.
#' @return A named list of diagnostic fields (see `vignette("selectionfragility")` for field-by-field guidance).
#' @seealso [report()] for the recommended one-screen summary built on top of this function;
#'   [decision_breakdown()] for k* alone; [winner_stability()] and [exchangeable_benchmark()] for
#'   the calibrated bootstrap companion to k*; [mcs_size()]/[identified()] for the independent
#'   Model Confidence Set engine.
#' @export
#' @examples
#' # Same shock-period panel as decision_breakdown(): ar1 is the pooled winner, but nbeats
#' # wins 4 of 5 periods -- fragility() surfaces that reversal alongside k* and stability.
#' L <- list(
#'   ar1    = c(1.0, 1.0, 1.0, 1.0, 1.0),
#'   nbeats = c(0.5, 0.5, 0.5, 0.5, 5.0)
#' )
#' f <- fragility(L)
#' f$pooled_winner      # "ar1"
#' f$per_period_winner   # "nbeats"
#' f$reversal            # TRUE -- the pooled and per-period winners disagree
#' f$k_star               # 1 -- deleting a single period flips the pooled decision
fragility <- function(L, w = NULL, shock_periods = NULL, labels = NULL, n_boot = 2000, seed = 0,
                       stability_block = NULL, benchmark = NULL) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  .validate_losses(L, w)
  if (is.null(w)) w <- rep(1, length(L[[1]]))
  w <- as.numeric(w)
  T <- length(w)
  a <- pooled_winner(L, w)

  rest <- L[setdiff(names(L), a)]
  pooled_runner_up <- if (length(rest) == 1) names(rest)[1] else pooled_winner(rest, w)

  db <- decision_breakdown(L, w, a, return_ties = TRUE)
  k <- db$k; b <- db$binding_opponent; removed <- db$removed; binding_ties <- db$ties
  la <- as.numeric(L[[a]])
  lb <- if (!is.null(b)) as.numeric(L[[b]]) else la
  c_ <- w * (lb - la)
  M <- sum(c_)
  pscale_ab <- .pair_scale(la, lb, w, T)
  concentration <- if (M > 0 && (M / T) > .MARGIN_REL_FLOOR * pscale_ab) max(c_) / M else NaN

  conc_by_opp <- list()
  for (m in binding_ties) {
    lm <- as.numeric(L[[m]])
    cm <- w * (lm - la)
    Mm <- sum(cm)
    pscale_am <- .pair_scale(la, lm, w, T)
    conc_by_opp[[m]] <- if (Mm > 0 && (Mm / T) > .MARGIN_REL_FLOOR * pscale_am) max(cm) / Mm else NaN
  }
  fin <- Filter(is.finite, conc_by_opp)
  concentration_range <- if (length(fin) > 0) c(min(unlist(fin)), max(unlist(fin))) else c(NaN, NaN)

  pp_res <- per_period_winner(L, return_ties = TRUE)
  pp <- pp_res$winner; plurality_ties <- pp_res$tied
  cw <- condorcet_winner(L)
  cstat <- condorcet_status(L)
  lab <- if (!is.null(labels)) labels else (seq_len(T) - 1L)
  resp <- if (length(removed) > 0) lab[removed] else list()
  coincide <- NULL
  if (!is.null(shock_periods)) {
    sset <- shock_periods
    coincide <- (length(resp) > 0) && all(vapply(resp, function(r) r %in% sset, logical(1)))
  }

  degenerate <- isTRUE(k == 0 || (M / T) <= .MARGIN_REL_FLOOR * pscale_ab)
  if (degenerate) {
    stability <- NaN
    freq <- stats::setNames(rep(1 / length(L), length(L)), names(L))
  } else {
    ws <- winner_stability(L, w, n_boot = n_boot, block = stability_block, seed = seed)
    stability <- ws$stability; freq <- ws$freq
  }

  reversal <- (a != pp)
  screen <- isTRUE(degenerate || (stability < 0.60) || (reversal && (k / T < 0.25)))
  if (degenerate) {
    fragile <- TRUE
  } else if (is.null(benchmark)) {
    fragile <- NA
  } else {
    p05 <- if (is.list(benchmark)) benchmark$p05 else benchmark[1]
    fragile <- isTRUE(stability < p05)
  }

  list(
    pooled_winner = a, per_period_winner = pp, condorcet_winner = cw,
    screen = screen,
    binding_opponent = b, runner_up = b, pooled_runner_up = pooled_runner_up,
    binding_opponents = binding_ties,
    plurality_winners = plurality_ties,
    concentration_by_opponent = conc_by_opp,
    concentration_range = concentration_range,
    degenerate_margin = degenerate,
    k_star = as.integer(k), k_over_T = if (T > 0) k / T else NaN,
    concentration = concentration, reversal = reversal,
    reversal_condorcet = (!is.null(cw) && a != cw),
    condorcet_status = cstat,
    condorcet_cycle = (cstat == "cycle"),
    condorcet_undefined = is.null(cw),
    responsible = resp, shock_coincidence = coincide,
    winner_stability = stability, winner_freq = freq, fragile = fragile
  )
}
