# fragility.R — the decision breakdown point (k*) and ranking-fragility diagnostics.
#
# Port of fragility.py. `removed` (breakdown_number()/decision_breakdown()) is a 1-based R
# position; `responsible` (fragility()) holds period labels, 0-based positional by default as in
# Python. Deterministic results match Python exactly; winner_stability() and
# exchangeable_benchmark() use R's RNG and match statistically, not bit-for-bit.

.MARGIN_REL_FLOOR <- 1e-12

#' @keywords internal
.unwrap_panel <- function(L, w) {
  labels <- NULL
  if (inherits(L, "loss_panel")) {
    if (is.null(w)) w <- L$weights
    # Surface only user-supplied labels; auto-generated positional labels are left
    # NULL so label-resolving callers (fragility/surprise_concentration) keep their
    # documented positional fallback instead of printing 0,1,2,... as if named.
    if (isFALSE(L$labels_are_positional)) labels <- L$labels
    L <- L$losses
  }
  list(L = L, w = w, labels = labels)
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
  if (anyDuplicated(names(L))) {
    stop(sprintf(
      paste0(
        "model names must be unique -- got duplicate name(s): %s. R's L[[name]] lookup silently ",
        "returns only the FIRST match for a duplicated name, so a second model sharing a name ",
        "would be dropped from the comparison without any error. Rename before calling."
      ), paste(unique(names(L)[duplicated(names(L))]), collapse = ", ")
    ), call. = FALSE)
  }
  T <- NULL
  for (m in names(L)) {
    v <- L[[m]]
    if (!is.null(dim(v))) {
      stop(sprintf("each model's loss must be a non-empty 1-D vector; '%s' has dim %s.", m,
                    paste(dim(v), collapse = "x")), call. = FALSE)
    }
    if (!is.numeric(v)) {
      # Refuse non-numeric input before coercion can turn it into NAs.
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
    # Same type guards as panel.R's .validate_weights(): factors and logicals coerce silently.
    if (is.character(w)) {
      stop(sprintf(
        "weights must be a numeric vector of %d values (one per period), not a string; got %s.",
        T, paste(w, collapse = ", ")
      ), call. = FALSE)
    }
    if (is.factor(w)) {
      stop(sprintf(
        "weights must be a numeric vector of %d values (one per period), not a factor; got levels %s. ",
        T, paste(levels(w), collapse = ", ")
      ), call. = FALSE)
    }
    if (is.logical(w)) {
      stop(sprintf(
        "weights must be a numeric vector of %d values (one per period), not logical (TRUE/FALSE); got %s.",
        T, paste(w, collapse = ", ")
      ), call. = FALSE)
    }
    # Report a failed numeric coercion as such, not as a misleading length/NA error.
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
    if (length(wa) != T) {
      stop(sprintf("weights must have length %d (one per period); got length %d.", T, length(wa)), call. = FALSE)
    }
    if (!all(is.finite(wa))) {
      stop("weights must not contain NA/NaN/Inf values.", call. = FALSE)
    }
    if (any(wa < 0)) {
      stop("weights must not contain negative values -- weights are observation counts, not signed contributions.", call. = FALSE)
    }
    if (all(wa == 0)) {
      stop("weights are all zero; at least one period must carry positive weight.", call. = FALSE)
    }
    if (any(wa > .MAX_ABS_LOSS)) {
      # Same overflow cap as panel.R's .validate_weights().
      stop(sprintf(
        paste0("weights must not exceed %.0e in magnitude -- a weight this large can make ",
               "sum(weights) overflow to Inf and silently corrupt every weighted statistic downstream."),
        .MAX_ABS_LOSS
      ), call. = FALSE)
    }
  }
  T
}

#' Pooled (score) winner — model with the lowest weighted-mean loss
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @return The winning model's name.
#' @seealso [per_period_winner()] (plurality winner), [condorcet_winner()] (pairwise-majority
#'   winner), [decision_breakdown()] (how fragile this pooled winner is), [report()].
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
  # abs() both terms (as Python), so ties among negative-mean models are still recognized.
  scale <- (abs(means[best_m]) + abs(means)) / 2.0
  tied <- names(L)[(means - best) <= .MARGIN_REL_FLOOR * scale]
  sort(tied, method = "radix")[1]
}

#' Per-period (plurality) winner — the model winning the most periods
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param return_ties If TRUE, return `list(winner, tied)` instead of just the winner.
#' @return The winning model's name, or a list with the full tied set.
#' @seealso [pooled_winner()] (score winner), [condorcet_winner()] (pairwise-majority winner),
#'   [condorcet_status()] (classifies when these disagree).
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
  if (!is.logical(return_ties) || length(return_ties) != 1 || is.na(return_ties)) {
    stop(sprintf("per_period_winner(): return_ties must be a single TRUE/FALSE; got %s.",
                  paste(format(return_ties), collapse = ", ")), call. = FALSE)
  }
  # method = "radix" throughout: codepoint order, locale-independent, matching Python's sorted().
  models <- sort(names(L), method = "radix")
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
#' @seealso [condorcet_status()] (classifies why this returned `NULL`, if it did),
#'   [pooled_winner()], [per_period_winner()].
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
#' @seealso [condorcet_winner()], [pooled_winner()], [per_period_winner()].
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
  # .fsum() (pivot.R): permutation-invariant, so tied margins compare equal.
  M <- .fsum(c_)
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
#' metric, where `la` is the current pooled winner's losses. This is a model-selection
#' application of the robust-statistics breakdown-point concept (Hampel 1971, "A General
#' Qualitative Definition of Robustness," Annals of Mathematical Statistics 42(6)) to a selection
#' decision rather than an estimator.
#'
#' @param la,lb Numeric vectors, per-period loss for the champion (`la`) and the opponent (`lb`).
#' @param w Numeric vector of per-period weights.
#' @return A list `list(k = k_star, removed = removed_period_indices)` (1-based indices).
#' @seealso [decision_breakdown()] (the decision-level k*, searching over all opponents),
#'   [prop22_certifies()] (a zero-simulation significance certificate for this k*).
#' @export
#' @examples
#' # nbeats is cheaper than ar1 in 4 of 5 periods, but one shock period (5.0) is bad enough
#' # that ar1 wins on pooled mean (1.0 vs 1.4) -- removing just that one period flips it back.
#' ar1    <- c(1.0, 1.0, 1.0, 1.0, 1.0)
#' nbeats <- c(0.5, 0.5, 0.5, 0.5, 5.0)
#' breakdown_number(la = ar1, lb = nbeats, w = rep(1, 5))  # k = 1, removed = 5
breakdown_number <- function(la, lb, w) {
  la <- as.numeric(la); lb <- as.numeric(lb); w <- as.numeric(w)
  if (!(length(la) == length(lb) && length(la) == length(w))) {
    stop(sprintf(
      paste0(
        "breakdown_number(): la, lb, and w must all have the same length (one entry per period); ",
        "got length(la)=%d, length(lb)=%d, length(w)=%d. R's vector recycling would otherwise ",
        "silently reuse a mismatched w/la/lb against itself -- most dangerously when one length ",
        "happens to be an exact multiple of another, which recycles with NO warning at all."
      ), length(la), length(lb), length(w)
    ), call. = FALSE)
  }
  w <- .validate_weights(w, length(w))
  if (!(all(is.finite(la)) && all(is.finite(lb)))) {
    stop(paste0(
      "breakdown_number requires finite (non-NA/NaN/Inf) losses for both models; a single NA ",
      "silently poisons the margin sum and can report a confident, meaningless k*."
    ), call. = FALSE)
  }
  c_ <- w * (lb - la)
  M <- .fsum(c_)
  T <- length(w)
  scale <- if (T > 0) (mean(abs(la)) + mean(abs(lb))) / 2.0 else 0.0
  wscale <- if (T > 0) mean(w) else 0.0
  if (M <= 0 || (T > 0 && (M / T) <= .MARGIN_REL_FLOOR * scale * wscale)) {
    return(list(k = 0L, removed = integer(0)))
  }
  order_idx <- order(-c_)  # R's order() is stable: ties keep earliest original index
  s <- M
  # Neumaier compensation on the running remainder, matching .fsum(); naive subtraction can stop
  # one removal early on near-tied contributions and understate k*.
  comp <- 0.0
  # Preallocated buffer: appending inside the loop is O(k*^2).
  removed_buf <- integer(length(order_idx))
  n_removed <- 0L
  for (i in order_idx) {
    if (c_[i] <= 0) break
    ci <- -c_[i]
    t <- s + ci
    if (abs(s) >= abs(ci)) {
      comp <- comp + ((s - t) + ci)
    } else {
      comp <- comp + ((ci - t) + s)
    }
    s <- t
    n_removed <- n_removed + 1L
    removed_buf[n_removed] <- i
    s_corrected <- s + comp
    if (s_corrected <= 0 || s_corrected <= .MARGIN_REL_FLOOR * scale * T * wscale) break
  }
  removed <- removed_buf[seq_len(n_removed)]
  list(k = n_removed, removed = removed)
}

#' Decision-level k* — fewest periods whose deletion flips the pooled winner to ANY opponent
#'
#' See [breakdown_number()] for the pairwise primitive and its robust-statistics lineage
#' (Hampel 1971).
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param a Optional champion model name; defaults to `pooled_winner(L, w)`.
#' @param return_ties If TRUE, also return every opponent tied at k*.
#' @return A list `list(k, binding_opponent, removed, ties)`. `removed` is always a bare, 1-based
#'   POSITION into the loss vectors -- it is NOT resolved against `L`'s period labels even when
#'   `L` is a `loss_panel` built with `labels=`. Only [report()] resolves `removed` into actual
#'   period labels for display; call `panel$labels[removed]` yourself if you need them here.
#' @section Limitation: three different "which opponent" rules: when several opponents tie at the
#'   minimal k*, `binding_opponent` is the alphabetically first (a stable, citable name). Two other
#'   functions answer related questions differently, by design (as in the Python package):
#'   \itemize{
#'     \item [concentration_share()]/[pivot_agreement()] pick, from the same tied set, the opponent
#'       with the largest total margin. An alphabetical rule there made the named pivotal period
#'       change when a model was renamed.
#'     \item `resolution_report()$binding_rival` is the rival with the closest pooled mean (what the
#'       power/MDE calculation needs), not restricted to the k*-tied set.
#'   }
#'   On random panels with a strict winner, ~38% have a k*-tie; `binding_opponent` differs from the
#'   largest-margin pick on ~20% and from `binding_rival` on ~25%. [report()]'s PIVOT paragraph can
#'   therefore name two different opponents. With no k*-tie, all three agree.
#' @seealso [breakdown_number()] (the pairwise primitive this searches over), [prop22_certifies()]
#'   (significance certificate for the resulting k*), [winner_stability()] (a complementary,
#'   bootstrap-based fragility read).
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
  if (!is.logical(return_ties) || length(return_ties) != 1 || is.na(return_ties)) {
    stop(sprintf("decision_breakdown(): return_ties must be a single TRUE/FALSE; got %s.",
                  paste(format(return_ties), collapse = ", ")), call. = FALSE)
  }
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
  if (is.null(a)) {
    a <- pooled_winner(L, w)
  } else if (length(a) != 1 || is.na(a) || !(a %in% names(L))) {
    # Otherwise L[[a]] is NULL and breakdown_number() blames a length mismatch instead.
    stop(sprintf("decision_breakdown(): a='%s' is not one of the models in L (%s).",
                  paste(format(a), collapse = ", "), paste(names(L), collapse = ", ")), call. = FALSE)
  }
  la <- as.numeric(L[[a]])
  best <- NULL  # list(k, opponent, removed)
  ties <- character(0)
  for (m in sort(setdiff(names(L), a), method = "radix")) {
    res <- breakdown_number(la, as.numeric(L[[m]]), w)
    k <- res$k
    if (is.null(best) || k < best$k) {
      best <- list(k = k, opponent = m, removed = res$removed)
      ties <- m
    } else if (k == best$k) {
      ties <- c(ties, m)
    }
  }
  # return_ties toggles the return shape, as Python's 3- vs 4-tuple.
  if (is.null(best)) {
    out <- list(k = 0L, binding_opponent = NULL, removed = integer(0))
    if (return_ties) out$ties <- character(0)
    return(out)
  }
  out <- list(k = best$k, binding_opponent = best$opponent, removed = best$removed)
  if (return_ties) out$ties <- ties
  out
}

#' @keywords internal
.block_resample_idx <- function(T, block, circular = TRUE) {
  # Integer block: a fractional one pads the index vector with NA.
  block <- as.integer(block)
  if (is.na(block) || block <= 1) return(sample.int(T, size = T, replace = TRUE))
  if (!circular) {
    # Draw all starts up front (vectorized); growing the vector in a loop is O(T^2).
    n_starts <- ceiling(T / block) + 1L  # +1 covers the worst case where the last start barely fits
    starts <- sample.int(max(1, T - block + 1), size = n_starts, replace = TRUE)
    idx <- unlist(lapply(starts, function(s) seq(s, min(s + block - 1, T))))
    return(idx[1:T])
  }
  n_starts <- ceiling(T / block)
  starts <- sample.int(T, size = n_starts, replace = TRUE)
  idx <- unlist(lapply(starts, function(s) ((s - 1 + 0:(block - 1)) %% T) + 1))
  idx[1:T]
}

#' Block-bootstrap winner stability
#'
#' Resample evaluation periods (circular block bootstrap) and recompute the pooled winner on
#' each resample. Uses R's RNG, so results match Python statistically, not bit-for-bit.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param n_boot Number of bootstrap resamples.
#' @param block Block length; defaults to `round(T^(1/3))`.
#' @param seed RNG seed.
#' @param circular Use the circular (wrap-around) block bootstrap (default) or moving-block.
#' @return A list `list(stability, freq)`.
#' @seealso [exchangeable_benchmark()] (the null distribution to compare this against --
#'   required, not optional, for interpreting a `stability` value), [decision_breakdown()].
#' @export
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' ws <- winner_stability(L, seed = 1)
#' ws$stability  # fraction of bootstrap resamples where ar1 is still the pooled winner
#' # Not interpretable alone: with 3 models and 10 periods, chance alone produces mid-range values.
#' # Compare against exchangeable_benchmark(K = 3, T = 10)$p05.
winner_stability <- function(L, w = NULL, n_boot = 2000, block = NULL, seed = 0, circular = TRUE) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  if (is.null(block)) {
    block <- max(1, round(T ^ (1 / 3)))
  } else {
    block_in <- block
    block <- suppressWarnings(as.integer(block))
    if (length(block) != 1 || is.na(block)) {
      stop(sprintf("winner_stability(): block must be a single integer; got %s.",
                    paste(format(block_in), collapse = ", ")), call. = FALSE)
    }
  }
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
  # Floor before the check: 0 < n_boot < 1 would otherwise run zero resamples silently.
  n_boot_in <- n_boot
  n_boot <- suppressWarnings(as.integer(n_boot))
  if (length(n_boot) != 1 || is.na(n_boot) || n_boot <= 0) {
    stop(sprintf("n_boot must be a single positive integer; got %s.", paste(format(n_boot_in), collapse = ", ")), call. = FALSE)
  }
  if (!is.logical(circular) || length(circular) != 1 || is.na(circular)) {
    stop(sprintf("winner_stability(): circular must be a single TRUE/FALSE; got %s.",
                  paste(format(circular), collapse = ", ")), call. = FALSE)
  }
  seed_in <- seed; seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1 || is.na(seed) || seed < 0) {
    stop(sprintf("winner_stability(): seed must be a single non-negative integer; got %s.",
                  paste(format(seed_in), collapse = ", ")), call. = FALSE)
  }

  obs <- pooled_winner(L, w)
  db <- decision_breakdown(L, w, a = obs)
  la <- as.numeric(L[[obs]])
  lb <- if (!is.null(db$binding_opponent)) as.numeric(L[[db$binding_opponent]]) else la
  if (.is_degenerate_pair(db$k, la, lb, w, T)) {
    # No bootstrap runs on a degenerate pair; warn that freq is a placeholder, since stability=NaN
    # alone quietly becomes NA in ifelse() or "NaN%" in sprintf().
    warning(sprintf(
      paste0(
        "winner_stability(): the champion-vs-binding-rival pair is degenerate (k*=%d, margin below ",
        "the degeneracy floor) -- stability=NaN and freq is a hardcoded UNIFORM placeholder, NOT a ",
        "computed bootstrap win frequency (no bootstrap was run). Treat freq as uninformative here, ",
        "not as evidence any model is as likely to win as another."
      ), db$k
    ), call. = FALSE)
    return(list(stability = NaN, freq = stats::setNames(rep(1 / length(L), length(L)), names(L))))
  }
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
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
#' @section Assumption: the simulated null panels are drawn i.i.d. (independent across periods).
#'   Under positive autocorrelation -- the normal case for forecast-loss series -- the true null
#'   `winner_stability` distribution shifts UPWARD (e.g. at AR(1)=0.8 the null p05 is ~0.51 vs the
#'   i.i.d. ~0.42), so comparing an observed stability against this i.i.d. benchmark's p05 will
#'   UNDER-detect fragility on a persistent panel (it calls a genuinely-fragile winner "not
#'   fragile"). Treat a "not fragile relative to this benchmark" verdict with caution when your
#'   loss differentials are autocorrelated; check their ACF, or build a null that matches your
#'   panel's own serial-dependence structure.
#'
#' @section Runtime: cost is `O(n_rep * n_boot * T)`. The defaults finish in well under a second
#'   at the T typical of monthly or quarterly comparisons (tens to low hundreds). Intraday data is
#'   much slower: half-hourly call-center staffing data over six months (T ~ 8,700, K=5) took ~78
#'   seconds per call. For T in the thousands, explore with smaller `n_rep`/`n_boot` and keep the
#'   defaults for a final check. Do not extrapolate the 78s linearly: at four years of daily
#'   airline-crew data (T ~ 1,460) timings varied several-fold run to run because of fixed
#'   per-replicate overhead and machine load. Re-time at your own T.
#'
#' @param K Number of models (all drawn from the same distribution, so no model is truly better).
#' @param T Number of periods.
#' @param n_rep Number of simulated panels.
#' @param n_boot Bootstrap resamples passed to `winner_stability()` for each simulated panel.
#' @param block Block length passed to `winner_stability()`.
#' @param seed RNG seed.
#' @param circular Use the circular (wrap-around) block bootstrap (default) or moving-block.
#' @return A list `list(median, p05, p95)`.
#' @seealso [winner_stability()] (the statistic this provides a null distribution for).
#' @export
#' @examples
#' # n_rep/n_boot kept tiny for example speed; use the defaults or larger in real analyses.
#' eb <- exchangeable_benchmark(K = 3, T = 20, n_rep = 20, n_boot = 50, seed = 1)
#' eb$median  # typical winner_stability() under "no model is really better"
exchangeable_benchmark <- function(K, T, n_rep = 200, n_boot = 500, block = NULL, seed = 0, circular = TRUE) {
  K_in <- K
  K <- suppressWarnings(as.integer(K))
  if (length(K) != 1 || is.na(K) || K < 2) {
    stop(sprintf("exchangeable_benchmark(): K must be a single integer >= 2 (need at least 2 models); got %s.",
                  paste(format(K_in), collapse = ", ")), call. = FALSE)
  }
  T_in <- T
  T <- suppressWarnings(as.integer(T))
  if (length(T) != 1 || is.na(T) || T < 1) {
    stop(sprintf("exchangeable_benchmark(): T must be a single positive integer; got %s.",
                  paste(format(T_in), collapse = ", ")), call. = FALSE)
  }
  n_rep_in <- n_rep
  n_rep <- suppressWarnings(as.integer(n_rep))
  if (length(n_rep) != 1 || is.na(n_rep) || n_rep <= 0) {
    stop(sprintf("exchangeable_benchmark(): n_rep must be a single positive integer; got %s.",
                  paste(format(n_rep_in), collapse = ", ")), call. = FALSE)
  }
  seed_in <- seed; seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1 || is.na(seed) || seed < 0) {
    stop(sprintf("exchangeable_benchmark(): seed must be a single non-negative integer; got %s.",
                  paste(format(seed_in), collapse = ", ")), call. = FALSE)
  }
  # Outer draws use seed + r and inner bootstraps seed + 1000003 + r; the two streams first
  # collide at n_rep = 1000004.
  if (n_rep >= 1000004L) {
    stop(sprintf(
      paste0(
        "exchangeable_benchmark(): n_rep=%d is too large -- this function's internal seed ",
        "arithmetic (seed + r for the outer draw, seed + 1000003 + r for the inner bootstrap) ",
        "is only collision-free while n_rep stays at or below 1000003; at or above 1000004, two ",
        "supposedly-independent replicates would silently share an effective seed. Use n_rep <= 1000003."
      ), n_rep
    ), call. = FALSE)
  }
  # seed + r is integer arithmetic; keep it below .Machine$integer.max so set.seed() never gets NA.
  if (seed > .Machine$integer.max - 1000003L - n_rep) {
    stop(sprintf(
      paste0(
        "exchangeable_benchmark(): seed=%d is too large together with n_rep=%d -- this function's ",
        "internal seed arithmetic (seed + 1000003 + r for the inner bootstrap draw, r up to n_rep) ",
        "would overflow R's 32-bit integer range (max %d), silently becoming NA and then crashing ",
        "set.seed() with a raw, unworded error. Use seed <= %d for this n_rep, or a smaller n_rep."
      ), seed, n_rep, .Machine$integer.max, .Machine$integer.max - 1000003L - n_rep
    ), call. = FALSE)
  }
  w <- rep(1, T)
  out <- numeric(n_rep)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
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
#' @return A list `list(sci, eff_surprising_periods, hardest, d_max_over_median)`. `d_max_over_median`
#'   is a ratio of the hardest period's best-model loss to the median across periods, so on
#'   valid-looking input where per-period losses can be negative (e.g. signed loss differentials
#'   or P&L-style figures, which this package does not restrict to be non-negative), that ratio is
#'   not always finite by construction: it is `1.0` when both the numerator and denominator are
#'   exactly `0`, `Inf` when only the denominator is `0`, and `NaN` when the numerator and
#'   denominator have opposite signs (the ratio's sign, and so its magnitude as a "how much worse"
#'   multiplier, is undefined).
#' @seealso [pivot_agreement()] (a related period-sensitivity diagnostic), [decision_breakdown()].
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
  if (is.null(labels)) labels <- up$labels  # honor a panel's user-supplied labels
  L <- .as_loss_dict(L)
  T_sc <- .validate_losses(L)
  if (!is.null(labels) && length(labels) != T_sc) {
    stop(sprintf("labels has length %d, expected %d (one per period).", length(labels), T_sc), call. = FALSE)
  }
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

#' @keywords internal
.resolve_benchmark_p05 <- function(benchmark) {
  # Require a single finite p05: a list without $p05 would otherwise read as "not fragile", and a
  # multi-element vector would silently use its first element.
  if (is.list(benchmark)) {
    if (!("p05" %in% names(benchmark))) {
      stop(sprintf(
        paste0(
          "fragility(): benchmark is a list but has no $p05 element -- pass the full ",
          "exchangeable_benchmark() result (list(median, p05, p95)) or just its $p05 scalar. ",
          "Got a list with names: %s."
        ), paste(names(benchmark), collapse = ", ")
      ), call. = FALSE)
    }
    p05 <- benchmark$p05
  } else {
    p05 <- benchmark
  }
  if (!is.numeric(p05) || length(p05) != 1 || is.na(p05)) {
    stop(sprintf(
      paste0(
        "fragility(): benchmark must resolve to a single finite numeric p05 value; got %s ",
        "(class %s, length %d). Pass exchangeable_benchmark()'s result or its $p05 scalar, not an ",
        "ambiguous multi-element vector or a non-numeric value."
      ), paste(format(p05), collapse = ", "), class(p05)[1], length(p05)
    ), call. = FALSE)
  }
  p05
}

#' Full ranking-fragility diagnostic for a selection decision
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param shock_periods Optional vector of known shock-period labels.
#' @param labels Optional vector of period labels. If `L` is a `loss_panel` built with
#'   `labels=`, those labels are used automatically; pass `labels` here only to override them.
#'   The `responsible` field reports the k* periods by these labels when supplied, otherwise as
#'   0-based positional indices (matching the Python original; note `decision_breakdown()$removed`
#'   is 1-based instead).
#' @param n_boot,seed Bootstrap settings for `winner_stability()`.
#' @param stability_block Block length override for the stability bootstrap.
#' @param benchmark Optional `exchangeable_benchmark()` result (or its p05) to calibrate `fragile`.
#' @return A named list of 25 diagnostic fields:
#'   \describe{
#'     \item{pooled_winner, per_period_winner, condorcet_winner, condorcet_status}{The four
#'       winner-selection rules this package compares; see their own help pages.}
#'     \item{screen}{`TRUE` if a cheap screen (degenerate margin, stability < 0.60, or a
#'       reversal with k/T < 0.25) flags this decision as worth a closer look.}
#'     \item{binding_opponent, runner_up}{The same value (the pooled winner's closest rival by
#'       k*); `runner_up` is an alias kept for readability at call sites.}
#'     \item{pooled_runner_up}{The pooled winner among the remaining models once the champion is
#'       excluded (i.e. "who would win if the champion didn't exist").}
#'     \item{binding_opponents}{All opponents tied with `binding_opponent` at the minimal k*
#'       (length > 1 only on an exact tie).}
#'     \item{plurality_winners}{All models tied for most per-period wins (length > 1 on a tie).}
#'     \item{concentration_by_opponent}{Named list of `concentration` (see below), one per entry
#'       in `binding_opponents`.}
#'     \item{concentration_range}{`c(min, max)` of `concentration_by_opponent`'s finite values
#'       (`c(NaN, NaN)` if none are finite).}
#'     \item{degenerate_margin}{`TRUE` if k*=0 or the margin is below the degeneracy floor --
#'       `winner_stability`/`winner_freq` are not computed in this case; see their own entries.}
#'     \item{k_star, k_over_T}{`decision_breakdown()`'s k* and k*/T.}
#'     \item{concentration}{Share of the champion-vs-binding-opponent margin carried by the single
#'       largest-contribution period (`NaN` if degenerate); see [concentration_share()].}
#'     \item{reversal}{`TRUE` if the pooled winner differs from the per-period winner.}
#'     \item{reversal_condorcet}{`TRUE` if the pooled winner differs from the Condorcet winner
#'       (always `FALSE` if no Condorcet winner exists).}
#'     \item{condorcet_cycle, condorcet_undefined}{Whether `condorcet_status` is `"cycle"`, and
#'       whether no Condorcet winner exists at all (a cycle is one way that can happen).}
#'     \item{responsible}{The k* periods (by label if supplied, else 0-based positional index --
#'       note this differs from `decision_breakdown()$removed`, which is 1-based). Zero-length,
#'       of the same type as the labels, when `k_star == 0`.}
#'     \item{shock_coincidence}{`NULL` if `shock_periods` was not supplied; otherwise `TRUE` iff
#'       `responsible` is non-empty AND every period in it is also in `shock_periods` (so `FALSE`,
#'       not vacuously `TRUE`, when `k_star == 0`).}
#'     \item{winner_stability, winner_freq}{`winner_stability()`'s bootstrap stability and
#'       per-model win frequency. On a degenerate pair these are `NaN` and a uniform placeholder,
#'       as from [winner_stability()], but without its warning.}
#'     \item{fragile}{`TRUE` if degenerate; `NA` if no `benchmark` was supplied to calibrate
#'       against (see `?selectionfragility`'s "Sentinel conventions" section); otherwise `TRUE`
#'       iff `winner_stability` falls below `benchmark`'s p05.}
#'   }
#'   See `vignette("selectionfragility")` for a worked, narrative example using several of these
#'   fields together.
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
  if (is.null(labels)) labels <- up$labels  # honor a panel's user-supplied labels
  L <- .as_loss_dict(L)
  .validate_losses(L, w)
  if (is.null(w)) w <- rep(1, length(L[[1]]))
  w <- as.numeric(w)
  T <- length(w)
  if (!is.null(labels) && length(labels) != T) {
    stop(sprintf("labels has length %d, expected %d (one per period).", length(labels), T), call. = FALSE)
  }
  a <- pooled_winner(L, w)

  rest <- L[setdiff(names(L), a)]
  pooled_runner_up <- if (length(rest) == 1) names(rest)[1] else pooled_winner(rest, w)

  db <- decision_breakdown(L, w, a, return_ties = TRUE)
  k <- db$k; b <- db$binding_opponent; removed <- db$removed; binding_ties <- db$ties
  la <- as.numeric(L[[a]])
  lb <- if (!is.null(b)) as.numeric(L[[b]]) else la
  c_ <- w * (lb - la)
  M <- .fsum(c_)
  pscale_ab <- .pair_scale(la, lb, w, T)
  concentration <- if (M > 0 && (M / T) > .MARGIN_REL_FLOOR * pscale_ab) max(c_) / M else NaN

  conc_by_opp <- list()
  for (m in binding_ties) {
    lm <- as.numeric(L[[m]])
    cm <- w * (lm - la)
    Mm <- .fsum(cm)
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
  resp <- lab[removed]  # zero-length (same type as the labels) when k* == 0
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
    p05 <- .resolve_benchmark_p05(benchmark)
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
