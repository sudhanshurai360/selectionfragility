# pivot.R — concentration_share and pivot_agreement, the two statistics that answer
# "is the named pivotal period real?" Faithful port of pivot.py.

.DEGENERATE_FLOOR_PIVOT <- 1e-12

#' Permutation-invariant summation (Python's `math.fsum` analogue for the opponent tie-break).
#'
#' `pivot.py`'s own header documents this bug being found and fixed FIVE separate times already:
#' a raw `sum()` over per-period contributions is order-dependent at the bit level, so two
#' opponents whose contributions are PERMUTATIONS of each other (mathematically identical total
#' margin, by construction of the tie) can sum to different float64 values purely from
#' accumulation order -- which flips which opponent (and therefore which pivotal period) gets
#' named, a real rename/reorder-dependence bug, not cosmetic. `math.fsum` fixes this on the Python
#' side by being exactly order-invariant (Shewchuk's algorithm). R's own `sum()` is NOT provably
#' order-invariant despite using a long-double accumulator on most platforms -- verified directly:
#' summing a permutation-tied adversarial vector (one huge contribution plus many tiny ones) at
#' weight scales 1e-6/1e-3/1/1e3 gave four DIFFERENT results depending on permutation order before
#' this fix. Sorting first makes any two permutations of the same multiset collapse to the exact
#' same input sequence (so they can only ever sum identically, by construction, regardless of
#' R's internal summation algorithm); Neumaier (improved Kahan) compensated summation on top of
#' the sorted sequence keeps the result close to the mathematically exact sum, matching
#' `math.fsum`'s intent without needing to reimplement Shewchuk's distillation algorithm.
#' @keywords internal
.fsum <- function(x) {
  x <- sort(x)
  s <- 0.0
  comp <- 0.0
  for (xi in x) {
    t <- s + xi
    if (abs(s) >= abs(xi)) {
      comp <- comp + ((s - t) + xi)
    } else {
      comp <- comp + ((xi - t) + s)
    }
    s <- t
  }
  s + comp
}

#' @keywords internal
.champ_opp_contributions <- function(L, w) {
  champ <- pooled_winner(L, w)
  db <- decision_breakdown(L, w, a = champ, return_ties = TRUE)
  if (db$k == 0) return(list(champ = NULL, opp = NULL, c = NULL))
  la <- as.numeric(L[[champ]])
  best_opp <- NULL; best_M <- -Inf; best_c <- NULL
  for (t in db$ties) {
    c_ <- w * (as.numeric(L[[t]]) - la)
    M <- .fsum(c_)
    if (M > best_M) {
      best_M <- M; best_opp <- t; best_c <- c_
    }
  }
  list(champ = champ, opp = best_opp, c = best_c)
}

#' Share of the champion's margin concentrated in the single most influential period
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @return The concentration share, or `NA` if there is no strict winner or the margin is degenerate.
#' @export
#' @examples
#' # ar1 wins steadily, but almost its whole margin over ets comes from one shock period
#' # (period 10) where ets has a bad miss -- exactly the case this statistic flags.
#' L <- list(
#'   ar1 = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
#'   ets = c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 5.0)
#' )
#' panel <- loss_panel(L)
#' concentration_share(panel)  # ~0.82 -- the margin is highly concentrated in one period
concentration_share <- function(L, w = NULL) {
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  cc <- .champ_opp_contributions(L, w)
  if (is.null(cc$opp)) return(NA)
  denom <- sum(abs(cc$c))
  scale <- mean(abs(as.numeric(L[[cc$champ]])) + abs(as.numeric(L[[cc$opp]]))) / 2.0
  if (denom <= .DEGENERATE_FLOOR_PIVOT * scale * T) return(NA)
  max(cc$c) / denom
}

#' Stability of the named pivotal period under period subsampling
#'
#' Fraction of `frac`*T-sized subsamples of periods (without replacement) that name the same
#' pivotal period as the full sample. NOTE: uses R's own RNG, so results are not bit-for-bit
#' identical to the Python package's numpy-based subsampling on the same seed.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param seed RNG seed.
#' @param n_boot Number of subsamples to draw.
#' @param frac Fraction of periods kept in each subsample (strictly between 0 and 1).
#' @return The agreement fraction, or `NA` if there is no strict winner, the subsample is too
#'   small, or every subsample drawn had no strict winner.
#' @seealso [concentration_share()] (how much of the margin a single period accounts for),
#'   [surprise_concentration()], [report()].
#' @export
#' @examples
#' # Same shock-year panel as concentration_share(): is the single-period pivot stable
#' # under subsampling, or an artifact of that one period? This is a resampling-based
#' # statistic -- fix `seed` for reproducibility, as done here.
#' L <- list(
#'   ar1 = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
#'   ets = c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 5.0)
#' )
#' panel <- loss_panel(L)
#' pivot_agreement(panel, seed = 42)  # ~0.78 -- the named pivot period is fairly stable
pivot_agreement <- function(L, w = NULL, seed = 0, n_boot = 200, frac = 0.8) {
  if (as.integer(n_boot) <= 0) stop(sprintf("pivot_agreement(): n_boot must be a positive integer; got %s.", n_boot), call. = FALSE)
  if (!(frac > 0.0 && frac < 1.0)) {
    stop(sprintf(
      paste0(
        "pivot_agreement(): frac must be strictly between 0 and 1 (it is the fraction of periods ",
        "kept in each subsample); got %s."
      ), frac
    ), call. = FALSE)
  }
  up <- .unwrap_panel(L, w); L <- up$L; w <- up$w
  L <- .as_loss_dict(L)
  T <- .validate_losses(L, w)
  w <- if (is.null(w)) rep(1, T) else as.numeric(w)
  cc <- .champ_opp_contributions(L, w)
  if (is.null(cc$opp)) return(NA)
  full_pivot <- which.max(cc$c)  # 1-based period index
  n_sub <- round(frac * T)
  if (n_sub < 3 || n_sub >= T) return(NA)

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  agree <- 0L; valid <- 0L
  for (b in seq_len(n_boot)) {
    idx <- sort(sample.int(T, size = n_sub, replace = FALSE))
    Lsub <- lapply(L, function(v) as.numeric(v)[idx])
    wsub <- w[idx]
    cc_s <- .champ_opp_contributions(Lsub, wsub)
    if (is.null(cc_s$opp)) next
    valid <- valid + 1L
    sub_pivot <- idx[which.max(cc_s$c)]
    if (sub_pivot == full_pivot) agree <- agree + 1L
  }
  if (valid == 0L) return(NA)
  agree / valid
}
