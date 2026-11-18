# pivot.R — concentration_share and pivot_agreement, the two statistics that answer
# "is the named pivotal period real?" Port of pivot.py.

.DEGENERATE_FLOOR_PIVOT <- 1e-12

#' Permutation-invariant summation (stand-in for Python's `math.fsum` in the opponent tie-break).
#'
#' Two opponents whose per-period contributions are permutations of each other have the same
#' margin, but a raw `sum()` can give them different float totals depending on order, which would
#' change which opponent (and pivotal period) is named. Sorting first makes permutations sum
#' identically; Neumaier compensation then keeps the total close to exact. Unlike `math.fsum` it is
#' not exact when several magnitude tiers cancel at once (e.g. `rep(c(1, 1e100, 1, -1e100), 5)`
#' returns 0, not 10) -- not a pattern real loss panels produce.
#' @keywords internal
#' @noRd
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

#' The champion, its binding opponent, and the signed per-period margin contributions.
#'
#' Opponents tied at the minimal k* are broken by the largest total margin M, not by name as in
#' `decision_breakdown()`: the pivotal period then depends on the data, not on what the models are
#' called. Exact ties in M fall back to the first name in `db$ties` (radix-sorted, so
#' locale-independent).
#' @keywords internal
#' @noRd
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
#' @return The concentration share, or `NA` if there is no strict winner or the margin is
#'   degenerate -- see `?selectionfragility`'s "Sentinel conventions" section.
#' @seealso [pivot_agreement()] (whether this concentration is stable under subsampling),
#'   [surprise_concentration()], [report()].
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
#' pivotal period as the full sample. Uses R's RNG, so results differ from Python's on the same
#' seed.
#'
#' @param L A named list `{model: vector}` or a `loss_panel`.
#' @param w Optional numeric vector of per-period weights.
#' @param seed RNG seed.
#' @param n_boot Number of subsamples to draw.
#' @param frac Fraction of periods kept in each subsample (strictly between 0 and 1).
#' @return The agreement fraction, or `NA` if there is no strict winner, the subsample is too
#'   small, or every subsample drawn had no strict winner -- see `?selectionfragility`'s
#'   "Sentinel conventions" section.
#' @section Limitation: this statistic is weaker evidence than it may appear. Subsampling reuses
#'   the same realized losses, so the subsample names the full-sample pivot whenever that period
#'   survives the draw -- probability close to `frac` by construction. On both a single-shock panel (`concentration_share` ~0.99) and an evenly spread one (~0.11)
#'   it returns about `frac`. Treat it as a weak signal; `concentration_share()` is the more
#'   informative diagnostic. The Python package has the same design, and resampling with
#'   replacement does not fix it (it is still bounded by the top period's chance of being drawn).
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
  n_boot_in <- n_boot
  n_boot <- suppressWarnings(as.integer(n_boot))
  if (length(n_boot) != 1 || is.na(n_boot) || n_boot <= 0) stop(sprintf("pivot_agreement(): n_boot must be a single positive integer; got %s.", paste(format(n_boot_in), collapse = ", ")), call. = FALSE)
  if (length(frac) != 1 || !is.numeric(frac) || !is.finite(frac)) {
    stop(sprintf(
      "pivot_agreement(): frac must be a single finite number strictly between 0 and 1; got a %s of length %d.",
      class(frac)[1], length(frac)
    ), call. = FALSE)
  }
  if (!(frac > 0.0 && frac < 1.0)) {
    stop(sprintf(
      paste0(
        "pivot_agreement(): frac must be strictly between 0 and 1 (it is the fraction of periods ",
        "kept in each subsample); got %s."
      ), frac
    ), call. = FALSE)
  }
  seed_in <- seed; seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1 || is.na(seed) || seed < 0) {
    stop(sprintf("pivot_agreement(): seed must be a single non-negative integer; got %s.",
                  paste(format(seed_in), collapse = ", ")), call. = FALSE)
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
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
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
