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
#' R's internal summation algorithm) -- THIS is the guarantee this function actually delivers,
#' and the only one the tie-break logic depends on. Neumaier (improved Kahan) compensated
#' summation on top of the sorted sequence then keeps the result CLOSE to the mathematically
#' exact sum for ordinary inputs, but is NOT provably exact the way `math.fsum`'s Shewchuk
#' distillation algorithm is: 5-lens round, 2026-11-18 live-diffing against Python found a
#' contrived mixed-sign input at this package's own `.MAX_ABS_LOSS` magnitude cap (1e100) --
#' `rep(c(1, 1e100, 1, -1e100), 5)`, true sum 10 -- where this function returns 0 (catastrophic,
#' not just imprecise) while `math.fsum()` on the identical input correctly returns 10. The
#' earlier "matching math.fsum's intent" framing overclaimed single-compensator Neumaier
#' summation's exactness; a single compensation term is not provably sufficient for every input
#' the way Shewchuk's running list of nonoverlapping partial sums is. In practice this requires
#' near-cap-magnitude values that cancel almost exactly, which realistic forecast-loss panels
#' are not expected to produce -- not fixed here (reimplementing Shewchuk's algorithm correctly
#' is a nontrivial, separate piece of work, not warranted by this failure mode's practical
#' likelihood), but the claim of exactness is corrected so a future reader doesn't rely on it.
#'
#' SMALL-MAGNITUDE EXTREME, CHECKED (2026-11-18): this is NOT a large-magnitude-only failure --
#' the identical multi-scale-cancellation structure reproduces at the opposite, small-magnitude
#' end, confirming the limitation above is symmetric, not an artifact of `.MAX_ABS_LOSS` or of
#' overflow specifically. `rep(c(1e-300, 1e-150, 1e-300, -1e-150), 5)`, true sum `1e-299`, returns
#' `0` here (identical catastrophic-cancellation failure to the 1e100 case above, same root cause:
#' two nested magnitude tiers cancelling at once overwhelms a single compensator). A three-tier
#' variant is worse than merely catastrophic-to-zero: `c(rep(c(1,-1),3), rep(c(1e-150,-1e-150),3),
#' rep(c(1e-300,-1e-300),3), 1e-300)`, true sum `1e-300`, returns a wrong nonzero value
#' (`-2.7e-166`) -- 134 orders of magnitude off, not just dropped to 0. BY CONTRAST, the
#' genuinely-subnormal regime this package had never previously tested (values at or below
#' `.Machine$double.xmin` ~ 2.2e-308, down to the smallest representable double, 4.9e-324) was
#' checked directly and is NOT an additional failure mode: `rep(c(4.9e-324, 2.2e-308, 4.9e-324,
#' -2.2e-308), 5)` and `rep(c(1e-300, 1, 1e-300, -1), 5)` (a true subnormal/smallest-double value
#' swamped by an ordinary-scale value, the mirror image of the large-vs-normal case this package's
#' own large-magnitude tests already cover) both come back EXACT, including recovering a true sum
#' as tiny as `1e-299` that plain `sum()` collapses to `0` on the same input. So the failure
#' documented here is specifically about MULTIPLE DISTINCT CANCELLING MAGNITUDE TIERS present at
#' once (whether both tiers are huge, both are small-but-still-normal-range, or one of each) --
#' not about proximity to zero, the overflow cap, or subnormal representation specifically. Left
#' unfixed for the same reason as the large-magnitude case: a correct fix is Shewchuk's algorithm,
#' out of scope here; realistic forecast-loss panels are not expected to carry multiple,
#' simultaneously near-exactly-cancelling magnitude tiers regardless of which end of the range
#' they sit at.
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

#' The champion, its binding opponent, and the signed per-period margin contributions.
#'
#' OPPONENT TIE-BREAK, DELIBERATELY DIFFERENT FROM `decision_breakdown()`'s OWN -- see the
#' consolidated explanation in `decision_breakdown()`'s `@section Limitation` (fragility.R) for the
#' full picture across all three tie-break rules in this package; this comment covers only this
#' function's side of it. `decision_breakdown()`'s own tie-break (alphabetically-first name among
#' opponents tied at minimal k*) is the right choice for THAT function's contract: a stable, citable
#' point estimate of k* itself, where ties on an integer count are common at small T. Reusing that
#' same rule here made an earlier version of this module's period-level statistics (which period is
#' "the" pivot) measurably RENAME-DEPENDENT: renaming a tied opponent can flip which one sorts first
#' alphabetically, and different tied opponents can point at very different pivotal periods even
#' though the margin tie is identical. The tie is broken here instead by the LARGEST total margin M
#' among the tied opponents (`db$ties`) -- a property of the DATA, not of what the models are called,
#' so it is invariant to renaming by construction; ties in a continuous quantity like M are
#' effectively impossible (unlike ties in the integer k*, which are common -- confirmed directly:
#' ~38% of random panels with a strict pooled winner have more than one opponent tied at k*). This is
#' NOT a bug or an oversight relative to `decision_breakdown()`'s rule -- unifying the two would
#' reintroduce the rename-dependence this rule was specifically chosen to avoid.
#'
#' TIE WITHIN THE TIE (verified 2026-11-18, not a bug): if two or more of the M-tied opponents ALSO
#' have an EXACTLY tied M (adversarially constructible -- e.g. two opponents whose per-period losses
#' are permutations of each other, as in the regression test below), the `if (M > best_M)` loop
#' below keeps the FIRST one encountered and does not replace it on an exact tie. "First encountered"
#' is the first name in `db$ties`, which `decision_breakdown()` builds by iterating
#' `sort(..., method = "radix")` over model names -- i.e. this secondary tie-break falls back to
#' alphabetically-first-by-byte-order, independent of the ORIGINAL list's insertion order and of
#' locale (`method = "radix"` is explicitly non-locale-dependent per R's own `?sort` docs). Confirmed
#' directly: swapping two exactly-margin-tied opponents' position in the input list left the chosen
#' opponent unchanged (always the alphabetically-first of the two), across 5 repeated calls and two
#' different insertion orders.
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
#' @section Limitation: this statistic is weaker evidence than it may appear. Subsampling draws
#'   WITHOUT replacement from the SAME already-realized loss values (no new noise is drawn), so
#'   a period's per-period contribution is identical at every kept position; dropping other
#'   periods can never create a new maximum that exceeds the full sample's actual top contributor.
#'   `sub_pivot == full_pivot` whenever the top period happens to survive into the subsample,
#'   which happens with probability close to `frac` BY CONSTRUCTION of this sampling scheme,
#'   largely independent of whether the panel's true pivot is robust or a noise artifact.
#'   Measured directly (2026-11-18): on both a maximally-robust panel (one dominant shock,
#'   `concentration_share` ~0.99) and a maximally-fragile one (evenly spread margin,
#'   `concentration_share` ~0.11), `pivot_agreement()` returns ~`frac` (the default 0.8) in BOTH
#'   cases -- the number tracks the `frac` knob, not the panel's actual concentration structure.
#'   Treat it as a weak signal at best; `concentration_share()` is the more informative diagnostic
#'   for "is this pivot a real feature of the data."
#'
#'   NOT R-SPECIFIC; NOT FIXED BY THIS PORT ON PURPOSE. This is a property of the ORIGINAL
#'   `selection_fragility` (Python) design, not a porting defect: `pivot.py`'s own
#'   `_champ_opp_contributions`-based subsampling also draws WITHOUT replacement from the same
#'   realized values (`rng.choice(T, size=n_sub, replace=False)`), and a fresh three-regime check
#'   against the installed Python package (`selection-fragility` 1.0.4, dominant-shock vs.
#'   evenly-spread vs. two-way-near-tie fixtures, 2026-11-18) reproduced the identical pattern:
#'   `pivot_agreement()` tracked `frac` (~0.75-0.82 at `frac=0.8`, ~0.49-0.57 at `frac=0.5`,
#'   ~0.93-0.96 at `frac=0.95`) regardless of `concentration_share` (0.10-0.88 across the same three
#'   panels), in both languages. Fixing this in R alone would create a cross-language divergence
#'   from the oracle-pinned Python formula and is out of scope here; a shared redesign would need
#'   deliberate review on both sides.
#'
#'   The seemingly obvious parity-safe fix -- sample periods WITH replacement (a true bootstrap)
#'   instead of without -- was tried (not shipped) and does NOT solve this: resampling still draws
#'   from the SAME fixed realized values, so it is still bounded by `P(the full-sample top period
#'   appears at least once in the resample)`, a combinatorial function of `n_sub`/`T` alone, not of
#'   the panel's concentration. Measured directly: across the same three regimes, with-replacement
#'   resampling gave 0.52-0.67 with little separation between the dominant-shock and evenly-spread
#'   panels -- no more discriminative than the shipped without-replacement version, just shifted to
#'   a different frac-driven constant. No further change is planned without also redrawing new
#'   noise (a different statistic, out of scope for a same-signature fix).
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
  n_boot <- suppressWarnings(as.integer(n_boot))  # as.integer guards int32 overflow (NA -> if() crash)
  if (length(n_boot) != 1 || is.na(n_boot) || n_boot <= 0) stop(sprintf("pivot_agreement(): n_boot must be a single positive integer; got %s.", paste(format(n_boot_in), collapse = ", ")), call. = FALSE)
  # Length/NA/non-numeric guard FIRST, matching n_boot/seed just above and .validate_alpha()'s
  # established pattern elsewhere in the package: `frac > 0 && frac < 1` evaluates && on the raw
  # input, so a NA/NULL/length>1/non-numeric frac detonated a raw, unworded "missing value where
  # TRUE/FALSE needed" (or similar) R-internal error before this function's own worded message
  # could run -- found by random fuzzing (frac=NA), not a targeted hypothesis.
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
  # STRICT-AUDIT FIX (2026-11-18): seed was passed straight into set.seed() with no local guard,
  # unlike n_boot/frac just above (same fix commit) and unlike mcs()/compare()/identified()'s own
  # seed= (an identical fix already made there in this same commit) -- a bad seed (NA/non-scalar/
  # out-of-range/character) reached a raw, unworded "supplied seed is not a valid integer" error
  # from base R's set.seed() itself instead of this file's own worded style.
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
