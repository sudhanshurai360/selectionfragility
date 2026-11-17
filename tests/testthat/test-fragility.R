# Oracle values generated from the Python `selection_fragility` package (v1.0.4) on the same
# inputs -- see release/selection-fragility/ (the Python package this ports). Indices here are
# 1-based (R); the Python oracle's 0-based `removed`/`responsible` indices are shifted by +1.

quickstart_L <- list(
  ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
  nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
  ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
)

test_that("pooled_winner/per_period_winner/condorcet match the Python oracle on the quickstart panel", {
  expect_equal(pooled_winner(quickstart_L), "ar1")
  expect_equal(per_period_winner(quickstart_L), "nbeats")
  expect_equal(condorcet_winner(quickstart_L), "nbeats")
  expect_equal(condorcet_status(quickstart_L), "winner")
})

test_that("decision_breakdown and breakdown_number match the Python oracle", {
  db <- decision_breakdown(quickstart_L)
  expect_equal(db$k, 1L)
  expect_equal(db$binding_opponent, "nbeats")
  expect_equal(db$removed, 9L)  # python index 8 + 1

  bn <- breakdown_number(quickstart_L$ar1, quickstart_L$nbeats, rep(1, 10))
  expect_equal(bn$k, 1L)
  expect_equal(bn$removed, 9L)
})

test_that("fragility() deterministic fields match the Python oracle", {
  frag <- fragility(quickstart_L)
  expect_equal(frag$k_star, 1L)
  expect_equal(frag$k_over_T, 0.1)
  expect_equal(frag$concentration, 2.323529411764705, tolerance = 1e-9)
  expect_equal(frag$concentration_range, c(2.323529411764705, 2.323529411764705), tolerance = 1e-9)
  expect_false(frag$degenerate_margin)
  expect_true(frag$reversal)
  expect_true(frag$reversal_condorcet)
  expect_equal(frag$responsible[[1]], 8)  # label value, 0-based by default (matches Python)
})

test_that("an exact tie reports k*=0 and a degenerate margin, not a spurious winner", {
  L <- list(a = c(1, 2, 3, 4), b = c(1, 2, 3, 4))
  db <- decision_breakdown(L)
  expect_equal(db$k, 0L)
  frag <- fragility(L)
  expect_true(frag$degenerate_margin)
  expect_true(is.nan(frag$concentration))
  expect_true(frag$fragile)
})

test_that("breakdown_number rejects negative weights and non-finite losses", {
  expect_error(breakdown_number(c(1, 2, 3), c(2, 3, 4), c(-1, 1, 1)), "negative")
  expect_error(breakdown_number(c(1, NA, 3), c(2, 3, 4), c(1, 1, 1)), "finite")
})

test_that("fragility() rejects fewer than 2 models and mismatched period counts", {
  expect_error(fragility(list(a = c(1, 2, 3))), "at least 2 models|>=2 models")
  expect_error(fragility(list(a = c(1, 2, 3), b = c(1, 2))), "same number of periods")
})

test_that("decision_breakdown() refuses loss magnitudes beyond the float64-overflow floor", {
  L <- list(a = c(1, 2, 1e101), b = c(2, 3, 4))
  expect_error(decision_breakdown(L), "exceeds")
})

test_that("winner_stability degenerates to NaN (not 1.0) on an exact tie", {
  L <- list(a = c(1, 2, 3, 4), b = c(1, 2, 3, 4))
  ws <- winner_stability(L)
  expect_true(is.nan(ws$stability))
})

test_that("winner_stability refuses block >= T", {
  L <- list(a = c(1.1, 2.2, 3.3, 0.9), b = c(1.0, 2.0, 3.5, 4.0))
  expect_error(winner_stability(L, block = 4), "block=4 >= T=4")
})

test_that("non-numeric weights raise a clear coercion error, not a misleading length message", {
  # REGRESSION (found in R-port correctness audit, 2026-11-17): `.validate_losses()`'s weight
  # check coerced with `suppressWarnings(as.numeric(w))`, so a weight vector of unconvertible
  # strings silently became all-NA (right LENGTH, wrong VALUES) and fell into the generic
  # "weights must be finite and of length %d; got length %d" branch -- which reports the length
  # that was actually fine, misleading a caller into debugging the wrong thing. The Python oracle
  # (`selection_fragility.fragility._validate_losses`) raises a dedicated TypeError at the
  # coercion step itself ("weights must be an array-like of numeric values ... got list that
  # could not be converted") with no reference to length at all. This pins the fixed R message to
  # match that distinction: it must name the real problem (non-numeric values) and must NOT claim
  # anything about the (correct) length.
  L <- list(a = c(1, 2, 3), b = c(2, 3, 1))
  err <- tryCatch(pooled_winner(L, w = c("x", "y", "z")), error = function(e) conditionMessage(e))
  expect_match(err, "could not be converted|not numeric|non-numeric")
  expect_false(grepl("got length 3", err, fixed = TRUE))
  # A fully valid numeric-as-string weight vector (e.g. from a CSV read) must still work -- this
  # fix must not break legitimate numeric coercion.
  expect_equal(pooled_winner(L, w = c("1", "1", "1")), pooled_winner(L))
})

test_that("surprise_concentration reports concentrated difficulty on a single hard period", {
  # One period (index 3, 0-based) is hard for every model at once; every other period is flat.
  L <- list(
    ar1    = c(1.0, 1.0, 1.0, 3.0, 1.0),
    nbeats = c(0.9, 0.9, 0.9, 2.8, 0.9),
    ets    = c(1.2, 1.2, 1.2, 3.2, 1.2)
  )
  sc <- surprise_concentration(L)
  expect_equal(sc$sci, 1)
  expect_equal(sc$hardest, 3)
  expect_true(sc$eff_surprising_periods >= 1 && sc$eff_surprising_periods <= 1 + 1e-9)
  expect_true(is.finite(sc$d_max_over_median) || sc$d_max_over_median == Inf)
})

test_that("surprise_concentration reports sci = 0 when difficulty is perfectly flat", {
  L <- list(
    ar1    = c(1.0, 1.0, 1.0, 1.0),
    nbeats = c(0.9, 0.9, 0.9, 0.9),
    ets    = c(1.2, 1.2, 1.2, 1.2)
  )
  sc <- surprise_concentration(L)
  expect_equal(sc$sci, 0)
  expect_equal(sc$eff_surprising_periods, 4)
})

test_that("exchangeable_benchmark returns a sane null distribution with p05 <= median <= p95", {
  eb <- exchangeable_benchmark(K = 3, T = 15, n_rep = 15, n_boot = 40, seed = 1)
  expect_true(eb$p05 <= eb$median)
  expect_true(eb$median <= eb$p95)
  expect_true(eb$p05 >= 0 && eb$p95 <= 1)
})

test_that("exchangeable_benchmark is reproducible given the same seed", {
  eb1 <- exchangeable_benchmark(K = 3, T = 12, n_rep = 10, n_boot = 30, seed = 7)
  eb2 <- exchangeable_benchmark(K = 3, T = 12, n_rep = 10, n_boot = 30, seed = 7)
  expect_equal(eb1, eb2)
})

test_that("tie-breaks on mixed-case model names match Python's codepoint sort, not locale collation", {
  # REGRESSION (found by a cross-language parity audit, 2026-11-17): R's sort()/order() on
  # character vectors defaults to LC_COLLATE locale collation, which disagrees with Python's
  # codepoint-based sorted()/min() for mixed-case names -- e.g. under en_US.UTF-8, R's sort()
  # orders "alpha" < "Zeta" (dictionary order) while Python's sorted() orders "Zeta" < "alpha"
  # (codepoint order, uppercase sorts first). On a genuinely tied panel this silently picked a
  # DIFFERENT model as the per-period/pooled winner depending on which language (or even which
  # locale) ran the comparison. Fixed by sorting with method="radix" everywhere model names are
  # tie-broken, which is always byte/codepoint order regardless of locale.
  L <- list(Zeta = c(1, 2, 1, 2), alpha = c(1, 2, 1, 2))
  expect_equal(per_period_winner(L), "Zeta")
  expect_equal(pooled_winner(L), "Zeta")
})

test_that("duplicate model names are rejected, not silently aliased to the first match", {
  # REGRESSION (found in round-4 R-engineering review): R's L[[name]] lookup silently returns
  # only the FIRST match for a duplicated name, so a second model sharing a name was previously
  # dropped from the comparison without any error -- a silent-correctness footgun on the raw-list
  # entry point (loss_panel() already guarded against this; the shared validators didn't).
  L <- list(a = c(1, 1, 1, 1, 1), a = c(100, 100, 100, 100, 100), b = c(2, 2, 2, 2, 2))
  expect_error(pooled_winner(L), "unique.*duplicate|duplicate.*unique")
  expect_error(decision_breakdown(L), "unique.*duplicate|duplicate.*unique")
})

test_that("breakdown_number() rejects mismatched la/lb/w lengths instead of silently recycling", {
  # REGRESSION (found by an adversarial-fuzz review, 2026-11-17): the length check was
  # `.validate_weights(as.numeric(w), length(w))` -- a self-referential no-op that never actually
  # compared against length(la)/length(lb). When length(w) happened to be an exact divisor of
  # length(la)/length(lb), R's silent vector recycling activated with NO warning at all and
  # returned a confident, wrong k*.
  la <- c(1, 1, 1, 1, 1, 1)
  lb <- c(0.9, 0.9, 0.9, 0.9, 0.9, 1.3)
  expect_error(breakdown_number(la, lb, c(1, 50)), "same length")
  # The correctly-sized call still works.
  res <- breakdown_number(la, lb, rep(1, 6))
  expect_equal(res$k, 0L)
})

test_that("exchangeable_benchmark() rejects K < 2 with a clean error, not a raw R internal error", {
  # REGRESSION (found by an adversarial-fuzz review, 2026-11-17): K=0 previously reached a raw
  # "'names' attribute [1] must be the same length as the vector [0]" internal error instead of
  # a clean, worded validation message matching the rest of this package's discipline.
  expect_error(exchangeable_benchmark(K = 0, T = 10), "at least 2 models")
})

test_that("pooled_winner recognizes a degenerate tie among NEGATIVE means (abs() on both scale terms)", {
  # REGRESSION (differential R-vs-Python fuzzing, 2026-11-17): the tie-tolerance scale was
  # (abs(best) + means)/2 -- the per-model term not abs()'d. With negative means, (|v|+v)/2 -> ~0
  # collapsed the relative tie floor to ~0, so a genuine degenerate tie among negative-mean models
  # (common with decimal-quantized log/skill-score losses) was not recognized and the winner was
  # broken toward the raw floating-point minimum, diverging from the Python oracle.
  expect_equal(pooled_winner(list(a = c(-1, -1), b = c(-1, -1.000000000001))), "a")
  # Positive-mean control still behaves (both pick "a").
  expect_equal(pooled_winner(list(a = c(1, 1), b = c(1, 0.999999999999))), "a")
})

test_that("decision_breakdown()'s return_ties actually toggles the return shape (matches Python)", {
  # REGRESSION (doc-vs-code contract audit + parity, 2026-11-17): return_ties was an inert
  # parameter -- `ties` was always in the returned list regardless. Python returns a 3- vs 4-tuple;
  # R now omits/includes the `ties` field accordingly.
  L <- list(ar1 = c(1, 1, 1, 1, 1), nbeats = c(0.5, 0.5, 0.5, 0.5, 5.0))
  expect_false("ties" %in% names(decision_breakdown(L)))
  expect_true("ties" %in% names(decision_breakdown(L, return_ties = TRUE)))
})

test_that("winner_stability() rejects a fractional 0<n_boot<1 instead of silently returning stability=0", {
  # REGRESSION (type-coercion review, 2026-11-17): 0 < n_boot < 1 passed the `n_boot <= 0` guard
  # but made seq_len(n_boot) empty, so the bootstrap never ran and stability silently returned 0
  # ("maximally fragile") with no error.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0, 1.2), b = c(1.2, 1.1, 1.3, 1.1, 1.4))
  expect_error(winner_stability(L, n_boot = 0.9), "positive integer")
})

test_that("a non-integer block does not inject NA into the bootstrap resample index", {
  # REGRESSION (type-coercion review, 2026-11-17): ceiling(T/block) used the fractional block while
  # 0:(block-1) truncated it, so the index vector was padded with NA and the bootstrap then blamed
  # the (finite) input data for "non-finite loss values". .block_resample_idx now floors block.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0, 1.2, 0.8, 1.0, 1.1, 0.95, 1.05),
            b = c(1.2, 1.1, 1.3, 1.1, 1.4, 1.0, 1.2, 1.1, 1.25, 1.15))
  expect_no_error(winner_stability(L, block = 3.7, seed = 1))
})

test_that("fragility()/surprise_concentration() honor a loss_panel's user-supplied labels (8-reviewer round, 2026-11-17)", {
  # REGRESSION: .unwrap_panel() dropped the panel's $labels, so fragility(panel)$responsible and
  # surprise_concentration(panel)$hardest fell back to 0-based positional indices even when the
  # user had attached real labels -- unlike report(panel), which resolved them. A forecaster who
  # built loss_panel(L, labels=years) and read $responsible got the WRONG (positional) period id.
  # Passing labels= explicitly already worked and matched the Python oracle; only the panel path
  # dropped them. Fix flows user-supplied labels (labels_are_positional == FALSE) through.
  L <- list(
    ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
    ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18))
  p <- loss_panel(L, labels = 2015:2024)
  # The shock is at period 9 (1-based); label 2023. Panel path must resolve to the label now.
  expect_equal(fragility(p)$responsible[[1]], 2023)
  expect_equal(fragility(L, labels = 2015:2024)$responsible[[1]], 2023)  # explicit still works
  # An explicit labels= argument still overrides the panel's labels.
  expect_equal(fragility(p, labels = 2100:2109)$responsible[[1]], 2108)
  # A panel WITHOUT user labels keeps the documented 0-based positional fallback (unchanged).
  expect_equal(fragility(loss_panel(L))$responsible[[1]], 8)
  # surprise_concentration()'s hardest field has the identical path.
  sp <- loss_panel(list(a = c(1, 1, 1, 1, 5), b = c(0.5, 0.5, 0.5, 0.5, 0.6)), labels = 2020:2024)
  expect_equal(surprise_concentration(sp)$hardest, 2024)
  expect_equal(surprise_concentration(loss_panel(list(a = c(1, 1, 1, 1, 5), b = c(0.5, 0.5, 0.5, 0.5, 0.6))))$hardest, 4)
})

test_that("winner_stability()/exchangeable_benchmark() do NOT leave a .Random.seed on a fresh session (8-reviewer round, 2026-11-17)", {
  # REGRESSION: the native bootstrap wrappers saved/restored .Random.seed only when one already
  # existed; on a fresh session (no prior set.seed) old_seed was NULL and the on.exit did nothing,
  # leaving the set.seed()-created seed in .GlobalEnv -- silently pinning the caller's later
  # "random" draws to the package's seed. We simulate the fresh-session state by removing
  # .Random.seed first, then assert each wrapper leaves it absent again (mirrors .run_mcs_arch).
  L <- list(a = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0), b = c(0.5, 0.5, 0.5, 0.5, 0.5, 5.0))
  fresh <- function(f) {
    if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
    f()
    exists(".Random.seed", envir = .GlobalEnv)
  }
  expect_false(fresh(function() winner_stability(L, n_boot = 10, seed = 1)))
  expect_false(fresh(function() pivot_agreement(L, n_boot = 10, seed = 1)))
  expect_false(fresh(function() mcs(cbind(a = L$a, b = L$b), B = 50, seed = 1)))
  expect_false(fresh(function() exchangeable_benchmark(K = 2, T = 6, n_rep = 3, n_boot = 10, seed = 1)))
  # And a normal (non-fresh) session is still restored byte-identically.
  set.seed(99); before <- get(".Random.seed", envir = .GlobalEnv)
  invisible(winner_stability(L, n_boot = 10, seed = 1))
  expect_identical(before, get(".Random.seed", envir = .GlobalEnv))
})

test_that("pivot_agreement() rejects an out-of-int32 n_boot with a worded error, not an NA crash (8-reviewer round)", {
  # REGRESSION: `if (as.integer(n_boot) <= 0)` coerced 3e9 to NA, so if(NA) crashed with the
  # opaque base-R "missing value where TRUE/FALSE needed". Now mirrors winner_stability's guard.
  L <- list(a = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0), b = c(0.5, 0.5, 0.5, 0.5, 0.5, 5.0))
  expect_error(pivot_agreement(L, n_boot = 3e9), "single positive integer")
})

test_that("breakdown_number() scans in O(T), not O(T^2), and matches the old c()-growth reference exactly (24-agent round, 2026-11-18)", {
  # REGRESSION: the elimination loop built `removed` via `removed <- c(removed, i)`, which
  # reallocates and copies the whole vector every iteration -- O(k*^2) instead of O(k*) whenever
  # many periods must be removed to flip the decision (large k* relative to T, i.e. exactly the
  # closely-tied/fragile panels this package exists to flag). Now preallocates at the loop's own
  # upper bound and trims at the end. Correctness re-checked against the literal old c()-growth
  # logic across 300 random panels.
  set.seed(123)
  for (i in 1:300) {
    Tn <- sample(2:25, 1)
    la <- rnorm(Tn); lb <- rnorm(Tn, mean = 0.3); w <- rep(1, Tn)
    r <- breakdown_number(la, lb, w)
    c_ <- w * (lb - la); M <- sum(c_)
    scale <- (mean(abs(la)) + mean(abs(lb))) / 2; wscale <- mean(w)
    if (M <= 0 || (M / Tn) <= 1e-9 * scale * wscale) {
      ref <- list(k = 0L, removed = integer(0))
    } else {
      order_idx <- order(-c_); s <- M; removed_ref <- integer(0)
      for (j in order_idx) {
        if (c_[j] <= 0) break
        s <- s - c_[j]; removed_ref <- c(removed_ref, j)
        if (s <= 0 || s <= 1e-9 * scale * Tn * wscale) break
      }
      ref <- list(k = length(removed_ref), removed = removed_ref)
    }
    expect_identical(r$k, ref$k)
    expect_identical(r$removed, ref$removed)
  }
  # Performance sanity: a long, fully-tied panel (k* proportional to T) must stay fast (would be
  # several seconds under the old O(T^2) growth at this size).
  Tn <- 20000
  la <- rep(1.0, Tn); lb <- rep(1.0, Tn); lb[1:12000] <- 1.0001
  t0 <- Sys.time()
  r <- breakdown_number(la, lb, rep(1, Tn))
  elapsed <- as.numeric(Sys.time() - t0)
  expect_equal(r$k, 12000L)
  expect_lt(elapsed, 2.0)
})

test_that(".block_resample_idx(circular=FALSE) is O(T), not O(T^2), and matches the old while-loop exactly under the same seed (meta-review round, 2026-11-18)", {
  # REGRESSION: the non-circular branch built its index via `out <- c(out, seq(...))` inside a
  # while loop whose iteration count scales with T -- the identical anti-pattern fixed in
  # breakdown_number()'s elimination loop earlier this round, left unfixed in this sibling.
  # Reachable via the public circular=FALSE argument on winner_stability()/exchangeable_benchmark()
  # (default n_boot=2000, so the quadratic cost is paid once per bootstrap replicate). Confirmed
  # 14+ seconds at T=80,000 before the fix; rewritten to draw all block starts up front
  # (vectorized, matching the already-fast circular branch) instead of growing a vector in a loop.
  old_resample <- function(T, block) {
    out <- integer(0)
    while (length(out) < T) {
      start <- sample.int(max(1, T - block + 1), 1)
      out <- c(out, seq(start, min(start + block - 1, T)))
    }
    out[1:T]
  }
  for (i in 1:50) {
    Tn <- sample(3:60, 1); bl <- sample(2:min(Tn, 15), 1)
    set.seed(i); r_new <- selectionfragility:::.block_resample_idx(Tn, bl, circular = FALSE)
    set.seed(i); r_old <- old_resample(Tn, bl)
    expect_identical(r_new, r_old)
    expect_equal(length(r_new), Tn)
    expect_true(all(r_new >= 1 & r_new <= Tn))
  }
  # Performance sanity: would be many seconds under the old O(T^2) growth at this size.
  t0 <- Sys.time()
  idx <- selectionfragility:::.block_resample_idx(80000L, 5, circular = FALSE)
  elapsed <- as.numeric(Sys.time() - t0)
  expect_equal(length(idx), 80000L)
  expect_lt(elapsed, 2.0)
})
