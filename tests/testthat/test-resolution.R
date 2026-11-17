# Oracle values generated from the Python `selection_fragility` package (v1.0.4) on the same inputs.

noisy_L <- list(
  champ = c(1.0, 1.05, 0.92, 1.10, 0.88, 1.02, 0.95, 1.08, 0.91, 1.03),
  rival = c(1.15, 1.00, 1.20, 1.05, 1.18, 0.98, 1.22, 1.01, 1.19, 1.04),
  far   = c(3.0, 3.1, 2.9, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.1)
)

test_that("minimum_detectable_edge/significance_boundary/mcb_bound/selection_regret match the Python oracle", {
  expect_equal(minimum_detectable_edge(noisy_L), 0.14028959960450463, tolerance = 1e-9)
  expect_equal(significance_boundary(noisy_L), 0.09466780403389015, tolerance = 1e-9)
  expect_equal(mcb_bound(noisy_L), 0.2254769292659032, tolerance = 1e-9)
  expect_equal(selection_regret(noisy_L), 0.02100000000000002, tolerance = 1e-9)
})

test_that("resolution_report() matches the Python oracle on champion/rival/edge/resolved", {
  rr <- resolution_report(noisy_L)
  expect_equal(rr$champion, "champ")
  expect_equal(rr$binding_rival, "rival")
  expect_true(rr$resolved)
  expect_equal(rr$observed_edge, 0.10865191146881276, tolerance = 1e-9)
  expect_equal(rr$fragility_read, "resolved")
})

test_that("a constant per-period differential gives SE~0 (MDE~0) and the correct constant edge", {
  L0 <- list(
    champ = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    rival = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08),
    far   = c(3.0, 3.1, 2.9, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.1)
  )
  expect_lt(minimum_detectable_edge(L0), 1e-10)
  expect_equal(mcb_bound(L0), 0.1, tolerance = 1e-9)
})

test_that("resolution functions validate alpha/power ranges and minimum T", {
  expect_error(minimum_detectable_edge(noisy_L, alpha = 0.7), "alpha must be < 0.5")
  expect_error(minimum_detectable_edge(noisy_L, power = 0.3), "power must be > 0.5")
  expect_error(selection_regret(list(a = c(1, 2), b = c(2, 3))), "at least 3 periods")
})

test_that("selection_regret() rejects passing both weights and w", {
  expect_error(selection_regret(noisy_L, weights = rep(1, 3), w = rep(1, 3)), "got both")
})

test_that("significance_boundary/mcb_bound validate alpha range too (not just minimum_detectable_edge)", {
  expect_error(significance_boundary(noisy_L, alpha = 0.7), "alpha must be < 0.5")
  expect_error(mcb_bound(noisy_L, alpha = 0.5), "alpha must be < 0.5")
})

test_that("R1 refuses (resolved=FALSE, fragility_read='undetermined') on a weak/noisy edge -- oracle match", {
  # Oracle values generated from the Python `selection_fragility` package (v1.0.4) on the same inputs:
  # T=5, high within-model noise relative to the champion/rival gap.
  weak_L <- list(champ = c(1.7308, -0.572, 0.933, -0.172, 0.4817),
                 riv   = c(2.5312, 1.6575, 0.3211, 0.0063, 1.0528))
  expect_equal(minimum_detectable_edge(weak_L), 2.979289584117425, tolerance = 1e-9)
  expect_equal(significance_boundary(weak_L), 2.066963498232777, tolerance = 1e-9)
  expect_equal(mcb_bound(weak_L), 3.3858891696881175, tolerance = 1e-9)
  expect_equal(selection_regret(weak_L, weights = NULL, w = NULL), 0.12238000000000002, tolerance = 1e-9)

  rr <- resolution_report(weak_L)
  expect_equal(rr$champion, "champ")
  expect_equal(rr$binding_rival, "riv")
  expect_false(rr$resolved)
  expect_equal(rr$observed_edge, 1.3189256714553406, tolerance = 1e-9)
  expect_true(isTRUE(rr$identified))
  # R1 gates fragility_read to "undetermined" even though the decision IS identified (R2 passes) --
  # resolved=FALSE alone must be enough to refuse, proving R1 and R2 are independent gates.
  expect_equal(rr$fragility_read, "undetermined")
})

test_that("R2 refusal on non-uniform weights preserves the already-computed resolved/observed_edge/mde fields (oracle match)", {
  # Oracle values generated from the Python `selection_fragility` package (v1.0.4): `identified()`
  # refuses outright on genuinely non-uniform weights (MCS::MCSprocedure()/arch.bootstrap.MCS have no
  # native support for weighted periods), but resolution_report() must still report the REAL,
  # already-computed resolved/observed_edge/mde/significance_boundary values -- only is_identified/
  # fragility_read should be affected by the refusal, exactly matching resolution_report()'s own
  # try/except contract in the Python original.
  L2 <- list(
    a = c(1.0001, 1.0299, 0.9726, 0.9109, 0.9545, 0.9008, 1.006, 1.134, 0.9508, 0.938,
          1.049, 1.0357, 1.0105, 0.907, 0.9971, 1.0695, 0.8656, 0.9542, 0.8099, 0.871),
    b = c(1.1158, 1.2765, 1.1733, 1.3271, 1.3157, 1.2813, 1.0483, 1.2461, 1.2951, 1.3113,
          1.147, 1.2522, 1.2021, 1.2191, 1.4061, 1.2192, 1.2967, 1.3884, 1.2416, 1.2888)
  )
  w2 <- c(4.461, 2.794, 4.312, 3.379, 3.838, 0.912, 2.935, 2.785, 4.421, 2.126,
          3.192, 0.767, 2.244, 1.954, 1.176, 4.174, 2.208, 4.904, 3.155, 3.223)
  rr <- resolution_report(L2, w2)
  expect_equal(rr$champion, "a")
  expect_equal(rr$binding_rival, "b")
  expect_true(rr$resolved)
  expect_equal(rr$observed_edge, 0.2864215140167742, tolerance = 1e-9)
  expect_equal(rr$mde, 0.08537299354775091, tolerance = 1e-9)
  expect_equal(rr$significance_boundary, 0.056994785009228645, tolerance = 1e-9)
  expect_null(rr$identified)
  expect_equal(rr$fragility_read, "undetermined")
})

test_that(".res_scale()'s champion-only normalization stays finite under an extreme cross-model scale mismatch (known limitation, documented, not fixed)", {
  # Found by an extreme-input stability review, 2026-11-17: normalizing by the champion's own
  # mean alone produces a nonsensical, unbounded "percentage" when the champion and rival sit at
  # very different loss scales (both individually valid, unflagged loss magnitudes) -- e.g. a
  # champion mean ~1e-8 and a rival mean ~1e8 prints an "observed edge" over 9,999,999,650% into
  # report()'s RESOLUTION line. This is SHARED, oracle-pinned behavior with the Python original's
  # identical `_scale(mean_champ)` formula (resolution.py) -- deliberately left unfixed here to
  # avoid creating a cross-language divergence; see the @section Assumption on
  # minimum_detectable_edge()/significance_boundary()/mcb_bound() for the documented caveat. This
  # test only pins that the result stays a finite (if uninterpretable) number rather than
  # NaN/Inf/erroring, so a future change to this formula is a deliberate, reviewed decision.
  a <- rep(1e-8, 10)
  b <- rep(1e8, 10)
  L <- list(a = a, b = b)
  expect_true(is.finite(minimum_detectable_edge(L)))
  expect_true(is.finite(significance_boundary(L)))
  expect_true(is.finite(mcb_bound(L)))
})

test_that("resolution_report() warns on a 'resolved' verdict driven by weight/volatility correlation (strict-regression round, 2026-11-18)", {
  # REGRESSION: a dedicated verification agent measured a 15-39% false-confidence rate among
  # "resolved" verdicts under realistic (non-adversarial) weight/volatility correlation -- not a
  # rare edge case. resolution_report() now warn()s when weights are non-uniform AND POSITIVELY
  # correlate with the differential's volatility AND it is about to report "resolved". Gated on
  # `resolved` (the R1-only field report()'s RESOLUTION line actually prints), NOT `fragility_read`
  # -- MCS already rejects non-uniform weights, forcing fragility_read to "undetermined" regardless,
  # which would make a fragility_read-gated guard unreachable dead code (the exact bug class already
  # found once in report.R's own Prop 2.2 branch).
  #
  # STRICT-AUDIT FIX (2026-11-18, found by a later round): the guard originally fired on
  # abs(cor) > 0.2, but a NEGATIVE correlation (high weight on the CALM periods) makes
  # .weighted_se() OVERSTATE the true SE -- conservative, not the false-confidence risk this guard
  # exists to catch. Fixed to a one-sided cor > 0.2 test. The same audit also found the shipped
  # "Control 2" below (seed=5, unrelated w/d3) was not a real invariant -- it passed only because
  # that seed happens to land outside the ~22-77%-at-small-T false-fire rate the noisy cor()-on-
  # one-realization proxy has even when the assumption genuinely holds; scanning seeds 1-30 found
  # 15/30 fired under the old abs() rule. Control 2 is now a DETERMINISTIC negative-correlation
  # construction (large weight on the definitely-small-diff periods, small weight on the
  # definitely-large-diff periods) that can never fire under the new one-sided rule, by
  # construction, not by seed luck.
  set.seed(123)
  T <- 20
  w <- c(rep(1, 18), rep(100, 2))
  diff_lowwt <- rnorm(18, mean = 0.0, sd = 0.0005)
  diff_hiwt  <- c(1.3, 1.1)
  diffs <- c(diff_lowwt, diff_hiwt)
  A <- rep(1.00, T); B <- A + diffs
  L <- list(champ = A, rival = B)
  expect_warning(resolution_report(L, w), "non-uniform.*correlate")
  r <- suppressWarnings(resolution_report(L, w))
  expect_true(r$resolved)  # confirms this is exactly the risky "resolved"-on-weighted-panel case

  # Control 1: uniform weights (default) -- no warning.
  L2 <- list(a = rnorm(20, 1, 0.05), b = rnorm(20, 1.3, 0.05))
  expect_no_warning(resolution_report(L2))

  # Control 2: non-uniform weights NEGATIVELY correlated with volatility, by construction, while
  # still exercising a TRUE `resolved` (unlike naively reusing the panel above with inverted
  # weights, which flips observed_edge below the significance boundary and short-circuits the
  # guard's `isTRUE(resolved)` gate before the correlation check is ever reached -- a real mistake
  # caught while writing this test). All periods share a solid positive base so the weighted mean
  # (dominated by the 18 calm, high-weight periods) stays clearly significant, while volatility is
  # concentrated on the 2 LOW-weight periods -- cor(w, resid2) must be negative regardless of seed.
  set.seed(7)
  w5 <- rep(100, 18); w5 <- c(w5, 1, 1)
  base <- 1.0
  diff_calm <- base + rnorm(18, 0, 0.01)
  diff_volatile <- c(base + 4, base - 4)
  diffs5 <- c(diff_calm, diff_volatile)
  A5 <- rep(1.0, T); B5 <- A5 + diffs5
  L5 <- list(a = A5, b = B5)
  r5 <- resolution_report(L5, w5)
  expect_true(r5$resolved)  # confirms this control genuinely exercises the gated branch
  expect_no_warning(resolution_report(L5, w5))
})
