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
