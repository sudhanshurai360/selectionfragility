# Oracle values generated from the Python `selection_fragility.compare` module (v1.0.4) on the
# same panels. old_champion_still_in_mcs/act depend on the MCS engine (arch vs. MCS::MCSprocedure
# here) and are checked for sanity rather than exact cross-language equality.

make_prev_curr <- function() {
  prev <- loss_panel(list(
    a = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    b = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08)
  ), labels = as.character(2020:2029))
  curr <- loss_panel(list(
    a = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 5.0),
    b = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08, 0.5)
  ), labels = as.character(2020:2030))
  list(prev = prev, curr = curr)
}

test_that("compare() matches the Python oracle on deterministic fields", {
  pc <- make_prev_curr()
  r <- compare(pc$prev, pc$curr, alpha = 0.10, n_perm = 200, seed = 0)
  expect_equal(r$previous_champion, "a")
  expect_equal(r$current_champion, "b")
  expect_true(r$champion_changed)
  expect_equal(r$champion_without_new_periods, "a")
  expect_equal(r$n_new_periods, 1)
  expect_equal(r$k_star, 1L)
  expect_true(r$churn_base_rate >= 0 && r$churn_base_rate <= 1)
})

test_that("compare()/.churn_base_rate() reject a bad n_perm, and a fractional one no longer returns a silently wrong statistic (strict-regression round, 2026-11-18)", {
  # REGRESSION: n_perm never got the overflow-safe coerce-and-check treatment its siblings
  # n_boot/n_rep/B all received elsewhere in the package -- NA/Inf/non-numeric reached a raw,
  # unworded crash. Worse: a FRACTIONAL n_perm (e.g. 7.9) used to run seq_len(n_perm) (floor(n_perm)
  # = 7 real iterations) but divide by the ORIGINAL untruncated 7.9, mismatching numerator and
  # denominator and silently returning a wrong statistic. The fix coerces n_perm to an integer
  # ONCE and reuses that same coerced value for both the loop and the final division (matching
  # the established truncate-consistently convention n_boot/B/block already use elsewhere in the
  # package) -- a fractional n_perm is accepted, not rejected, but numerator and denominator can
  # no longer disagree: confirm n_perm=7.9 and n_perm=7 now give IDENTICAL results at a fixed seed.
  pc <- make_prev_curr()
  expect_error(compare(pc$prev, pc$curr, n_perm = NA, seed = 2), "single positive integer")
  expect_error(compare(pc$prev, pc$curr, n_perm = "x", seed = 2), "single positive integer")
  expect_error(compare(pc$prev, pc$curr, n_perm = Inf, seed = 2), "single positive integer")
  expect_error(compare(pc$prev, pc$curr, n_perm = 0, seed = 2), "single positive integer")
  r_frac <- compare(pc$prev, pc$curr, n_perm = 7.9, seed = 2)
  r_int <- compare(pc$prev, pc$curr, n_perm = 7, seed = 2)
  expect_equal(r_frac$churn_base_rate, r_int$churn_base_rate)
})

test_that("compare() rejects non-loss_panel input and mismatched model sets", {
  pc <- make_prev_curr()
  expect_error(compare(list(a = 1), pc$curr), "loss_panel objects")
  prev2 <- loss_panel(list(a = c(1.1, 2.3, 0.9, 4.2), c = c(2.5, 3.1, 4.4, 5.9)))
  expect_error(compare(prev2, pc$curr), "different model sets")
})

test_that("print.change_report() runs without error and mentions the champion change", {
  pc <- make_prev_curr()
  r <- compare(pc$prev, pc$curr, alpha = 0.10, n_perm = 100, seed = 0)
  out <- paste(capture.output(print(r)), collapse = " ")
  expect_match(out, "champion changed a -> b")
})

test_that("compare() rejects reversed arguments (current missing periods previous had)", {
  # REGRESSION (found in round-4 user-adoption review): calling compare(current, previous) by
  # mistake previously ran silently and produced a plausible-looking but wrong report, because
  # n_new was clamped to 0 instead of flagging that `current` is missing periods `previous` had.
  pc <- make_prev_curr()
  err <- tryCatch(compare(pc$curr, pc$prev), error = function(e) conditionMessage(e))
  expect_match(err, "missing period label|wrong order")
})

test_that("compare() rejects reversed arguments with positional labels too", {
  prev <- loss_panel(list(a = c(1, 1, 1, 1, 1), b = c(2, 2, 2, 2, 2)))
  curr <- loss_panel(list(a = c(1, 1, 1, 1, 1, 1), b = c(2, 2, 2, 2, 2, 2)))
  expect_error(compare(curr, prev), "fewer periods|wrong order")
})

test_that("compare() supports rolling-window and disjoint label sets (not just plain append)", {
  # REGRESSION (cross-language parity + compare() deep-dive, 2026-11-17): the R-only reversed-order
  # guard fired on ANY dropped old label, wrongly rejecting a rolling window (drop oldest, append
  # newest at the same T) and a fully disjoint current -- both of which the Python original supports
  # and documents. The guard is now narrowed to the unambiguous reversed case (current adds no new
  # period yet is missing old ones). Rolling window and disjoint must NOT error; a strict earlier
  # subset must STILL error.
  prev <- loss_panel(list(a = rep(1, 10), b = c(1.2,1.1,1.0,1.3,0.9,1.1,1.2,1.0,1.1,1.0)), labels = 2015:2024)
  roll <- loss_panel(list(a = c(rep(1,9), 0.5), b = c(1.1,1.0,1.3,0.9,1.1,1.2,1.0,1.1,1.0,1.4)), labels = 2016:2025)
  expect_no_error(compare(prev, roll))
  disj <- loss_panel(list(a = rep(1, 10), b = c(1.2,1.1,1.0,1.3,0.9,1.1,1.2,1.0,1.1,1.0)), labels = 2100:2109)
  cr <- compare(prev, disj)
  expect_s3_class(cr, "change_report")
  # Strict earlier subset (reversed order) must still be caught.
  later <- loss_panel(list(a = rep(1, 11), b = c(1.2,1.1,1.0,1.3,0.9,1.1,1.2,1.0,1.1,1.0,0.8)), labels = 2015:2025)
  earlier <- loss_panel(list(a = rep(1, 10), b = c(1.2,1.1,1.0,1.3,0.9,1.1,1.2,1.0,1.1,1.0)), labels = 2015:2024)
  expect_error(compare(later, earlier), "subset|wrong order")
})
