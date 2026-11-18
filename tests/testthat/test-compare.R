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

test_that("compare()/.churn_base_rate() reject a bad seed with a worded error, not a raw base-R error that aborts the whole call (5-lens round, 2026-11-18)", {
  # REGRESSION: seed was the one parameter in .churn_base_rate() passed straight into set.seed()
  # with no local guard, unlike n_perm (same fix commit, see above) -- a bad seed reached a raw,
  # unworded "supplied seed is not a valid integer" error from base R's set.seed() itself, and
  # since this call site (unlike the MCS call just above it in compare()) isn't wrapped in
  # tryCatch, it aborted the whole compare() call rather than degrading gracefully.
  pc <- make_prev_curr()
  expect_error(compare(pc$prev, pc$curr, seed = NA), "single non-negative integer")
  expect_error(compare(pc$prev, pc$curr, seed = c(1, 2)), "single non-negative integer")
  expect_error(compare(pc$prev, pc$curr, seed = -1), "single non-negative integer")
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

test_that("print.change_report() escapes a champion model name containing an embedded newline (output-injection regression, sibling to LEADERBOARD's fix)", {
  # REGRESSION: print.change_report() interpolates previous_champion/current_champion into its
  # one-line sprintf() summary with a bare %s and NO escaping at all -- unlike LEADERBOARD's
  # per-model rows (report.R), which at least had a collision-triggered escaping path before its
  # own control-character fix, this print method had no escaping path whatsoever. Confirmed
  # directly: a model named "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE_INJECTED_LINE\n
  # act=TRUE", used as the previous_champion, printed as three separate lines, with the middle
  # line rendering byte-for-byte like a genuine VERDICT section header and a genuine "act=TRUE".
  # Fixed with the same encodeString(..., quote = "'") escaping already established for
  # LEADERBOARD; this test confirms the fabricated line can no longer appear as a bare,
  # line-initial fake section header once routed through that fix.
  evil <- "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE_INJECTED_LINE\nact=TRUE"
  years <- 2015:2024
  L_prev <- list(x = rep(2.0, 10), tmp = rep(0.5, 10))
  names(L_prev)[2] <- evil
  L_curr <- list(x = rep(0.5, 10), tmp = rep(2.0, 10))
  names(L_curr)[2] <- evil
  prev <- loss_panel(L_prev, labels = years)
  curr <- loss_panel(L_curr, labels = years)
  cr <- compare(prev, curr, n_perm = 50)
  out <- capture.output(print(cr))
  # The whole summary must still be exactly one line -- an unescaped embedded newline would have
  # split it into three.
  expect_length(out, 1)
  # No fabricated line-initial "VERDICT:" header may appear.
  expect_false(any(grepl("^VERDICT:", out)))
  # The escaped name (backslash-n, not a real newline) must appear in the single-line output.
  expect_match(out, "evil\\\\nVERDICT", perl = TRUE)
})

test_that("compare()'s mismatched-model-set error message lists model names in locale-independent (radix) order", {
  # LOCALE-AUDIT FIX (2026-11-18): the mismatched-model-set stop() sorted previous$models/
  # current$models with a bare sort() -- locale-sensitive LC_COLLATE collation on character
  # vectors. Confirmed directly: under LC_COLLATE="C" these three names sort as
  # c("Beta_model", "Zeta_model", "alpha_model"), but under LC_COLLATE="en_US.UTF-8" or
  # "tr_TR.UTF-8" they sort as c("alpha_model", "Beta_model", "Zeta_model") -- the same error
  # printed a different model listing depending on the session's locale. Doesn't change whether
  # the error fires, but is the same non-reproducibility class the package's radix-sort hardening
  # elsewhere exists to eliminate. Fixed with method="radix"; this test pins the message's exact,
  # locale-independent text under a non-default LC_COLLATE.
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", old)), add = TRUE)
  avail <- "tr_TR.UTF-8" %in% suppressWarnings(tryCatch(system("locale -a", intern = TRUE), error = function(e) character(0)))
  skip_if(!avail, "tr_TR.UTF-8 locale not available on this machine")
  set_ok <- !is.na(suppressWarnings(Sys.setlocale("LC_COLLATE", "tr_TR.UTF-8")))
  skip_if(!set_ok, "Sys.setlocale(\"LC_COLLATE\", \"tr_TR.UTF-8\") failed on this machine despite locale -a listing it")

  L1 <- list(Zeta_model = c(1, 1, 1), alpha_model = c(2, 2, 2), Beta_model = c(3, 3, 3))
  L2 <- list(Zeta_model = c(1, 1, 1), Beta_model = c(2, 2, 2))
  p1 <- loss_panel(L1)
  p2 <- loss_panel(L2)
  # Radix (byte/codepoint) order is always "Beta_model, Zeta_model, alpha_model" regardless of
  # LC_COLLATE -- confirming this pins the fix, not an accidental locale-dependent match.
  expect_error(
    compare(p1, p2),
    "Beta_model, Zeta_model, alpha_model vs Beta_model, Zeta_model",
    fixed = TRUE
  )
})

test_that("compare() detects a mismatched model set even when the cached $models field is stale (cross-function consistency audit, 2026-11-18)", {
  # REGRESSION: the mismatched-model-set guard compared the CACHED previous$models/current$models
  # fields -- the same "trusts a pre-validated object even if it's since been mutated" blind spot
  # already found and fixed in print.loss_panel()/as.data.frame.loss_panel()/summary.loss_panel()
  # (panel.R), which all moved to live names(x$losses) for exactly this reason. Confirmed directly
  # reachable: dropping a model from previous$losses by hand (without touching the cached
  # previous$models) left the guard comparing two IDENTICAL, now-stale $models vectors, so it
  # silently passed and compare() went on to compare a 2-model previous panel against a 3-model
  # current panel with no error at all.
  L <- list(
    ar1    = c(1.0, 1.0, 1.0, 1.0, 1.0),
    nbeats = c(0.5, 0.5, 0.5, 0.5, 5.0),
    ets    = c(2.0, 2.0, 2.0, 2.0, 2.0)
  )
  prev <- loss_panel(L)
  curr <- loss_panel(L)
  prev$losses <- prev$losses[c("ar1", "nbeats")]  # drop "ets" without touching the cached $models
  expect_identical(prev$models, c("ar1", "nbeats", "ets"))  # the cache is indeed stale
  expect_error(compare(prev, curr), "different model sets")
})
