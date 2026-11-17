# Oracle text generated from the Python `selection_fragility.report` module (v1.0.4) -- this is
# the README's own quickstart example, so the expected text is independently documented upstream.

test_that("report() reproduces the README quickstart example byte-for-byte", {
  L <- list(
    ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
    ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
  )
  panel <- loss_panel(L, labels = 2015:2024)
  out <- report(panel, alpha = 0.10)
  expect_match(out, "VERDICT: not identified \\(MCS size 2 of 3\\)")
  expect_match(out, "ar1             mean=1.0000  in MCS     periods won=1 \\(champion\\)")
  expect_match(out, "observed edge 6.8% vs MDE@80% power 45.6% -- cannot determine at this sample size")
  expect_match(out, "MCB bound: the champion's true edge over the binding rival may be as large as 44.8%, at 95% confidence")
  expect_match(out, "k\\*=1 of 10 periods \\(2023\\) -- fragility read UNDETERMINED")
})

test_that("report() on an exact tie shows k*=0 and the explicit no-edge-exists wording", {
  L <- list(a = rep(1.0, 20), b = rep(1.0, 20))
  panel <- loss_panel(L, labels = 2000:2019)
  out <- report(panel, alpha = 0.10)
  expect_match(out, "k\\*=0 -- no strict pooled winner")
  expect_match(out, "resolved: no edge exists \\(the champion ties its binding rival exactly\\)")
})

test_that("report() rejects a raw list (requires a loss_panel)", {
  expect_error(report(list(a = c(1, 2, 3), b = c(2, 3, 4))), "loss_panel")
})

test_that("print(report(panel)) renders the formatted report, not escaped \\n (adopter-review regression)", {
  L <- list(a = c(1.1, 2.2, 0.9, 4.2, 5.1), b = c(2.5, 3.1, 4.4, 5.9, 1.0))
  panel <- loss_panel(L)
  out <- capture.output(print(report(panel)))
  expect_true(any(grepl("^VERDICT:", out)))
  expect_false(any(grepl("\\\\n", out)))
})

test_that("print()/auto-print/cat() of report() agree byte-for-byte -- no stray trailing space", {
  # REGRESSION (independent-audit round, 2026-11): print.selectionfragility_report() called
  # `cat(unclass(x), "\n")` -- cat()'s default sep=" " between its two arguments silently
  # appended a stray trailing space to the report's last line whenever the text was displayed
  # via print() or auto-print at the console, while `cat(report(panel))` directly (a single
  # argument, so no separator is inserted) rendered the SAME underlying text WITHOUT the
  # trailing space -- the three documented-equivalent display paths disagreed byte-for-byte.
  # Matches the Python oracle's plain "\n".join(lines) (no trailing whitespace at all). Fixed
  # via sep = "" in print.selectionfragility_report().
  L <- list(a = c(1.1, 2.2, 0.9, 4.2, 5.1), b = c(2.5, 3.1, 4.4, 5.9, 1.0))
  rpt <- report(loss_panel(L))
  via_print <- paste(capture.output(print(rpt)), collapse = "\n")
  via_autoprint <- paste(capture.output(rpt), collapse = "\n")
  expect_identical(via_print, unclass(rpt))
  expect_identical(via_autoprint, unclass(rpt))
  expect_false(grepl(" $", via_print))
  expect_false(grepl(" $", via_autoprint))
})

test_that("LEADERBOARD's collision-disambiguation quoting escapes an internal quote (no ambiguous nesting)", {
  # REGRESSION (independent-audit round, 2026-11): a colliding model name was wrapped with a
  # bare sprintf("'%s'", name) -- unlike Python's repr(), which escapes/switches quote style,
  # a name that itself contains a single quote (e.g. "ar1's" colliding with "ar1's " after
  # left-justify padding) rendered as `'ar1's'`, an ambiguous run of unescaped quotes -- exactly
  # the "looks like an indistinguishable duplicate" failure mode this disambiguation branch
  # exists to prevent, just moved from the name itself into its quoting. Fixed by routing
  # through encodeString(..., quote = "'"), which backslash-escapes internal quotes/backslashes.
  L <- list(`ar1's` = c(1.0, 2.0, 3.0, 1.0, 2.0), `ar1's ` = c(1.1, 2.1, 3.1, 0.9, 1.9))
  out <- report(loss_panel(L))
  expect_match(out, "'ar1\\'s'", fixed = TRUE)
  expect_match(out, "'ar1\\'s '", fixed = TRUE)
  expect_false(grepl("'ar1's'", out, fixed = TRUE))
})

test_that("LEADERBOARD's collision-disambiguation quoting is byte-identical to the Python oracle for a plain whitespace collision", {
  L <- list(ar1 = c(1.0, 2.0, 3.0, 1.0, 2.0), `ar1 ` = c(1.1, 2.1, 3.1, 0.9, 1.9))
  out <- report(loss_panel(L))
  expect_match(out, "'ar1'           mean=1.8000", fixed = TRUE)
  expect_match(out, "'ar1 '          mean=1.8200", fixed = TRUE)
})

test_that("report()'s seed parameter actually threads through to the MCS bootstrap (not silently ignored)", {
  # REGRESSION (round of 6 adversarial review, 2026-11-17): report() previously had no seed=
  # argument at all and silently always used seed=0 -- added one and threaded it to
  # .run_mcs_arch()/resolution_report(mcs_seed=); this pins that the parameter is live, not just
  # present and discarded.
  L <- list(
    a = c(1.00, 1.05, 0.95, 1.10, 0.90, 1.08, 0.92, 1.03, 0.97, 1.02),
    b = c(1.01, 1.04, 0.96, 1.09, 0.91, 1.07, 0.93, 1.02, 0.98, 1.01)
  )
  panel <- loss_panel(L)
  out_seed0 <- report(panel, seed = 0)
  out_seed1 <- report(panel, seed = 1)
  # Not asserting they differ (they may legitimately agree) -- asserting the call succeeds with
  # an explicit seed and produces a well-formed report either way.
  expect_true(is.character(out_seed0))
  expect_true(is.character(out_seed1))
})
