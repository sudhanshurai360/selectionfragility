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

test_that("LEADERBOARD escapes a model name containing an embedded newline (output-injection regression, exhaustive state-space audit 2026-11-18)", {
  # REGRESSION: a model name containing a literal "\n" was embedded into LEADERBOARD's one-
  # row-per-model text with NO escaping at all, because the escaping path only triggered on a
  # padding-equality COLLISION with another model name -- a lone model with a newline in its
  # name never collides with anything, so it sailed straight through unescaped. Confirmed
  # directly: a model named "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE\nb" printed as
  # THREE separate lines in the report, with the middle line rendering byte-for-byte like a
  # genuine VERDICT section header -- a one-screen reader has no way to tell a fabricated
  # section line from this function's own real prose. Fixed by also escaping any model name
  # containing a C0/DEL control character, independent of whether it collides with another name.
  set.seed(1)
  L <- list(a = 1 + rnorm(10, 0, 0.1), b = 1.2 + rnorm(10, 0, 0.1))
  names(L)[2] <- "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE INJECTED LINE\nb"
  panel <- loss_panel(L)
  out <- report(panel)
  # The literal injected text must not appear as a bare, line-initial "VERDICT:" -- i.e. it must
  # not have become a second, fake section header distinguishable from the real one.
  verdict_lines <- grep("^VERDICT:", strsplit(out, "\n")[[1]])
  expect_length(verdict_lines, 1)
  # The escaped name (backslash-n, not a real newline) must appear somewhere in the output.
  expect_match(out, "evil\\nVERDICT", fixed = TRUE)
  # A genuine embedded newline must not survive into the LEADERBOARD row for this model -- the
  # row for model "a" and the row for the pathological model must together be exactly 2 lines,
  # not 4 (1 "a" line + 3 lines from the unescaped, newline-splitting name).
  leaderboard_start <- grep("^LEADERBOARD$", strsplit(out, "\n")[[1]])
  resolution_start <- grep("^RESOLUTION$", strsplit(out, "\n")[[1]])
  lb_body <- strsplit(out, "\n")[[1]][(leaderboard_start + 1):(resolution_start - 2)]
  expect_length(lb_body, 2)
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
  # .run_mcs_arch()/resolution_report(mcs_seed=). A first version of this test only checked that
  # report(seed=0)/report(seed=1) both ran without error -- which would pass identically even if
  # `seed` were accepted and silently discarded, so it pinned nothing. This version traces
  # set.seed() itself (the actual mechanism by which `seed` reaches the RNG) to confirm the value
  # the caller passed is genuinely the value reaching the bootstrap, not just a parameter that
  # exists on paper.
  L <- list(
    a = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    b = c(1.01, 0.99, 1.03, 1.02, 1.00, 1.04, 0.98, 1.01, 1.03, 0.99)
  )
  panel <- loss_panel(L)
  # `<<-` inside a trace() tracer searches enclosing environments from the TRACED FUNCTION's own
  # call frame, not from this test_that() block's local frame -- a `seen <<- ...` tracer silently
  # records nothing when run inside a wrapped function (verified directly: 0 calls observed).
  # Assigning explicitly into globalenv() by name sidesteps that scoping trap.
  assign(".seen_seeds_test_report_seed", c(), envir = globalenv())
  trace("set.seed", tracer = quote(assign(
    ".seen_seeds_test_report_seed",
    c(get(".seen_seeds_test_report_seed", envir = globalenv()), seed),
    envir = globalenv()
  )), print = FALSE, where = globalenv())
  on.exit({
    suppressMessages(try(untrace("set.seed"), silent = TRUE))
    if (exists(".seen_seeds_test_report_seed", envir = globalenv())) {
      rm(".seen_seeds_test_report_seed", envir = globalenv())
    }
  }, add = TRUE)
  report(panel, seed = 77)
  seen <- get(".seen_seeds_test_report_seed", envir = globalenv())
  # STRICT-AUDIT FIX (2026-11-18): a prior version of this test asserted exactly 2 of 3 set.seed()
  # calls equal the user's seed, with a THIRD call at seed=0 from PIVOT's pivot_agreement(L, w,
  # seed=0) treated as "intentionally independent" -- that was itself the bug: pivot_agreement()
  # is just as RNG-dependent (a 200-rep bootstrap) as the two MCS calls, so hardcoding its seed
  # silently defeated the seed-sensitivity check seed= exists for. pivot_agreement() is now
  # threaded report()'s own seed, so ALL THREE internal set.seed() calls (VERDICT's
  # .run_mcs_arch(), RESOLUTION's resolution_report(mcs_seed=) -> identified(), and PIVOT's
  # pivot_agreement()) must equal the user's seed with nothing left over.
  expect_equal(seen, rep(77, 3))
})

test_that("report() explains a not-identified + resolved panel instead of printing a bare, unqualified contradiction (5-lens round, 2026-11-18)", {
  # REGRESSION: VERDICT only had a caveat for the OPPOSITE direction (identified but unresolved,
  # "identification alone does not mean the edge is powered"). "Not identified" + "resolved" on
  # the same panel is not a contradiction -- VERDICT (the MCS bootstrap, R2) and RESOLUTION (a
  # power check against the binding rival alone, R1) answer different questions -- but a
  # one-screen reader seeing only the top two sections had nothing telling them that.
  set.seed(99)
  Tn <- 12
  make <- function(base, sd) pmax(base + rnorm(Tn, 0, sd), 0.01)
  L <- list(arima = make(1.0, 0.12), ets = make(1.08, 0.12), naive = make(1.3, 0.1), lgbm = make(0.98, 0.12))
  panel <- loss_panel(L, labels = paste0("Q", 1:Tn))
  out <- report(panel)
  expect_match(out, "not identified")
  expect_match(out, "resolved")
  expect_match(out, "VERDICT and\\s*\\n\\s*RESOLUTION answer different questions and can legitimately disagree")
})

test_that("report() explains a resolved + Prop-2.2-certifies-non-significance panel instead of printing a bare, unqualified contradiction (5-lens round, 2026-11-18)", {
  # REGRESSION: RESOLUTION's "resolved" and PIVOT's "certifies non-significance" can co-occur on
  # the identical rival pair (different variance conventions/critical values -- see R/report.R),
  # but PIVOT printed nothing connecting the two, reading as a flat self-contradiction.
  set.seed(51)
  Tn <- 40
  v <- 0.075
  champ <- rnorm(Tn, 0, 0.01)
  rival <- champ
  idx <- sample(Tn, 3)
  rival[idx] <- rival[idx] + v
  panel <- loss_panel(list(champ = 1 + champ, rival = 1 + rival))
  out <- report(panel)
  expect_match(out, "certifies non-significance")
  expect_match(out, "not a contradiction")
})
