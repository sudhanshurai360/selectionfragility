test_that("loss_panel builds from a named list", {
  L <- list(
    ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
    ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
  )
  p <- loss_panel(L)
  expect_equal(p$models, c("ar1", "nbeats", "ets"))
  expect_equal(length(p$labels), 10)
  expect_equal(p$weights, rep(1, 10))
})

test_that("loss_panel builds from a matrix and a data.frame", {
  m <- matrix(c(1.1, 0.9, 1.3, 0.8, 2.2, 1.8, 2.6, 1.9, 3.1, 2.9, 3.4, 3.0), nrow = 4, ncol = 3)
  colnames(m) <- c("a", "b", "c")
  p <- loss_panel(m)
  expect_equal(sort(p$models), c("a", "b", "c"))

  df <- data.frame(a = c(1.5, 2.3, 0.9, 4.1, 5.7), b = c(1.1, 2.2, 3.3, 4.4, 5.5))
  p2 <- loss_panel(df, labels = c("2020", "2021", "2022", "2023", "2024"))
  expect_equal(p2$labels, c("2020", "2021", "2022", "2023", "2024"))
})

test_that("loss_panel rejects fewer than 2 models, ragged lengths, and NA values", {
  expect_error(loss_panel(list(a = c(1.1, 2.2, 3.3, 0.9, 5.5))), "at least 2 models")
  expect_error(loss_panel(list(a = c(1.1, 2.2, 3.3, 4.1, 5.9, 0.9, 2.1, 3.9, 1.2, 2.8),
                                b = c(1.1, 2.9, 3.3, 4.1, 5.9, 1.0, 2.0, 3.0, 4.0))),
               "same number of periods")
  expect_error(loss_panel(list(a = c(1.1, 2.2, NA, 4.4, 5.5), b = c(1.1, 2.9, 3.3, 4.1, 5.9))),
               "NA/NaN/Inf")
})

test_that("loss_panel rejects an arithmetic-sequence metadata column", {
  expect_error(
    loss_panel(list(horizon = as.numeric(1:10), model_a = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2))),
    "metadata column"
  )
})

test_that("loss_panel rejects duplicate labels and length-mismatched labels", {
  L <- list(a = c(1.1, 2.2, 3.3, 0.9, 5.5), b = c(1.0, 2.0, 3.5, 4.0, 5.0))
  expect_error(loss_panel(L, labels = c("x", "x", "y", "z", "w")), "duplicate")
  expect_error(loss_panel(L, labels = c("x", "y")), "expected 5")
})

test_that("metadata heuristic uses exact equality, not all.equal() tolerance (cross-checked against the Python oracle)", {
  # Regression for a real mismatch found against the Python oracle (selection_fragility.panel's
  # _looks_like_metadata_column uses np.array_equal, i.e. bit-for-bit equality). The old R
  # implementation used isTRUE(all.equal(arr, tiled)), which allows a ~1.5e-8 relative tolerance --
  # a genuine, non-periodic loss column that merely comes CLOSE to tiling a short pattern was wrongly
  # rejected as "metadata" here even though the Python oracle accepts it outright.
  T <- 12
  base <- rep(c(1.0, 2.0, 3.0), 4)[1:T]
  noisy <- base
  noisy[6] <- noisy[6] + 1e-9   # within all.equal()'s default tolerance, but NOT bit-identical

  expect_false(selectionfragility:::.looks_like_metadata_column(noisy, T))
  expect_true(selectionfragility:::.looks_like_metadata_column(base, T))  # exact tiling: still rejected

  other <- c(10.1, 11.3, 9.8, 12.4, 10.9, 11.1, 9.5, 12.0, 10.3, 11.8, 9.9, 12.2)
  p <- loss_panel(list(noisy_model = noisy, other = other))
  expect_equal(sort(p$models), c("noisy_model", "other"))
})

test_that("weights rejects a factor instead of silently using level codes (cross-checked against the Python oracle)", {
  # Regression for a real silent-corruption bug: as.numeric(factor(...)) returns the INTEGER LEVEL
  # CODES, not the labels (e.g. factor(c("10","20","30","40")) -> c(1,2,3,4)), and a factor is
  # neither is.character() nor is.numeric(), so it slipped past every existing guard in
  # .validate_weights() and was silently miscoerced. The Python oracle (passed an equivalent
  # pandas Categorical) correctly recovers the real values (10,20,30,40); the fix makes R reject a
  # factor outright instead of silently returning a wrong answer.
  L <- list(a = c(1.5, 2.3, 0.9, 4.1, 5.7, 2.2, 3.3, 1.1), b = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 1.2, 3.4))
  fw <- factor(c("10", "20", "30", "40", "10", "20", "30", "40"))
  expect_error(loss_panel(L, weights = fw), "not a factor")
})

test_that("weights rejects a logical vector instead of silently coercing TRUE/FALSE -> 1/0 (strict-regression round, 2026-11-18)", {
  # REGRESSION: every model-column constructor in this file (.from_list, .from_data_frame,
  # .from_matrix) already rejects a logical column with this exact rationale -- a flag/indicator
  # column silently coerced to 1/0 and crowned a model with no warning. .validate_weights() carries
  # the identical risk but never got the sibling is.logical() guard: a logical weights vector used
  # to silently become a 0/1 numeric weight vector with zero warning.
  L <- list(a = c(1.1, 2.3, 1.5, 4.2, 5.9), b = c(2.2, 3.1, 1.9, 4.8, 5.1))
  lw <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  expect_error(loss_panel(L, weights = lw), "not logical")
})

test_that("weights reject a magnitude that can overflow sum(weights) to Inf (5-lens round, 2026-11-18)", {
  # REGRESSION: a weight that is individually finite/non-negative (e.g. 1e308) could still make
  # sum(weights) overflow to Inf inside stats::weighted.mean(), producing NaN for every model's
  # mean loss, which pooled_winner() then fed into sort()'s NA-dropping default to silently
  # return NA_character_ as the "champion" -- with no error anywhere. loss_panel() now caps
  # weight magnitude the same way it already caps loss magnitude.
  L <- list(a = c(1, 2, 3), b = c(2, 3, 1))
  expect_error(loss_panel(L, weights = rep(1e308, 3)), "must not exceed")
  expect_no_error(loss_panel(L, weights = rep(1e50, 3)))
})

test_that("labels reject NA/NULL -- a lone NA label used to silently defeat compare()'s reversed-argument guard (5-lens round, 2026-11-18)", {
  # REGRESSION: an NA label passed silently as long as it wasn't duplicated, but NA %in% x is
  # always FALSE, so an NA label is "unique" by construction -- defeating compare()'s
  # reversed-argument guard (which relies on label-set matching) by making a genuinely-missing
  # period look like a genuinely-new one instead. Reachable via load_loss_panel() too, since a
  # JSON "labels" array can contain null.
  L <- list(a = c(1.1, 2.3, 1.5, 4.2, 5.9), b = c(2.2, 3.1, 1.9, 4.8, 5.1))
  expect_error(loss_panel(L, labels = c(NA, "2", "3", "4", "5")), "must not contain NA/NULL")
})

test_that("save_loss_panel / load_loss_panel round-trips exactly", {
  L <- list(a = c(1.1, 2.2, 3.3, 0.9, 5.5), b = c(1.0, 2.0, 3.5, 4.0, 5.0))
  p <- loss_panel(L)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  save_loss_panel(p, tmp)
  p2 <- load_loss_panel(tmp)
  expect_equal(p$losses, p2$losses)
  expect_equal(p$weights, p2$weights)
})

test_that("save_loss_panel() errors loudly if file.rename() fails", {
  L <- list(a = c(1.1, 2.2, 3.3, 0.9, 5.5), b = c(1.0, 2.0, 3.5, 4.0, 5.0))
  p <- loss_panel(L)
  # Destination is itself an existing, non-empty directory: the temp file writes fine (its
  # directory, dest_dir, genuinely exists), but file.rename() cannot replace a non-empty
  # directory with a file and returns FALSE -- exercising the checked-return-value branch, not
  # an earlier (and differently-messaged) write failure.
  bad_path <- tempfile()
  dir.create(bad_path)
  on.exit(unlink(bad_path, recursive = TRUE), add = TRUE)
  file.create(file.path(bad_path, "placeholder"))
  # base::file.rename() itself also emits a warning on this failure (platform-dependent wording);
  # suppress it here since this test is about save_loss_panel()'s own stop(), not that warning.
  expect_error(suppressWarnings(save_loss_panel(p, bad_path)), "failed to rename")
})

test_that("as.data.frame.loss_panel() returns a tidy long data.frame", {
  L <- list(
    ar1 = c(1.00, 0.98, 1.02, 1.01, 0.99),
    ets = c(1.20, 1.18, 1.22, 1.21, 1.19)
  )
  panel <- loss_panel(L, labels = 2020:2024)
  df <- as.data.frame(panel)
  expect_equal(nrow(df), 10)
  expect_equal(sort(unique(df$model)), c("ar1", "ets"))
  expect_equal(df$loss[df$model == "ar1" & df$period == 2020], 1.00)
  expect_equal(df$loss[df$model == "ets" & df$period == 2023], 1.21)
  expect_true(all(df$weight == 1))
})

test_that("summary.loss_panel() returns a sorted-by-mean-loss data.frame", {
  L <- list(
    ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
    ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
  )
  panel <- loss_panel(L)
  s <- summary(panel)
  expect_equal(nrow(s), 3)
  expect_equal(s$model[1], "ar1")
  expect_true(is.numeric(s$mean_loss))
  expect_true(all(diff(s$mean_loss) >= 0))
  expect_true("in_mcs" %in% names(s))
})

test_that("summary.loss_panel()'s ... goes to the MCS check, not to fragility() -- a real argument typo errors loudly", {
  # REGRESSION (6-reviewer strict-verification pass, 2026-11-17): @param ... previously claimed to
  # route to fragility() but actually went to the internal MCS check (.run_mcs_arch); an argument
  # that belongs to fragility() (e.g. full=TRUE) but not to the MCS check silently degraded to
  # in_mcs=NA with no indication the argument did anything different from what the docs implied.
  L <- list(
    ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
    nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
    ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
  )
  panel <- loss_panel(L)
  expect_error(summary(panel, full = TRUE), "unused argument")
  # A genuinely MCS-relevant argument (alpha=) still works and is not mistaken for a typo.
  s <- summary(panel, alpha = 0.5)
  expect_true("in_mcs" %in% names(s))
  # A real MCS-unavailable failure (non-uniform weights) must still degrade to NA, not error.
  w <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  panel_w <- loss_panel(L, weights = w)
  s_w <- summary(panel_w)
  expect_true(all(is.na(s_w$in_mcs)))
})

test_that("loss_panel() from a matrix rejects duplicate column names, matching the list/data.frame constructors", {
  # REGRESSION (found by an adversarial-fuzz review, 2026-11-17): .from_list() and
  # .from_data_frame() both called .check_no_name_collision(), but .from_matrix() did not --
  # a duplicate-named column passed construction silently, and R's `[[` on a duplicated name
  # always returns the FIRST match, so as.data.frame.loss_panel()/summary.loss_panel() (which
  # index losses by name directly, unlike pooled_winner()/decision_breakdown() which re-validate)
  # silently dropped the second column's real data with zero error.
  M <- matrix(c(1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5), ncol = 2)
  colnames(M) <- c("a", "a")
  expect_error(loss_panel(M), "duplicate")
})

test_that("loss_panel() from a LOGICAL matrix is rejected, matching the list/data.frame paths", {
  # REGRESSION (type-coercion fuzz, 2026-11-17): the matrix constructor had no is.logical guard, so
  # a logical/indicator matrix was silently coerced TRUE/FALSE -> 1/0 and crowned as "losses",
  # while the list and data.frame constructors already rejected the identical input.
  M <- matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE), ncol = 2)
  colnames(M) <- c("a", "b")
  expect_error(loss_panel(M), "logical")
})

test_that("loss_panel() rejects an NA or empty-string model/column name consistently across all three construction paths, without crashing (strict-audit round, 2026-11-18)", {
  # A prior round found that an empty-string name is only rejected in .from_list() by a
  # deliberate, explicit check; .from_matrix()/.from_data_frame() happened to reject it too, but
  # only by accident -- R's `[[` returns NULL for a lookup by name "" or NA_character_ even when
  # a same-named element exists (confirmed: `setNames(list(1:2), "")[[""]]` is NULL, not 1:2), so
  # the real rejection came from a later, unrelated, confusingly-worded check ("has 0 periods,
  # expected N" / "is not numeric (class NULL)"), not from any deliberate "empty name" guard.
  # Worse, .from_list()'s own check (`any(nm == "")`) silently assumed no name could be NA: an NA
  # name made `nm == ""` produce NA, any(c(FALSE, NA)) resolve to NA, and `if (is.null(nm) || NA)`
  # crash with a raw, unworded "missing value where TRUE/FALSE needed" -- bypassing this file's
  # entire stop()-with-a-clear-message policy. All three paths now reject both empty-string and
  # NA names explicitly, with the same deliberate wording, and none of them crash.
  a <- c(1.0, 1.1, 0.9, 1.05, 0.95, 1.02)
  b <- c(1.2, 1.15, 1.25, 1.18, 1.22, 1.19)

  # empty-string name
  expect_error(loss_panel(stats::setNames(list(a, b), c("", "b"))), "named|empty|NA")
  M1 <- matrix(c(a, b), ncol = 2)
  colnames(M1) <- c("", "b")
  expect_error(loss_panel(M1), "empty|NA")
  df1 <- data.frame(x = a, y = b)
  colnames(df1) <- c("", "b")
  expect_error(loss_panel(df1), "empty|NA")

  # NA name -- must raise this package's own worded error, NOT a raw base-R
  # "missing value where TRUE/FALSE needed" crash.
  L2 <- stats::setNames(list(a, b), c("x", "b"))
  names(L2)[1] <- NA
  err2 <- tryCatch(loss_panel(L2), error = function(e) e)
  expect_s3_class(err2, "error")
  expect_false(grepl("missing value where", conditionMessage(err2), fixed = TRUE))
  expect_match(conditionMessage(err2), "named|empty|NA")

  M2 <- matrix(c(a, b), ncol = 2)
  colnames(M2) <- c(NA, "b")
  expect_error(loss_panel(M2), "empty|NA")

  df2 <- data.frame(x = a, y = b)
  colnames(df2) <- c(NA, "b")
  expect_error(loss_panel(df2), "empty|NA")

  # whitespace-only names and the literal two-character string "NA" are NOT empty/NA and must
  # still be accepted consistently across all three paths (unchanged by this fix).
  for (nm in c(" ", "\t", "NA")) {
    p1 <- loss_panel(stats::setNames(list(a, b), c(nm, "b")))
    expect_identical(p1$models, c(nm, "b"))
    M3 <- matrix(c(a, b), ncol = 2); colnames(M3) <- c(nm, "b")
    p2 <- loss_panel(M3)
    expect_identical(p2$models, c(nm, "b"))
    df3 <- data.frame(x = a, y = b); colnames(df3) <- c(nm, "b")
    p3 <- loss_panel(df3)
    expect_identical(p3$models, c(nm, "b"))
  }
})

test_that("load_loss_panel() gives a package message for a missing file, not a raw connection error", {
  expect_error(load_loss_panel(file.path(tempdir(), "definitely-not-here-xyz.json")), "does not exist")
})

test_that("as.data.frame/summary read live model names and fail loudly on an inconsistent object", {
  # REGRESSION (S3 object-model review, 2026-11-17): both methods read a cached x$models field and
  # as.data.frame silently recycled a wrong-length weight vector. They now read names(x$losses) and
  # validate lengths, so they fail as loudly as report()/their siblings on a mutated object.
  p <- loss_panel(list(a = c(1.1, 2.2, 3.3, 0.9, 5.5), b = c(1.0, 2.0, 3.5, 4.0, 5.0)))
  p$weights <- c(1, 2)  # deliberately inconsistent
  expect_error(as.data.frame(p), "inconsistent")
})

test_that("save_loss_panel()/load_loss_panel() round-trip bit-exactly under a comma-decimal numeric locale (24-agent round, 2026-11-18)", {
  # REGRESSION: jsonlite::write_json() formats numbers via R's own LC_NUMERIC-sensitive
  # formatting. Under a comma-decimal locale (de_DE, fr_FR, and many others) this silently wrote
  # e.g. "1,23" for 1.23, colliding with JSON's own field separator -- the file was syntactically
  # invalid with NO error at write time, and load_loss_panel() on that same file then failed with
  # a raw jsonlite parse error. save_loss_panel()/load_loss_panel() now force LC_NUMERIC="C" for
  # the duration of the write/read (restored via on.exit either way).
  # NOTE (test-quality fix, 2026-11-18): base R's Sys.setlocale() emits a warning
  # ("may cause R to function strangely") on ANY attempt to set LC_NUMERIC away from "C",
  # REGARDLESS of whether the target locale is actually installed -- so the original
  # tryCatch(..., warning=function(w) NA) skip condition fired every time on every machine, even
  # when de_DE.UTF-8 genuinely was available, and this test never actually ran. Detect
  # availability from `locale -a` instead, and use suppressWarnings() (not tryCatch-on-warning)
  # for the actual Sys.setlocale() calls.
  old <- Sys.getlocale("LC_NUMERIC")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", old)), add = TRUE)
  avail <- "de_DE.UTF-8" %in% suppressWarnings(tryCatch(system("locale -a", intern = TRUE), error = function(e) character(0)))
  skip_if(!avail, "de_DE.UTF-8 locale not available on this machine")
  set_ok <- !is.na(suppressWarnings(Sys.setlocale("LC_NUMERIC", "de_DE.UTF-8")))
  skip_if(!set_ok, "Sys.setlocale(\"LC_NUMERIC\", \"de_DE.UTF-8\") failed on this machine despite locale -a listing it")
  set.seed(42)
  L <- list(a = 1 + stats::rnorm(20)^2, b = 1 + (stats::rnorm(20) + 1)^2)
  p <- loss_panel(L)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(save_loss_panel(p, tmp))
  # NOTE (test-quality fix, 2026-11-18): a raw-text regex for "no comma-decimal digits leaked"
  # is fundamentally ambiguous -- JSON's own array-separator comma between two correctly-formatted
  # numbers (e.g. "1.234,5.678") is indistinguishable, at the text level, from a genuinely
  # comma-corrupted decimal (e.g. "1,234" meaning 1.234). Confirmed empirically: this exact file,
  # written correctly under the LC_NUMERIC="C" fix, still has literal ",1,1," substrings from its
  # all-equal weights array, so any such regex false-positive-fails on correct output. The
  # decisive, unambiguous check is simply that the file parses and round-trips bit-identically --
  # jsonlite's reader either fails outright (confirmed on the pre-fix bug) or parses into the WRONG
  # values if the decimal corruption changes array element counts, either of which the
  # expect_no_error(load_loss_panel(...))/expect_identical() pair below already catches directly.
  p2 <- expect_no_error(load_loss_panel(tmp))
  expect_identical(p$losses, p2$losses)
})

test_that("save_loss_panel()/load_loss_panel() round-trip BIT-IDENTICALLY (not just all.equal-close) on realistic, non-hand-rounded losses", {
  # REGRESSION: digits=NA only gets jsonlite 15 significant digits, not the 17 a double needs for
  # an exact round-trip -- the package's own documented contract is identical(panel$losses,
  # reloaded$losses), which digits=NA silently violated (~1e-15 relative error) on any loss panel
  # computed from real floating-point arithmetic rather than hand-typed 1-2 decimal values. Fixed
  # via digits=17.
  set.seed(7)
  L <- list(a = stats::rnorm(15)^2, b = (stats::rnorm(15) + 1)^2, c = stats::rnorm(15)^2 * pi)
  p <- loss_panel(L)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  save_loss_panel(p, tmp)
  p2 <- load_loss_panel(tmp)
  expect_identical(p$losses, p2$losses)
})

test_that("summary.loss_panel() fails loudly, not with a raw base-R error, on a mutated/inconsistent object (24-agent round, 2026-11-18)", {
  # REGRESSION: as.data.frame.loss_panel() validates length(x$weights)==length(x$labels) and
  # per-model lengths before use and raises a clear package error; summary.loss_panel() claimed
  # (in a code comment) the same consistency but had no such guard, so stats::weighted.mean()
  # threw a raw, unhelpful "'x' and 'w' must have the same length" with no indication the real
  # cause was a malformed/mutated loss_panel. Now mirrors as.data.frame.loss_panel()'s guard.
  L <- list(a = c(1.1, 2.2, 3.3, 0.9, 5.5), b = c(1.0, 2.0, 3.5, 4.0, 5.0))
  p <- loss_panel(L, labels = 2020:2024)
  p$weights <- c(1, 2)  # deliberately inconsistent (same mutation as the as.data.frame test above)
  expect_error(summary(p), "inconsistent")
})

test_that("print.loss_panel() warns instead of silently hiding a report() failure on a mutated/inconsistent object (fresh-eyes round, 2026-11-18)", {
  # REGRESSION: print.loss_panel()'s fallback (used when report(x) itself errors) discarded the
  # error entirely (`error = function(e) NULL`) and printed a plain, normal-looking
  # "<loss_panel> models=..., T=..." line with NO indication anything was wrong. Confirmed
  # directly reachable with no control characters involved: mutating a field after construction
  # (here, shortening one model's loss vector) makes report(p) throw a raw base-R length-mismatch
  # error, which summary()/as.data.frame() already surface loudly (see the test above) but
  # print()/auto-print -- by far the most common way a panel is ever actually viewed -- used to
  # swallow completely. print.loss_panel() must now capture and show that failure.
  L <- list(a = c(1.1, 2.3, 1.9, 4.4, 3.5), b = c(1.1, 2.1, 2.9, 4.2, 5.3))
  p <- loss_panel(L)
  p$losses$a <- c(1.1, 2.3, 1.9)  # deliberately shorten one model's loss vector
  expect_output(print(p), "WARNING: report\\(\\) failed")
  expect_output(print(p), "mutated")
})

test_that("loss_panel()'s duplicate-labels error message lists the duplicates in locale-independent (radix) order", {
  # LOCALE-AUDIT FIX (2026-11-18): the duplicate-labels stop() sorted the duplicate label set with
  # a bare sort() -- locale-sensitive LC_COLLATE collation on character vectors. Confirmed
  # directly: under LC_COLLATE="C" these duplicates sort as c("Beta", "Zeta", "alpha"), but under
  # "en_US.UTF-8" or "tr_TR.UTF-8" they sort as c("alpha", "Beta", "Zeta") -- the same error listed
  # the duplicates in a different order (and, with >5 distinct duplicates, head(dupes, 5) could
  # truncate to a DIFFERENT subset) depending on the session's locale. Doesn't change whether the
  # error fires, but is the same non-reproducibility class the package's radix-sort hardening
  # elsewhere exists to eliminate. Fixed with method="radix"; this test pins the message's exact,
  # locale-independent text under a non-default LC_COLLATE.
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", old)), add = TRUE)
  avail <- "tr_TR.UTF-8" %in% suppressWarnings(tryCatch(system("locale -a", intern = TRUE), error = function(e) character(0)))
  skip_if(!avail, "tr_TR.UTF-8 locale not available on this machine")
  set_ok <- !is.na(suppressWarnings(Sys.setlocale("LC_COLLATE", "tr_TR.UTF-8")))
  skip_if(!set_ok, "Sys.setlocale(\"LC_COLLATE\", \"tr_TR.UTF-8\") failed on this machine despite locale -a listing it")

  L <- list(a = rep(1, 5), b = rep(2, 5))
  labs <- c("Zeta", "alpha", "Beta", "Zeta", "alpha")
  # Only "Zeta" and "alpha" actually repeat (each twice); "Beta" appears once and is not a
  # duplicate. Radix (byte/codepoint) order is always "Zeta, alpha" -- uppercase "Z" (90) sorts
  # before lowercase "a" (97) in codepoint order -- regardless of LC_COLLATE, confirming this
  # pins the fix, not an accidental locale-dependent match.
  expect_error(
    loss_panel(L, labels = labs),
    "duplicate value\\(s\\) Zeta, alpha ",
    perl = TRUE
  )
})
