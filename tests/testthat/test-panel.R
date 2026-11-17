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
