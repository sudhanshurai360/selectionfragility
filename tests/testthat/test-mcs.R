# Deterministic-path oracle values generated from the Python `selection_fragility.mcs` package
# (v1.0.4) on the same inputs -- these paths (certain rejection / exact tie) do not depend on the
# bootstrap draw, so they match exactly even though R and Python use different RNG streams.

test_that("mcs() gives a singleton MCS when one model dominates every period", {
  L1 <- matrix(rep(c(1.0, 2.0, 3.0, 1.1, 2.1, 3.1, 0.9, 1.9, 2.9, 1.0, 2.0, 3.0), 3), ncol = 3, byrow = TRUE)
  res <- mcs(L1, alpha = 0.10, B = 500, block = 2, seed = 0)
  expect_equal(res$surviving, 1L)
  expect_equal(res$p, 0.0)
})

test_that("mcs() keeps an exact tie non-identified", {
  col <- rep(c(1.0, 2.0, 0.5, 3.0, 1.5, 2.5), 2)
  L2 <- cbind(col, col, rep(c(5.0, 6.0, 5.5, 6.5, 5.2, 6.2), 2))
  res <- mcs(L2, alpha = 0.10, B = 500, block = 2, seed = 0)
  expect_equal(sort(res$surviving), c(1L, 2L))
  expect_equal(res$p, 1.0)
})

test_that("mcs() validates alpha, B ceiling, loss magnitude, and finiteness", {
  L <- matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 2)
  expect_error(mcs(L, alpha = 10), "alpha must be strictly between 0 and 1")
  expect_error(mcs(L, B = 200000), "exceeds the 100000")
  expect_error(mcs(matrix(c(1, NA, 3, 4), 2, 2)), "non-finite")
  expect_error(mcs(matrix(c(1, 2, 3, 4, 1e101, 2, 3, 4), ncol = 2)), "exceeds")
  expect_error(mcs(L, elimination = "bogus"), "studentized.*raw")
})

test_that("mcs() rejects a non-scalar elimination with a worded message, not a raw base-R error (strict-regression round, 2026-11-18)", {
  # REGRESSION: elimination sat in the same validation block as alpha/B/block (all hardened for
  # length/type in 5897e0c) but never got the same length-1 guard -- elimination=character(0) or
  # a length-2 vector reached the bare `!elimination %in% c(...)` check directly, producing a raw
  # "argument is of length zero" / "the condition has length > 1" error instead of this package's
  # own worded message, for both mcs() and model_confidence_set() (which forwards unchecked).
  L <- matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 2)
  expect_error(mcs(L, elimination = character(0)), "single string")
  expect_error(mcs(L, elimination = c("studentized", "raw")), "single string")
  expect_error(mcs(L, elimination = NA), "single string")
})

test_that("mcs() rejects a bad seed with a worded error, not a raw base-R error (5-lens round, 2026-11-18)", {
  # REGRESSION: seed was the one parameter here passed straight into set.seed() with no local
  # guard, unlike B/block (same fix commit) -- a bad seed reached a raw, unworded "supplied seed
  # is not a valid integer" error from base R's set.seed() itself.
  L <- matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 2)
  expect_error(mcs(L, seed = NA), "single non-negative integer")
  expect_error(mcs(L, seed = c(1, 2)), "single non-negative integer")
  expect_error(mcs(L, seed = -1), "single non-negative integer")
})

test_that("mcs() rejects a data.frame with a non-numeric column instead of a raw crash or silent list-coercion (5-lens round, 2026-11-18)", {
  # REGRESSION: mcs()'s documented contract is "a numeric matrix," but nothing stopped a caller
  # from passing a data.frame straight through -- as.matrix()/storage.mode<- ran unconditionally
  # with no check that the original columns were numeric, unlike loss_panel()'s own data.frame
  # constructor which explicitly rejects this. A length-1-per-row list-column was silently
  # accepted as ordinary numeric data; a length>1-per-row list-column crashed with a raw
  # "'list' object cannot be coerced to type 'double'" error instead of a worded one.
  df1 <- data.frame(a = 1:8, b = c(1, 2, 1, 2, 1, 2, 1, 2) + 0.1)
  df1$c <- I(list(10, 20, 30, 40, 50, 60, 70, 80))
  expect_error(mcs(df1, B = 20, seed = 1), "not numeric")
  df2 <- data.frame(a = 1:8, b = c(1, 2, 1, 2, 1, 2, 1, 2) + 0.1)
  df2$c <- I(list(c(1, 2), c(3, 4), c(5, 6), c(7, 8), c(9, 10), c(11, 12), c(13, 14), c(15, 16)))
  expect_error(mcs(df2, B = 20, seed = 1), "not numeric")
  expect_no_error(mcs(data.frame(a = c(1, 2, 1, 2, 1, 2, 1, 2), b = c(1, 2, 1, 2, 1, 2, 1, 2) + 0.1), B = 20, seed = 1))
})

test_that("mcs() rejects a plain list/loss_panel with a worded message instead of a raw base-R error (fresh-eyes round, 2026-11-18)", {
  # REGRESSION: the data.frame guard just above (same commit family as this test) closed the
  # data.frame case but missed the sibling case -- the single most natural misuse of this exact
  # function: calling mcs(L, ...) on the named {model: vector} list (or a loss_panel object,
  # itself a list) that model_confidence_set()/mcs_size()/identified() take, instead of routing
  # it through model_confidence_set() first as documented. Both used to reach a raw, unworded
  # "'list' object cannot be coerced to type 'double'" from storage.mode(L) <- "double".
  L <- list(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  expect_error(mcs(L, B = 20, seed = 1), "model_confidence_set")
  panel <- loss_panel(L)
  expect_error(mcs(panel, B = 20, seed = 1), "model_confidence_set")
})

test_that("model_confidence_set() wraps mcs() and returns model names", {
  L1 <- matrix(rep(c(1.0, 2.0, 3.0, 1.1, 2.1, 3.1, 0.9, 1.9, 2.9, 1.0, 2.0, 3.0), 3), ncol = 3, byrow = TRUE)
  Ld <- list(a = L1[, 1], b = L1[, 2], c = L1[, 3])
  res <- model_confidence_set(Ld, alpha = 0.10, B = 500, block = 2, seed = 0)
  expect_equal(res$surviving, "a")
  expect_error(model_confidence_set(list(a = 1:3)), "at least 2|>=2")
  expect_error(model_confidence_set(list(a = 1:3, b = 1:4)), "same number of periods")
})

test_that("model_confidence_set() rejects duplicate model names", {
  L <- list(a = 1:4, a = 5:8, b = 2:5)
  expect_error(model_confidence_set(L), "unique.*duplicate|duplicate.*unique")
})

test_that("mcs() coerces/validates B and block instead of crashing its own %d error strings", {
  # REGRESSION (type-coercion review, 2026-11-17): a fractional/character B or block crashed the
  # very sprintf("%d") error/warning strings meant to describe it ("invalid format '%d'"), and a
  # non-integer block injected NA into the resample index. B/block are now coerced up front.
  L <- matrix(c(1.0, 1.1, 0.9, 1.0, 1.2, 1.2, 1.1, 1.3, 1.1, 1.4), ncol = 2)
  colnames(L) <- c("a", "b")
  expect_error(mcs(L, B = "x"), "single positive integer")
  expect_error(mcs(L, B = NA), "single positive integer")
  expect_no_error(mcs(L, B = 200.0, block = 3.0, seed = 1))  # whole-number doubles are fine
})

test_that("mcs()'s large-T*B warning message doesn't itself overflow to 'NA' (24-agent round, 2026-11-18)", {
  # REGRESSION: the guard CONDITION was fixed to as.double(T)*B (avoiding an int32 if()-crash at
  # large T), but the warning MESSAGE TEXT two lines later still computed plain-integer T*B,
  # silently overflowing back to NA and printing "= NA" in the user-facing diagnostic (plus R's
  # own spurious "NAs produced by integer overflow" warning) for any T*B combination above both
  # the 2e7 warning threshold and int32 max.
  #
  # TEST-QUALITY FIX (test-suite self-audit, 2026-11-18): the original version of this test never
  # called mcs() at all -- it independently re-derived the same sprintf() expression inline and
  # checked its own re-derivation, so it would still pass even with the pre-fix bug (reverting
  # R/mcs.R's fix and rerunning confirmed this: the old test provided zero real coverage). This
  # version calls the REAL mcs() at the real int32-overflow magnitude (T*B=2.15e9, matching the
  # original bug report) and captures the warning via tryCatch(warning=...), which unwinds the
  # call stack immediately once the warning fires -- so the (slow, multi-second) bootstrap body
  # never actually executes, keeping this test fast while still exercising the genuine code path.
  set.seed(1)
  T <- 21500L
  L <- cbind(a = rnorm(T), b = rnorm(T) + 0.01)
  msg <- tryCatch(mcs(L, B = 100000, block = 3, seed = 1), warning = function(w) conditionMessage(w))
  expect_false(grepl("= NA", msg, fixed = TRUE))
  expect_true(grepl("2.15e\\+09|2,150,000,000", msg))  # format() may use scientific notation at this magnitude; either is fine, "NA" is not
})

test_that("model_confidence_set() propagates elimination=, mirroring mcs() (strict-regression round, 2026-11-18)", {
  # REGRESSION: mcs()'s return list was stamped with elimination= so two "identical" calls across
  # sessions aren't silently incomparable under a quietly-set global
  # options(selectionfragility.elimination=...). model_confidence_set() -- the convenience wrapper
  # a typical user is MORE likely to call directly, since it takes named lists/loss_panels instead
  # of raw matrices -- reintroduced the exact same gap one layer up by dropping elimination= on
  # the floor. Found independently by a dedicated audit of every mcs()-return consumer.
  L <- list(ar1 = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
            ets = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
            nbeats = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18))
  res <- model_confidence_set(L, B = 200, seed = 1)
  expect_equal(res$elimination, "studentized")
  res2 <- model_confidence_set(L, B = 200, seed = 1, elimination = "raw")
  expect_equal(res2$elimination, "raw")
})

test_that("model_confidence_set() refuses a non-uniformly weighted loss_panel instead of dropping the weights", {
  # REGRESSION: .unwrap_panel()'s weights were discarded, so a weighted panel silently got an
  # unweighted MCS while identified()/report() on the same panel correctly refused.
  set.seed(3)
  L <- list(a = rnorm(40, 1, .3), b = rnorm(40, 1.05, .3), c = rnorm(40, 1.3, .3))
  p_w <- loss_panel(L, weights = c(rep(10, 20), rep(1, 20)))
  expect_error(model_confidence_set(p_w, B = 200, seed = 1), "non-uniform weights")
  # the documented escape hatch, and uniform-weight panels, still work
  expect_type(model_confidence_set(p_w$losses, B = 200, seed = 1)$surviving, "character")
  expect_type(model_confidence_set(loss_panel(L, weights = rep(3, 40)), B = 200, seed = 1)$surviving, "character")
  expect_type(model_confidence_set(loss_panel(L), B = 200, seed = 1)$surviving, "character")
})
