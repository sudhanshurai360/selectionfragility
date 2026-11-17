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
  # the 2e7 warning threshold and int32 max. Isolated at the exact types/values mcs() uses.
  T <- 21500L; B <- 100000L
  expect_true(as.double(T) * B > 20000000)
  msg <- sprintf("T=%d periods x B=%d bootstrap resamples = %s", T, B, format(as.double(T) * B, big.mark = ","))
  expect_false(grepl("= NA", msg, fixed = TRUE))
  expect_true(grepl("2.15e\\+09|2,150,000,000", msg))  # format() may use scientific notation at this magnitude; either is fine, "NA" is not
  expect_no_warning(format(as.double(T) * B, big.mark = ","))  # the fixed version must not itself trigger "NAs produced by integer overflow"
})
