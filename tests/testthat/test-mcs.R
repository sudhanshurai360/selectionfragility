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
