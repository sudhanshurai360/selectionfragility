# Oracle values generated from the Python `selection_fragility.prop22` module (v1.0.4).

test_that("prop22_certifies matches the Python oracle", {
  expect_true(prop22_certifies(3, 14))
  expect_true(prop22_certifies(3, 30))
  expect_false(prop22_certifies(10, 30))
  expect_true(is.na(prop22_certifies(0, 30)))
  expect_true(is.na(prop22_certifies(30, 30)))
})

test_that("prop22_certifies validates finiteness and z > 0", {
  expect_error(prop22_certifies(NaN, 30), "finite")
  expect_error(prop22_certifies(3, 30, z = -1), "z > 0")
})

test_that("certified_tied_subset matches the Python oracle", {
  L <- list(
    champ        = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 1.0, 0.99, 1.01, 1.0),
    rival        = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08, 1.1, 1.09, 1.11, 1.1),
    tiewithchamp = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 1.0, 0.99, 1.01, 1.0),
    far          = c(3.0, 3.1, 2.9, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.1, 3.0, 3.1, 2.9, 3.0)
  )
  expect_equal(certified_tied_subset(L), c("champ", "tiewithchamp"))
  expect_error(certified_tied_subset(L, w = c(1, 2, rep(1, 12))), "uniform weights")
})

test_that("certified_tied_subset's uniform-weight check matches numpy's np.allclose tolerance", {
  # REGRESSION. The R port used to gate uniformity with a bespoke tolerance
  # (1e-8 * max(1, abs(w[1]))) far tighter than the Python oracle's `np.allclose(w, w.flat[0])`
  # default (atol=1e-8, rtol=1e-5 -> tolerance ~2e-5 at w~2). A weight vector that is uniform up
  # to ordinary float noise (e.g. recomputed from a slightly perturbed source) was wrongly
  # rejected here while the Python oracle accepted it -- a real cross-language behavioral
  # divergence. Oracle value from certified_tied_subset() on this exact L/w (Python v1.0.4):
  # ["champ", "tiewithchamp"].
  L <- list(
    champ        = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 1.0, 0.99, 1.01, 1.0),
    rival        = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08, 1.1, 1.09, 1.11, 1.1),
    tiewithchamp = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 1.0, 0.99, 1.01, 1.0),
    far          = c(3.0, 3.1, 2.9, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.1, 3.0, 3.1, 2.9, 3.0)
  )
  w <- rep(2.0, 14)
  w[4] <- 2.0 + 1e-7  # within np.allclose's default tolerance, outside the old buggy R tolerance
  expect_equal(certified_tied_subset(L, w = w), c("champ", "tiewithchamp"))
  # A genuinely non-uniform vector at the same scale must still be rejected.
  w_bad <- rep(2.0, 14)
  w_bad[4] <- 2.1
  expect_error(certified_tied_subset(L, w = w_bad), "uniform weights")
})
