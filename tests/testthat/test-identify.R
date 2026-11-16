test_that("identified()/mcs_size() give a singleton MCS when one model dominates", {
  L1 <- list(
    a = c(1.0, 1.1, 0.9, 1.0, 1.2, 0.8, 1.0, 1.1, 0.9, 1.0, 1.0, 1.1),
    b = c(2.0, 2.1, 1.9, 2.0, 2.2, 1.8, 2.0, 2.1, 1.9, 2.0, 2.0, 2.1),
    c = c(3.0, 3.1, 2.9, 3.0, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.0, 3.1)
  )
  expect_equal(mcs_size(L1, alpha = 0.10, reps = 500, block_size = 2, seed = 0), 1L)
  expect_true(identified(L1, alpha = 0.10, reps = 500, block_size = 2, seed = 0))
})

test_that("mcs_size() keeps an exact tie non-identified", {
  col <- c(1, 2, 0.5, 3, 1.5, 2.5, 1, 2, 0.5, 3, 1.5, 2.5)
  L2 <- list(a = col, b = col, c = c(5, 6, 5.5, 6.5, 5.2, 6.2, 5, 6, 5.5, 6.5, 5.2, 6.2))
  expect_equal(mcs_size(L2, alpha = 0.10, reps = 500, block_size = 2, seed = 0), 2L)
  expect_false(identified(L2, alpha = 0.10, reps = 500, block_size = 2, seed = 0))
})

test_that("mcs_size()/identified() use the TR (range) statistic, matching the Python oracle's arch default -- not MCSprocedure()'s own 'Tmax' default", {
  # BUG FOUND IN AUDIT (2026-11-17): `.run_mcs_arch()` originally called `MCS::MCSprocedure(..., statistic =
  # "Tmax")`. `identify.py` never passes `method` to `arch.bootstrap.MCS`, so it gets arch's own default,
  # which is the RANGE statistic ("R" in arch's naming, "TR" in MCS::MCSprocedure()'s naming) -- the same
  # T_R range statistic this package's own native mcs()/model_confidence_set() computes. "Tmax" is a
  # genuinely different HLN (2011) test (the MAX statistic), and it happened to be MCS::MCSprocedure()'s
  # own default -- an easy, easy-to-miss mismatch since both packages ship *a* default, just not the same
  # one. Confirmed verdict-changing, not cosmetic: on 200 simulated K=5/T=25 panels (matched B/seed), TR
  # and Tmax disagreed on the included set in 12/200 trials (6%). This fixed panel is one of those 12 --
  # pins the fix down exactly.
  Lvec <- c(1.764712, 2.442879, 2.096841, 2.204687, 3.019615,
            2.286418, 3.230324, 3.056204, 2.273232, 2.90205, 2.330696, 1.786103,
            2.020692, 2.141695, 2.538889, 2.479524, 2.42576, 2.666009, 1.588425, 2.65279,
            2.217301, 2.444536, 2.380322, 1.750403, 2.033198, 0.053274,
            0.689098, 0.445875, 0.370227, 0.233418, 0.176764, 0.80062, -0.040438,
            0.344699, 0.527612, 0.889034, 0.121887, 0.287441, -0.123007,
            0.245209, 0.241004, 0.941821, 0.63809, 0.441824, 0.774043, 0.296517,
            0.398547, 0.548475, 1.084479, 0.69111, 0.845268, 0.025706, 0.780154,
            0.71925, 0.292891, 0.683402, 0.716516, 0.870323, 0.934777, 0.780441,
            1.096554, 0.960515, 1.114243, 1.157127, 0.933103, 0.825905, 0.283813,
            0.40662, 0.30619, -0.002953, 0.122518, 0.297239, 0.429813, 0.627893,
            1.621931, 0.930888, 0.795553, 0.805541, 0.636484, 0.476772, 0.669173,
            0.655426, 0.67201, 0.079496, 1.028763, 1.448171, 0.535684, 1.04145,
            0.888411, 1.132454, 0.564395, 1.241051, 0.177853, 0.618604, 0.733584,
            1.001477, 1.028869, 0.76212, 0.324109, 0.917342, -0.126888, 0.911179,
            0.923952, 0.850722, 0.562695, 0.344161, 0.698234, 0.772683, 0.759276,
            0.158979, 1.13222, 0.017958, 0.956042, 0.907772, 0.636394, -0.107224,
            1.14262, 0.790043, 0.443622, 0.806344, 1.127964, 0.717295, 0.527994,
            -0.072357, 0.922326)
  M <- matrix(Lvec, nrow = 25, ncol = 5, dimnames = list(NULL, c("m1", "m2", "m3", "m4", "m5")))
  L <- setNames(lapply(colnames(M), function(cn) M[, cn]), colnames(M))
  res <- selectionfragility:::.run_mcs_arch(L, alpha = 0.10, reps = 300, block_size = 3, seed = 1)
  tr <- MCS::MCSprocedure(do.call(cbind, L), alpha = 0.10, B = 300, statistic = "TR",
                           k = 3, verbose = FALSE, seed = 1)@Info$included
  tmax <- MCS::MCSprocedure(do.call(cbind, L), alpha = 0.10, B = 300, statistic = "Tmax",
                             k = 3, verbose = FALSE, seed = 1)@Info$included
  expect_false(identical(sort(tr), sort(tmax)))  # sanity: this panel really is one where TR != Tmax
  expect_equal(sort(res), sort(tr))
  expect_false(identical(sort(res), sort(tmax)))
})

test_that("identify validates alpha, model count, and rejects non-uniform weights", {
  L1 <- list(a = c(1.0, 1.1, 0.9, 1.0), b = c(2.0, 2.1, 1.9, 2.0))
  expect_error(identified(list(a = 1:3)), "at least 2 models")
  expect_error(mcs_size(L1, alpha = 10), "alpha must be strictly between 0 and 1")
  expect_error(mcs_size(L1, w = c(1, 2, 1, 1)), "non-uniform")
})
