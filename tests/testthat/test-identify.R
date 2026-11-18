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

test_that("mcs_size()/identified() reject duplicate model names", {
  L <- list(a = c(1.0, 1.1, 0.9, 1.0), a = c(5.0, 5.1, 4.9, 5.0), b = c(2.0, 2.1, 1.9, 2.0))
  expect_error(mcs_size(L), "unique.*duplicate|duplicate.*unique")
})

test_that("identify() fails loudly, not silently, if MCS::MCSprocedure()'s Info list ever loses its 'included' field", {
  # REGRESSION for the defensive guard in .run_mcs_arch() (R/identify.R): a future MCS release
  # could restructure its Info list, and R's `$` on a list silently returns NULL for a missing
  # name -- which would otherwise make mcs_size()/identified() silently report "nothing
  # identified" on every call with no warning. Mock MCSprocedure() to return a real result with
  # that one field stripped, and confirm the guard actually fires.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0, 1.2, 0.8, 1.0, 1.1), b = c(1.2, 1.3, 1.1, 1.2, 1.4, 1.0, 1.2, 1.3))
  real_MCSprocedure <- MCS::MCSprocedure
  testthat::local_mocked_bindings(
    MCSprocedure = function(...) {
      res <- real_MCSprocedure(...)
      res@Info$included <- NULL
      res
    },
    .package = "MCS"
  )
  expect_error(mcs_size(L, alpha = 0.5, reps = 300, block_size = 1, seed = 1), "no longer has an 'included' field")
})

test_that("mcs_size()/identified() use the SAME uniform-weight tolerance as certified_tied_subset()", {
  # REGRESSION (weights-semantics review, 2026-11-17): .validate_mcs_weights2 used a stricter
  # ad-hoc tolerance than prop22.R's .weights_are_uniform (numpy allclose), so a float-noisy-but-
  # uniform weight vector (deviation ~1e-6) was accepted as uniform by certified_tied_subset() but
  # rejected as non-uniform by mcs_size()/identified(). Both now use the numpy-allclose tolerance.
  L <- list(a = c(1, 1.1, 0.9, 1, 1.2, 0.8, 1, 1.1, 0.95, 1.05),
            b = c(1.2, 1.1, 1.3, 1.1, 1.4, 1.0, 1.2, 1.1, 1.25, 1.15),
            c = c(1.5, 1.4, 1.6, 1.4, 1.7, 1.3, 1.5, 1.4, 1.55, 1.45))
  w <- c(1, 1.000001, 1, 1, 1, 1, 1, 1, 1, 1)  # ~1e-6 deviation: within numpy allclose
  expect_no_error(mcs_size(L, w = w))
  expect_no_error(certified_tied_subset(L, w = w))
})

test_that("identified()/report() restore the caller's global .Random.seed (MCS path no longer leaks)", {
  # REGRESSION (concurrency/global-state review, 2026-11-17): the MCS::MCSprocedure()-backed path
  # (.run_mcs_arch) consumed RNG without saving/restoring .Random.seed, unlike every native
  # bootstrap in the package -- so identified()/mcs_size()/resolution_report()/compare()/report()
  # silently corrupted the caller's RNG stream.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0, 1.2, 0.8), b = c(1.2, 1.1, 1.3, 1.1, 1.4, 1.0))
  set.seed(321); runif(3); before <- get(".Random.seed", envir = .GlobalEnv)
  invisible(identified(L, seed = 77))
  expect_identical(before, get(".Random.seed", envir = .GlobalEnv))
})

test_that(".validate_alpha gives a worded error for NA/NULL/length>1 alpha, not a raw base-R error", {
  L <- list(a = c(1.0, 1.1, 0.9, 1.0), b = c(1.2, 1.1, 1.3, 1.1))
  expect_error(mcs_size(L, alpha = NA), "single finite number")
  expect_error(mcs_size(L, alpha = c(0.1, 0.2)), "single finite number")
})

test_that("mcs_size()/identified() reject out-of-int32 reps and block_size with worded errors, not NA crashes (8-reviewer round, 2026-11-17)", {
  # REGRESSION: `if (as.integer(reps) < 2)` / `if (as.integer(block_size) < 0)` coerced an
  # out-of-int32 count to NA, so if(NA) crashed with the opaque "missing value where TRUE/FALSE
  # needed". The Python oracle (int()) has no overflow and proceeds. Now both coerce-and-check NA.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0), b = c(2.0, 2.1, 1.9, 2.0))
  expect_error(mcs_size(L, reps = 3e9), "single integer >= 2")
  expect_error(mcs_size(L, block_size = 3e9), "single non-negative integer")
})

test_that("mcs_size()/identified() reject a bad seed with a worded error, not a raw coercion warning (strict-regression round, 2026-11-18)", {
  # REGRESSION: seed was the one scalar passed into MCS::MCSprocedure() that never got the same
  # overflow-safe coerce-and-check treatment as reps/block_size (same fix commit as the test
  # above) -- NA/out-of-range/non-scalar seed reached a raw "NAs introduced by coercion" warning
  # from this file's own inline as.integer(seed), with validation entirely load-bearing on
  # MCS::MCSprocedure()'s own internal check continuing to exist in its current form.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0), b = c(2.0, 2.1, 1.9, 2.0))
  expect_error(mcs_size(L, seed = NA), "single non-negative integer")
  expect_error(mcs_size(L, seed = 3e9), "single non-negative integer")
  expect_error(mcs_size(L, seed = c(1, 2)), "single non-negative integer")
  expect_error(mcs_size(L, seed = -1), "single non-negative integer")
})

test_that("mcs_size()/identified() warn on large K like mcs() does, when called directly on a named list (adversarial-review round, 2026-11-18)", {
  # REGRESSION: mcs()/model_confidence_set() (mcs.R) self-guard with a K > .LARGE_K_WARN_THRESHOLD
  # warning on EVERY call. This engine only ever got that warning as a side effect of routing
  # through loss_panel() first (panel.R's .check_K_vs_T) -- a caller who builds a named list
  # directly, exactly as this file's own @examples do, got NO warning at any K, silently breaking
  # loss_panel()'s own promise that its large-K warning text covers "the
  # MCS::MCSprocedure()-backed report()/identified()/mcs_size() path." Below .LARGE_K_WARN_THRESHOLD
  # (100), no warning should fire; above it, mcs_size()/identified() must now warn even when the
  # caller bypasses loss_panel() entirely.
  set.seed(1)
  # T=60 (not the file's usual T=20) keeps T/K=12 >= .SMALL_T_RATIO_WARN=10, so this case tests
  # ONLY the K threshold, not colliding with the small-T/K-ratio warning added in a later round.
  K_small <- 5
  L_small <- stats::setNames(lapply(seq_len(K_small), function(i) rnorm(60, mean = i)), paste0("m", seq_len(K_small)))
  expect_no_warning(mcs_size(L_small, reps = 500, seed = 1))

  K_big <- 110
  L_big <- stats::setNames(lapply(seq_len(K_big), function(i) rnorm(20, mean = i * 0.01)), paste0("m", seq_len(K_big)))
  expect_warning(mcs_size(L_big, reps = 500, seed = 1), "K=110 models")
  expect_warning(identified(L_big, reps = 500, seed = 1), "K=110 models")
})

test_that("mcs_size()/identified() put w second, like every other (L, w, ...) function in the package (strict-regression round, 2026-11-18)", {
  # REGRESSION: w used to sit LAST in these two signatures while every other exported function
  # taking both L and w (pooled_winner, fragility, decision_breakdown, winner_stability, etc.)
  # puts w second, right after L -- a real positional-call footgun, since identified(L, w) used
  # to silently bind w to alpha instead. Confirmed fixed by calling both functions POSITIONALLY
  # with w in the second slot and checking the result matches the named-argument equivalent.
  L <- list(a = c(1.0, 1.1, 0.9, 1.0, 1.2, 0.8, 1.0, 1.1), b = c(2.0, 2.1, 1.9, 2.0, 2.2, 1.8, 2.0, 2.1))
  w <- rep(1, 8)
  expect_equal(mcs_size(L, w, 0.10, 500, 2, 1), mcs_size(L, w = w, alpha = 0.10, reps = 500, block_size = 2, seed = 1))
  expect_equal(identified(L, w, 0.10, 500, 2, 1), identified(L, w = w, alpha = 0.10, reps = 500, block_size = 2, seed = 1))
})
