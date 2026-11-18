# Oracle value for concentration_share generated from the Python `selection_fragility.pivot`
# module (v1.0.4); pivot_agreement is bootstrap-based (R's own RNG) and only checked for sanity.

test_that("concentration_share matches the Python oracle", {
  L <- list(
    champ = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 1.05, 0.95, 1.1, 0.9),
    rival = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08, 2.5, 1.0, 1.1, 0.9),
    far   = c(3.0, 3.1, 2.9, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.1, 3.0, 3.1, 2.9, 3.0)
  )
  expect_equal(concentration_share(L), 0.5799999999999997, tolerance = 1e-9)
})

test_that("concentration_share and pivot_agreement return NA on an exact tie", {
  Ltie <- list(a = c(1, 2, 3, 4), b = c(1, 2, 3, 4))
  expect_true(is.na(concentration_share(Ltie)))
  expect_true(is.na(pivot_agreement(Ltie)))
})

test_that("pivot_agreement validates n_boot and frac, and returns a sane fraction", {
  L <- list(
    champ = c(1.0, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98, 1.05, 0.95, 1.1, 0.9),
    rival = c(1.10, 1.08, 1.12, 1.11, 1.09, 1.13, 1.07, 1.10, 1.12, 1.08, 2.5, 1.0, 1.1, 0.9),
    far   = c(3.0, 3.1, 2.9, 3.2, 2.8, 3.0, 3.1, 2.9, 3.0, 3.1, 3.0, 3.1, 2.9, 3.0)
  )
  expect_error(pivot_agreement(L, n_boot = 0), "positive integer")
  expect_error(pivot_agreement(L, frac = 1.5), "strictly between 0 and 1")
  pa <- pivot_agreement(L, seed = 0, n_boot = 100, frac = 0.8)
  expect_true(pa >= 0 && pa <= 1)
})

test_that(".fsum is permutation-invariant (the opponent tie-break summation)", {
  # REGRESSION. pivot.py's own header documents this exact bug class -- order-dependent
  # summation of permutation-tied per-period margin contributions -- being found and fixed FIVE
  # separate times on the Python side (fixed there with math.fsum). The R port used a raw
  # `sum()`, relying on R's long-double accumulator to be "order-stable enough"; that claim was
  # never actually verified and is false: on this adversarial fixture (one huge contribution plus
  # many tiny ones, summed in two different, data-identical orders) a raw `sum()` disagrees at
  # 4 of 5 weight scales tested (including a 0.25 absolute difference at scale 1e9), which would
  # silently flip which opponent -- and therefore which pivotal period -- gets named purely from
  # model-naming/iteration order, not from any real difference in the data.
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(7)
  n <- 50
  base_c <- c(1e6, rexp(n - 1, rate = 1 / 1e-3))
  perm <- sample(n)
  c1 <- base_c
  c2 <- base_c[perm]
  for (scale in c(1e-6, 1e-3, 1, 1e3, 1e9)) {
    m1 <- selectionfragility:::.fsum(c1 * scale)
    m2 <- selectionfragility:::.fsum(c2 * scale)
    expect_identical(m1, m2, info = sprintf("scale=%g", scale))
  }
  # NOTE: an earlier version of this test also asserted that base R's naive `sum()` must
  # disagree across permutations on this fixture, to prove the fixture was adversarial rather
  # than the fix being a no-op. That assertion depends on an implementation detail of base R's
  # floating-point summation (observed on R 4.6.2, but not reproduced on R-devel across linux/
  # macOS/windows in CI) -- it is not a property of this package's code, so it is not safe to
  # assert portably. The permutation-invariance check above is the actual regression test.
})

test_that(".fsum() is permutation-invariant even where it is NOT exact (known limitation, documented, not fixed, 5-lens round 2026-11-18)", {
  # REGRESSION/LIMITATION: live-diffing against Python's math.fsum() (Shewchuk's algorithm,
  # provably exact) found a contrived mixed-sign input at this package's own .MAX_ABS_LOSS cap
  # (1e100) where .fsum()'s single-compensator Neumaier summation is not just imprecise but
  # CATASTROPHICALLY wrong (true sum 10, .fsum() returns 0) -- math.fsum() on the identical input
  # correctly returns 10. Not fixed (reimplementing Shewchuk's algorithm is a nontrivial separate
  # piece of work, and this needs near-cap-magnitude values that cancel almost exactly, which
  # realistic forecast-loss panels aren't expected to produce). What DOES still hold, and is what
  # the tie-break logic actually depends on (per .fsum()'s own doc comment): the result is still
  # permutation-invariant, i.e. two permutations of the same multiset -- even this adversarial
  # one -- can never flip which opponent gets named, because sorting first collapses them to the
  # identical input sequence before summation ever starts. Pinning both facts so a future partial
  # fix (e.g. one that restores exactness but reintroduces permutation-sensitivity) gets caught.
  x <- rep(c(1, 1e100, 1, -1e100), 5)
  result <- selectionfragility:::.fsum(x)
  expect_equal(result, 0)  # confirmed wrong (true sum is 10) -- pins the known limitation
  perm <- rev(x)
  expect_identical(result, selectionfragility:::.fsum(perm))  # but still permutation-invariant
})

test_that(".champ_opp_contributions()'s tie-within-a-tie falls back to alphabetical, not
           insertion/iteration order", {
  # REGRESSION (verified 2026-11-18, confirmed correct, not a bug -- see .champ_opp_contributions()'s
  # own @keywords internal doc comment). When two opponents are tied at k* AND have an EXACTLY tied
  # total margin M (constructed here as permutations of the same per-period-loss multiset), the
  # largest-margin tie-break's `if (M > best_M)` loop keeps the first-encountered of the two, which is
  # the alphabetically-first name in decision_breakdown()'s own radix-sorted `ties` vector -- NOT
  # whichever model happened to be listed first in the caller's original list. Confirmed deterministic
  # (5 repeated calls) and insertion-order-independent (both orderings of the two tied names below
  # choose the same, alphabetically-first, opponent).
  ar1 <- c(1, 1, 1, 1, 1, 1)
  base_opp <- c(1.5, 1.2, 1.3, 1.1, 1.4, 1.0)  # contributions over ar1: .5,.2,.3,.1,.4,.0
  zeta <- base_opp
  alpha <- rev(base_opp)  # exact permutation of the same multiset -> exactly tied M

  panel_zeta_first <- loss_panel(list(ar1 = ar1, zeta = zeta, alpha = alpha))
  panel_alpha_first <- loss_panel(list(ar1 = ar1, alpha = alpha, zeta = zeta))

  for (i in 1:5) {
    cc1 <- selectionfragility:::.champ_opp_contributions(panel_zeta_first$losses, rep(1, 6))
    cc2 <- selectionfragility:::.champ_opp_contributions(panel_alpha_first$losses, rep(1, 6))
    expect_identical(cc1$opp, "alpha")
    expect_identical(cc2$opp, "alpha")
  }
})
