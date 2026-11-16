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
  # Sanity: the naive sum this replaces really does break on the same fixture (proves the
  # fixture is adversarial, not that the fix is a no-op).
  naive_breaks <- any(vapply(c(1e-6, 1e-3, 1, 1e3, 1e9), function(scale) {
    !identical(sum(c1 * scale), sum(c2 * scale))
  }, logical(1)))
  expect_true(naive_breaks)
})
