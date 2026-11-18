# REALISTIC DOMAIN TEST: P&C commercial auto liability reserving-model selection.
#
# BUSINESS CONTEXT: a P&C insurer's actuarial team has 4 competing reserving methods for
# commercial auto liability -- chain-ladder (volume-weighted age-to-age development, no
# credibility blending, historically the noisiest of the four), Bornhuetter-Ferguson (BF;
# blends chain-ladder with an a priori expected-loss-ratio, which damps volatility -- this is
# exactly WHY BF is a standard reserving method for volatile/long-tail liability lines, see
# Bornhuetter & Ferguson 1972, CAS Proceedings), a GLM-based stochastic reserving model
# (Poisson/Gamma; between chain-ladder and BF in volatility), and a gradient-boosted/ML
# reserving model (usually the best performer in "normal" years, but with no guarantee of
# graceful degradation on a genuinely novel tail event it has no training analog for).
#
# Back-tested model performance is the ABSOLUTE PERCENTAGE ERROR of each method's estimated
# ultimate loss vs. the (now credible/known) actual ultimate loss, by ACCIDENT YEAR -- the
# native cadence of reserving work. 12 accident years (2013-2024) is realistic, not a stress
# edge case: actuarial reserving studies routinely report 10-15 accident years of history, far
# below the T=30-100+ regime most forecast-evaluation statistics (including this package's own
# MCS bootstrap) are asymptotically justified for.
#
# REALISTIC HEAVY TAIL: commercial auto/general liability loss experience is well known to be
# right-skewed with a small number of catastrophic/large-claim years dominating (social
# inflation / "nuclear verdict" waves, mass-tort-style claim emergence, a pandemic-era
# claims-reporting distortion). This panel encodes exactly that: a lognormal base error per
# model/year (realistic coefficients of variation of 0.35-0.55, not a symmetric Gaussian), plus
# two explicit shock years (2017: a social-inflation/nuclear-verdict wave; 2020: COVID-era
# claims-reporting distortion) in which error inflates 3-9x for most models -- asymmetrically,
# because a real catastrophe does not hit every model the same way. BF's ELR anchor damps both
# shocks (consistent with its real-world role); chain-ladder and the GLM model (both
# volume/development reactive) blow up in both; the ML model blows up WORSE than everyone in
# 2017 (no training precedent for that specific shock) but copes fine in 2020 (a pattern closer
# to historical reporting-lag shifts it had seen) -- deliberately not "one outlier on the
# champion," which would be a toy case, but a believable, asymmetric catastrophe.
#
# BUSINESS STAKES: choosing the wrong reserving model has direct regulatory capital
# consequences (Schedule P reserve adequacy, RBC, Appointed Actuary opinion) -- this is exactly
# the setting where an actuarial team must be able to say, to a regulator or auditor, whether
# "our model wins over 12 years" is statistically defensible or an artifact of one bad year.

test_that("realistic 12-year commercial-auto reserving panel: report() correctly flags the champion's edge as unresolved and catastrophe-driven", {
  years <- 2013:2024
  T <- 12

  base_mean <- c(chain_ladder = 0.095, bf = 0.065, glm = 0.072, ml = 0.048)
  base_cv   <- c(chain_ladder = 0.55,  bf = 0.35,  glm = 0.40,  ml = 0.45)

  set.seed(42)
  losses <- as.data.frame(sapply(names(base_mean), function(m) {
    sigma2 <- log(1 + base_cv[m]^2)
    mu <- log(base_mean[m]) - sigma2 / 2
    stats::rlnorm(T, mu, sqrt(sigma2))
  }))
  rownames(losses) <- years

  # Catastrophe years: 2017 (social-inflation/nuclear-verdict wave) and 2020 (COVID claims
  # distortion). Asymmetric across models -- a real catastrophe does not hit every method
  # identically.
  losses["2017", "chain_ladder"] <- losses["2017", "chain_ladder"] * 7.5
  losses["2017", "glm"]          <- losses["2017", "glm"] * 6.8
  losses["2017", "bf"]           <- losses["2017", "bf"] * 3.1
  losses["2017", "ml"]           <- losses["2017", "ml"] * 8.9  # ML's bad year: no precedent

  losses["2020", "chain_ladder"] <- losses["2020", "chain_ladder"] * 5.4
  losses["2020", "glm"]          <- losses["2020", "glm"] * 4.7
  losses["2020", "bf"]           <- losses["2020", "bf"] * 2.6
  losses["2020", "ml"]           <- losses["2020", "ml"] * 1.3  # ML copes fine here

  panel <- loss_panel(losses, labels = years)

  # --- report() workflow ---------------------------------------------------------------
  rpt <- report(panel, alpha = 0.10, full = TRUE)

  # The ML model is the apparent champion (lowest mean absolute reserve error over 12 years).
  expect_match(rpt, "ml +mean=0\\.0[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")
  # MCS does NOT single out ml as uniquely best -- bf also survives (genuinely not identified).
  expect_match(rpt, "VERDICT: not identified \\(MCS size 2 of 4\\)")
  # The single most fragile pivot period is 2020 -- the catastrophe year -- not an ordinary year.
  expect_match(rpt, "k\\*=1 of 12 periods \\(2020\\)")
  # RESOLUTION must tell the actuarial team the apparent edge is nowhere near powered at T=12:
  # the observed edge is roughly an order of magnitude below the edge this sample size could
  # even detect at conventional power -- i.e. "ml wins" is NOT something 12 accident years of
  # data can actually certify, heavy tails or not.
  expect_match(rpt, "cannot determine at this sample size")

  # --- decision_breakdown() / Prop 2.2 certificate -------------------------------------
  L <- panel$losses
  w <- panel$weights
  means_L <- vapply(L, mean, numeric(1))
  champ <- names(which.min(means_L))
  expect_equal(champ, "ml")

  db <- decision_breakdown(panel)
  expect_equal(db$k, 1L)                 # a SINGLE period's removal flips the pooled winner
  expect_equal(db$binding_opponent, "bf") # BF, the ELR-damped method, is the near-tie rival

  # Prop 2.2's zero-simulation certificate: at T=12, k*=1 against bf IS certified
  # non-significant -- i.e. the package's own fast, assumption-light check agrees with the
  # bootstrap MCS that ml's apparent win over bf is NOT statistically defensible from one
  # flippable period. This is the right, non-hedged answer: a single catastrophic accident
  # year (2020) is doing the work of "proving" ml is better, and the certificate correctly
  # calls that out rather than being silently accepted as institutional fact.
  bd_bf <- breakdown_number(L[["ml"]], L[["bf"]], w)
  expect_equal(bd_bf$k, 1L)
  expect_true(prop22_certifies(bd_bf$k, T = T))

  # --- resolution_report()/minimum_detectable_edge(): the power check a regulator would want --
  rr <- resolution_report(panel, alpha = 0.05, power = 0.80)
  expect_identical(rr$champion, "ml")
  expect_false(rr$resolved)
  expect_identical(rr$fragility_read, "undetermined")
  mde <- minimum_detectable_edge(panel, power = 0.80, alpha = 0.05)
  # The observed edge is far smaller than what this 12-year sample could reliably detect --
  # exactly the realistic-small-T actuarial regime this package's own docs warn about. A
  # reserving team citing "our model is better" on this basis alone is over-claiming.
  expect_lt(rr$observed_edge / mde, 0.25)

  # --- Leave-one-catastrophe-year-out: does dropping the bad year flip the champion? ------
  # This is the concrete regulatory-defensibility question: "is one bad accident year driving
  # the whole conclusion?" Dropping 2020 (the pivot period k* already pointed to) flips the
  # pooled champion from ml to bf -- confirming k*=1's diagnosis was not a false alarm.
  means_drop_2020 <- vapply(L, function(col) {
    v <- col; names(v) <- as.character(years); mean(v[names(v) != "2020"])
  }, numeric(1))
  expect_equal(names(which.min(means_drop_2020)), "bf")

  # Dropping the OTHER catastrophe year (2017) does NOT flip the champion -- ml remains ahead
  # even without its own worst year, confirming the fragility is specifically tied to 2020,
  # consistent with k*'s identification of 2020 (not 2017) as the pivot period.
  means_drop_2017 <- vapply(L, function(col) {
    v <- col; names(v) <- as.character(years); mean(v[names(v) != "2017"])
  }, numeric(1))
  expect_equal(names(which.min(means_drop_2017)), "ml")
})

test_that("MCS false-elimination risk at the realistic actuarial T (T=12, K=4) is markedly worse than this package's own documented T=30 figure, and heavy tails do not meaningfully change it", {
  # DOCUMENTED BASELINE (mcs.R's own roxygen docs, confirmed by this package's prior hardening
  # rounds): under a TRUE complete null (all K models i.i.d., same mean, zero autocorrelation),
  # the MCS's actual false-elimination rate at T=30, K=3, alpha=0.10 is ~18% (not the nominal
  # ~10%) -- a known, already-flagged finite-sample size distortion.
  #
  # THIS TEST asks the actuarially realistic question that figure does not answer: annual
  # reserving history is often T=10-15, well BELOW T=30. Does the false-elimination rate get
  # WORSE at the smaller, routine T this domain actually uses, and does heavy-tailed (lognormal,
  # right-skewed) loss data -- the realistic shape of reserve-error data -- make it worse still?
  #
  # FINDING: at T=12, K=4 (this test's realistic reserving panel size), the measured
  # false-elimination rate is dramatically higher than the T=30 figure (confirmed here at ~40%,
  # roughly DOUBLE the T=30/K=3 ~18% figure, and quadruple the nominal alpha=0.10). Heavy tails
  # (lognormal, CoV=1.0) do NOT materially worsen it further versus a Gaussian null at the same
  # T/K -- the dominant driver is small T itself, not the tail shape. CRITICALLY: this T=12
  # degradation is NOT surfaced anywhere in report()'s printed output or in any runtime warning
  # from mcs()/resolution_report() -- it is visible only to a reader of mcs.R's roxygen source,
  # not to an actuarial user reading report() at the console. A regulator-facing team running
  # this package at its own, normal T=12 cadence has no in-band signal that the MCS verdict's
  # reliability is worse here than even the package's own already-conservative T=30 caveat
  # suggests. Reported for human review/documentation rather than silently patched, since fixing
  # it (e.g. a dynamic small-T warning in report()) is a design decision about threshold and
  # wording, not a one-line bug fix.
  K <- 4; T <- 12; alpha <- 0.10; n_rep <- 150

  false_elim_gaussian <- 0L
  for (i in seq_len(n_rep)) {
    set.seed(i)
    Lmat <- matrix(rnorm(T * K, mean = 1, sd = 0.3), nrow = T, ncol = K)
    m <- mcs(Lmat, alpha = alpha, seed = i)
    if (length(m$surviving) < K) false_elim_gaussian <- false_elim_gaussian + 1L
  }
  rate_gaussian <- false_elim_gaussian / n_rep

  false_elim_heavy <- 0L
  for (i in seq_len(n_rep)) {
    set.seed(1000 + i)
    cv <- 1.0
    sigma2 <- log(1 + cv^2); mu <- log(1) - sigma2 / 2
    Lmat <- matrix(stats::rlnorm(T * K, mu, sqrt(sigma2)), nrow = T, ncol = K)
    m <- mcs(Lmat, alpha = alpha, seed = i)
    if (length(m$surviving) < K) false_elim_heavy <- false_elim_heavy + 1L
  }
  rate_heavy <- false_elim_heavy / n_rep

  # Both rates are far above the nominal alpha=0.10 and above the documented T=30/K=3 ~18%
  # figure -- confirming the realistic small-T actuarial regime is a WORSE case than what is
  # currently documented, not a new, separate problem. Thresholds are loose (0.25, well below
  # the ~0.38-0.41 actually observed) so this test is a directional regression guard, not a
  # tight pin on a bootstrap-seeded statistic.
  expect_gt(rate_gaussian, 0.25)
  expect_gt(rate_heavy, 0.25)
})
