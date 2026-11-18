# REALISTIC DOMAIN TEST: catastrophe (re)insurance PRICING model selection -- US hurricane peril.
#
# BUSINESS CONTEXT: a reinsurer's pricing actuaries are choosing among 4 competing models for
# pricing a US hurricane excess-of-loss treaty layer: a frequency-severity ACTUARIAL model
# (Poisson frequency x lognormal severity curves, fit to historical landfall data, carrying an
# explicit tail/risk load -- the traditional approach, CAS/Bernegger-style), a cat-modeling-
# VENDOR-style physical/engineering model (RMS/AIR/Verisk-style hazard-vulnerability-financial
# modules -- considered the industry's closest thing to a "gold standard" for tail realism, but
# known to need periodic recalibration after genuinely novel events), an ML model trained on
# historical event data (best fit to the historical record, no guarantee of graceful degradation
# on an event shape it has never seen), and a naive CLIMATOLOGY benchmark (plain historical
# average, included because every serious model bake-off keeps a naive baseline -- it is not a
# strawman, it is the auditability floor a model must beat to justify its complexity).
#
# SAMPLE SIZE -- WHY THIS IS A DIFFERENT (SMALLER) REGIME THAN THE RESERVING TEST: this file's
# panel is INDEXED BY CALENDAR YEAR, not accident year, because catastrophe pricing works off
# the actual annual hurricane-season loss experience, not a claims-development triangle. 18
# calendar years (2007-2024) is realistic for cat pricing -- vendor cat models themselves are
# typically calibrated/validated against 20-40 years of simulated and ~15-25 years of credible
# observed loss experience, and treaty pricing committees routinely work with LESS actual
# calendar-year history than a reserving triangle has accident years, because a hurricane season
# is a single draw per year (T can only grow 1/year) while an accident year's claims keep
# developing for a decade after the year closes. T=18, K=4 here (T/K=4.5) sits BELOW this
# package's own already-tolerated T=30/K=3 anchor and close to (not quite as bad as) its newly
# measured T=12/K=4 regime -- genuinely the normal cadence for this sub-line, not a contrived
# edge case.
#
# ZERO-INFLATED / HEAVY-TAILED LOSS SHAPE -- THE OTHER DISTINCTIVE FEATURE: unlike the reserving
# test's uniformly lognormal (always-positive, merely skewed) panel, US hurricane annual
# aggregate loss is famously zero-inflated at the peril/layer level -- most Atlantic hurricane
# seasons produce little or no loss to a given excess-of-loss layer, and a handful of seasons
# (compound/clustered landfalls) produce enormous losses. This panel encodes exactly that: model
# forecast error (the pricing-relevant loss metric here, as in the reserving test) is tiny in 14
# of 18 "quiet" seasons and spikes by 2-14x in 3 explicit catastrophe seasons -- 2008 (Ike,
# moderate), 2017 (Harvey/Irma/Maria, an UNPRECEDENTED triple continental-US landfall clustering
# within one season -- the worst year for every model, but asymmetrically worst for the models
# with no training/historical precedent for that specific clustering shape), and 2022 (Ian,
# moderate, post-2017-recalibration). A separate sub-test also injects literal EXACT-ZERO loss
# cells (a quiet season where a model's predicted loss and the realized ~$0 loss coincide
# exactly) to check the package tolerates true zeros, not just "small", at this T.
#
# REGULATORY/RATING-AGENCY STAKES: a reinsurer's choice of pricing model feeds directly into
# rate adequacy, rating-agency capital-model validation (S&P/AM Best/Moody's cat-model review),
# and -- for a cat bond -- the offering document's modeled-loss representations to investors.
# "Our ML model has the lowest average pricing error over 18 hurricane seasons" is exactly the
# kind of claim that needs k*/MCS/resolution scrutiny before it goes into a rate filing or an
# offering circular, and this test exercises the FULL report() -> decision_breakdown() ->
# prop22_certifies() -> resolution_report() workflow end to end.

test_that("18-year US hurricane cat-pricing panel: report() flags small-T, and k* lands on ordinary quiet seasons, NOT the catastrophe seasons -- a genuinely different fragility shape than the reserving test's cat-year pivot", {
  years <- 2007:2024
  T <- 18L

  base_mean <- c(freq_sev = 0.018, vendor = 0.012, ml = 0.009, climatology = 0.030)
  base_cv   <- c(freq_sev = 0.45, vendor = 0.40,  ml = 0.50,  climatology = 0.35)

  set.seed(101)
  losses <- as.data.frame(sapply(names(base_mean), function(m) {
    sigma2 <- log(1 + base_cv[m]^2)
    mu <- log(base_mean[m]) - sigma2 / 2
    stats::rlnorm(T, mu, sqrt(sigma2))
  }))
  rownames(losses) <- years

  # Catastrophe seasons, asymmetric across models (a real hurricane season does not hit every
  # pricing model the same way):
  #  - 2008 (Ike): moderate, all models degrade similarly.
  #  - 2017 (Harvey/Irma/Maria clustering): the worst season for everyone, but WORST for ml
  #    (no historical precedent for triple-landfall clustering) and second-worst for the
  #    vendor model (pre-2017 vendor models' well-documented clustering/correlation blind spot).
  #    The actuarial model's explicit tail/risk load damps it some; climatology (no physical
  #    modeling at all, but also no false precision to be surprised by) is in between.
  #  - 2022 (Ian): moderate; vendor copes comparatively better here, consistent with real-world
  #    post-2017 vendor-model recalibration for multi-peril clustering.
  losses["2008", "freq_sev"]    <- losses["2008", "freq_sev"]    * 4.0
  losses["2008", "vendor"]      <- losses["2008", "vendor"]      * 3.0
  losses["2008", "ml"]          <- losses["2008", "ml"]          * 3.5
  losses["2008", "climatology"] <- losses["2008", "climatology"] * 2.0

  losses["2017", "freq_sev"]    <- losses["2017", "freq_sev"]    * 5.5
  losses["2017", "vendor"]      <- losses["2017", "vendor"]      * 9.0
  losses["2017", "ml"]          <- losses["2017", "ml"]          * 14.0
  losses["2017", "climatology"] <- losses["2017", "climatology"] * 6.0

  losses["2022", "freq_sev"]    <- losses["2022", "freq_sev"]    * 3.0
  losses["2022", "vendor"]      <- losses["2022", "vendor"]      * 2.0
  losses["2022", "ml"]          <- losses["2022", "ml"]          * 2.2
  losses["2022", "climatology"] <- losses["2022", "climatology"] * 2.5

  panel <- loss_panel(losses, labels = years)

  # --- report() workflow ---------------------------------------------------------------
  # T/K = 4.5 here falls below this package's already-disclosed small-T regime (T/K<10), so the
  # runtime warning from the earlier (reserving-scenario) hardening round must fire on THIS
  # panel shape too -- confirming it generalizes beyond the T=12/K=4 case it was written for,
  # not just reconfirming that one number.
  expect_warning(
    rpt <- report(panel, alpha = 0.10, full = TRUE),
    "T=18 periods for K=4 models \\(T/K=4\\.5\\)"
  )
  # Given the regulatory stakes named above, this caution is the RIGHT level: it is a blanket,
  # non-data-dependent caveat (not a false "this specific panel is broken" claim) -- exactly what
  # a pricing actuary should see attached to a T=18 cat-pricing comparison before it reaches a
  # rate filing, no stronger and no weaker.

  # ml is the apparent champion (lowest mean pricing error over 18 hurricane seasons).
  expect_match(rpt, "ml +mean=0\\.0[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")
  # MCS does not single ml out -- vendor also survives (genuinely not identified at this T).
  expect_match(rpt, "VERDICT: not identified \\(MCS size 2 of 4\\)")
  # DISTINCTIVE FINDING vs. the reserving test: there, k* pinpointed the single catastrophe year
  # (2020) as the pivot. HERE, the pivot periods are two ORDINARY, non-catastrophe seasons (2013,
  # 2021) -- not 2008/2017/2022. With zero-inflated losses, the top two models' catastrophe-season
  # errors are close enough to each other (both get hit hard by the same hurricane clusters) that
  # ordinary-season noise, not the catastrophe experience itself, is what is fragile. This is a
  # SHARPER caution for a regulator/rating-agency audience than "a bad cat year drove this": the
  # model ranking isn't even sensitive to the catastrophe performance it's nominally about.
  expect_match(rpt, "k\\*=2 of 18 periods \\(2013, 2021\\)")
  expect_false(grepl("2008|2017|2022", sub(".*PIVOT", "", rpt)))

  # RESOLUTION must caution the pricing team the apparent edge is nowhere near powered at T=18.
  expect_match(rpt, "cannot determine at this sample size")

  # --- decision_breakdown() / Prop 2.2 certificate -------------------------------------
  L <- panel$losses
  w <- panel$weights
  means_L <- vapply(L, mean, numeric(1))
  champ <- names(which.min(means_L))
  expect_equal(champ, "ml")
  expect_equal(names(sort(means_L))[2], "vendor")

  db <- decision_breakdown(panel)
  expect_equal(db$k, 2L)                     # it takes BOTH 2013 and 2021 together to flip it
  expect_equal(db$binding_opponent, "vendor")

  bd <- breakdown_number(L[["ml"]], L[["vendor"]], w)
  expect_equal(bd$k, 2L)
  # Prop 2.2's zero-simulation certificate agrees with the bootstrap MCS: ml's apparent edge
  # over vendor, resting on only 2 of 18 flippable seasons, is certified non-significant at this
  # T -- the right, non-hedged read for a rate filing: this is not yet statistically defensible.
  expect_true(prop22_certifies(bd$k, T = T))

  # --- resolution_report()/minimum_detectable_edge(): the power check a rating agency would want
  rr <- resolution_report(panel, alpha = 0.05, power = 0.80)
  expect_identical(rr$champion, "ml")
  expect_false(rr$resolved)
  expect_identical(rr$fragility_read, "undetermined")
  mde <- minimum_detectable_edge(panel, power = 0.80, alpha = 0.05)
  # The observed edge is roughly a TENTH of what T=18 could reliably detect -- a wider gap than
  # even the reserving test's T=12 case (<0.25 there), consistent with T=18 having somewhat more
  # (though still inadequate) power than T=12.
  expect_lt(rr$observed_edge / mde, 0.15)

  # --- Leave-one-catastrophe-season-out: does any SINGLE cat season drive the result? ----------
  # The naive regulatory assumption -- "the champion only wins because of how it handled the big
  # hurricane years" -- is checked directly and found FALSE here: dropping any one of the three
  # catastrophe seasons individually, including 2017 (the worst season for every model, and ml's
  # single worst miss by far), leaves ml the pooled champion. The catastrophe experience is not
  # what is fragile.
  drop_one <- function(y) {
    vapply(L, function(col) {
      v <- col; names(v) <- as.character(years); mean(v[names(v) != y])
    }, numeric(1))
  }
  expect_equal(names(which.min(drop_one("2008"))), "ml")
  expect_equal(names(which.min(drop_one("2017"))), "ml")  # the biggest shock alone doesn't flip it
  expect_equal(names(which.min(drop_one("2022"))), "ml")

  # Dropping either SINGLE pivot season alone (2013 or 2021) also does not flip it -- consistent
  # with k*=2 (it takes both together, exactly matching decision_breakdown()'s reported k).
  expect_equal(names(which.min(drop_one("2013"))), "ml")
  expect_equal(names(which.min(drop_one("2021"))), "ml")

  # But dropping BOTH pivot seasons together DOES flip the champion from ml to vendor --
  # confirming k*=2's identification of (2013, 2021) was not a false alarm.
  means_drop_both <- vapply(L, function(col) {
    v <- col; names(v) <- as.character(years)
    mean(v[!(names(v) %in% c("2013", "2021"))])
  }, numeric(1))
  expect_equal(names(which.min(means_drop_both)), "vendor")
})

test_that("exact-zero loss cells (a quiet hurricane season where predicted and realized loss coincide at ~0) do not crash or silently corrupt the workflow at this small, zero-inflated T", {
  # Re-derive the same base panel, then force three individual model/season cells to a literal
  # exact 0.0 -- not just "small" (every quiet-season cell is already small/lognormal), but
  # bit-for-bit zero, the genuinely realistic case for a layer with no attachment-eroding loss
  # at all in a quiet season and a model that priced it at exactly zero expected loss too.
  years <- 2007:2024
  T <- 18L
  base_mean <- c(freq_sev = 0.018, vendor = 0.012, ml = 0.009, climatology = 0.030)
  base_cv   <- c(freq_sev = 0.45, vendor = 0.40,  ml = 0.50,  climatology = 0.35)
  set.seed(101)
  losses <- as.data.frame(sapply(names(base_mean), function(m) {
    sigma2 <- log(1 + base_cv[m]^2)
    mu <- log(base_mean[m]) - sigma2 / 2
    stats::rlnorm(T, mu, sqrt(sigma2))
  }))
  rownames(losses) <- years
  losses["2008", "freq_sev"]    <- losses["2008", "freq_sev"]    * 4.0
  losses["2008", "vendor"]      <- losses["2008", "vendor"]      * 3.0
  losses["2008", "ml"]          <- losses["2008", "ml"]          * 3.5
  losses["2008", "climatology"] <- losses["2008", "climatology"] * 2.0
  losses["2017", "freq_sev"]    <- losses["2017", "freq_sev"]    * 5.5
  losses["2017", "vendor"]      <- losses["2017", "vendor"]      * 9.0
  losses["2017", "ml"]          <- losses["2017", "ml"]          * 14.0
  losses["2017", "climatology"] <- losses["2017", "climatology"] * 6.0
  losses["2022", "freq_sev"]    <- losses["2022", "freq_sev"]    * 3.0
  losses["2022", "vendor"]      <- losses["2022", "vendor"]      * 2.0
  losses["2022", "ml"]          <- losses["2022", "ml"]          * 2.2
  losses["2022", "climatology"] <- losses["2022", "climatology"] * 2.5

  losses["2009", "ml"]     <- 0  # a quiet season ml priced exactly right
  losses["2014", "vendor"] <- 0  # a quiet season vendor priced exactly right
  losses["2020", "ml"]     <- 0  # a second exact-zero cell for the same model, different season

  panel <- loss_panel(losses, labels = years)
  expect_true(all(is.finite(as.matrix(losses))))

  expect_warning(
    rpt <- report(panel, alpha = 0.10, full = TRUE),
    "T=18 periods for K=4 models"
  )
  # No NaN/Inf leaked into the printed report, and the champion/verdict structure survives
  # exact zeros intact -- the zero-handling already confirmed clean for small-but-nonzero
  # lognormal losses in the earlier reserving test extends to literal zeros at this smaller T.
  expect_false(grepl("NaN|Inf|NA", rpt))
  expect_match(rpt, "ml +mean=0\\.0[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")

  db <- decision_breakdown(panel)
  expect_true(is.finite(db$k) && db$k >= 1L)
  expect_identical(db$binding_opponent, "vendor")

  rr <- resolution_report(panel, alpha = 0.05, power = 0.80)
  expect_true(is.finite(rr$observed_edge) && is.finite(rr$mde))
  expect_false(rr$resolved)
})