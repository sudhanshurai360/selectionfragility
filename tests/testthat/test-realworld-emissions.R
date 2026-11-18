# Realistic corporate ESG/sustainability scope-1/2 carbon-emissions-forecasting use case.
#
# THE SCENARIO: a large corporation's sustainability/ESG reporting team compares 4 competing
# models for forecasting annual scope-1/2 carbon emissions, to decide which one to DISCLOSE
# AGAINST in next year's public sustainability report (a decision with real reputational/legal
# exposure under increasing investor and regulatory scrutiny -- e.g. SEC climate-disclosure rules,
# CSRD, greenwashing-litigation risk):
#   - bottom_up:    an activity-based, bottom-up engineering model (fuel/electricity/process data
#                   rolled up from operational records) -- noisier year to year, but tracks real
#                   activity swings (including demand shocks) reasonably well because it is built
#                   FROM activity data.
#   - topdown_econ: a top-down economic-intensity regression model (emissions ~ revenue/output) --
#                   smooth and low-noise in ordinary years, but structurally blind to a shock that
#                   decouples economic activity from emissions intensity for one or two years.
#   - ml_ops:       an ML model incorporating operational data -- the best model most years, with
#                   some (smaller) sensitivity to the same shock as topdown_econ, since it still
#                   needs retraining to fully absorb an out-of-sample regime change.
#   - trend_naive:  a simple trend-extrapolation baseline -- cheap, consistently mediocre, and
#                   worst exactly when something disrupts the trend, since it has no mechanism to
#                   detect a demand shock at all.
#
# DOMAIN CHARACTERISTIC #1 -- STRUCTURALLY SMALL T VIA A FOURTH MECHANISM: corporate/regulatory
# emissions reporting is ANNUAL (one scope-1/2 inventory per fiscal year, per GHG Protocol /
# CDP / SEC-style disclosure cadence), so a "15 years of history" comparison -- already a long
# look-back by real ESG-reporting standards -- is T=15, K=4 (T/K=3.75). Unlike this package's
# existing small-T domains (agriculture's growing-season cadence, education's academic-year
# cadence, actuarial reserving's claims-development cadence), the mechanism forcing small T here
# is purely administrative/regulatory: emissions are reported once a year because disclosure
# RULES say so, not because of a biological or claims-development cycle. T is not merely small
# today -- it is small PERMANENTLY, by construction of the reporting regime itself.
#
# DOMAIN CHARACTERISTIC #2 -- A GENUINE, WELL-DOCUMENTED COVID-19 EMISSIONS ANOMALY: global and
# corporate emissions fell sharply in 2020 (lockdowns cut industrial activity, travel, and energy
# demand) and partially rebounded in 2021-2022 without fully returning to the pre-COVID trend
# immediately (a widely-studied pattern -- see e.g. Le Quere et al. 2020, Nature Climate Change,
# "Temporary reduction in daily global CO2 emissions during the COVID-19 forced confinement", and
# the IEA's Global Energy Review 2021 rebound estimates). This is a real INDUSTRIAL-ACTIVITY
# structural break, distinct in cause from every other COVID scenario already covered by this
# package's test suite (disease spread, travel demand, enrollment).
#
# DOMAIN CHARACTERISTIC #3 -- REAL STAKES FOR "WHICH MODEL TO DISCLOSE AGAINST": picking the
# wrong forecast model for a public ESG disclosure is not a purely statistical question once it's
# published -- a materially wrong forward emissions number invites investor/regulator scrutiny
# and greenwashing-litigation exposure, so the sustainability team needs a DEFENSIBLE, AUDITOR-
# FACING answer, not just a leaderboard winner.
#
# This test builds that panel, runs the full report() -> decision_breakdown() -> resolution_report()
# workflow, and specifically interrogates whether the COVID anomaly year is what makes the
# champion's selection fragile (k*-pivotal) -- the central question a sustainability team worried
# about COVID-driven distortion would actually ask.

set.seed(2026)
.T_EMI <- 15L
.year_e <- 2010:2024
.covid_idx_e <- which(.year_e == 2020)  # position 11 -- the COVID-19 shock year

.build_emissions_panel <- function() {
  bottom_up    <- abs(rnorm(.T_EMI, 3.0, 1.0))
  topdown_econ <- abs(rnorm(.T_EMI, 2.5, 0.8))
  ml_ops       <- abs(rnorm(.T_EMI, 2.0, 0.7))
  trend_naive  <- abs(rnorm(.T_EMI, 4.0, 1.2))

  # 2020: industrial-activity collapse. topdown_econ and trend_naive (no activity-shock mechanism)
  # miss badly; bottom_up (built from real activity data) and ml_ops (partial operational signal)
  # are hit much less hard -- the realistic, documented asymmetry in who handles a COVID-style
  # emissions anomaly well.
  bottom_up[.covid_idx_e]    <- bottom_up[.covid_idx_e]    + 2.0
  topdown_econ[.covid_idx_e] <- topdown_econ[.covid_idx_e] + 14.0
  ml_ops[.covid_idx_e]       <- ml_ops[.covid_idx_e]       + 4.5
  trend_naive[.covid_idx_e]  <- trend_naive[.covid_idx_e]  + 16.0

  # 2021: partial rebound, still a smaller miss for the same models (the documented "not fully
  # back to trend immediately" rebound pattern).
  bottom_up[.covid_idx_e + 1]    <- bottom_up[.covid_idx_e + 1]    + 1.0
  topdown_econ[.covid_idx_e + 1] <- topdown_econ[.covid_idx_e + 1] + 7.0
  ml_ops[.covid_idx_e + 1]       <- ml_ops[.covid_idx_e + 1]       + 2.5
  trend_naive[.covid_idx_e + 1]  <- trend_naive[.covid_idx_e + 1]  + 9.0

  list(bottom_up = bottom_up, topdown_econ = topdown_econ, ml_ops = ml_ops, trend_naive = trend_naive)
}

.emi_L <- .build_emissions_panel()
.emi_panel <- loss_panel(.emi_L, labels = .year_e)  # equal weights -- the ordinary ESG-reporting
                                                      # workflow has no reason to down/up-weight
                                                      # any single disclosure year

test_that("the panel is well-formed and ml_ops is the pooled champion", {
  expect_equal(.emi_panel$models, c("bottom_up", "topdown_econ", "ml_ops", "trend_naive"))
  expect_equal(length(.emi_panel$labels), .T_EMI)
  expect_equal(pooled_winner(.emi_L, rep(1, .T_EMI)), "ml_ops")
  # The COVID-blind models are visibly worse on the pooled mean, as a real ESG team would see on
  # a first look at the leaderboard.
  means <- sapply(.emi_L, mean)
  expect_true(means[["ml_ops"]] < means[["topdown_econ"]])
  expect_true(means[["ml_ops"]] < means[["trend_naive"]])
})

test_that(
  paste(
    "report() runs cleanly on the T=15 annual emissions panel, names ml_ops champion, and its",
    "small-T disclosure fires (T/K=3.75 is well inside the documented small-sample regime) --",
    "exactly the structurally-permanent small-T situation an annual regulatory reporting cadence",
    "produces, distinct in MECHANISM from every other small-T domain this suite covers"
  ), {
  warn_msgs <- character(0)
  r <- withCallingHandlers(
    report(.emi_panel, seed = 0),
    warning = function(w) { warn_msgs <<- c(warn_msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  expect_s3_class(r, "selectionfragility_report")
  txt <- unclass(r)
  expect_match(txt, "ml_ops.*\\(champion\\)")
  expect_match(txt, "RESOLUTION")
  expect_match(txt, "PIVOT")

  # The small-T warning fires, naming the true T/K=3.75 of this panel.
  expect_true(any(grepl("small-sample regime", warn_msgs, fixed = TRUE)))
  small_t_msg <- warn_msgs[grepl("small-sample regime", warn_msgs, fixed = TRUE)][1]
  expect_match(small_t_msg, "T=15 periods for K=4 models", fixed = TRUE)

  # AUDITOR/REGULATORY-DISCLOSURE AUDIENCE CHECK (observation, not a fix): the warning's own
  # wording is phrased for a quantitatively literate reader ("false-elimination rate", "nominal
  # alpha", "bootstrap", "MCS size"), not for the auditor-facing, plain-language register this
  # specific use case needs ("can we defend this choice of model to an auditor or regulator").
  # Confirmed directly: none of the auditor-relevant framing words appear anywhere in the
  # message, even though the warning's SUBSTANCE (small-T unreliability) is exactly what an
  # auditor would need translated into disclosure language. This is a genuine communication-
  # register gap for THIS audience, flagged for human review rather than "fixed" here, since the
  # same wording is correct and necessary for the statistically literate caller the package
  # otherwise targets.
  expect_false(grepl("auditor|disclos|regulat", small_t_msg, ignore.case = TRUE))
  expect_true(grepl("bootstrap|false-elimination|nominal alpha", small_t_msg))
})

test_that(
  paste(
    "FINDING: the COVID-19 emissions anomaly does NOT drive k* for the actual, close selection",
    "decision (ml_ops vs its true binding rival, bottom_up) -- both models absorbed the COVID",
    "shock reasonably well, so the real competition between them is decided by ordinary,",
    "non-COVID years. This is the honest, nuanced answer a sustainability team worried about",
    "'is our pick just a COVID-distorted artifact' actually needs, not a reflexive yes/no."
  ), {
  w1 <- rep(1, .T_EMI)
  champ <- pooled_winner(.emi_L, w1)
  expect_equal(champ, "ml_ops")

  db <- decision_breakdown(.emi_L, w1, a = champ)
  expect_equal(db$binding_opponent, "bottom_up")
  expect_true(db$k > 0 && db$k < .T_EMI)  # a genuine, non-trivial, non-bulletproof pivotal set

  removed_years <- .year_e[db$removed]
  # Neither the COVID shock year (2020) nor its rebound year (2021) is among the k*-pivotal
  # periods for the CHAMPION-VS-TRUE-RIVAL pair: the decision that actually matters for next
  # year's disclosure is not COVID-driven at all.
  expect_false(2020 %in% removed_years)
  expect_false(2021 %in% removed_years)
})

test_that(
  paste(
    "CONTRAST: against the COVID-BLIND rival (topdown_econ), the COVID shock year and its",
    "rebound ARE among the first periods breakdown_number() would need to remove to flip that",
    "pairwise comparison -- confirming the anomaly is real and material for models that cannot",
    "see it, even though (per the previous test) it is irrelevant to the actual champion-vs-",
    "closest-rival decision. The package correctly distinguishes these two questions."
  ), {
  w1 <- rep(1, .T_EMI)
  bn <- breakdown_number(la = .emi_L$ml_ops, lb = .emi_L$topdown_econ, w = w1)
  removed_years_bn <- .year_e[bn$removed]
  # 2020 (the shock) and 2021 (the rebound) are both among the earliest-removed, highest-margin
  # periods in this pairing -- i.e. among the periods that most help ml_ops beat topdown_econ.
  expect_true(2020 %in% removed_years_bn)
  expect_true(2021 %in% removed_years_bn)
  # But topdown_econ is so far behind ml_ops overall that it takes removing MOST of the panel
  # (not just the two COVID years) to flip even this pairwise comparison -- ml_ops's edge over
  # topdown_econ is not merely "we got lucky in the COVID years," it is broad and robust.
  expect_gt(bn$k, .T_EMI / 2)
})

test_that(
  paste(
    "resolution_report()'s power check at T=15: the observed edge is far below the minimum",
    "detectable edge at 80% power, so the champion-vs-rival comparison is formally UNRESOLVED --",
    "the honest, auditor-relevant conclusion is that 15 years of ANNUAL disclosure data cannot",
    "statistically settle this choice, however defensible ml_ops looks on the leaderboard"
  ), {
  w1 <- rep(1, .T_EMI)
  rr <- withCallingHandlers(
    resolution_report(.emi_L, w1),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_equal(rr$champion, "ml_ops")
  expect_equal(rr$binding_rival, "bottom_up")
  expect_false(rr$resolved)
  expect_identical(rr$fragility_read, "undetermined")
  # The MDE required for 80% power is a large multiple of the actually-observed edge: with only
  # T=15 annual observations, the power check cannot resolve a champion-vs-rival margin this
  # small. This is a structural consequence of annual reporting cadence, not a flaw in this
  # particular panel's data.
  expect_true(is.finite(rr$mde) && is.finite(rr$observed_edge))
  expect_gt(rr$mde, rr$observed_edge * 3)

  # Lengthening the effective sample (as if the SAME annual cadence had instead produced T=30
  # years of history) markedly tightens the detectable edge -- quantifying how much annual-only
  # reporting cadence costs this decision in resolving power, without in any way claiming T=30
  # years of real disclosure history will ever exist for most reporting entities.
  mde_t15 <- minimum_detectable_edge(.emi_L, w1)
  L_t30 <- lapply(.emi_L, function(x) rep(x, 2))
  mde_t30 <- minimum_detectable_edge(L_t30, rep(w1, 2))
  expect_lt(mde_t30, mde_t15)
})
