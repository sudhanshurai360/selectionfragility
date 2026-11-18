# REALISTIC DOMAIN TEST: university institutional-research (IR) enrollment-forecast model
# selection, reported to a board of trustees for budget planning.
#
# BUSINESS CONTEXT: a university's IR office compares 4 competing total-enrollment forecast
# models over 18 academic years (2007-2024): a demographic-cohort-flow model (tracks the
# slow-moving birth-rate-cohort wave through the school-age/college-age pipeline well, but has
# no mechanism for a genuinely novel shock), a simple trend-extrapolation baseline (naive,
# slow to adapt to any regime change), a regression model with economic/policy covariates
# (explicitly encodes the 2015 tuition-policy change, so it tracks that break well), and an ML
# model (best average one-step-ahead accuracy in ordinary years, but -- realistically -- no
# training precedent for a true black-swan disruption). Loss is squared one-step-ahead
# forecast error in total headcount.
#
# WHY T=18 IS THE PERMANENT NORMAL, NOT AN EDGE CASE, HERE (a THIRD distinct small-T mechanism
# from the agriculture/actuarial-reserving panels already tested elsewhere in this test suite):
# enrollment is forecast once per academic year. There is no way to observe more than one
# genuine enrollment cycle per year -- unlike a market or sensor panel, higher-frequency
# sampling cannot manufacture more independent observations. 18 years of institutional history
# is already a LONG back-test by IR standards; most offices have far less.
#
# DEMOGRAPHIC-TREND STRUCTURE: `cohort_cycle` below is a slow sinusoidal wave (period ~11 years)
# standing in for a real birth-rate cohort moving through the K-12-to-college pipeline -- genuine,
# slow-moving autocorrelation distinct from macro/credit-risk business-cycle correlation (already
# covered by this package's test-realworld-macro.R).
#
# DISCRETE POLICY-DRIVEN STRUCTURAL BREAKS: a permanent tuition-policy-change step from 2015
# onward (`tuition_break`), and a COVID-driven two-year enrollment disruption in 2020-2021
# (`covid_shock`) plus a partial-recovery dip in 2022 -- exactly the kind of discrete break this
# domain (and the use case's framing) calls for, distinct from a slow demographic trend.
#
# BOARD-OF-TRUSTEES AUDIENCE: this is a genuinely different COMMUNICATION context from the other
# small-T domains already tested (actuaries/analysts internally vs. a non-technical governance
# board). The assertions below confirm the small-T warning fires (T/K=4.5 here), and the finding
# section documents how well its wording translates to that audience.
test_that("realistic 18-year university enrollment panel: report() flags the champion's edge as unresolved and COVID-driven", {
  years <- 2007:2024
  T <- length(years)

  baseline <- 12000 + seq(0, 600, length.out = T)
  cohort_cycle <- 300 * sin(2 * pi * (years - 2007) / 11)
  tuition_break <- ifelse(years >= 2015, -350, 0)
  covid_shock <- ifelse(years %in% c(2020, 2021), -900, 0)
  covid_recovery <- ifelse(years == 2022, -250, 0)
  true_enroll <- baseline + cohort_cycle + tuition_break + covid_shock + covid_recovery

  set.seed(42)
  cohort_err <- stats::rnorm(T, 0, 60)
  cohort_err[years %in% c(2020, 2021)] <- cohort_err[years %in% c(2020, 2021)] + c(-700, -500)
  cohort_fcst <- true_enroll + cohort_err

  trend_err <- stats::rnorm(T, 0, 90)
  trend_err[years >= 2015] <- trend_err[years >= 2015] + 150
  trend_err[years %in% c(2020, 2021)] <- trend_err[years %in% c(2020, 2021)] - 850
  trend_fcst <- true_enroll + trend_err

  reg_err <- stats::rnorm(T, 0, 70)
  reg_err[years >= 2015] <- reg_err[years >= 2015] - 20
  reg_err[years %in% c(2020, 2021)] <- reg_err[years %in% c(2020, 2021)] - 400
  reg_fcst <- true_enroll + reg_err

  ml_err <- stats::rnorm(T, 0, 50)
  ml_err[years %in% c(2020, 2021)] <- ml_err[years %in% c(2020, 2021)] - c(1400, 1100)
  ml_fcst <- true_enroll + ml_err

  models <- list(
    cohort_flow = (cohort_fcst - true_enroll)^2,
    trend_baseline = (trend_fcst - true_enroll)^2,
    regression_covariates = (reg_fcst - true_enroll)^2,
    ml_model = (ml_fcst - true_enroll)^2
  )

  # loss_panel() ------------------------------------------------------------------------
  panel <- loss_panel(models, labels = years)
  expect_equal(length(panel$labels), 18L)
  expect_setequal(names(panel$losses), c("cohort_flow", "trend_baseline", "regression_covariates", "ml_model"))

  # report() ------------------------------------------------------------------------------
  # T/K = 18/4 = 4.5 is well under the package's own T/K<10 small-sample threshold: the
  # small-T warning MUST fire, worded in a way a board-facing reader has to be able to parse.
  expect_warning(
    rpt <- report(panel, alpha = 0.10),
    "small-sample regime"
  )
  rpt_txt <- as.character(rpt)

  # The regression-with-covariates model (it alone was built to anticipate BOTH the 2015
  # tuition break and partially the COVID shock) is the pooled-mean champion over all 18 years,
  # even though the raw ML model wins more INDIVIDUAL years (see periods_won below) -- a
  # realistic "the flashy model wins most years but loses on the one year that matters" story,
  # exactly the kind of thing a board needs flagged rather than buried in an average.
  expect_match(rpt_txt, "regression_covariates .*\\(champion\\)")

  # MCS does not single out one winner at T=18/K=4: this is a genuinely NOT IDENTIFIED panel,
  # i.e. the board should be told the ranking cannot be trusted outright.
  expect_match(rpt_txt, "VERDICT: not identified \\(MCS size 4 of 4\\)")

  # RESOLUTION: the observed edge is nowhere near the minimum detectable edge at 80% power --
  # 18 annual observations is simply not enough data to call this margin real.
  expect_match(rpt_txt, "cannot determine at this sample size")

  # PIVOT: decision_breakdown()'s k* must name the COVID years (2020, 2021) specifically as
  # the periods whose removal flips the pooled ranking -- confirming the registrar's suspicion
  # that the champion's apparent edge is a COVID artifact, not a general planning-year result.
  expect_match(rpt_txt, "k\\*=2 of 18 periods \\(2020, 2021\\)")
  expect_match(rpt_txt, "fragility read UNDETERMINED")
})

test_that("decision_breakdown() on the enrollment panel: COVID years (2020-2021) are exactly k*", {
  years <- 2007:2024
  T <- length(years)
  baseline <- 12000 + seq(0, 600, length.out = T)
  cohort_cycle <- 300 * sin(2 * pi * (years - 2007) / 11)
  tuition_break <- ifelse(years >= 2015, -350, 0)
  covid_shock <- ifelse(years %in% c(2020, 2021), -900, 0)
  covid_recovery <- ifelse(years == 2022, -250, 0)
  true_enroll <- baseline + cohort_cycle + tuition_break + covid_shock + covid_recovery

  set.seed(42)
  cohort_err <- stats::rnorm(T, 0, 60)
  cohort_err[years %in% c(2020, 2021)] <- cohort_err[years %in% c(2020, 2021)] + c(-700, -500)
  cohort_fcst <- true_enroll + cohort_err
  trend_err <- stats::rnorm(T, 0, 90)
  trend_err[years >= 2015] <- trend_err[years >= 2015] + 150
  trend_err[years %in% c(2020, 2021)] <- trend_err[years %in% c(2020, 2021)] - 850
  trend_fcst <- true_enroll + trend_err
  reg_err <- stats::rnorm(T, 0, 70)
  reg_err[years >= 2015] <- reg_err[years >= 2015] - 20
  reg_err[years %in% c(2020, 2021)] <- reg_err[years %in% c(2020, 2021)] - 400
  reg_fcst <- true_enroll + reg_err
  ml_err <- stats::rnorm(T, 0, 50)
  ml_err[years %in% c(2020, 2021)] <- ml_err[years %in% c(2020, 2021)] - c(1400, 1100)
  ml_fcst <- true_enroll + ml_err

  models <- list(
    cohort_flow = (cohort_fcst - true_enroll)^2,
    trend_baseline = (trend_fcst - true_enroll)^2,
    regression_covariates = (reg_fcst - true_enroll)^2,
    ml_model = (ml_fcst - true_enroll)^2
  )
  panel <- loss_panel(models, labels = years)

  db <- decision_breakdown(panel$losses, panel$weights, a = "regression_covariates")
  # k*=2: deleting exactly 2 periods flips the pooled ranking.
  expect_equal(db$k, 2L)
  # Those 2 periods are precisely the COVID years -- not some other pair -- confirming the
  # board's specific question ("is this just COVID?") has a precise, checkable answer: yes.
  expect_equal(sort(as.character(years[db$removed])), c("2020", "2021"))
  # The model that becomes champion once those periods are deleted is the ml_model -- the one
  # whose COVID-year losses are catastrophic, i.e. the one COVID is suppressing from winning.
  expect_equal(db$binding_opponent, "ml_model")

  # Directly confirm the ranking flip by rebuilding the panel with COVID years excluded: the
  # champion changes from regression_covariates to ml_model, demonstrating the board's "is this
  # ranking a COVID artifact" concern is correct, not a false alarm -- the whole-panel ranking is
  # entirely contingent on 2 of 18 years.
  keep <- !(years %in% c(2020, 2021))
  models_no_covid <- lapply(models, function(v) v[keep])
  panel_no_covid <- loss_panel(models_no_covid, labels = years[keep])
  expect_equal(pooled_winner(panel$losses, panel$weights), "regression_covariates")
  expect_equal(pooled_winner(panel_no_covid$losses, panel_no_covid$weights), "ml_model")
})

test_that("resolution_report()'s power check correctly refuses to call the enrollment-panel edge resolved at T=18", {
  years <- 2007:2024
  T <- length(years)
  baseline <- 12000 + seq(0, 600, length.out = T)
  cohort_cycle <- 300 * sin(2 * pi * (years - 2007) / 11)
  tuition_break <- ifelse(years >= 2015, -350, 0)
  covid_shock <- ifelse(years %in% c(2020, 2021), -900, 0)
  covid_recovery <- ifelse(years == 2022, -250, 0)
  true_enroll <- baseline + cohort_cycle + tuition_break + covid_shock + covid_recovery

  set.seed(42)
  cohort_err <- stats::rnorm(T, 0, 60)
  cohort_err[years %in% c(2020, 2021)] <- cohort_err[years %in% c(2020, 2021)] + c(-700, -500)
  cohort_fcst <- true_enroll + cohort_err
  trend_err <- stats::rnorm(T, 0, 90)
  trend_err[years >= 2015] <- trend_err[years >= 2015] + 150
  trend_err[years %in% c(2020, 2021)] <- trend_err[years %in% c(2020, 2021)] - 850
  trend_fcst <- true_enroll + trend_err
  reg_err <- stats::rnorm(T, 0, 70)
  reg_err[years >= 2015] <- reg_err[years >= 2015] - 20
  reg_err[years %in% c(2020, 2021)] <- reg_err[years %in% c(2020, 2021)] - 400
  reg_fcst <- true_enroll + reg_err
  ml_err <- stats::rnorm(T, 0, 50)
  ml_err[years %in% c(2020, 2021)] <- ml_err[years %in% c(2020, 2021)] - c(1400, 1100)
  ml_fcst <- true_enroll + ml_err

  models <- list(
    cohort_flow = (cohort_fcst - true_enroll)^2,
    trend_baseline = (trend_fcst - true_enroll)^2,
    regression_covariates = (reg_fcst - true_enroll)^2,
    ml_model = (ml_fcst - true_enroll)^2
  )
  panel <- loss_panel(models, labels = years)

  res <- suppressWarnings(resolution_report(panel$losses, panel$weights))

  # The champion is regression_covariates, matching report()'s LEADERBOARD above.
  expect_equal(res$champion, "regression_covariates")

  # The observed edge over the binding rival is real in direction (positive) but is NOT
  # resolved: 18 annual loss observations is not enough statistical power at this margin for
  # the registrar to tell the board "this edge is reliably real," only "this edge is observed."
  expect_false(res$resolved)
  expect_true(is.finite(res$observed_edge))
  expect_gt(res$observed_edge, 0)
  # The minimum detectable edge at 80% power is substantially larger than what was observed --
  # the honest power-check message a board needs, in contrast to a naive "the mean is lower"
  # claim that ignores sample-size-driven uncertainty entirely.
  expect_gt(res$mde, res$observed_edge)
  expect_identical(res$fragility_read, "undetermined")

  # identified() independently refuses to call the champion uniquely best at this T/K -- R1
  # (power) and R2 (MCS identification) agree here that the decision is not settled.
  expect_false(isTRUE(res$identified))
})
