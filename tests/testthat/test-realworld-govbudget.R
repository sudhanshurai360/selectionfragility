# test-realworld-govbudget.R -- a GENUINELY REALISTIC state-legislative-fiscal-office general-fund
# REVENUE FORECASTING use case, kept as a PERMANENT regression test. This is a distinct domain
# from macro GDP-nowcasting (test-realworld-macro.R) and corporate credit risk: it is ANNUAL,
# NOT quarterly/high-frequency (state budget offices re-forecast once a fiscal year, sometimes with
# a mid-year revision -- not monthly/daily vintages), it has a SHORTER real history (T=20-40 years
# is "a lot" here, not "barely enough" -- most state revenue-estimating offices' usable comparable
# series only go back to the 1980s-90s), and its autocorrelation structure is driven by
# INSTITUTIONAL/POLICY LAG as much as by business-cycle persistence: property tax is deliberately
# reassessed on a multi-year cycle (by policy design, not by behavioral momentum), so a
# property-tax-heavy forecasting model's errors lag the business cycle by design, not by accident.
#
# Context for readers unfamiliar with this domain:
#   - A STATE LEGISLATIVE FISCAL OFFICE (e.g. a nonpartisan legislative fiscal bureau/analyst's
#     office) is asked, once a year, which of several competing general-fund revenue forecasting
#     models should anchor next year's budget. This is a real, recurring, board-presentable
#     decision with asymmetric real-world stakes this package has no concept of but that a
#     practitioner must supply when reading its output: an OVER-optimistic forecast leads to a
#     mid-year revenue shortfall and painful mid-year budget cuts; an OVER-conservative forecast
#     artificially constrains appropriations for public services (schools, Medicaid, public safety)
#     that could otherwise have been funded. This package's loss function (whatever error metric
#     the office chose -- e.g. absolute or squared percent forecast error) typically treats both
#     directions symmetrically, so a "winning" model by pooled mean loss is not automatically the
#     right model to anchor a budget on without separately asking which direction its errors run.
#   - FOUR COMPETING MODELS, in this spirit:
#       trend_seasonal  -- a simple trend-plus-seasonal baseline: cheap, steady in ordinary years,
#                          but has no cyclical-indicator information, so it is blindsided by a
#                          sharp recession/rebound turning point in income- and sales-tax revenue.
#       econ_indicator  -- a regression on leading economic indicators (employment, wages): absorbs
#                          most of a cyclical shock's hit to income/sales tax revenue faster than
#                          trend_seasonal, because it ingests the same employment/wage signals that
#                          drive those revenue sources contemporaneously.
#       proptax_heavy   -- a model that weights its jurisdiction's property-tax revenue stream
#                          heavily. Property tax is DELIBERATELY SMOOTHED/LAGGED BY POLICY DESIGN
#                          (assessments are only updated every 1-3 years in most US states, not
#                          continuously), so this model looks deceptively GOOD in the recession
#                          year itself (assessed values haven't caught up yet) but takes its real
#                          hit 1-2 years LATER, when delayed reassessments finally reflect the
#                          recession/rebound just as the rest of the economy has already moved on.
#                          This creates a genuinely different autocorrelation/lag-timing signature
#                          from the other three models' same-year shock response -- an
#                          INSTITUTIONAL lag, not a behavioral/momentum one.
#       ml_ensemble     -- a flexible ML ensemble: good average performance across the panel.
#   - TWO REAL, DATED STRUCTURAL BREAKS: the 2008-09 Global Financial Crisis (modeled here as
#     FY2008-FY2010, since state revenue impacts and the associated forecasting misses typically
#     run through the first budget cycle AFTER the acute national recession) and the 2020
#     pandemic-driven revenue shock (FY2020-FY2021, capturing both the initial shock and the
#     unusually sharp federal-stimulus-driven rebound that caught many state revenue estimators by
#     surprise in the OTHER direction).
#
# These tests were built by first running this exact scenario interactively against the package
# (`devtools::load_all()` -> `loss_panel()` -> `report()`/`decision_breakdown()`/`fragility()`/
# `compare()`) with a fixed seed to find a realistic, reproducible scenario. Two real, confirmed
# findings on this panel are pinned down permanently below:
#
#   FINDING 1 (report() on a panel that is GENUINELY both small-T AND autocorrelated): this
#   domain's realistic T (25 annual fiscal years, T/K=6.25 for 4 models) is well below this
#   package's own small-sample regime threshold (T/K < 10), AND its loss differentials are
#   genuinely autocorrelated (lag-1 ACF > 0.3 on the champion-vs-binding-rival differential, driven
#   by both ordinary business-cycle persistence and proptax_heavy's institutional reassessment lag)
#   -- i.e. BOTH of this package's own documented interpretation risks are simultaneously, actually
#   present, not merely hypothetical. A single `report(panel)` call on this panel DOES surface both:
#   the small-T warning() fires (T/K=6.25 < 10), AND the one-screen printed text always carries the
#   static "RESOLUTION assumes no autocorrelation... NOT checked at runtime" disclosure. For a
#   legislative-staff (not purely-technical) reader, this reads sensibly together -- the warning
#   names the exact numeric risk (false-elimination rate), and the printed disclosure names the
#   exact assumption being made, with neither one silently relying on the other to carry the point.
#
#   FINDING 2 (decision_breakdown()/fragility() -- the ranking's fragility is NOT simply "the crisis
#   years drive it"): on this panel, the pooled-mean champion is ml_ensemble, and its k*=1 pivotal
#   period is FY2022 -- an ORDINARY, non-crisis year, not FY2008-10 or FY2020-21. Excluding the
#   GFC years alone, or excluding BOTH crisis windows jointly, leaves ml_ensemble the champion;
#   excluding ONLY the COVID window flips the champion to econ_indicator. A legislative fiscal
#   office that assumed "our model ranking is just an artifact of the pandemic shock years" would
#   be concretely wrong in both directions: the pandemic years are not what k*=1 is pointing at,
#   and the crisis-driven sensitivity that does exist does not decompose simply into "GFC" and
#   "COVID" contributing independently. Separately, `fragility()`'s selection-as-election view adds
#   a genuinely board-relevant wrinkle: the pooled (score) winner (ml_ensemble) is NOT the
#   per-period (plurality) winner OR the Condorcet (pairwise-majority) winner -- both of those
#   name econ_indicator instead -- so which voting rule the office presents to the board changes
#   the headline answer, on the identical data.
#
#   FINDING 3 (compare() across a realistic "next fiscal year" re-evaluation): re-running the
#   comparison after FY2024's actual revenue outturn becomes available (one additional year beyond
#   the FY2023 cut the office would have last checked) does NOT flip the champion and correctly
#   reports act=FALSE -- a genuinely reassuring, non-overclaiming signal for a board presentation
#   ("one more year of data did not change our recommendation").

# Build the shared T=25 annual fiscal-year skeleton (FY2000-FY2024) and its two real structural-
# break masks, plus proptax_heavy's distinctive ONE-YEAR-LAGGED crisis response.
.govbudget_years <- function() 2000:2024

# A simple AR(1) generator for the per-period noise -- NOT i.i.d. rnorm() -- consistent with real
# forecast-loss series being serially dependent rather than independent across fiscal years.
.govbudget_ar1 <- function(n, rho, sd_innov) {
  e <- rnorm(n, 0, sd_innov)
  x <- numeric(n)
  x[1] <- e[1] / sqrt(1 - rho^2)
  for (t in 2:n) x[t] <- rho * x[t - 1] + e[t]
  x
}

# Shifts a logical crisis mask forward by one period -- used to model proptax_heavy's
# institutional (policy-driven) reassessment lag: its real loss hit lands the YEAR AFTER the
# shock, not during it.
.govbudget_lag1 <- function(mask, Tn) {
  m <- rep(FALSE, Tn)
  idx <- which(mask)
  m[pmin(idx + 1, Tn)] <- TRUE
  m
}

.build_govbudget_panel <- function(seed = 42) {
  years <- .govbudget_years()
  Tn <- length(years)
  gfc <- years %in% 2008:2010     # 2008-09 GFC + the first lagged state-budget-cycle aftermath
  covid <- years %in% 2020:2021   # 2020 pandemic shock + its unusually sharp fiscal-year rebound
  crisis <- gfc | covid

  set.seed(seed)
  loss_trend <- 0.60 + .govbudget_ar1(Tn, 0.3, 0.04)
  loss_econ  <- 0.55 + .govbudget_ar1(Tn, 0.3, 0.04)
  loss_prop  <- 0.58 + .govbudget_ar1(Tn, 0.5, 0.03)  # higher rho: institutional lag persists longer
  loss_ml    <- 0.52 + .govbudget_ar1(Tn, 0.3, 0.045)

  # Same-year crisis response: trend_seasonal (no cyclical info) is hurt worst, econ_indicator
  # (ingests employment/wage signals contemporaneously) is hurt least among the three "normal"
  # models.
  loss_trend[gfc] <- loss_trend[gfc] + 1.1
  loss_econ[gfc]  <- loss_econ[gfc]  + 0.4
  loss_ml[gfc]    <- loss_ml[gfc]    + 0.5
  loss_trend[covid] <- loss_trend[covid] + 1.3
  loss_econ[covid]  <- loss_econ[covid]  + 0.5
  loss_ml[covid]    <- loss_ml[covid]    + 0.45

  # proptax_heavy's DISTINCTIVE institutional lag: a small same-year bump (assessments haven't
  # caught up yet), but a much larger bump ONE YEAR LATER, when delayed reassessments finally
  # reflect the recession/rebound -- a genuinely different timing signature from the other three
  # models' same-year shock response, driven by policy design (the reassessment cycle), not by
  # behavioral momentum.
  loss_prop[gfc]   <- loss_prop[gfc]   + 0.2
  loss_prop[covid] <- loss_prop[covid] + 0.2
  loss_prop[.govbudget_lag1(gfc, Tn)]   <- loss_prop[.govbudget_lag1(gfc, Tn)]   + 1.4
  loss_prop[.govbudget_lag1(covid, Tn)] <- loss_prop[.govbudget_lag1(covid, Tn)] + 1.2

  L <- list(trend_seasonal = loss_trend, econ_indicator = loss_econ,
            proptax_heavy = loss_prop, ml_ensemble = loss_ml)
  list(L = L, years = years, gfc = gfc, covid = covid, crisis = crisis,
       panel = loss_panel(L, labels = years))
}

test_that("realistic T=25 annual state general-fund revenue panel is BOTH a small-T regime AND genuinely autocorrelated, and report() surfaces both", {
  built <- .build_govbudget_panel(seed = 42)
  panel <- built$panel
  L <- built$L

  # K=4 models, T=25 fiscal years -- T/K=6.25, below this package's own small-sample threshold
  # (T/K < 10), matching a real state legislative fiscal office's realistic comparable history.
  expect_identical(length(names(panel$losses)), 4L)
  expect_identical(length(panel$labels), 25L)

  # Confirm the panel really is autocorrelated (not accidentally i.i.d.) before trusting the rest
  # of this file's story: lag-1 ACF of the champion-vs-binding-rival differential should be
  # clearly positive, consistent with both ordinary cyclical persistence and proptax_heavy's
  # institutional reassessment lag feeding into these loss series.
  diffseries <- L$ml_ensemble - L$econ_indicator
  acf1 <- stats::acf(diffseries, plot = FALSE)$acf[2]
  expect_gt(acf1, 0.3)

  # report() on this panel should raise the small-T warning (T/K=6.25 < the documented T/K=10
  # anchor) -- confirming this domain's realistic sample size genuinely reaches that risk, not
  # just in a contrived toy example.
  expect_warning(
    out <- report(panel, alpha = 0.10, seed = 1),
    "small-sample regime"
  )
  # ...AND, on the exact same call, the printed one-screen text ALSO carries the static
  # autocorrelation disclosure (always printed, not gated on this specific data draw) -- i.e. a
  # legislative-staff reader who reads report()'s prose (not just watches for a console warning)
  # still gets the second, independent caveat. Both risks are real on this panel, and both are
  # visible from a single ordinary call -- this is the concrete "do they read sensibly together"
  # check: the warning names the numeric small-T risk, the disclosure names the untested
  # autocorrelation assumption, and neither depends on the other to make its own point.
  expect_match(out, "RESOLUTION assumes no autocorrelation in the per-period loss differential")

  # The pooled champion on the full panel is ml_ensemble, but the MCS bootstrap at this small T
  # cannot narrow down to a singleton -- econ_indicator survives alongside it. This is the honest
  # "we have a leaderboard leader, but cannot confidently call it THE model" read a legislative
  # fiscal office needs before anchoring a budget cycle on one model's output.
  expect_identical(pooled_winner(panel$losses, panel$weights), "ml_ensemble")
  expect_match(out, "VERDICT: not identified")
  expect_match(out, "ml_ensemble.*\\(champion\\)")
})

test_that("decision_breakdown()/fragility(): the ranking's fragility is driven by an ORDINARY year, not simply 'the crisis years', and the pooled/plurality/Condorcet winners disagree", {
  built <- .build_govbudget_panel(seed = 42)
  panel <- built$panel
  L <- built$L
  years <- built$years
  gfc <- built$gfc
  covid <- built$covid
  crisis <- built$crisis

  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ml_ensemble")

  # FINDING 2a: k*=1 -- a SINGLE period's deletion flips the pooled winner -- and that period is
  # an ORDINARY, non-crisis fiscal year (2022), not one of the two real recession/shock windows.
  db <- decision_breakdown(panel$losses, panel$weights)
  expect_identical(db$k, 1L)
  expect_identical(db$binding_opponent, "econ_indicator")
  pivotal_year <- years[db$removed]
  expect_identical(pivotal_year, 2022L)
  expect_false(pivotal_year %in% years[crisis])

  # FINDING 2b: the champion does NOT simply track "with vs. without the crisis years" in the
  # naive way a legislative staffer might assume. Excluding the GFC window alone, or excluding
  # BOTH crisis windows jointly, leaves ml_ensemble on top; excluding ONLY the COVID window flips
  # the champion to econ_indicator. The fragility here is real, but it does not decompose into
  # "GFC drives it" or "COVID drives it" independently -- a board-presentable answer has to say
  # this precisely, not just "it's the pandemic years."
  champ_excl_gfc_only   <- pooled_winner(lapply(L, function(v) v[!gfc]))
  champ_excl_covid_only <- pooled_winner(lapply(L, function(v) v[!covid]))
  champ_excl_both       <- pooled_winner(lapply(L, function(v) v[!crisis]))
  expect_identical(champ_excl_gfc_only, "ml_ensemble")
  expect_identical(champ_excl_covid_only, "econ_indicator")
  expect_identical(champ_excl_both, "ml_ensemble")

  # FINDING 2c: selection-as-election disagreement. The pooled (score) winner is ml_ensemble, but
  # the per-period (plurality) winner AND the Condorcet (pairwise-majority) winner are BOTH
  # econ_indicator instead -- on the identical data. Which voting rule a legislative fiscal office
  # chooses to present changes the headline recommendation.
  frag <- fragility(panel)
  expect_identical(frag$pooled_winner, "ml_ensemble")
  expect_identical(frag$per_period_winner, "econ_indicator")
  expect_identical(frag$condorcet_winner, "econ_indicator")
  expect_true(frag$reversal)
  expect_identical(frag$k_star, 1L)
  expect_identical(frag$responsible, 2022L)

  # winner_stability (bootstrap re-sampling of fiscal years) is close to a coin flip here --
  # independent, resampling-based confirmation that this champion is genuinely fragile, not an
  # artifact of decision_breakdown()'s single-period k* alone.
  expect_true(frag$winner_stability > 0.40 && frag$winner_stability < 0.65)
})

test_that("compare(): a realistic 'next fiscal year' re-evaluation (FY2024 outturn arrives) does not flip the champion and does not overclaim 'act'", {
  built <- .build_govbudget_panel(seed = 42)
  L <- built$L
  years <- built$years

  # "previous": the office's last check, through FY2023 (24 years) -- the realistic cut a board
  # would have seen at last year's budget cycle.
  previous <- loss_panel(lapply(L, function(v) v[1:24]), labels = years[1:24])
  # "current": this year's re-evaluation, with FY2024's actual revenue outturn now available.
  current <- loss_panel(L, labels = years)

  expect_identical(pooled_winner(previous$losses, previous$weights), "ml_ensemble")
  expect_identical(pooled_winner(current$losses, current$weights), "ml_ensemble")

  # compare()'s internal MCS check runs at this same small T/K=6.25, so it raises the identical
  # small-sample-regime warning report() does above -- disclosure of a real, measured limitation
  # at this domain's realistic sample size, not a sign this call is broken.
  cr <- withCallingHandlers(
    compare(previous, current, alpha = 0.10, seed = 1),
    warning = function(w) {
      expect_match(conditionMessage(w), "small-sample regime")
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(cr$previous_champion, "ml_ensemble")
  expect_identical(cr$current_champion, "ml_ensemble")
  expect_false(cr$champion_changed)
  expect_equal(cr$n_new_periods, 1L)
  # No champion change -> nothing for a promotion/decision pipeline to act on. This is the
  # reassuring, board-presentable answer: one additional year of real outturn data did not
  # overturn last year's model choice.
  expect_false(cr$act)

  printed <- paste(capture.output(print(cr)), collapse = "\n")
  expect_match(printed, "no champion change \\(ml_ensemble\\)")
  expect_match(printed, "act=FALSE")
})
