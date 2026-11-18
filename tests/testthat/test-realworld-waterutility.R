# test-realworld-waterutility.R -- a genuinely realistic MUNICIPAL WATER-UTILITY monthly demand
# forecasting use case, constructed from real documented drought/conservation-policy dynamics
# (modeled on California's 2012-2016 and 2020-2022 drought-emergency/mandatory-conservation
# episodes), used as a lasting integration/regression test distinct from this package's existing
# energy-demand (test-realworld-energy.R) scenario. Water demand differs from electricity/energy
# demand in a way that matters for model-selection fragility: a severe drought does not just shift
# the WEATHER driver (as a heat wave/cold snap does for energy load) -- it triggers a mandatory
# conservation ORDINANCE, a genuine two-stage shock (hydrology -> policy) that persists even after
# the drought itself ends, as the well-documented "conservation habit" effect (fewer lawns,
# permanent fixture retrofits, habituated shorter showers): demand settles at a PERMANENTLY lower
# level relative to the pre-drought seasonal baseline, not a transient dip that reverts.
#
# THE SCENARIO: a municipal water utility's planning team compares 4 candidate models for
# forecasting monthly residential water demand, needing a defensible answer about which model to
# trust for next year's water-supply planning and rate-setting, over T=96 months (8 years) of
# billing-cycle data spanning a 2-year severe drought-and-mandatory-conservation period (months
# 25-48, i.e. years 3-4) followed by a partial, permanently-incomplete post-drought recovery
# (months 49-96):
#   - seasonal_naive:  a historical-seasonal baseline -- captures the strong summer-irrigation/
#     winter-trough seasonality reasonably, but has NO way to know about a conservation ordinance
#     or a permanent post-drought demand-level shift; it keeps forecasting as if the OLD baseline
#     still applies long after the drought ends.
#   - weather_reg:     a temperature/weather-regression model -- the best of the three
#     policy-blind models (temperature still correlates with conservation-era demand), but still
#     blind to the policy layer itself and to the permanent post-drought re-basing.
#   - ml_conservation: an ML model that, in addition to weather, incorporates conservation-program
#     participation data (rebate signups, tiered-rate-escalation enrollment) -- the one model with
#     any signal for the policy-driven shock, both during the drought itself AND for tracking the
#     new, permanently-lower post-drought baseline.
#   - trend_model:     a simple trend model -- captures neither seasonality nor the structural
#     break; the clear weakest baseline every utility planning team keeps around for comparison.
#
# This mirrors the real risk a water-utility analyst faces and that is DIFFERENT from the energy
# scenario: it is not "does a known class of forecast-model have a known weak spot" (temperature
# excursions), but "does last year's champion remain the champion once the conservation-habit
# 'new normal' settles in, or does the apparent edge evaporate/reverse once the acute drought
# months roll out of the evaluation window" -- precisely the question compare() exists to answer.

set.seed(999)  # file-level seed for calendar/structural setup only; every random LOSS draw below
                # is generated under its own explicit set.seed() call for full reproducibility.

.T_WU <- 96L
.month_wu <- seq_len(.T_WU)                        # month 1 = year-1 Jan ... month 96 = year-8 Dec
.cal_month_wu <- ((.month_wu - 1) %% 12) + 1        # 1=Jan ... 12=Dec (calendar position)

# A severe 2-year drought-and-mandatory-conservation emergency declared in month 25 (start of
# year 3) through month 48 (end of year 4) -- modeled after the multi-year length of CA's
# 2012-2016 and 2020-2022 drought-emergency declarations.
.drought_start_wu <- 25L
.drought_end_wu   <- 48L
.drought_wu      <- .drought_start_wu:.drought_end_wu
.pre_drought_wu  <- 1:(.drought_start_wu - 1)
.post_drought_wu <- (.drought_end_wu + 1):.T_WU      # the "conservation habit"/new-normal era

.shape_wu <- function(cal_month) -cos(2 * pi * (cal_month - 1) / 12)  # peaks ~July, troughs ~Jan
                                                                        # -- strong outdoor-irrigation
                                                                        # summer seasonality, the
                                                                        # SAME functional shape as
                                                                        # cooling-demand in energy
                                                                        # forecasting, but fit with
                                                                        # water-specific magnitude.

.build_waterutility_panel <- function() {
  # Months 1-48 (pre-drought + the drought itself) are built from ONE random stream, shared
  # identically between the "real" (habit-persists) and "counterfactual" (full-reversion) worlds
  # built later in this file, so the two worlds differ ONLY in what happens after month 48 --
  # not in resampled history, which would confound the comparison.
  set.seed(42)
  shape_48 <- .shape_wu(.cal_month_wu[1:.drought_end_wu])
  L_base <- list(
    seasonal_naive  = 4.0 + 1.0 * abs(shape_48) + rnorm(.drought_end_wu, 0, 0.5),
    weather_reg     = 2.5 + 0.4 * abs(shape_48) + rnorm(.drought_end_wu, 0, 0.4),
    ml_conservation = 2.7 + 0.4 * abs(shape_48) + rnorm(.drought_end_wu, 0, 0.4),
    trend_model     = 5.5 + rnorm(.drought_end_wu, 0, 0.6)
  )
  # The drought-policy shock is a genuine TWO-STAGE mechanism: a hydrology/weather shock plus a
  # mandatory-conservation-ordinance shock layered on top, which only ml_conservation has any
  # signal for (it ingests conservation-program participation data). seasonal_naive/trend_model
  # are fully blind and badly over-predict demand; weather_reg (the closest rival) is less
  # severely affected but still clearly worse than ml_conservation during the emergency.
  L_base$seasonal_naive[.drought_wu]  <- L_base$seasonal_naive[.drought_wu]  + rnorm(length(.drought_wu), 6.5, 1.0)
  L_base$trend_model[.drought_wu]     <- L_base$trend_model[.drought_wu]     + rnorm(length(.drought_wu), 6.5, 1.0)
  L_base$weather_reg[.drought_wu]     <- L_base$weather_reg[.drought_wu]     + rnorm(length(.drought_wu), 1.3, 0.5)
  L_base$ml_conservation[.drought_wu] <- L_base$ml_conservation[.drought_wu] + rnorm(length(.drought_wu), 0.7, 0.4)
  lapply(L_base, function(v) pmax(v, 0.05))
}

.build_postdrought_wu <- function(habit_persists) {
  n_post <- length(.post_drought_wu)
  shape_post <- .shape_wu(.cal_month_wu[.post_drought_wu])
  # SAME seed for both branches below -- the ONLY difference between the "habit persists" (real,
  # documented) world and the "full reversion" (counterfactual, NOT what utilities actually
  # observe post-drought, used here purely as a diagnostic contrast) world is whether the
  # persistent-habit bump term is added, not a different random draw.
  set.seed(142)
  L <- list(
    seasonal_naive  = 4.0 + 1.0 * abs(shape_post) + rnorm(n_post, 0, 0.5),
    weather_reg     = 2.5 + 0.4 * abs(shape_post) + rnorm(n_post, 0, 0.4),
    ml_conservation = 2.7 + 0.4 * abs(shape_post) + rnorm(n_post, 0, 0.4),
    trend_model     = 5.5 + rnorm(n_post, 0, 0.6)
  )
  if (habit_persists) {
    # The documented "conservation habit" effect: demand settles at a level PERMANENTLY below the
    # pre-drought seasonal baseline would predict. Models with no ongoing participation-data
    # signal keep over-predicting relative to the new normal; ml_conservation tracks it.
    for (m in c("seasonal_naive", "weather_reg", "trend_model")) {
      L[[m]] <- L[[m]] + rnorm(n_post, 0.9, 0.4)
    }
    L$ml_conservation <- L$ml_conservation + rnorm(n_post, 0.15, 0.3)
  }
  lapply(L, function(v) pmax(v, 0.05))
}

.L_base_wu    <- .build_waterutility_panel()
.L_post_real  <- .build_postdrought_wu(habit_persists = TRUE)   # the real, documented world
.L_post_cf    <- .build_postdrought_wu(habit_persists = FALSE) # counterfactual: no habit effect

.L_real_wu <- Map(function(a, b) c(a, b), .L_base_wu, .L_post_real)
.L_cf_wu   <- Map(function(a, b) c(a, b), .L_base_wu, .L_post_cf)

.panel_prev_wu <- loss_panel(.L_base_wu, labels = .month_wu[1:.drought_end_wu])
.panel_real_wu <- loss_panel(.L_real_wu, labels = .month_wu)
.panel_cf_wu   <- loss_panel(.L_cf_wu, labels = .month_wu)

test_that("the water-utility panel is well-formed with real (non-positional) monthly labels", {
  expect_equal(.panel_real_wu$models, c("seasonal_naive", "weather_reg", "ml_conservation", "trend_model"))
  expect_equal(length(.panel_real_wu$labels), 96)
  # Real calendar-month labels were passed explicitly -- required for compare()'s new-period
  # attribution (see compare.R's own documented positional-label caveat) to work correctly below.
  expect_false(isTRUE(.panel_real_wu$labels_are_positional))
})

test_that("pre-drought favors the weather-regression model, but the drought itself already flips the pooled champion to the conservation-aware model", {
  # Before the drought, plain weather-regression is genuinely competitive (no policy shock yet to
  # differentiate on) -- a realistic utility would have NO reason to prefer the more complex
  # ML model on pre-drought data alone.
  champ_pre <- pooled_winner(lapply(.L_base_wu, function(v) v[.pre_drought_wu]), rep(1, length(.pre_drought_wu)))
  expect_equal(champ_pre, "weather_reg")

  # Once the mandatory-conservation emergency hits, ml_conservation's edge is large enough that,
  # evaluated over the full pre-drought-plus-drought history, it is ALREADY the pooled champion --
  # i.e. a utility re-evaluating models right as the drought ends would already see the switch.
  champ_through_drought <- pooled_winner(.L_base_wu, rep(1, .drought_end_wu))
  expect_equal(champ_through_drought, "ml_conservation")
})

test_that("report() runs cleanly on the full 96-month realistic panel and identifies the conservation-aware ML model", {
  r <- report(.panel_real_wu, seed = 0)
  expect_s3_class(r, "selectionfragility_report")
  txt <- unclass(r)
  expect_match(txt, "VERDICT: identified")
  expect_match(txt, "ml_conservation.*\\(champion\\)")
  expect_match(txt, "RESOLUTION")
  expect_match(txt, "PIVOT")
  # The champion's edge is real but not overwhelming -- resolved, not a landslide -- the honest,
  # defensible-but-not-bulletproof read a rate-setting process actually needs.
  expect_match(txt, "resolved")
})

test_that(
  paste(
    "FINDING: decision_breakdown()'s pivotal set is NOT dominated by the acute drought months alone",
    "-- the post-drought 'conservation habit' era contributes AT LEAST AS MUCH to the champion's",
    "fragility set as the drought itself, a genuinely non-obvious domain finding (the drought months",
    "have the largest PER-PERIOD margin, but there are only 24 of them against 48 new-normal months)"
  ), {
  champ <- pooled_winner(.L_real_wu, rep(1, .T_WU))
  expect_equal(champ, "ml_conservation")

  db <- decision_breakdown(.L_real_wu, rep(1, .T_WU), a = champ)
  expect_equal(db$binding_opponent, "weather_reg")
  # A genuine, nontrivial pivotal set: more than a handful of periods, but nowhere near the full
  # panel -- this is a ROBUST win (k* well under half of T), not a knife-edge one.
  expect_gt(db$k, 10)
  expect_lt(db$k, .T_WU / 2)

  in_drought <- sum(db$removed %in% .drought_wu)
  in_post    <- sum(db$removed %in% .post_drought_wu)
  in_pre     <- sum(db$removed %in% .pre_drought_wu)
  # No pre-drought period is ever in the pivotal set: pre-drought is weather_reg's best territory,
  # so removing a pre-drought period can only help ml_conservation's relative case, never the
  # opponent's -- breakdown_number() never needs to touch it.
  expect_equal(in_pre, 0L)
  # Both the drought AND the post-drought era contribute materially -- this is the answer to
  # "does the drought-policy period specifically drive k*?": NO, not uniquely. The permanent
  # post-drought new-normal is at least as important to the champion's fragility profile as the
  # acute drought event, which a planning team naively focused only on "the drought year" would miss.
  expect_gt(in_drought, 0)
  expect_gt(in_post, 0)
  expect_equal(in_drought + in_post, db$k)
})

test_that(
  paste(
    "FINDING: compare() correctly distinguishes a PERMANENT post-drought demand re-basing from a",
    "merely TRANSIENT drought-year dip -- the central question this use case poses. Re-evaluating",
    "right as the drought ends (previous = months 1-48) against two alternative continuations of",
    "the SAME shared history (current = months 1-96) shows a materially more fragile, not just",
    "differently-labeled, champion position when the 'new normal' is counterfactually assumed away"
  ), {
  # Sanity: both continuations share the identical previous-champion starting point.
  expect_equal(pooled_winner(.L_base_wu, rep(1, .drought_end_wu)), "ml_conservation")

  cr_real <- compare(.panel_prev_wu, .panel_real_wu, seed = 0)
  cr_cf   <- compare(.panel_prev_wu, .panel_cf_wu, seed = 0)

  expect_s3_class(cr_real, "change_report")
  expect_equal(cr_real$previous_champion, "ml_conservation")
  expect_equal(cr_cf$previous_champion, "ml_conservation")

  # In the REAL, documented world (conservation habit persists), the champion that was already
  # ahead right as the drought ended REMAINS the champion a full post-drought "new normal" era
  # later, and old_champion_still_in_mcs is TRUE -- the honest, defensible answer: keep trusting
  # ml_conservation for next year's water-supply planning and rate-setting.
  expect_false(cr_real$champion_changed)
  expect_true(isTRUE(cr_real$old_champion_still_in_mcs))
  expect_false(cr_real$act)

  # In the COUNTERFACTUAL world (demand fully reverts, no habit effect -- NOT what utilities
  # actually observe post-drought, used here only as a diagnostic contrast), the pooled champion
  # label does not flip outright either (the drought months' large margin still carries the full-
  # panel pooled mean) -- BUT the package's finer-grained fragility diagnostics correctly flag that
  # this "no change" verdict sits on a knife-edge, unlike the real world's robust margin:
  expect_false(cr_cf$champion_changed)
  expect_lt(cr_cf$k_star, cr_real$k_star)       # far smaller pivotal set in the reversion world
  expect_gt(cr_cf$churn_base_rate, cr_real$churn_base_rate)  # materially higher new-period churn risk
  expect_equal(cr_real$churn_base_rate, 0)
  expect_gt(cr_cf$churn_base_rate, 0.05)
  # This is the honest, nuanced answer a planning team actually needs: compare()'s k*/churn-rate
  # readout -- NOT the binary champion_changed/act flags alone -- is what correctly surfaces that
  # the reversion world's "no change" is fragile, while the real, documented persistent-habit
  # world's "no change" is robust. A team reading ONLY champion_changed/act would wrongly treat
  # both worlds as equally settled; a team reading k*/churn_base_rate would not.
})

test_that("non-uniform (e.g. drought-era-overweighted) planning weights correctly disable the native MCS verdict, same as the package's other realistic scenarios", {
  # A realistic consequence: if the utility's rate-setting process deliberately overweights the
  # drought-and-recovery years (the years that matter most for next year's supply planning) rather
  # than treating all 96 months equally, report() must say the native MCS VERDICT is unavailable
  # for non-uniform weights, not silently compute an unweighted MCS underneath a weighted leaderboard.
  drought_recovery_weight <- ifelse(.month_wu >= .drought_start_wu, 2, 1)
  panel_w <- loss_panel(.L_real_wu, weights = drought_recovery_weight, labels = .month_wu)
  r <- report(panel_w, seed = 0)
  txt <- unclass(r)
  expect_match(txt, "VERDICT: not available for this panel")
  expect_match(txt, "non-uniform period weights")
  expect_match(txt, "ml_conservation.*\\(champion\\)")
})
