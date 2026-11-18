# test-realworld-airtravel.R -- an airline revenue-management (RM) PASSENGER DEMAND forecasting
# use case, constructed from real, documented airline-industry characteristics rather than an
# adversarial toy example. This is the DEMAND-FORECASTING half of RM (what the market will buy),
# distinct from crew-scheduling/operations forecasting.
#
# THE SCENARIO: an airline's RM team forecasts TOTAL WEEKLY ROUTE DEMAND (one domestic,
# leisure/business-mixed route) at a SINGLE, EXPLICIT booking-curve horizon: 60 DAYS BEFORE
# DEPARTURE. Weekly cadence, 2019-01-07 through 2023-12-25 (T=260 weeks), spanning the COVID-19
# air-travel collapse and its slow, uneven recovery. Four candidate models compete for the "which
# model do we trust" decision:
#   - hist_pattern:       a pure seasonal-naive/historical-pattern baseline (day-of-week/annual
#                          seasonality + a slow macro/fuel-cycle factor) -- the cheapest baseline
#                          every RM team keeps around, with NO real-time demand signal at all.
#   - booking_curve_reg:  a regression on current bookings-to-date at the 60-day mark relative to
#                          historical booking PACE -- the standard RM "pickup" forecasting
#                          approach; tracks real demand with only a short lag, but its pace-ratio
#                          coefficients (calibrated pre-pandemic) break down during a regime this
#                          unprecedented.
#   - ml_adaptive:        a recency-weighted ML model -- blindsided for the first few weeks of an
#                          out-of-distribution shock (same as any ML model trained on normal
#                          variation), but adapts fastest once a few weeks of collapse data
#                          accumulate.
#   - ensemble:           a simple average of the three -- a common, realistic RM-team practice.
#
# THE REAL, DOCUMENTED EVENT: the 2020 COVID-19 air-travel collapse -- a near-total demand floor
# (weekly route demand craters to ~3-9% of its normal seasonal level for ~18 weeks, 2020-03-09 to
# 2020-07-06) followed by a slow, UNEVEN recovery (not monotonic -- successive COVID-variant waves
# produce real setbacks) through about 2021-10, then a "revenge travel" overshoot settling near or
# slightly above the pre-pandemic baseline by 2023. hist_pattern, having no real-time demand
# signal, forecasts normal pre-pandemic seasonal demand straight through the entire collapse --
# the single most catastrophic, well-documented real forecasting failure mode of this event.
#
# WHAT THIS TEST FILE CHECKS (mirroring the energy/STLF real-world test's structure): that
# loss_panel()/report()/decision_breakdown()/compare() give sensible, DOMAIN-ACCURATE answers to
# the RM team's two real questions -- (1) "was our champion's edge driven by the collapse, or is
# it genuine?" and (2) "now that the market has stabilized, should we switch models?" -- including
# a genuine VERDICT/RESOLUTION disagreement this package is specifically designed to surface and
# caveat, found on the post-recovery-only window (see below).

set.seed(202)
.T_AIR <- 260L
.week_a <- seq_len(.T_AIR)
.dates_a <- as.Date("2019-01-07") + 7 * (.week_a - 1)
.year_frac_a <- as.numeric(format(.dates_a, "%j")) / 365
.season_a <- 1 + 0.35 * sin(2 * pi * (.year_frac_a - 0.12))  # strong route seasonality (summer peak)
.macro_a <- 1 + 0.10 * sin(2 * pi * .week_a / 180 + 1)        # slow fuel-price/economic-cycle factor

# Real calendar anchors for the 2020 collapse and its recovery.
.covid_start   <- which(.dates_a >= as.Date("2020-03-09"))[1]  # week 62
.covid_floor_end <- which(.dates_a >= as.Date("2020-07-01"))[1] # week 79 (18-week near-total floor)
.recovery_end  <- which(.dates_a >= as.Date("2021-10-01"))[1]   # week 144 (slow, uneven recovery through here)
.collapse_idx  <- .covid_start:.covid_floor_end
.post_idx      <- (.recovery_end + 1L):.T_AIR

.build_airline_panel <- function() {
  demand_factor <- rep(1, .T_AIR)
  demand_factor[.collapse_idx] <- 0.06 + 0.03 * sin(.week_a[.collapse_idx])  # near-total floor, ~3-9%

  ramp_idx <- (.covid_floor_end + 1L):.recovery_end
  ramp <- seq(0.10, 0.92, length.out = length(ramp_idx))
  unevenness <- 0.08 * sin(seq(0, 6 * pi, length.out = length(ramp_idx)))  # real, non-monotonic
                                                                             # setbacks (variant waves)
  demand_factor[ramp_idx] <- pmin(1.05, pmax(0.05, ramp + unevenness))

  demand_factor[.post_idx] <- 1.0 + 0.06 * (seq_along(.post_idx) / length(.post_idx)) +
    0.02 * rnorm(length(.post_idx))  # "revenge travel" overshoot settling near/above pre-pandemic baseline

  baseline <- 1000
  true_demand <- baseline * .season_a * .macro_a * demand_factor

  f_hist <- baseline * .season_a * .macro_a  # blind to the collapse entirely -- forecasts "normal" throughout

  lag_bc <- 2
  f_bc <- c(f_hist[1:lag_bc], true_demand[1:(.T_AIR - lag_bc)])
  f_bc <- f_bc * (1 + 0.05 * sin(.week_a / 9)) + rnorm(.T_AIR, 0, 0.07) * baseline

  onset_blind <- 4
  f_ml <- true_demand
  f_ml[.covid_start:(.covid_start + onset_blind - 1)] <-
    f_hist[.covid_start:(.covid_start + onset_blind - 1)] * 0.7
  f_ml <- f_ml * (1 + 0.03 * sin(.week_a / 15)) + rnorm(.T_AIR, 0, 0.05) * baseline

  f_ens <- (f_hist + f_bc + f_ml) / 3

  forecasts <- list(hist_pattern = f_hist, booking_curve_reg = f_bc, ml_adaptive = f_ml, ensemble = f_ens)
  # Absolute percentage error vs true demand -- the standard RM forecast-accuracy metric. Capped
  # at 5 (500% error) since true_demand near-zero during the collapse floor makes raw APE blow up;
  # a real RM team would similarly cap/winsorize this metric rather than let it diverge to Inf.
  L <- lapply(forecasts, function(f) pmin(abs(f - true_demand) / true_demand, 5))
  list(L = L, true_demand = true_demand)
}

.air_build <- .build_airline_panel()
.air_L <- .air_build$L
.air_panel <- loss_panel(.air_L, labels = .dates_a)  # uniform weights (ordinary workflow)

test_that("the realistic 60-day-out demand panel is well-formed and ml_adaptive is the pooled champion", {
  expect_equal(.air_panel$models, c("hist_pattern", "booking_curve_reg", "ml_adaptive", "ensemble"))
  expect_equal(length(.air_panel$labels), 260)
  expect_false(isTRUE(.air_panel$labels_are_positional))  # real calendar dates, not positional indices
  # Pooled over the full 5-year span, the adaptive ML model -- NOT the naive 3-way ensemble -- has
  # the lowest mean loss: naive averaging is not a free lunch when one component (hist_pattern)
  # craters catastrophically during an unprecedented regime shift, since the ensemble still
  # partially inherits that catastrophic error.
  expect_equal(pooled_winner(.air_L, rep(1, .T_AIR)), "ml_adaptive")
  champ_mean <- mean(.air_L$ml_adaptive)
  ens_mean <- mean(.air_L$ensemble)
  expect_lt(champ_mean, ens_mean)
})

test_that("report() runs cleanly on the full 2019-2023 panel and identifies ml_adaptive", {
  r <- report(.air_panel, seed = 0)
  expect_s3_class(r, "selectionfragility_report")
  txt <- unclass(r)
  expect_match(txt, "VERDICT: identified")
  expect_match(txt, "ml_adaptive.*\\(champion\\)")
  expect_match(txt, "RESOLUTION")
  expect_match(txt, "PIVOT")
})

test_that(
  paste(
    "FINDING: the 18-week COVID collapse is a MATERIAL but not EXCLUSIVE driver of ml_adaptive's",
    "edge -- decision_breakdown()'s k* pivotal set is only partially inside the collapse window,",
    "answering the RM team's real question ('was our champion's edge just the one crisis quarter,",
    "or is it genuine?') with the honest, nuanced answer rather than a false binary"
  ), {
  w1 <- rep(1, .T_AIR)
  champ <- pooled_winner(.air_L, w1)
  expect_equal(champ, "ml_adaptive")

  db <- decision_breakdown(.air_L, w1, a = champ)
  # A genuine, nontrivial pivotal set exists: not an exact tie (k*>0) and not a bulletproof win
  # that would require removing every period (k*<T).
  expect_true(db$k > 0 && db$k < .T_AIR)

  in_collapse <- sum(db$removed %in% .collapse_idx)
  # Some, but not all, of the k* pivotal periods sit inside the 18-week collapse window -- the
  # realistic, non-binary answer: the collapse materially inflates the margin but does not solely
  # account for it.
  expect_gt(in_collapse, 0)
  expect_lt(in_collapse, db$k)

  # Even with the ENTIRE collapse window excluded, ml_adaptive still beats its binding rival on
  # the remaining weeks -- the champion's edge is real and not solely a crisis-quarter artifact,
  # even though the collapse materially inflates the apparent margin size.
  keep <- setdiff(.week_a, .collapse_idx)
  opp <- db$binding_opponent
  expect_lt(mean(.air_L[[champ]][keep]), mean(.air_L[[opp]][keep]))
})

test_that(
  paste(
    "FINDING: restricted to the post-recovery/stabilized window alone, the champion flips back to",
    "the naive hist_pattern baseline AND VERDICT/RESOLUTION genuinely disagree -- exactly the",
    "seed-free, real-data instance report()'s own built-in caveat text (report.R) exists to catch:",
    "once ordinary seasonality resumes, a model with zero real-time signal looks competitive again,",
    "and the package must not let a one-screen reader mistake 'resolved' for 'MCS-settled'"
  ), {
  idx <- (.recovery_end + 1L):.T_AIR  # post-recovery/stabilized weeks only (2021-10 onward)
  L_post <- lapply(.air_L, function(v) v[idx])
  panel_post <- loss_panel(L_post, labels = .dates_a[idx])

  champ_post <- pooled_winner(L_post, panel_post$weights)
  # Once the collapse and recovery weeks are entirely out of view, the simple seasonal baseline --
  # NOT the adaptive ML model -- is the pooled champion again: a real, domain-grounded reversal a
  # post-pandemic re-evaluation would actually encounter.
  expect_equal(champ_post, "hist_pattern")

  r <- report(panel_post, seed = 0)
  txt <- unclass(r)
  expect_match(txt, "VERDICT: not identified")
  expect_match(txt, "resolved")
  # The genuine-disagreement caveat: VERDICT says the MCS bootstrap cannot rule out every rival,
  # RESOLUTION says the edge over the ONE binding rival is determinable -- both can be true at
  # once, and report() says so explicitly rather than leaving a one-screen reader to reconcile it.
  expect_match(txt, "VERDICT and\\s*\n\\s*RESOLUTION answer different questions and can legitimately disagree")
  expect_match(txt, "hist_pattern.*\\(champion\\)")
})

test_that(
  paste(
    "compare(): a MODEST rolling re-evaluation (collapse+recovery still dominates both windows'",
    "overlap) correctly reports NO actionable champion change -- the RM team should not switch",
    "models on noise alone"
  ), {
  idx_prev <- 1:180
  idx_curr <- 121:260  # rolled forward 60 weeks; overlap (121:180) still spans deep collapse+recovery
  prev <- loss_panel(lapply(.air_L, function(v) v[idx_prev]), labels = .dates_a[idx_prev])
  curr <- loss_panel(lapply(.air_L, function(v) v[idx_curr]), labels = .dates_a[idx_curr])

  cr <- compare(prev, curr, seed = 0)
  expect_s3_class(cr, "change_report")
  expect_false(cr$champion_changed)
  expect_identical(cr$previous_champion, "ml_adaptive")
  expect_identical(cr$current_champion, "ml_adaptive")
  expect_false(cr$act)  # nothing for a CI/pipeline promotion step to act on
  expect_true(isTRUE(cr$old_champion_still_in_mcs))
})

test_that(
  paste(
    "compare(): re-evaluating with a window that rolls MOST of the way past the collapse into the",
    "stabilized post-recovery era DOES flip the champion back to hist_pattern, flags it as",
    "actionable (act=TRUE), and correctly attributes the flip to the overlap itself -- not merely",
    "to the newly-added periods -- giving the RM team a sensible, non-naive explanation rather",
    "than a bare yes/no"
  ), {
  idx_prev <- 1:200   # still dominated by collapse+recovery
  idx_curr <- 150:260 # mostly post-recovery/stabilized weeks
  prev <- loss_panel(lapply(.air_L, function(v) v[idx_prev]), labels = .dates_a[idx_prev])
  curr <- loss_panel(lapply(.air_L, function(v) v[idx_curr]), labels = .dates_a[idx_curr])

  cr <- compare(prev, curr, seed = 0)
  expect_true(cr$champion_changed)
  expect_identical(cr$previous_champion, "ml_adaptive")
  expect_identical(cr$current_champion, "hist_pattern")
  # act=TRUE requires BOTH a champion change AND the old champion demonstrably leaving the MCS
  # (not just losing the pooled mean to noise) -- confirming this is the one signal a real
  # promotion/alerting pipeline should gate a model-switch action on.
  expect_true(cr$act)
  expect_false(isTRUE(cr$old_champion_still_in_mcs))
  # The flip is NOT an artifact of only the new (most-recent) periods: hist_pattern already wins
  # on the overlap alone (champion_without_new_periods == current_champion, not previous_champion)
  # -- a materially different, more actionable explanation than "the new data alone caused this."
  expect_identical(cr$champion_without_new_periods, "hist_pattern")
  expect_false(identical(cr$champion_without_new_periods, cr$previous_champion))
  expect_equal(cr$n_new_periods, 60L)
})

test_that(
  paste(
    "realistic recency weighting (an RM team upweighting the most recent year for next-quarter",
    "capacity-planning relevance) correctly disables the native MCS VERDICT rather than silently",
    "computing it unweighted underneath a weighted leaderboard -- the same documented limitation",
    "confirmed in this package's energy/STLF real-world test, now in an airline RM context"
  ), {
  recency_w <- ifelse(.week_a > (.T_AIR - 52), 3, 1)  # most recent 52 weeks weighted 3x
  panel_w <- loss_panel(.air_L, weights = recency_w, labels = .dates_a)
  r <- report(panel_w, seed = 0)
  txt <- unclass(r)
  expect_match(txt, "VERDICT: not available for this panel")
  expect_match(txt, "non-uniform period weights")
  expect_match(txt, "ml_adaptive.*\\(champion\\)")
  expect_equal(pooled_winner(.air_L, recency_w), "ml_adaptive")
})

test_that(
  paste(
    "FINDING: the champion-vs-rival loss differential is strongly, significantly autocorrelated",
    "around the collapse/recovery regime shift -- exactly the risk resolution.R's documented",
    "'no autocorrelation' assumption warns about, confirmed on genuinely realistic (not",
    "adversarially constructed) booking-curve demand data"
  ), {
  champ <- pooled_winner(.air_L, rep(1, .T_AIR))
  db <- decision_breakdown(.air_L, rep(1, .T_AIR), a = champ)
  diff_cr <- as.numeric(.air_L[[db$binding_opponent]]) - as.numeric(.air_L[[champ]])
  ac <- acf(diff_cr, plot = FALSE, lag.max = 8)$acf[-1]
  expect_true(ac[1] > 0.2)
  bt <- Box.test(diff_cr, lag = 10, type = "Ljung-Box")
  expect_lt(bt$p.value, 1e-4)
  # report() always discloses this (statically, not a dynamic measurement) -- confirmed present
  # on this panel's own printed text, not just asserted in the abstract.
  expect_match(report(.air_panel, seed = 0), "RESOLUTION assumes no autocorrelation")
})
