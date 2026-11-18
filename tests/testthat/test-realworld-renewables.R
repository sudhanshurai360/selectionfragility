# test-realworld-renewables.R -- a distinct, realistic WIND-GENERATION forecasting use case
# (different angle from test-realworld-energy.R's STLF demand scenario). This package diagnoses
# forecast-model-selection fragility; this test checks whether its already-documented caveats
# (mcs()'s block=3 default understating uncertainty on correlated real data; resolution.R's "no
# autocorrelation" assumption) generalize to a SUPPLY-side forecasting problem with genuinely
# different statistical texture, rather than just re-running the demand scenario with new names.
#
# THE SCENARIO: a grid operator evaluating 4 candidate DAILY-aggregate wind-generation forecasting
# models over 2 years (T=730) to decide which one to deploy operationally:
#   - persistence: a climatology/persistence baseline (tomorrow's generation looks like today's /
#     the seasonal average) -- the cheapest, weakest model every renewables-forecasting team keeps.
#   - nwp:         a numerical-weather-prediction-based model -- physically grounded, a solid
#     all-rounder, but only as good as the NWP wind-field forecast feeding it.
#   - ml:          a machine-learning model (e.g. gradient-boosted trees / shallow net on NWP +
#     historical features) -- the apparent near-champion in ordinary conditions.
#   - ensemble:    a multi-model ensemble -- near-tied with ml day-to-day, but hopefully more
#     robust specifically when the wind resource itself misbehaves.
#
# Wind generation has a GENUINELY different error texture from demand: short-term wind output is
# weakly autocorrelated / close to chaotic day-to-day (unlike demand's strong daily/weekly
# seasonality), BUT real wind resource assessment is punctuated by multi-day "lull" events --
# synoptic-scale low-wind spells (e.g. a persistent high-pressure ridge) lasting the better part of
# a week to beyond -- during which EVERY model's forecast error rises together, because the
# meteorological driver itself (the lull's onset/duration/intensity) is what's hard to predict, not
# any one model's internals. This is the wind-forecasting analogue of the heat-wave/cold-snap shared
# shock already shown to matter for demand forecasting in test-realworld-energy.R. This test builds
# exactly that: six lull events of 6-11 days each (up to a maximum of 11 days, deliberately longer
# than mcs()'s hardcoded block=3 default), hitting all four models as one shared, correlated shock,
# with persistence hurt hardest (climatology has no idea a lull is coming) and ml specifically
# weaker than the ensemble DURING lulls (an ML model trained mostly on ordinary wind regimes
# generalizes worse to the tail than a model-averaging ensemble) -- but with ml and ensemble kept
# near-tied in ordinary, non-lull conditions, so any edge is a real test of fragility/resolution,
# not a foregone conclusion.

set.seed(11)
.T_WIND <- 730L
.day_w <- seq_len(.T_WIND)

# Up to 11-day lull, deliberately exceeding mcs()'s hardcoded block=3 default by ~4x, to re-test
# (on a supply-forecasting panel with a different autocorrelation texture than demand) this
# package's own documented claim that block=3 is "tuned for roughly-i.i.d. losses" and can
# understate uncertainty when real shocks are more persistent than that.
.lulls_w <- list(60:65, 140:148, 300:305, 410:420, 550:556, 670:678)
.all_lull_days_w <- unlist(.lulls_w)

.build_wind_panel <- function() {
  base_loss <- list(
    persistence = 4.00 + rnorm(.T_WIND, 0, 1.00),  # weak, noisy climatology baseline
    nwp         = 2.60 + rnorm(.T_WIND, 0, 0.70),   # solid physically-grounded all-rounder
    ml          = 2.30 + rnorm(.T_WIND, 0, 0.55),   # apparent near-champion on ordinary days
    ensemble    = 2.28 + rnorm(.T_WIND, 0, 0.50)    # near-TIED with ml on ordinary days
  )

  inject_lull <- function(L, days, mean_shock, sd_shock, extra = NULL) {
    # ONE shared shock path per lull across ALL models (the lull itself -- its onset, duration,
    # and depth -- is what's hard to forecast, so every model misses together), plus a small
    # per-model idiosyncratic wobble, plus an optional model-specific "extra" term capturing how
    # each model's architecture handles the tail differently (persistence has literally no signal
    # a lull is coming; ml's learned patterns are thinner on rare low-wind regimes than the
    # ensemble's averaged-out robustness).
    shared <- rnorm(length(days), mean_shock, sd_shock)
    for (m in names(L)) {
      idio <- rnorm(length(days), 0, sd_shock * 0.2)
      bump <- if (!is.null(extra[[m]])) extra[[m]] else 0
      L[[m]][days] <- L[[m]][days] + shared + idio + bump
    }
    L
  }

  L <- base_loss
  for (ll in .lulls_w) {
    L <- inject_lull(L, ll, mean_shock = 3.0, sd_shock = 0.9,
                      extra = list(persistence = 1.8, ml = 1.1, ensemble = 0))
  }

  # Losses are an absolute-error-style generation-forecasting metric; can't be negative.
  lapply(L, function(v) pmax(v, 0.05))
}

.wind_L <- .build_wind_panel()
.wind_panel <- loss_panel(.wind_L, labels = .day_w)  # uniform weights (ordinary workflow)
.w1_wind <- rep(1, .T_WIND)

test_that("the wind-generation panel is well-formed and the ensemble is the pooled champion", {
  expect_equal(.wind_panel$models, c("persistence", "nwp", "ml", "ensemble"))
  expect_equal(length(.wind_panel$labels), .T_WIND)
  # ensemble has the lowest pooled mean loss -- near-tied with ml in ordinary conditions, but
  # edged out overall once the lull events are included.
  expect_equal(pooled_winner(.wind_L, .w1_wind), "ensemble")
})

test_that("report() runs cleanly on the 2-year daily wind-generation panel and identifies ensemble", {
  r <- report(.wind_panel, seed = 0)
  expect_s3_class(r, "selectionfragility_report")
  txt <- unclass(r)
  expect_match(txt, "VERDICT: identified")
  expect_match(txt, "ensemble.*\\(champion\\)")
  expect_match(txt, "RESOLUTION")
  expect_match(txt, "PIVOT")
})

test_that(
  paste(
    "FINDING: ensemble's edge over ml is a real, nontrivial pivotal set that is PARTIALLY --",
    "but not entirely, and not even mostly -- explained by the multi-day low-wind lull events;",
    "the ensemble's robustness advantage holds up even outside the lulls"
  ), {
  champ <- pooled_winner(.wind_L, .w1_wind)
  expect_equal(champ, "ensemble")

  db <- decision_breakdown(.wind_L, .w1_wind, a = champ)
  expect_equal(db$binding_opponent, "ml")
  # k* must be strictly between 0 and T: a genuine, nontrivial pivotal set, not a bulletproof win
  # and not an exact tie.
  expect_true(db$k > 0 && db$k < .T_WIND)

  # A real, material share of the k* pivotal days sit inside a lull -- the lulls are a genuine
  # contributor to the champion's margin -- but they do NOT account for all (or even most) of k*,
  # unlike a scenario where the entire edge rides on one event.
  in_lull <- sum(db$removed %in% .all_lull_days_w)
  expect_gt(in_lull, 0)
  expect_lt(in_lull, db$k)

  # Even with every lull day fully excluded, ensemble still beats ml on the remaining (ordinary,
  # chaotic-but-lull-free) days -- confirming the ensemble's edge is real on ordinary wind
  # variability too, not solely an artifact of the lull events.
  keep <- setdiff(.day_w, .all_lull_days_w)
  mean_ens_excl <- mean(.wind_L$ensemble[keep])
  mean_ml_excl  <- mean(.wind_L$ml[keep])
  expect_lt(mean_ens_excl, mean_ml_excl)
})

test_that(
  paste(
    "FINDING: the 'default block=3 understates uncertainty on correlated real weather data'",
    "result (already found for demand/STLF in test-realworld-energy.R) GENERALIZES to daily",
    "wind-generation forecasting -- mcs()'s p-value for the identified champion rises",
    "substantially, though less dramatically than the cold-snap-dominated demand scenario, as",
    "block length grows to span the ~11-day lull events, confirming this is a property of",
    "serially-correlated shared shocks in general and not an artifact specific to one domain"
  ), {
  M <- do.call(cbind, .wind_L)
  mcs_b3  <- mcs(M, alpha = 0.10, B = 2000, block = 3, seed = 0)   # mcs()'s own hardcoded default
  mcs_b11 <- mcs(M, alpha = 0.10, B = 2000, block = 11, seed = 0)  # spans the longest (11-day)
                                                                     # lull as one resampled unit
  expect_equal(colnames(M)[mcs_b3$surviving], "ensemble")
  expect_equal(colnames(M)[mcs_b11$surviving], "ensemble")
  # Both still "identify" ensemble at alpha=0.10 on this draw, but the realistically-blocked
  # p-value is markedly less confident than the default's -- a real analyst trusting block=3
  # alone would understate how close this call actually is, even on data with much weaker
  # autocorrelation than demand's strongly seasonal load series.
  expect_gt(mcs_b3$p, 0)
  expect_gt(mcs_b11$p, mcs_b3$p * 2)  # at least a 2x rise in p from block=3 to block=11

  blocks <- c(1, 3, 7, 11, 14, 21)
  ps <- sapply(blocks, function(b) mcs(M, alpha = 0.10, B = 2000, block = b, seed = 0)$p)
  # A broadly rising p-value as block grows toward the longest lull length -- the same qualitative
  # signature as the demand scenario, confirming the mechanism (not the exact magnitude: wind's
  # weaker day-to-day autocorrelation means the swing here is smaller, single-digit-x rather than
  # 50x+) generalizes to a genuinely different forecasting domain.
  expect_gt(ps[length(ps)], ps[1] * 5)  # block=21 vs block=1: a clear, not marginal, swing
  # Not strictly monotone is acceptable here (wind's weaker autocorrelation adds more bootstrap
  # noise to the trend than demand's strongly seasonal series), but the overall direction must be
  # upward: the largest block must beat the smallest by a wide margin (checked above) and the
  # block sized to the lull (11) must exceed the hardcoded default (3).
  expect_gt(mcs_b11$p, mcs_b3$p)
})

test_that(
  paste(
    "winner_stability() stays comparatively much flatter across the same block sweep that swings",
    "mcs()'s p-value by multiple x -- confirming this package's own documented claim that",
    "winner_stability() (unlike the MCS/identification layer) is NOT block-exploitable, now on a",
    "wind-generation panel whose short-term variability is weakly autocorrelated and closer to",
    "chaotic, i.e. genuinely different texture from demand's strongly seasonal series"
  ), {
  stabilities <- sapply(c(3, 7, 11, 14, 21), function(b) {
    winner_stability(.wind_L, w = NULL, block = b, seed = 1, n_boot = 1000)$stability
  })
  # All comfortably high (the ensemble's win is not fragile)...
  expect_true(all(stabilities > 0.95))
  # ...and the spread across this block sweep is far smaller (in relative terms) than the 2x+
  # swing mcs()'s p-value shows over the same sweep -- winner_stability() is the more
  # block-robust diagnostic on this panel too.
  expect_lt(max(stabilities) - min(stabilities), 0.05)
})

test_that(
  paste(
    "fragility()'s shock_coincidence correctly reports FALSE: the lulls are a genuine, material",
    "contributor (not a fiction) but do not account for the FULL k*-pivotal set -- the honest,",
    "partial-attribution answer a grid operator needs when asking 'did the lulls decide this?'"
  ), {
  champ <- pooled_winner(.wind_L, .w1_wind)
  eb <- exchangeable_benchmark(K = 4, T = .T_WIND, n_rep = 50, n_boot = 300, block = 11, seed = 2)
  expect_true(is.finite(eb$p05) && eb$p05 > 0 && eb$p05 < 1)

  f <- fragility(.wind_L, shock_periods = .all_lull_days_w, n_boot = 1000, seed = 1,
                 stability_block = 11, benchmark = eb)
  expect_equal(f$pooled_winner, "ensemble")
  expect_equal(f$binding_opponent, "ml")
  expect_false(isTRUE(f$reversal))
  expect_true(f$k_star > 0 && f$k_star < .T_WIND)
  # shock_coincidence is strictly "every k*-period is a lull day" -- FALSE here because we already
  # confirmed (above) that some k*-pivotal days fall OUTSIDE the lulls.
  expect_false(isTRUE(f$shock_coincidence))
  # Against a benchmark calibrated at the lull-spanning block length, the ensemble's win is not
  # flagged as unusually fragile relative to the exchangeable null.
  expect_false(isTRUE(f$fragile))
})
