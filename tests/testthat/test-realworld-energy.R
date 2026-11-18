# test-realworld-energy.R -- a genuinely realistic electricity/utility short-term load
# forecasting (STLF) use case, constructed from real domain characteristics rather than an
# adversarial toy example, used as a lasting integration/regression test of this package's own
# documented caveats (resolution.R's "no autocorrelation in the differential" assumption and
# mcs()'s "block=3 is tuned for roughly-i.i.d. losses" default).
#
# THE SCENARIO: a grid operator's forecasting team has 4 candidate daily peak-load forecasting
# models and must pick ONE to deploy for next winter's peak-demand planning:
#   - dow_naive: a day-of-week/seasonal climatology model (no temperature information at all --
#     the cheapest, weakest baseline every STLF team keeps around).
#   - temp_reg:  a temperature-regression model -- captures the smooth heat/cold response well,
#     but (like any regression) is weakest exactly on temperature excursions outside its fitted
#     range, which is precisely where getting it right matters most operationally.
#   - gbm:       a gradient-boosted-trees model -- the apparent year-round champion, handles
#     temperature/day-of-week nonlinearities and interactions well.
#   - lstm:      a recurrent deep-learning model -- competitive in ordinary conditions, but with
#     a learned recurrent state that has never seen an excursion as extreme as a once-a-decade
#     cold snap, so it overshoots harder than gbm specifically during that one event.
#
# This mirrors a real, well-known STLF risk: short-term load is dominated by daily/weekly/annual
# seasonality and heavy temperature-dependence, but the periods that matter most for reliability
# planning (extreme heat waves and cold snaps / "polar vortex" events, e.g. the kind of event
# behind the February 2021 ERCOT crisis) are also the periods where the weather driver ITSELF is
# hardest to forecast -- so forecast errors spike for every model AT ONCE (a shared, correlated
# shock), not independently. This test builds exactly that: a shared weather shock common to all
# four models, with gbm's apparent edge over lstm concentrated in -- but not solely due to -- one
# 9-day winter cold-snap event. This is the realistic version of the exact worry stated by the
# grid operator in this package's own motivating use case: "was our champion's edge driven by one
# unusual cold snap, or is it genuine year-round superiority?"

set.seed(42)
.T_ENERGY <- 365L
.day_e <- seq_len(.T_ENERGY)

.build_energy_panel <- function() {
  base_loss <- list(
    dow_naive = 3.2 + rnorm(.T_ENERGY, 0, 0.6),
    temp_reg  = 2.3 + rnorm(.T_ENERGY, 0, 0.45),
    gbm       = 2.00 + rnorm(.T_ENERGY, 0, 0.35),
    lstm      = 2.05 + rnorm(.T_ENERGY, 0, 0.35)  # near-tied with gbm on an ORDINARY day
  )

  heat_wave  <- 185:190  # 6-day July heat wave -- shared shock, similar effect on gbm/lstm
  cold_snap  <- 20:28    # 9-day January cold-snap/"polar vortex" event -- the scenario a grid
                          # operator worries about most for WINTER peak-demand planning
  cold_snap2 <- 340:343  # a smaller, more balanced December cold event

  inject_shared <- function(L, days, mean_shock, sd_shock, extra = NULL) {
    # ONE shock path shared across ALL models (the weather driver itself is hard to forecast, so
    # every model misses together), plus a small per-model idiosyncratic wobble, plus an optional
    # model-specific "extra" tail-response term (e.g. lstm's unprecedented-excursion overshoot).
    shared <- rnorm(length(days), mean_shock, sd_shock)
    for (m in names(L)) {
      idio <- rnorm(length(days), 0, sd_shock * 0.15)
      bump <- if (!is.null(extra[[m]])) extra[[m]] else 0
      L[[m]][days] <- L[[m]][days] + shared + idio + bump
    }
    L
  }

  L <- base_loss
  L <- inject_shared(L, heat_wave, mean_shock = 5.5, sd_shock = 1.2)
  L <- inject_shared(L, cold_snap, mean_shock = 6.0, sd_shock = 1.3,
                      extra = list(gbm = 0, lstm = 5.0))
  L <- inject_shared(L, cold_snap2, mean_shock = 3.5, sd_shock = 1.0)

  # Losses are an absolute-percentage-error-style metric; can't be negative. Floor just in case.
  lapply(L, function(v) pmax(v, 0.05))
}

.energy_L <- .build_energy_panel()
.energy_panel <- loss_panel(.energy_L, labels = .day_e)  # uniform weights (ordinary workflow)

.month_e <- cut(.day_e, breaks = c(0, cumsum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))),
                 labels = 1:12)
.is_winter_e <- .month_e %in% c(12, 1, 2)
.winter_w <- ifelse(.is_winter_e, 3, 1)  # the team legitimately weights winter months more
                                           # heavily, since the whole point of the exercise is
                                           # NEXT WINTER's peak-demand planning

test_that("the realistic panel is well-formed and the gradient-boosted model is the pooled champion", {
  expect_equal(.energy_panel$models, c("dow_naive", "temp_reg", "gbm", "lstm"))
  expect_equal(length(.energy_panel$labels), 365)
  # gbm has the lowest pooled mean loss -- the "apparent champion" a real STLF team would deploy
  # for next winter if they only looked at the leaderboard mean.
  expect_equal(pooled_winner(.energy_L, rep(1, .T_ENERGY)), "gbm")
})

test_that("report() runs cleanly on realistic daily STLF data and identifies gbm", {
  r <- report(.energy_panel, seed = 0)
  expect_s3_class(r, "selectionfragility_report")
  txt <- unclass(r)
  expect_match(txt, "VERDICT: identified")
  expect_match(txt, "gbm.*\\(champion\\)")
  expect_match(txt, "RESOLUTION")
  expect_match(txt, "PIVOT")
})

test_that(
  paste(
    "FINDING: gbm's edge over lstm is driven in substantial part -- but not ENTIRELY -- by the",
    "one 9-day cold-snap event; this is the quantitative answer to the grid operator's stated worry"
  ), {
  w1 <- rep(1, .T_ENERGY)
  champ <- pooled_winner(.energy_L, w1)
  expect_equal(champ, "gbm")

  db <- decision_breakdown(.energy_L, w1, a = champ)
  expect_equal(db$binding_opponent, "lstm")
  # k* must be strictly less than T (the win is not bulletproof) but strictly greater than 0
  # (gbm is not an exact/effective tie with lstm) -- i.e. a genuine, nontrivial pivotal set exists.
  expect_true(db$k > 0 && db$k < .T_ENERGY)

  cold_snap <- 20:28
  # A meaningful share, but NOT all, of the k* pivotal periods sit inside the cold snap: removing
  # the cold snap alone does not explain the whole fragility set, but it is a material contributor
  # -- exactly the nuanced, non-binary answer a real analyst needs (not "it's all one bad week"
  # and not "the cold snap is irrelevant").
  in_snap <- sum(db$removed %in% cold_snap)
  expect_gt(in_snap, 0)
  expect_lt(in_snap, db$k)

  # Even with the cold snap fully excluded, gbm still wins on the remaining days -- so the
  # champion's superiority is REAL and not solely a one-event artifact, even though the cold
  # snap materially inflates the apparent margin. This is the honest, domain-grounded answer:
  # not "it's fragile," not "it's bulletproof," but "partially cold-snap-driven, still real."
  keep <- setdiff(.day_e, cold_snap)
  mean_gbm_excl  <- mean(.energy_L$gbm[keep])
  mean_lstm_excl <- mean(.energy_L$lstm[keep])
  expect_lt(mean_gbm_excl, mean_lstm_excl)
})

test_that(
  paste(
    "FINDING: shared weather shocks induce strong, statistically significant autocorrelation in",
    "the champion-vs-rival loss differential -- exactly the risk resolution.R's own documented",
    "'no autocorrelation' assumption warns about, confirmed on genuinely realistic (not",
    "adversarially constructed) STLF data, not just a hypothetical"
  ), {
  diff_cr <- .energy_L$lstm - .energy_L$gbm
  ac <- acf(diff_cr, plot = FALSE, lag.max = 7)$acf[-1]
  # Positive, decaying autocorrelation at short lags -- the signature of a multi-day weather event
  # creating a temporary, serially-correlated regime in the loss differential, not i.i.d. noise.
  expect_true(ac[1] > 0.3)
  bt <- Box.test(diff_cr, lag = 10, type = "Ljung-Box")
  # Overwhelmingly significant. GAP NOW PARTIALLY CLOSED (2026-11-18): report() does NOT run this
  # (or any) autocorrelation diagnostic itself -- a real analyst still gets no quantified ACF/
  # Ljung-Box readout from the package -- but report() now ALWAYS prints a static, unconditional
  # disclosure immediately under RESOLUTION naming exactly this assumption ("RESOLUTION assumes no
  # autocorrelation in the per-period loss differential ... this is NOT checked at runtime"), so an
  # analyst reading report()'s printed text is no longer left with zero in-workflow signal that the
  # assumption exists and is unchecked -- only with no quantified measurement of how badly it is
  # violated on their specific panel.
  expect_lt(bt$p.value, 1e-6)
  expect_match(report(loss_panel(.energy_L)), "RESOLUTION assumes no autocorrelation")
})

test_that(
  paste(
    "FINDING: mcs()'s hardcoded block=3 default materially understates uncertainty relative to a",
    "block sized to the realistic 9-day cold-snap event, on genuinely realistic data -- the exact",
    "failure mode mcs()'s own @param block documentation warns about, now confirmed in a concrete",
    "STLF scenario rather than only an abstract AR(1) simulation"
  ), {
  M <- do.call(cbind, .energy_L)
  mcs_b3  <- mcs(M, alpha = 0.10, B = 2000, block = 3, seed = 0)   # mcs()'s own hardcoded default;
                                                                     # report()/report-driven
                                                                     # workflows use this with ZERO
                                                                     # extra effort from the caller
  mcs_b14 <- mcs(M, alpha = 0.10, B = 2000, block = 14, seed = 0)  # comfortably spans the 9-day
                                                                     # cold snap as one resampled unit
  expect_equal(colnames(M)[mcs_b3$surviving], "gbm")
  expect_equal(colnames(M)[mcs_b14$surviving], "gbm")
  # Both still "identify" gbm at alpha=0.10 on this particular draw, but the default block is
  # markedly overconfident: its p-value is far smaller (more confident) than the realistically-
  # blocked p-value -- a real analyst trusting the default alone would understate how close this
  # call actually is. This is a quantitative, reproducible instance of the documented risk, not a
  # one-off. A monotone-in-block-length p-value rise (not just a single pair) is the robust signal.
  expect_gt(mcs_b3$p, 0)
  expect_gt(mcs_b14$p, mcs_b3$p * 3)   # at least a 3x rise in p from block=3 to block=14

  blocks <- c(1, 3, 7, 14, 30, 45)
  ps <- sapply(blocks, function(b) mcs(M, alpha = 0.10, B = 2000, block = b, seed = 0)$p)
  # Monotone non-decreasing p-value as block grows toward (and past) the cold-snap length --
  # confirms the understatement is systematic, not sampling noise from one block pair.
  expect_true(all(diff(ps) >= -1e-9))
  expect_gt(ps[length(ps)], ps[1] * 50)  # block=45 vs block=1: a dramatic, not marginal, swing
})

test_that(
  paste(
    "winner_stability() stays essentially flat across the same block sweep that swings mcs()'s",
    "p-value by 50x+ -- empirically confirming this package's own documented claim that",
    "winner_stability() (unlike the MCS/identification layer) is NOT block-exploitable, now on",
    "realistic weather-correlated data rather than only the adversarial panel in mcs.R's own docs"
  ), {
  stabilities <- sapply(c(3, 7, 14, 21), function(b) {
    winner_stability(.energy_L, w = NULL, block = b, seed = 1, n_boot = 1000)$stability
  })
  expect_true(all(stabilities > 0.99))
  expect_lt(max(stabilities) - min(stabilities), 0.02)
})

test_that(
  paste(
    "FINDING (gap for human review, not fixed here): resolution_report()'s weight/volatility",
    "warning is silently defeated on this realistic panel because its 'binding_rival' (closest",
    "pooled mean) differs from decision_breakdown()'s 'binding_opponent' (the k*-relevant rival)",
    "-- the exact mismatch resolution.R's own .binding_rival() documentation says occurs on",
    "18-25% of panels, here concretely preventing the warning from firing on the one risk a real",
    "winter-weighting choice actually creates"
  ), {
  champ <- pooled_winner(.energy_L, .winter_w)
  expect_equal(champ, "gbm")

  rr <- resolution_report(.energy_L, w = .winter_w)
  # .binding_rival() names temp_reg (closest weighted pooled mean to gbm), NOT lstm -- even though
  # lstm is the model whose cold-snap-concentrated tail weakness is what the winter weighting
  # scheme actually amplifies. This is the pre-existing, documented tie-break mismatch between
  # .binding_rival() and decision_breakdown()'s binding_opponent (see resolution.R's own comment
  # above .binding_rival()), concretely confirmed to matter for THIS warning on realistic data.
  expect_equal(rr$binding_rival, "temp_reg")

  db <- decision_breakdown(.energy_L, .winter_w, a = champ)
  expect_equal(db$binding_opponent, "lstm")

  # No warning fires for the gbm-vs-temp_reg pair resolution_report() actually checks...
  warnings_seen <- character(0)
  withCallingHandlers(
    resolution_report(.energy_L, w = .winter_w),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("POSITIVELY correlate", warnings_seen, fixed = TRUE)))

  # ...even though the SAME diagnostic, computed against the realistically-relevant gbm-vs-lstm
  # pair instead, WOULD have crossed the exact threshold resolution_report() itself uses (CV>0.3
  # and correlation>0.2) -- confirming the risk is real and present, just not surfaced, on this
  # panel. Winter weight correlates strongly with gbm-vs-lstm differential volatility because the
  # cold snap (where lstm's extra tail-response term lives) sits in winter.
  cv_w <- sd(.winter_w) / mean(.winter_w)
  expect_gt(cv_w, 0.3)
  diff_gbm_lstm <- as.numeric(.energy_L$lstm) - as.numeric(.energy_L$gbm)
  resid2 <- (diff_gbm_lstm - mean(diff_gbm_lstm))^2
  wv_cor_real_risk <- cor(.winter_w, resid2)
  expect_gt(wv_cor_real_risk, 0.2)
})

test_that("non-uniform (winter-weighted) planning weights correctly disable the native MCS verdict", {
  # A realistic consequence a grid-operator team following report()'s normal workflow would hit
  # immediately: weighting winter months more heavily for winter-peak planning purposes means
  # VERDICT becomes unavailable (MCS has no native non-uniform-weight support), NOT silently
  # wrong -- report() must say so explicitly rather than compute an unweighted MCS underneath a
  # weighted leaderboard.
  panel_w <- loss_panel(.energy_L, weights = .winter_w, labels = .day_e)
  r <- report(panel_w, seed = 0)
  txt <- unclass(r)
  expect_match(txt, "VERDICT: not available for this panel")
  expect_match(txt, "non-uniform period weights")
  # The LEADERBOARD/RESOLUTION sections still compute and print using the real weights.
  expect_match(txt, "gbm.*\\(champion\\)")
  expect_match(txt, "RESOLUTION")
})
