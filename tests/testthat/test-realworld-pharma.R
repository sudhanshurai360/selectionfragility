# test-realworld-pharma.R -- a distinct, PERMANENT regression suite built from a genuinely
# realistic PHARMACEUTICAL DEMAND FORECASTING use case, as opposed to the small adversarial/toy
# panels exercised elsewhere in this test suite.
#
# Business setting: a pharmaceutical manufacturer's commercial forecasting team compares 4
# competing models for monthly demand (T=72, 6 years) of a branded drug that goes through a
# KNOWN, PREDICTABLE patent-cliff event partway through its life. Unlike most "shocks" tested
# elsewhere in this package's test suite (supply disruptions, weather, competitive entry --
# genuinely unforeseeable at the time), a patent-expiration date is public and litigated YEARS in
# advance, so a model that fails to anticipate it is failing at something it had every opportunity
# to get right -- a more damning failure than missing a true surprise. This file is also the
# first in this test suite to combine, in one scenario, a genuine clinical-adoption S-curve launch
# ramp (qualitatively different in shape from retail/seasonal demand -- slow start, inflection,
# plateau) with an abrupt structural-break collapse, which is exactly the shape real branded-drug
# lifecycle demand takes and that regulatory/manufacturing lead times (built years ahead of the
# cliff) make costly to misjudge.
#
# Every set.seed() below is load-bearing: this file asserts on the concrete numbers that seed
# produces, not just "no crash."

## =========================================================================================
## SCENARIO -- 6 years of monthly demand (T=72) for a single branded drug, spanning a known
## patent-cliff event at month 42 (month 1 = 2021-01, cliff = 2024-06).
##
## True demand: a clinical-adoption S-curve ramp (slow launch uptake, steepening, plateau by
## ~month 36) with mild seasonality, followed by an abrupt ~85% collapse over 3 months once
## AB-rated generics enter (patent-cliff mechanics: generic substitution captures most volume
## within one quarter of entry, not gradually), settling into a small ~15%-of-peak long-run
## residual (persistent brand loyalty / indication niches generics don't fully displace).
##
## Four competing models, each with a DIFFERENT, domain-realistic failure mode around the cliff:
##   - trend_baseline:  historical-trend extrapolation (trailing 12-month trend). Has literally
##                       no representation of IP events -- it is blind to a date every other
##                       model in this comparison had years of advance public notice of. Tracks
##                       the smooth pre-cliff S-curve fine, but is blindsided exactly at the cliff.
##   - epi_model:        epidemiology/prevalence-based. Built around the treatable patient
##                       population, not market-share mechanics, so it gets the cliff's TIMING
##                       right (it has a scheduled generic-entry step) but badly underestimates
##                       its MAGNITUDE (assumes a persistent brand-loyal residual far above the
##                       true generic-driven collapse) -- a realistic epi-model failure mode.
##   - survey_model:     prescriber-survey-based, lagged ~3 months (quarterly survey cadence;
##                       physician self-report of prescribing intent lags point-of-dispense
##                       generic substitution) -- slow to recognize the cliff at all, in either
##                       direction, but not direction-wrong once it catches up.
##   - ml_competitor:    an ML model engineered with explicit competitor/generic-entry signals
##                       (ANDA-approval/days-to-patent-expiry features) -- anticipates both the
##                       timing AND magnitude of the collapse closely, with only ordinary noise.
## =========================================================================================

.pharma_scenario <- function() {
  set.seed(20261118)
  T_total <- 72
  month <- 1:T_total
  cliff_month <- 42  # 2024-06: the patent-cliff month, known years in advance

  s_curve <- 100000 / (1 + exp(-(month - 18) / 5))
  seasonal <- 1 + 0.04 * sin(2 * pi * (month - 1) / 12)
  pre_cliff_level <- s_curve * seasonal

  post_mult <- rep(1, T_total)
  post_mult[cliff_month]     <- 0.85
  post_mult[cliff_month + 1] <- 0.45
  post_mult[cliff_month + 2] <- 0.18
  tail_idx <- (cliff_month + 3):T_total
  post_mult[tail_idx] <- 0.15 + 0.02 * sin(2 * pi * (tail_idx - 1) / 12)

  true_demand <- pre_cliff_level
  true_demand[cliff_month:T_total] <- pre_cliff_level[cliff_month:T_total] * post_mult[cliff_month:T_total]
  true_demand <- pmax(true_demand, 500)

  # Model 1: historical-trend baseline -- IP-event-blind trailing-12-month trend extrapolation.
  trend_fc <- numeric(T_total)
  for (t in 1:T_total) {
    if (t <= 13) {
      trend_fc[t] <- true_demand[max(1, t - 1)]
    } else {
      recent <- true_demand[(t - 12):(t - 1)]
      slope <- (recent[12] - recent[1]) / 11
      trend_fc[t] <- recent[12] + slope
    }
  }
  trend_fc <- pmax(trend_fc, 0)

  # Model 2: epidemiology/prevalence model -- right timing, wrong (too high) magnitude.
  epi_fc <- pre_cliff_level
  epi_post_mult <- rep(1, T_total)
  epi_post_mult[cliff_month]     <- 0.88
  epi_post_mult[cliff_month + 1] <- 0.62
  epi_post_mult[cliff_month + 2] <- 0.48
  epi_post_mult[tail_idx] <- 0.45 + 0.02 * sin(2 * pi * (tail_idx - 1) / 12)
  epi_fc[cliff_month:T_total] <- pre_cliff_level[cliff_month:T_total] * epi_post_mult[cliff_month:T_total]
  epi_fc <- pmax(epi_fc * (1 + rnorm(T_total, 0, 0.03)), 0)

  # Model 3: prescriber-survey model -- ~3-month-lagged version of the true path.
  lag_k <- 3
  survey_fc <- sapply(1:T_total, function(t) true_demand[max(1, t - lag_k)])
  survey_fc <- pmax(survey_fc * (1 + rnorm(T_total, 0, 0.05)), 0)

  # Model 4: ML model with competitor/generic-entry signals -- anticipates timing AND magnitude.
  ml_fc <- pmax(true_demand * (1 + rnorm(T_total, 0, 0.06)), 0)

  L <- list(
    trend_baseline = abs(true_demand - trend_fc),
    epi_model      = abs(true_demand - epi_fc),
    survey_model   = abs(true_demand - survey_fc),
    ml_competitor  = abs(true_demand - ml_fc)
  )
  month_labels <- seq(as.Date("2021-01-01"), by = "month", length.out = T_total)

  list(
    L = L, labels = month_labels, T = T_total,
    cliff_month = cliff_month,
    cliff_window = cliff_month:(cliff_month + 5)  # 2024-06 .. 2024-11, the formal 6-month window
  )
}

test_that("Scenario builds as a valid loss_panel on realistic monthly pharma demand", {
  sc <- .pharma_scenario()
  panel <- loss_panel(sc$L, labels = sc$labels)
  expect_equal(panel$models, c("trend_baseline", "epi_model", "survey_model", "ml_competitor"))
  expect_equal(length(panel$labels), 72)
  for (m in panel$models) expect_true(all(is.finite(panel$losses[[m]])) && all(panel$losses[[m]] >= 0))
  # Business sanity on the underlying demand process itself: the patent cliff really is an
  # abrupt, large collapse (not a gradual decline) -- the defining pharma-specific feature this
  # scenario exists to exercise, distinct from any gradual/seasonal shock tested elsewhere.
  # trend_baseline tracks the smooth pre-cliff S-curve well (low loss), so its own loss jumping
  # sharply right at the cliff window is a direct signature of an abrupt, not gradual, collapse.
  expect_gt(mean(sc$L$trend_baseline[sc$cliff_window]), 5 * mean(sc$L$trend_baseline[1:(sc$cliff_month - 1)]))
})

test_that("report(): ml_competitor (anticipated the cliff) is pooled champion; trend_baseline (blind to a KNOWN event) is still in the MCS", {
  # Business meaning: the model that explicitly engineered in the publicly-known patent-expiry
  # timeline wins outright on pooled mean loss. The historical-trend baseline -- which had zero
  # representation of an event every competitor model (and the real-world patent registry) knew
  # about years in advance -- is NOT yet statistically eliminated at alpha=0.10 over the full
  # 6-year window, because its damage is concentrated in a handful of transition months rather
  # than smeared evenly across all 72. That concentration is exactly the kind of thing a
  # planning team must see named explicitly (via PIVOT/decision_breakdown below), not just
  # inferred from "not identified."
  sc <- .pharma_scenario()
  panel <- loss_panel(sc$L, labels = sc$labels)
  out <- report(panel, alpha = 0.10, seed = 1)
  expect_match(out, "VERDICT: not identified \\(MCS size 2 of 4\\)")
  expect_match(out, "ml_competitor\\s+mean=[0-9.]+\\s+in MCS\\s+periods won=\\d+ \\(champion\\)")
  expect_match(out, "trend_baseline\\s+mean=[0-9.]+\\s+in MCS\\s+periods won=\\d+")
  expect_match(out, "survey_model\\s+mean=[0-9.]+\\s+excluded")
  expect_match(out, "epi_model\\s+mean=[0-9.]+\\s+excluded")
  # RESOLUTION: the observed edge is not yet powered at 80% over 6 years of monthly data --
  # a genuinely useful, sobering number for a team deciding how much lifecycle history to
  # require before fully retiring the trend-baseline approach for the NEXT product launch.
  expect_match(out, "cannot determine at this sample size")
})

test_that("decision_breakdown(): the patent-cliff transition months are the dominant pivot, with one secondary knock-on period named explicitly", {
  # Business meaning (this is the core interpretive test the task asks for): k* identifies WHICH
  # periods are pivotal to the ml_competitor-vs-trend_baseline decision. If the champion's edge
  # were driven by ordinary noise scattered across 6 years, the pivotal periods would be a
  # scattered, unremarkable handful. Instead, the package should -- and does -- name periods that
  # fall overwhelmingly inside the formal 6-month cliff window (2024-06 .. 2024-11), which is the
  # quantitative confirmation that this is a "model failed to anticipate a KNOWN, dated event,"
  # not a "model got unlucky on an arbitrary month" story. That distinction matters for planning
  # the next product: a scattered k* would counsel nothing specific, while a cliff-concentrated
  # k* is a direct indictment of the trend-baseline approach for any future launch with a known
  # IP-expiry date on the calendar.
  sc <- .pharma_scenario()
  panel <- loss_panel(sc$L, labels = sc$labels)
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ml_competitor")
  db <- decision_breakdown(panel$losses, panel$weights, a = champ, return_ties = TRUE)
  expect_identical(db$binding_opponent, "trend_baseline")
  expect_identical(db$k, 4L)
  removed_months <- sort(db$removed)
  expect_identical(removed_months, c(42L, 43L, 44L, 52L))
  in_cliff_window <- sum(removed_months %in% sc$cliff_window)
  # At least 3 of the 4 k* periods fall inside the formal 6-month cliff window -- this is the
  # "is the patent cliff what's actually driving the ranking" check, answered quantitatively.
  expect_gte(in_cliff_window, 3)
  expect_lt(db$k / sc$T, 0.10)  # fewer than 10% of 72 months needed to flip the champion
})

test_that("fragility(): shock_coincidence correctly flags the one pivotal period OUTSIDE the formal cliff window as a genuine finding, not a false negative", {
  # Business meaning: fragility()'s shock_coincidence is TRUE only if EVERY k*-pivotal period
  # falls inside the declared shock/event window. Here it is correctly FALSE: one of the four
  # pivotal months (2025-04, ~10 months after the formal 6-month cliff window closes) is a
  # knock-on effect the narrow "cliff = 6 months" framing would have missed entirely -- e.g. a
  # year-over-year inventory/channel correction once wholesalers finish de-stocking the branded
  # product. A planning team reading ONLY "shock_coincidence = FALSE" without decision_breakdown's
  # `responsible` list might wrongly conclude the cliff wasn't the story; this test exists to
  # confirm the package's literal all-or-nothing semantics here, and to confirm a WIDER window
  # that actually covers the knock-on period flips it to TRUE, i.e. the field is behaving exactly
  # as documented, not silently degrading on a partial match.
  sc <- .pharma_scenario()
  panel <- loss_panel(sc$L, labels = sc$labels)
  cliff_window_labels <- as.character(sc$labels[sc$cliff_window])

  frag_narrow <- fragility(panel$losses, panel$weights,
                            shock_periods = cliff_window_labels,
                            labels = as.character(sc$labels))
  expect_identical(frag_narrow$pooled_winner, "ml_competitor")
  expect_false(frag_narrow$reversal)             # pooled winner also wins the per-period plurality
  expect_false(frag_narrow$degenerate_margin)
  expect_identical(frag_narrow$shock_coincidence, FALSE)
  expect_true("2025-04-01" %in% frag_narrow$responsible)

  wide_window_labels <- as.character(sc$labels[sc$cliff_month:(sc$cliff_month + 10)])
  frag_wide <- fragility(panel$losses, panel$weights,
                          shock_periods = wide_window_labels,
                          labels = as.character(sc$labels))
  expect_identical(frag_wide$shock_coincidence, TRUE)
})

test_that("resolution_report(): ml_competitor's edge over trend_baseline is real in direction but not yet resolved at 80% power over 6 years", {
  # Business meaning: this is the number that should actually govern the NEXT product's
  # forecasting-approach decision -- not "ml_competitor has the lower mean loss" (true, but by
  # itself insufficient) but "even 6 years of monthly history is not enough to call this edge
  # resolved at 80% power." For a manufacturer sizing years-ahead manufacturing capacity off a
  # single launch's forecast comparison, that is a materially different, more cautious
  # recommendation than "just switch to the ML model" would be on its own.
  sc <- .pharma_scenario()
  panel <- loss_panel(sc$L, labels = sc$labels)
  res <- resolution_report(panel$losses, panel$weights)
  expect_identical(res$champion, "ml_competitor")
  expect_identical(res$binding_rival, "trend_baseline")
  expect_false(res$resolved)
  expect_true(res$observed_edge > 0 && res$observed_edge < res$mde)
})

test_that("compare(): a post-cliff re-evaluation shows the pre-cliff champion (epi_model) was overtaken, and that this change is attributable to the new (post-cliff) periods, with act=TRUE", {
  # Business meaning: this is the "which model should we trust for the NEXT launch's lifecycle
  # forecasting" question, answered as a run-over-run comparison. Before the cliff (the first
  # 41 months), epi_model is actually the BEST model -- it has the lowest pre-cliff loss of all
  # four, because trend_baseline and ml_competitor both carry a bit more pre-cliff noise and
  # survey_model's lag matters more while demand is still ramping. A commercial-forecasting team
  # evaluating models ONLY on pre-cliff data (the kind of early-launch evaluation window that is
  # all that exists for a brand-new product) would have crowned epi_model. compare()'s
  # post-cliff re-evaluation panel (the full 72-month run) demonstrates exactly why that
  # pre-cliff-only evaluation would have been the wrong one to generalize from for lifecycle
  # planning: once the KNOWN, dated patent-cliff periods are included, the champion flips to
  # ml_competitor, that flip is attributable entirely to the new (post-cliff) periods (removing
  # them reverts to epi_model), and the old champion (epi_model) has demonstrably LEFT the MCS
  # -- not just lost the mean by noise -- so act=TRUE is the correct, actionable signal for
  # retiring the epi-model-only approach on the NEXT product's lifecycle forecast.
  sc <- .pharma_scenario()
  panel_prev <- loss_panel(lapply(sc$L, function(v) v[1:(sc$cliff_month - 1)]),
                            labels = sc$labels[1:(sc$cliff_month - 1)])
  panel_curr <- loss_panel(sc$L, labels = sc$labels)

  prev_champ <- pooled_winner(panel_prev$losses, panel_prev$weights)
  expect_identical(prev_champ, "epi_model")  # pre-cliff-only evaluation picks epi_model

  cr <- compare(panel_prev, panel_curr, alpha = 0.10, seed = 1)
  expect_s3_class(cr, "change_report")
  expect_identical(cr$previous_champion, "epi_model")
  expect_identical(cr$current_champion, "ml_competitor")
  expect_true(cr$champion_changed)
  expect_null(cr$mcs_error)
  expect_identical(cr$champion_without_new_periods, "epi_model")
  expect_identical(cr$n_new_periods, as.integer(sc$T - (sc$cliff_month - 1)))
  expect_identical(cr$old_champion_still_in_mcs, FALSE)
  expect_true(cr$act)
  expect_identical(cr$act, isTRUE(cr$champion_changed && isFALSE(cr$old_champion_still_in_mcs)))

  out <- capture.output(print(cr))
  expect_length(out, 1)
  expect_match(out, "champion changed epi_model -> ml_competitor")
  expect_match(out, "act=TRUE")
})
