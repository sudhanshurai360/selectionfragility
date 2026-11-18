# REALISTIC DOMAIN TEST: manufacturing predictive-maintenance / equipment-failure-risk
# forecast-model selection.
#
# BUSINESS CONTEXT: a manufacturing plant's reliability engineering team compares 4 competing
# failure-risk forecasting models for a specific critical machine class (e.g. a fleet of CNC
# spindle motors) -- survival_pof (a physics-of-failure/survival-analysis model that tracks
# degradation precursors against known failure-mode physics), vibration_threshold (a
# vibration/sensor-threshold model tuned to classic mechanical failure signatures), ml_sensor (a
# machine-learning model trained on historical sensor + failure-event data -- usually the best
# average performer, but with no guarantee of graceful degradation on a genuinely novel failure
# mode it never saw in training), and time_since_maint (a simple time-since-last-maintenance
# baseline with no sensor awareness at all) -- in order to decide which ONE model to standardize
# on across the whole plant's reliability program.
#
# CADENCE / SCALE, JUSTIFIED: daily aggregated sensor-derived risk scores over 2.5 years gives
# T = 913 periods -- the realistic daily-rollup cadence a plant reliability team actually acts
# on (raw sensor streams are much higher frequency, but the risk SCORE used for a go/no-go
# maintenance decision is normally aggregated to a daily figure), and comfortably in the
# "large T" regime this package's asymptotic machinery (MCS bootstrap, RESOLUTION power/MDE,
# winner_stability) is designed for -- unlike several of this package's other realistic domain
# fixtures (weather-risk T=198, insurance T=12) where T is small by construction.
#
# EXTREME RARE-EVENT CONCENTRATION (the defining feature of this domain, more extreme than the
# catastrophe-insurance/weather-risk fixtures elsewhere in this test suite): actual equipment
# failures for a well-maintained critical machine class are genuinely rare, BINARY events, not
# merely unusually-large continuous losses. The loss here is a Brier-type calibration loss on
# the daily failure-risk score: on the ~99.3% of days with no failure, loss = score^2 and is
# tiny (scores are near zero, as they should be); on the handful of days immediately preceding
# an actual failure, loss = (1 - score)^2 and is large unless the model's risk score was
# correctly elevated just before the failure. This means essentially ALL of the real decision
# signal is concentrated in a tiny fraction of periods -- in PANEL 1 below, 6 failure days out of
# 913 (0.66% of periods); in PANEL 2, 7 failure days out of 913 (0.77%) -- an even sharper
# concentration than the weather-risk fixture's 5-of-198 (2.5%) hurricane weeks.
#
# TWO REALISTIC SUB-SCENARIOS are used because they surface genuinely different (and both
# informative) aspects of how this package's fragility diagnostics handle extreme rare-event
# concentration:
#   PANEL 1: one single failure day is both NECESSARY and SUFFICIENT to flip the plant's
#     model-selection decision (k*=1 of 913) -- an even more extreme fragility signature than
#     any other realistic fixture in this suite, and a case where concentration_share()
#     correctly reports high concentration because there genuinely IS only one dominant period.
#   PANEL 2: the decision instead hinges on TWO comparably-sized failure days acting together
#     (k*=2 of 913), out of 7 total failures -- the realistic "multi-event" case, used to check
#     concentration_share()'s documented single-period-only limitation specifically in this
#     binary-rare-failure context (see test 4 below).

build_pdm_panel <- function(fail_days, risk_before_failure, T = 913L, seed_base = 20261118L,
                             seed_fail = 99L) {
  models <- c("survival_pof", "vibration_threshold", "ml_sensor", "time_since_maint")
  set.seed(seed_base)
  L <- stats::setNames(lapply(models, function(m) rep(NA_real_, T)), models)
  for (m in models) {
    # Non-failure days: tiny Brier-type calibration loss (score^2, actual=0). Shape parameters
    # reflect each model's typical false-positive-rate character: ml_sensor is the best
    # calibrated on ordinary days (tightest, lowest baseline score), vibration_threshold and
    # time_since_maint run "noisier"/less specific day to day.
    base <- switch(m,
      survival_pof         = stats::rbeta(T, 2, 400),
      vibration_threshold  = stats::rbeta(T, 2, 250),
      ml_sensor             = stats::rbeta(T, 2, 500),
      time_since_maint     = stats::rbeta(T, 2, 180)
    )
    L[[m]] <- base^2
  }
  set.seed(seed_fail)
  for (m in models) {
    L[[m]][fail_days] <- (1 - risk_before_failure[[m]])^2
  }
  labels <- format(seq(as.Date("2022-01-01"), by = "day", length.out = T))
  loss_panel(L, labels = labels)
}

# PANEL 1: 6 failures in 2.5 years (a realistic rate for a well-maintained critical machine
# class). survival_pof (physics-based) correctly anticipates most failures early, EXCEPT one
# genuinely novel failure mode (a contamination-induced bearing seizure with no physics-model
# precedent, failure #3). vibration_threshold misses failures with no vibration signature
# (electrical/contamination modes, #2 and #6). ml_sensor is the best average performer but,
# because it is trained on historical sensor+failure data, struggles on OUT-OF-DISTRIBUTION
# modes it never saw in training (#3, and critically #5 -- see test 1). time_since_maint is
# uninformative about any SPECIFIC failure, by construction.
panel1_fail_days <- sort(c(120, 285, 410, 520, 700, 860))
panel1_risk <- list(
  survival_pof         = c(0.85, 0.80, 0.05, 0.78, 0.82, 0.75),
  vibration_threshold  = c(0.70, 0.10, 0.65, 0.72, 0.68, 0.15),
  ml_sensor             = c(0.88, 0.84, 0.10, 0.86, 0.20, 0.82),
  time_since_maint     = c(0.30, 0.25, 0.28, 0.32, 0.29, 0.31)
)

test_that("PANEL 1: a single rare failure day (1 of 913) is both necessary and sufficient to flip the plant's model-selection decision, and report() correctly flags the champion's edge as unresolved rather than overstating confidence from the large T", {
  panel <- build_pdm_panel(panel1_fail_days, panel1_risk)
  expect_identical(length(panel$losses[[1]]), 913L)

  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "survival_pof")

  rpt <- report(panel, alpha = 0.10, seed = 1)
  # Despite T=913 (a large sample by this package's own usual standards), report() does NOT let
  # the large T manufacture false confidence: the champion is NOT alone in the MCS, exactly
  # because its edge is carried by a single rare event rather than a broad, consistent skill
  # advantage -- the correct, non-overconfident read for this domain.
  expect_match(rpt, "VERDICT: not identified \\(MCS size 4 of 4\\)")
  expect_match(rpt, "survival_pof +mean=0\\.00[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")

  # --- decision_breakdown(): literally ONE failure day is necessary and sufficient to flip -----
  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$k, 1L)
  expect_identical(db$binding_opponent, "ml_sensor")
  # The single pivotal day is failure #5 (day 700), where ml_sensor (OOD failure mode, risk
  # score only 0.20) badly underestimates risk while survival_pof (0.82) correctly anticipates
  # it -- i.e. the one failure day where the champion's physics-based approach is CLEARLY better
  # than the ML model is, by itself, the entire reason the decision is fragile: remove it and
  # survival_pof's pooled edge over ml_sensor disappears.
  expect_identical(sort(db$removed), 700L)
  expect_true(700L %in% panel1_fail_days)

  # --- margin accounting: this single failure day accounts for essentially the WHOLE net ------
  # margin (allowing for the other 5 failures' small, partly-offsetting contributions, and the
  # negligible ordinary-day noise) -- a cleaner, more extreme version of the "one shock drives
  # everything" pattern than the multi-catastrophe weather-risk fixture shows, because here
  # there really is only one dominant day among the 6 rare events, not several comparable ones.
  la <- panel$losses[[champ]]; lb <- panel$losses[["ml_sensor"]]
  contrib <- lb - la
  expect_gt(contrib[700] / sum(contrib), 1.0)  # single-day contribution exceeds the net margin
  expect_gt(sum(abs(contrib[panel1_fail_days])) / sum(abs(contrib)), 0.90)

  # --- Prop 2.2: even at T=913, the zero-simulation bound certifies this k*=1 edge as NOT -------
  # distinguishable from noise -- consistent with the MCS bootstrap's "not identified" read.
  # This is the package handling the extreme-rare-event case SENSIBLY: large T alone does not
  # inflate confidence when the decision's true k* is tiny relative to T.
  expect_true(prop22_certifies(db$k, T = 913))
})

test_that("PANEL 1: concentration_share() correctly reports HIGH concentration here, because there genuinely is only one dominant failure day (contrast with PANEL 2 below)", {
  panel <- build_pdm_panel(panel1_fail_days, panel1_risk)
  cs <- concentration_share(panel)
  # With a true single-dominant-event structure, concentration_share()'s single-period-share
  # design matches the real concentration well (not an understatement here) -- this is the
  # control case showing the statistic is NOT broken, only scoped to a single period by design.
  expect_gt(cs, 0.65)
})

# PANEL 2: 7 failures in 2.5 years. vibration_threshold (champion here) reliably catches
# mechanical-signature failures (risk ~0.76-0.82) across most of the fleet's failure history,
# EXCEPT the final failure (#7), a non-mechanical (electrical) mode it is structurally blind to
# (0.15). survival_pof misses one genuinely novel failure mode (#3, 0.05) and is otherwise
# moderate (~0.48-0.55) -- not sharp enough to beat vibration_threshold on its home turf.
# ml_sensor misses TWO out-of-distribution modes (#3 and #5) it never saw in training.
panel2_fail_days <- sort(c(90, 200, 340, 480, 610, 750, 860))
panel2_risk <- list(
  survival_pof         = c(0.55, 0.50, 0.05, 0.52, 0.48, 0.53, 0.75),
  vibration_threshold  = c(0.80, 0.78, 0.82, 0.76, 0.79, 0.81, 0.15),
  ml_sensor             = c(0.70, 0.65, 0.10, 0.68, 0.20, 0.66, 0.60),
  time_since_maint     = c(0.30, 0.25, 0.28, 0.32, 0.29, 0.31, 0.27)
)

test_that("PANEL 2: a TWO-failure-day decision (k*=2 of 913), and report() again correctly flags the decision as unresolved", {
  panel <- build_pdm_panel(panel2_fail_days, panel2_risk)

  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "vibration_threshold")

  rpt <- report(panel, alpha = 0.10, seed = 1)
  # time_since_maint (the uninformative baseline) is correctly excluded from the MCS; the three
  # sensor-aware models remain statistically indistinguishable from one another.
  expect_match(rpt, "VERDICT: not identified \\(MCS size 3 of 4\\)")
  expect_match(rpt, "time_since_maint +mean=0\\.00[0-9]+ +excluded")

  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$k, 2L)
  expect_identical(db$binding_opponent, "ml_sensor")
  removed <- sort(db$removed)
  expect_identical(removed, c(340L, 610L))
  expect_true(all(removed %in% panel2_fail_days))

  # Prop 2.2 again certifies this k* as not distinguishable from noise, consistent with "not
  # identified" -- the package gives a consistent fragility read across both its zero-simulation
  # certificate and its bootstrap MCS, even in this large-T, extreme-rare-event setting.
  expect_true(prop22_certifies(db$k, T = 913))
})

test_that("PANEL 2: concentration_share() UNDERSTATES multi-event concentration in this binary-rare-failure context -- even more severely than the weather-risk fixture's catastrophe-week case (genuine, pre-existing, documented limitation, re-confirmed here, not a new bug)", {
  # decision_breakdown()'s own k*=2 and the direct margin accounting below both show the
  # champion-vs-opponent decision is driven almost ENTIRELY by the handful of rare failure days
  # (7 of 913, 0.77% of periods) -- but concentration_share() is explicitly scoped (see
  # ?concentration_share) to the SINGLE most influential period only, so with two comparably-
  # sized pivotal failure days (not one dominant one, as in Panel 1), it reports a much smaller
  # number than the true multi-event concentration.
  panel <- build_pdm_panel(panel2_fail_days, panel2_risk)
  champ <- pooled_winner(panel$losses, panel$weights)
  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  la <- panel$losses[[champ]]; lb <- panel$losses[[db$binding_opponent]]
  contrib <- lb - la

  cs <- concentration_share(panel)
  # The single largest failure day accounts for well under half of the margin's gross
  # dispersion ...
  expect_lt(cs, 0.40)
  # ... even though ALL SEVEN rare failure days together account for essentially the ENTIRE
  # gross dispersion of the margin (>95%) -- i.e. this decision is, in truth, almost completely
  # driven by rare binary failure events, just spread across more than one of them.
  expect_gt(sum(abs(contrib[panel2_fail_days])) / sum(abs(contrib)), 0.95)
  # This is a SHARPER instance of the already-documented "concentration_share() understates
  # multi-event concentration" limitation than the weather-risk fixture elsewhere in this suite
  # (there: ~0.30 reported vs. ~0.75 true gross share; here: <0.40 reported vs. >0.95 true gross
  # share) -- consistent with the task's expectation that binary rare-event domains like this one
  # make the limitation even more visible than continuous-magnitude catastrophe domains do.
})

test_that("winner_stability()/exchangeable_benchmark() give a RESAMPLING-BASED read that is reassuring relative to decision_breakdown()'s ADVERSARIAL k*, on BOTH panels -- the same contradiction documented for the weather-risk fixture, reconfirmed in this even-more-extreme rare-binary-failure domain", {
  # decision_breakdown()'s k* asks the adversarial question (fewest periods whose deletion flips
  # the decision); winner_stability() instead resamples WITH REPLACEMENT across all 913 periods,
  # so it is numerically unlikely for a resample to omit the pivotal failure day(s) entirely --
  # giving a much less alarming stability read than k*=1 (panel 1) or k*=2 (panel 2) alone would
  # suggest. Both statistics are individually correct; a reliability team that checked ONLY
  # winner_stability()/exchangeable_benchmark() (the package's own documented "complementary
  # fragility read") could come away with a falsely reassuring sense of robustness for exactly
  # the extreme-rare-event scenario this domain is built around.
  panel1 <- build_pdm_panel(panel1_fail_days, panel1_risk)
  panel2 <- build_pdm_panel(panel2_fail_days, panel2_risk)

  ws1 <- winner_stability(panel1, seed = 1, n_boot = 2000)
  ws2 <- winner_stability(panel2, seed = 1, n_boot = 2000)
  bench <- exchangeable_benchmark(K = 4L, T = 913L, n_rep = 200, n_boot = 500, seed = 1)

  # Neither panel's bootstrap stability drops below the pure-chance null's 5th percentile --
  # i.e. neither is flagged as "unusually fragile" by winner_stability() alone, even though
  # decision_breakdown() (above) shows both decisions hinge on one or two specific rare days.
  expect_gt(ws1$stability, bench$p05)
  expect_gt(ws2$stability, bench$p05)
  # Panel 1's single-dominant-event structure makes it resample-fragile even on its own terms
  # (close to a coin flip): still numerically above, but not far above, the chance floor.
  expect_lt(ws1$stability, 0.60)
})

test_that("pivot_agreement() reconfirms its documented frac-tracking weakness on this extreme rare-binary-failure domain (pre-existing, documented limitation)", {
  # Per ?pivot_agreement's own Limitation section, this statistic tracks the `frac` subsampling
  # knob rather than the panel's true concentration, because it subsamples WITHOUT replacement
  # from already-realized values. Checked here on both panels for the realistic rare-event domain
  # this task specifically asks about.
  panel1 <- build_pdm_panel(panel1_fail_days, panel1_risk)
  panel2 <- build_pdm_panel(panel2_fail_days, panel2_risk)

  pa1_80 <- pivot_agreement(panel1, seed = 1, frac = 0.80)
  pa1_50 <- pivot_agreement(panel1, seed = 1, frac = 0.50)
  pa2_80 <- pivot_agreement(panel2, seed = 1, frac = 0.80)
  pa2_50 <- pivot_agreement(panel2, seed = 1, frac = 0.50)

  # Both panels track frac (close to 0.8 at frac=0.8, close to 0.5 at frac=0.5), reproducing the
  # documented pattern rather than reflecting the very different true concentration structures
  # (Panel 1 single-dominant-day vs. Panel 2 two-comparable-days) established above.
  for (pa in c(pa1_80, pa2_80)) expect_equal(pa, 0.80, tolerance = 0.06)
  for (pa in c(pa1_50, pa2_50)) expect_equal(pa, 0.50, tolerance = 0.10)
})
