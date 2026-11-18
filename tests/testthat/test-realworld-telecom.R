# test-realworld-telecom.R — telecom/network-traffic forecasting with a PERMANENT
# 5G-rollout step-change in baseline traffic.
#
# Real-world grounding for this scenario (distinct from test-realworld-techcapacity.R's viral-
# traffic TRANSIENT spike, which reverts after a few days):
#   - A regional network segment ("West-Valley-7," a cell-tower cluster) is forecast daily by
#     four models that commonly co-exist in a telecom capacity-planning roster:
#       * seasonal_naive  -- forecasts day t from day (t-7)'s ACTUAL traffic. Because it always
#         looks at the most recent same-weekday actual, it self-corrects to a new baseline within
#         about one week of any level shift -- it needs no retraining to "learn" a step change,
#         it just lags it briefly.
#       * sarima          -- a classical seasonal ARIMA fit once on history and NOT re-fit after
#         the rollout (the realistic failure mode: nobody re-ran the offline fit the week of a
#         network change). Its seasonal/level parameters stay anchored to the PRE-rollout scale
#         forever afterward.
#       * ml_gbm          -- a gradient-boosted ML model, same story: trained once on pre-rollout
#         history, not retrained, so its learned feature/level mapping is permanently stale once
#         the baseline moves.
#       * break_aware      -- a model built specifically to detect and adapt to structural breaks
#         (changepoint-aware). It pays a small accuracy cost before the rollout (conservatism) and
#         a short detection-lag penalty for ~2 weeks after the rollout, then re-bases and tracks
#         the new, permanently higher baseline accurately for the rest of the window.
#   - A 5G/fiber capacity upgrade on this cluster permanently raises baseline daily traffic
#     starting at day 400 of a T=650-day (~21-month) daily panel -- traffic does NOT revert; the
#     new, higher level persists for the rest of the observed history, exactly the "structural,
#     permanent step-change" telecom capacity planning has to handle differently from a transient
#     spike (a stadium event, a DDoS, a viral moment).
#   - sarima and ml_gbm are the models "well-tuned pre-rollout, miscalibrated post-rollout": their
#     per-period loss is LOW before day 400 and PERMANENTLY ELEVATED (not just briefly spiked) from
#     day 400 onward, because nobody re-fit them to the new baseline.
#
# All data below is synthetic (rnorm() around domain-motivated means/sds), built directly as
# per-period LOSS vectors (this package's unit of input), matching the convention already used by
# every other test-realworld-*.R file in this suite -- not raw traffic counts run through a metric.

.telecom_build_panel <- function() {
  set.seed(42)
  T <- 650
  rollout_day <- 400  # day index (1-based) of the 5G/fiber capacity upgrade

  seasonal_naive <- pmax(rnorm(T, 1.00, 0.10), 0.01)
  # One week of elevated loss right at the rollout (catching up to the new baseline via last
  # week's actuals), then back to its normal noise band -- seasonal_naive self-corrects fast.
  seasonal_naive[rollout_day:(rollout_day + 6)] <- pmax(rnorm(7, 2.5, 0.3), 0.01)

  sarima <- numeric(T)
  sarima[1:(rollout_day - 1)] <- pmax(rnorm(rollout_day - 1, 0.85, 0.08), 0.01)
  # PERMANENT step-change in loss, never recovering: sarima was fit once pre-rollout and never
  # re-fit, so its forecasts stay anchored to the old baseline for the rest of the window.
  sarima[rollout_day:T] <- pmax(rnorm(T - rollout_day + 1, 1.80, 0.15), 0.01)

  ml_gbm <- numeric(T)
  ml_gbm[1:(rollout_day - 1)] <- pmax(rnorm(rollout_day - 1, 0.80, 0.07), 0.01)
  # Same permanent-miscalibration story as sarima -- trained once, never retrained.
  ml_gbm[rollout_day:T] <- pmax(rnorm(T - rollout_day + 1, 1.90, 0.18), 0.01)

  break_aware <- numeric(T)
  # Slightly worse than sarima/ml_gbm pre-rollout -- the real cost of changepoint-readiness.
  break_aware[1:(rollout_day - 1)] <- pmax(rnorm(rollout_day - 1, 1.05, 0.09), 0.01)
  transition_end <- rollout_day + 13  # ~2-week detection lag before break_aware re-bases
  break_aware[rollout_day:transition_end] <-
    pmax(rnorm(transition_end - rollout_day + 1, 1.60, 0.20), 0.01)
  # After re-basing, break_aware tracks the new permanent baseline better than any model tracked
  # the OLD baseline -- the whole point of being changepoint-aware.
  break_aware[(transition_end + 1):T] <- pmax(rnorm(T - transition_end, 0.78, 0.08), 0.01)

  L <- list(
    seasonal_naive = seasonal_naive,
    sarima = sarima,
    ml_gbm = ml_gbm,
    break_aware = break_aware
  )
  start_date <- as.Date("2024-03-01")
  labels <- start_date + 0:(T - 1)
  list(
    panel = loss_panel(L, labels = labels),
    rollout_day = rollout_day,
    labels = labels
  )
}

test_that("report()/decision_breakdown() correctly identify break_aware as champion and attribute its edge to the POST-rollout period, not scattered noise", {
  built <- .telecom_build_panel()
  panel <- built$panel
  rollout_day <- built$rollout_day

  champ <- pooled_winner(panel$losses, panel$weights)
  # The structural-break-aware model wins the pooled mean overall, even though it was the WORST
  # of the four models pre-rollout (mean 1.05 vs sarima's 0.85 and ml_gbm's 0.80) -- its post-
  # rollout accuracy on the new permanent baseline outweighs that pre-rollout cost. This is the
  # central telecom business question report() must get right: "is the model that looked worse
  # before the upgrade actually the right choice once the upgrade is permanent?"
  expect_identical(champ, "break_aware")

  db <- decision_breakdown(panel$losses, panel$weights, a = champ)
  expect_true(db$k >= 1)
  # k* should be well below T (a few dozen out of 650), i.e. a real, non-trivial but
  # concentrated decision -- not vacuously robust, not a coin flip either.
  expect_lt(db$k, length(panel$labels) / 2)

  # THE key fragility-diagnostics check for a permanent step-change: decision_breakdown()'s
  # removed (pivotal) periods must be concentrated in the POST-rollout window, not scattered
  # evenly across the whole 650-day history. A diagnostic that misattributed this decision to
  # scattered individual days (rather than the structural break) would mislead an operations
  # engineer into thinking the champion choice is fragile to random noise, when in fact the whole
  # story is the 5G rollout.
  frac_post <- mean(db$removed >= rollout_day)
  expect_gt(frac_post, 0.9)

  rep_txt <- report(panel)
  expect_type(rep_txt, "character")
  expect_match(rep_txt, "LEADERBOARD", fixed = TRUE)
  expect_match(rep_txt, "PIVOT", fixed = TRUE)
  expect_match(rep_txt, "break_aware", fixed = TRUE)
  # report() should name break_aware as champion in LEADERBOARD.
  expect_match(rep_txt, "break_aware.*\\(champion\\)")
  # PIVOT's largest-single-contributor label (k* > 5 here, so report() uses that branch) should
  # fall on or after the rollout month (2025-04, i.e. start_date 2024-03-01 + 399 days) -- the
  # report's own PROSE, not just decision_breakdown()'s raw indices, must point operations at the
  # rollout period as the thing driving the decision.
  expect_match(rep_txt, "largest single contributor: 2025-0[4-9]|largest single contributor: 2025-1|largest single contributor: 2026")
})

test_that("report() does not overstate robustness on this concentrated, rollout-driven decision", {
  built <- .telecom_build_panel()
  rep_txt <- report(built$panel)
  # Even though VERDICT is "identified" and RESOLUTION is "resolved" on this panel (a real,
  # powered edge exists), PIVOT must still visibly disclose how concentrated that edge is --
  # report() should not let an operations engineer walk away thinking the champion choice is
  # trivially, uniformly robust across the whole 21-month history.
  expect_true(grepl("certif", rep_txt, ignore.case = TRUE))
  expect_match(rep_txt, "concentration_share", fixed = TRUE)
})

test_that("compare() correctly attributes a 6-months-post-rollout champion change to the step-change itself, not noise, via a rolling window spanning the rollout", {
  # Operations re-evaluates model choice with a trailing 200-day rolling window, 6 months
  # (~180 days) after the rollout. The two evaluation windows below share a 20-day overlap
  # (days 380-399, entirely PRE-rollout) and differ by 180 new days (400-579, entirely
  # POST-rollout) -- the realistic shape of a rolling-window re-evaluation that straddles a
  # structural break, built with real calendar-date labels so compare()'s label-based new-period
  # attribution applies (see compare()'s own documentation: positional labels would NOT support
  # this attribution on a same-length rolling window).
  built <- .telecom_build_panel()
  L <- lapply(built$panel$losses, as.numeric)
  labels <- built$labels

  prev_idx <- 200:399   # trailing window ending the day before the rollout
  curr_idx <- 380:579   # same-length window, 180 days later, straddling the rollout

  prev_panel <- loss_panel(lapply(L, function(v) v[prev_idx]), labels = labels[prev_idx])
  curr_panel <- loss_panel(lapply(L, function(v) v[curr_idx]), labels = labels[curr_idx])

  cr <- compare(prev_panel, curr_panel)
  expect_s3_class(cr, "change_report")

  # Pre-rollout, the roster's best model is one of the "well-tuned pre-rollout" pair (ml_gbm/
  # sarima), NOT break_aware -- confirming the pre-rollout baseline this scenario is built on.
  expect_true(cr$previous_champion %in% c("ml_gbm", "sarima"))

  # The 180 new (post-rollout) periods flip the champion to break_aware -- this IS the real,
  # structural effect of the 5G rollout entering the rolling window, not an artifact.
  expect_identical(cr$current_champion, "break_aware")
  expect_true(cr$champion_changed)

  # n_new_periods must correctly equal the 180 post-rollout days that are genuinely new to this
  # window (not 0, not the full 200) -- this is the load-bearing number telling an operations
  # engineer "the window moved past a real event," not just drifted.
  expect_identical(cr$n_new_periods, 180L)

  # THE key actionable-attribution check: champion_without_new_periods (computed on just the
  # 20-day pre-rollout overlap) must NOT be break_aware. In other words: if the 180 new
  # (post-rollout) periods are set aside, the pooled winner on what's left is one of the
  # pre-rollout-tuned models, not the newly-crowned champion -- which is exactly the honest,
  # actionable signal operations needs: the step-change itself, not noise, is what moved the
  # champion. A diagnostic that returned champion_without_new_periods == "break_aware" here would
  # be wrongly implying the champion change had nothing to do with the rollout.
  expect_false(is.null(cr$champion_without_new_periods))
  expect_false(identical(cr$champion_without_new_periods, cr$current_champion))
  expect_true(cr$champion_without_new_periods %in% c("ml_gbm", "sarima"))

  # The old champion should have been displaced hard enough to leave the Model Confidence Set
  # (not just lose the pooled mean by a hair) -- sarima/ml_gbm's post-rollout loss is ~2x
  # break_aware's, a large, unambiguous effect, so act=TRUE is the correct "yes, operations should
  # act on this" signal for a CI/promotion-gating step.
  expect_false(isTRUE(cr$old_champion_still_in_mcs) )
  expect_true(cr$act)

  # churn_base_rate (the permutation-based noise benchmark) should be at or near zero: this
  # champion change is NOT the kind of thing that happens by chance resampling alone -- it is a
  # real, structural effect, and compare() should say so quantitatively, not just qualitatively.
  expect_lt(cr$churn_base_rate, 0.05)
})

test_that("compare() across two fully-stable (no-rollout-in-window) checkpoints shows no spurious champion change, as a contrast case", {
  # Two consecutive, non-overlapping 90-day windows entirely AFTER the rollout and after
  # break_aware's transition has settled (both windows start well past transition_end) should
  # show a STABLE champion (break_aware both times) -- the structural break is a one-time event,
  # not a source of ongoing, period-over-period champion churn once the network has settled onto
  # its new baseline.
  built <- .telecom_build_panel()
  L <- lapply(built$panel$losses, as.numeric)
  labels <- built$labels

  idx_a <- 450:539
  idx_b <- 540:629
  panel_a <- loss_panel(lapply(L, function(v) v[idx_a]), labels = labels[idx_a])
  panel_b <- loss_panel(lapply(L, function(v) v[idx_b]), labels = labels[idx_b])

  cr <- compare(panel_a, panel_b)
  expect_identical(cr$previous_champion, "break_aware")
  expect_identical(cr$current_champion, "break_aware")
  expect_false(cr$champion_changed)
  expect_false(cr$act)
})
