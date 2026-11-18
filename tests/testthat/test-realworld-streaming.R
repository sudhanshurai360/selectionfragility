# test-realworld-streaming.R — content-release-driven streaming demand forecasting scenario.
#
# Real-world grounding for this scenario (not an abstract stress test):
#   - A streaming platform's capacity-planning team compares 4 competing models for forecasting
#     daily aggregate viewership-driven bandwidth demand over ~18 months of daily data (T=550)
#     across the whole content catalog, to decide which model(s) to trust for next quarter's
#     infrastructure capacity commitments.
#   - PLANNED/KNOWN-IN-ADVANCE DEMAND SPIKES: major show/season premieres are release-calendar
#     events the platform schedules itself -- the spike TIMING is known well ahead of the event,
#     unlike every other "shared shock" tested elsewhere in this suite (weather catastrophes,
#     viral/DDoS growth), which are genuinely unpredictable. This makes release spikes a FAIR
#     test of forecast quality: a good model SHOULD anticipate them, not merely survive them.
#   - BINGE-WATCHING AUTOCORRELATION: viewership on a newly released title is heavily front-loaded
#     on day 0 and decays over the following 1-2 weeks in a roughly exponential pattern (a
#     well-documented real pattern for season-drop content) -- a distinct autocorrelation shape
#     from business-cycle persistence or weather-driven persistence tested elsewhere.
#   - MULTIPLE CONCURRENT TITLES / PORTFOLIO VIEW: the forecast target is catalog-wide aggregate
#     demand, built from 8 known release events of varying size spread across the 550 days,
#     mirroring a real portfolio-level capacity-planning view rather than a single title.
#
# Four competing models, deliberately constructed so NONE of them is uniformly best, to test
# whether the package's diagnostics can surface a genuinely MIXED verdict:
#   - hist_avg: trailing 28-day historical average, blind to the release calendar -- uniformly
#     weak everywhere (the "don't do this" baseline).
#   - calendar: a release-calendar-aware regression that knows the exact (known-in-advance)
#     release dates and nails the spike shape almost exactly, but its baseline/seasonal
#     component is a coarser deterministic fit with more day-to-day noise than the ML model --
#     i.e. GOOD AT SPIKES, MEDIOCRE ON ORDINARY DAYS.
#   - ml: a machine-learning model that fits ordinary day-to-day baseline demand very tightly
#     (spikes are rare events in its training distribution) but systematically undershoots the
#     release-driven spikes -- the mirror image of `calendar`: GOOD ON BASELINE, MEDIOCRE ON
#     SPIKES.
#   - ensemble: a 50/50 blend of calendar and ml -- reasonable everywhere, best nowhere.
#
# Squared-error loss is used throughout (pred - actual)^2, periods = calendar days.
#
# NOTE ON LABEL CONVENTION (a genuine finding from building this test, not a package bug): when
# `loss_panel()` is NOT given explicit `labels=`, `fragility()` reports `responsible` using
# 0-based period indices (matching the Python port's convention) while `decision_breakdown()`'s
# `$removed` are raw 1-based R vector positions. If you then pass `shock_periods=` using your own
# 1-based day numbers (as this scenario naturally does -- "day 200" meaning the 200th day), the
# `shock_coincidence` check in `fragility()` can come back FALSE even when the pivotal periods
# genuinely are the release-spike days, purely because of the index-origin mismatch. The fix is
# not a package change: pass `labels = 1:T` explicitly to `loss_panel()` so the labels you see in
# `responsible`/`shock_coincidence` use the SAME 1-based convention as the `shock_periods` you
# pass in. Both label-convention cases are exercised below.

.streaming_build_panel <- function() {
  set.seed(42)
  T <- 550  # ~18 months of daily data

  # 8 known, platform-scheduled release events of varying size, spread across the window.
  release_days    <- c(40, 120, 200, 280, 310, 400, 470, 520)
  spike_amplitude <- c(220, 180, 300, 150, 90, 260, 140, 320)
  decay_rate      <- 0.35  # binge-watch decay: fast, front-loaded, roughly exponential

  t <- seq_len(T)
  baseline <- 100 + 15 * sin(2 * pi * t / 7) + 5 * sin(2 * pi * t / 365)  # weekly + mild annual
  noise <- rnorm(T, 0, 6)

  spike_contrib <- numeric(T)
  for (i in seq_along(release_days)) {
    d <- release_days[i]; amp <- spike_amplitude[i]
    idx <- t >= d
    days_since <- t[idx] - d
    spike_contrib[idx] <- spike_contrib[idx] + amp * exp(-decay_rate * days_since)
  }

  actual_demand <- baseline + spike_contrib + noise

  # Model 1: historical-average baseline, blind to the release calendar.
  hist_avg <- numeric(T)
  for (i in seq_len(T)) {
    lo <- max(1, i - 28)
    hist_avg[i] <- if (i == 1) actual_demand[1] else mean(actual_demand[lo:(i - 1)])
  }
  hist_avg[1] <- baseline[1]

  # Model 2: release-calendar-aware regression -- knows the schedule, nails spike shape, coarser
  # baseline fit (more day-to-day noise).
  calendar_spike_est <- sapply(t, function(tt) {
    contrib <- 0
    for (j in seq_along(release_days)) {
      d <- release_days[j]
      if (tt >= d) contrib <- contrib + spike_amplitude[j] * 0.97 * exp(-decay_rate * 1.02 * (tt - d))
    }
    contrib
  })
  calendar_model <- baseline + calendar_spike_est + rnorm(T, 0, 11)

  # Model 3: ML model -- tight baseline fit, undershoots spikes (trained mostly on non-spike days).
  ml_model <- baseline + spike_contrib * 0.40 + rnorm(T, 0, 1.5)

  # Model 4: 50/50 ensemble of the two above.
  ensemble <- 0.5 * calendar_model + 0.5 * ml_model + rnorm(T, 0, 2)

  sq_err <- function(pred) (pred - actual_demand)^2
  L <- list(
    hist_avg = sq_err(hist_avg),
    calendar = sq_err(calendar_model),
    ml       = sq_err(ml_model),
    ensemble = sq_err(ensemble)
  )

  spike_window <- unique(unlist(lapply(release_days, function(d) d:(d + 10))))
  spike_window <- spike_window[spike_window <= T]

  list(L = L, T = T, release_days = release_days, spike_window = spike_window)
}

test_that("loss_panel() builds cleanly from the 4-model, T=550 streaming-demand panel", {
  d <- .streaming_build_panel()
  panel <- loss_panel(d$L)
  expect_s3_class(panel, "loss_panel")
  expect_setequal(names(panel$losses), c("hist_avg", "calendar", "ml", "ensemble"))
  expect_equal(length(panel$labels), d$T)
  s <- summary(panel)
  expect_equal(nrow(s), 4)
})

test_that("report() surfaces a genuinely MIXED verdict: pooled champion != plurality (most-days) winner", {
  # This is the core domain question: can the package distinguish "handles release spikes well,
  # mediocre on ordinary days" (calendar) from "uniformly good/bad"? By construction, `calendar`
  # wins the pooled mean (its near-perfect spike handling dominates the few catastrophic spike
  # days), while `ml` wins the most individual days outright (it is better on the ~460 ordinary
  # days that make up most of the sample). A package that only ever reports a single flat
  # champion would hide this; LEADERBOARD's periods-won column should NOT agree with its own
  # champion here.
  d <- .streaming_build_panel()
  panel <- loss_panel(d$L)

  rep_txt <- report(panel)
  expect_type(rep_txt, "character")
  expect_match(rep_txt, "LEADERBOARD", fixed = TRUE)
  expect_match(rep_txt, "PIVOT", fixed = TRUE)

  s <- summary(panel)
  pooled_champion   <- s$model[which.min(s$mean_loss)]
  periods_won_leader <- s$model[which.max(s$periods_won)]

  expect_identical(pooled_champion, "calendar")
  expect_identical(periods_won_leader, "ml")
  # The headline distinction this scenario is built to test:
  expect_false(identical(pooled_champion, periods_won_leader))

  # calendar must win decisively on release-spike days, while ml wins decisively on ordinary
  # days -- confirming the construction actually produced the intended mixed pattern (not just
  # an artifact of the pooled-mean/periods-won split above).
  spike_loss <- vapply(d$L, function(x) mean(x[d$spike_window]), numeric(1))
  base_days  <- setdiff(seq_len(d$T), d$spike_window)
  base_loss  <- vapply(d$L, function(x) mean(x[base_days]), numeric(1))
  expect_identical(names(which.min(spike_loss)), "calendar")
  expect_identical(names(which.min(base_loss)), "ml")
})

test_that("fragility()$reversal flags the pooled-mean champion differing from the per-period plurality winner", {
  # The lumpy, spike-dominated shape of content-release demand is exactly the setting where a
  # pooled-mean comparison and a per-period "who wins more often" comparison can legitimately
  # disagree -- this is what fragility()$reversal exists to catch.
  d <- .streaming_build_panel()
  panel <- loss_panel(d$L)
  fr <- fragility(panel, shock_periods = d$spike_window)

  expect_identical(fr$pooled_winner, "calendar")
  expect_identical(fr$per_period_winner, "ml")
  expect_true(fr$reversal)
  # The Condorcet (pairwise-majority) winner sides with the per-period winner here too, meaning
  # calendar's pooled-mean win is not even a majority-preferred outcome -- a second, independent
  # signal (not just plurality) that the pooled verdict is a close/contested call, not a
  # landslide, in this release-spike-dominated demand pattern.
  expect_identical(fr$condorcet_winner, "ml")
  expect_true(fr$reversal_condorcet)

  # The decision is fragile: a small number of pivotal (mostly spike) days determine the pooled
  # champion. k* should be small relative to T=550 -- a handful of days, not hundreds.
  expect_true(fr$k_star >= 1)
  expect_lt(fr$k_star, 20)
  expect_lt(fr$k_over_T, 0.05)
})

test_that("shock_coincidence requires label/shock_periods index conventions to match (documented gotcha)", {
  # Without explicit labels, fragility()'s `responsible` uses 0-based period indices while the
  # shock_periods supplied here are naturally 1-based day numbers -- so shock_coincidence can
  # read FALSE even though the pivotal days genuinely are release-spike days. Passing
  # `labels = 1:T` to loss_panel() aligns the conventions and shock_coincidence correctly reads
  # TRUE. Both cases are asserted so this mismatch is documented in code, not just prose.
  d <- .streaming_build_panel()

  panel_unlabeled <- loss_panel(d$L)  # default 0-based period labels
  fr_unlabeled <- fragility(panel_unlabeled, shock_periods = d$spike_window)
  expect_false(fr_unlabeled$shock_coincidence)

  panel_labeled <- loss_panel(d$L, labels = seq_len(d$T))  # 1-based, matching shock_periods
  fr_labeled <- fragility(panel_labeled, shock_periods = d$spike_window)
  expect_true(fr_labeled$shock_coincidence)

  # Both labelings must agree on every substantive (non-label) diagnostic -- the index
  # convention only affects which period numbers are reported, not the underlying verdict.
  expect_identical(fr_unlabeled$pooled_winner, fr_labeled$pooled_winner)
  expect_identical(fr_unlabeled$per_period_winner, fr_labeled$per_period_winner)
  expect_identical(fr_unlabeled$k_star, fr_labeled$k_star)
  expect_identical(fr_unlabeled$reversal, fr_labeled$reversal)

  # The pivotal days, once shifted onto the same 1-based convention as shock_periods, must
  # actually land in (or immediately adjacent to) the known release windows -- confirming the
  # decision really is driven by the planned release spikes, not some unrelated artifact.
  shifted <- fr_unlabeled$responsible + 1L
  expect_true(all(vapply(shifted, function(r) any(abs(r - d$release_days) <= 10), logical(1))))
})

test_that("decision_breakdown()'s pivotal days are concentrated at/near the known release dates", {
  d <- .streaming_build_panel()
  panel <- loss_panel(d$L, labels = seq_len(d$T))
  L <- panel$losses; w <- panel$weights
  champ <- pooled_winner(L, w)
  expect_identical(champ, "calendar")

  db <- decision_breakdown(L, w, a = champ)
  expect_true(db$k >= 1)
  expect_lt(db$k, 20)  # a handful of days out of 550, not a broad swath
  # Every removed (1-based) day should be within or immediately adjacent to a release window --
  # this is the content-release-driven mechanism, not noise.
  expect_true(all(vapply(db$removed, function(r) any(abs(r - d$release_days) <= 10), logical(1))))
})

test_that("report() does not overclaim a flat, unconditional champion given the MCS is not identified", {
  # Because calendar's pooled edge is powered by a handful of spike days and the LEADERBOARD
  # is genuinely contested (ensemble survives alongside calendar in the MCS at the default
  # alpha), report() must visibly flag that identification/resolution has NOT cleanly settled
  # on one champion, rather than asserting an unqualified verdict a capacity-planning team could
  # over-trust for infrastructure commitments.
  d <- .streaming_build_panel()
  panel <- loss_panel(d$L)
  rep_txt <- report(panel)

  s <- summary(panel)
  expect_gt(sum(s$in_mcs), 1)  # more than one survivor -- genuinely contested
  expect_match(rep_txt, "not identified", fixed = TRUE)
})
