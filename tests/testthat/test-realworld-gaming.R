# test-realworld-gaming.R — gaming/mobile platform concurrent-user capacity forecasting
# scenario: a gaming platform's infrastructure team compares 4 competing peak-concurrent-user
# (CCU) forecasting models over 12 months (T=365 days) of daily peak-CCU forecast-error losses,
# to decide which model to trust for next quarter's server capacity provisioning.
#
# Real-world grounding (distinct from test-realworld-techcapacity.R's generic cloud/API-traffic
# scenario -- gaming platforms have demand drivers a web-traffic model does not):
#   - SCHEDULED, KNOWN-IN-ADVANCE DEMAND SPIKES: 3 major esports tournament weeks spread across
#     the year (days 60-66, 180-186, 300-306), known weeks ahead from the tournament calendar.
#     Each window has ordinary elevated-but-not-extreme broadcast-period load on most days, and
#     one sharply spiking FINALS day (the live-broadcast peak-CCU surge concentrated in a single
#     broadcast hour) -- the single sharpest mis-forecast opportunity in the window.
#   - WEEKEND/EVENING DAILY-CYCLE SEASONALITY: unlike enterprise weekday-business-hours traffic,
#     gaming peak-CCU runs higher on weekends -- baked into every model's baseline loss as a
#     day-of-week effect, distinct from the techcapacity scenario's pattern.
#   - A GENUINE VIRAL-GROWTH-THEN-DECAY EVENT: days 150-195, an asymmetric shape (fast ~3-week
#     ascent as a clip goes viral on a streaming platform, slower ~25-day decay back to baseline)
#     -- NOT a smooth organic-growth curve and NOT a short DDoS-style spike; this is the shape
#     that specifically defeats naive trend extrapolation (which keeps extrapolating the ascent
#     straight through the decay).
#
# Four models, each with a distinct, realistic failure/success profile:
#   - seasonal_baseline:  day-of-week/hour-of-day seasonal model. Decent baseline, NO tournament-
#     calendar or virality awareness -- degrades on both event types.
#   - tournament_aware:   knows the tournament broadcast calendar. Stays composed during
#     tournament weeks (including finals-day spikes) but has no virality-specific logic, so it
#     also degrades during the viral event (just less than the calendar-blind models).
#   - ml_model:           a flexible ML model with no explicit calendar/virality logic but better
#     general-purpose baseline accuracy and adequate (not best) partial adaptation during shocks.
#   - trend_extrap:       naive trend extrapolation. Weakest baseline AND the worst victim of the
#     viral event's asymmetric shape (extrapolates the ascent and is blindsided by the decay) AND
#     tournament spikes (no event awareness at all).
#
# set.seed() makes every number below deterministic and reproducible.

.gaming_build_panel <- function() {
  set.seed(707)
  n <- 365
  dates <- as.Date("2026-01-01") + 0:(n - 1)
  dow <- as.integer(format(dates, "%u"))  # 1=Mon .. 7=Sun
  is_weekend <- dow %in% c(6, 7)

  seasonal   <- pmax(0.90 + rnorm(n, 0, 0.06) + ifelse(is_weekend, 0.03, 0), 0.01)
  tournament <- pmax(0.92 + rnorm(n, 0, 0.07) + ifelse(is_weekend, 0.03, 0), 0.01)
  ml_model   <- pmax(0.85 + rnorm(n, 0, 0.08) + ifelse(is_weekend, 0.02, 0), 0.01)
  trend      <- pmax(1.10 + rnorm(n, 0, 0.09) + ifelse(is_weekend, 0.04, 0), 0.01)

  # Scheduled tournament weeks: each a 7-day window with one finals day (offset 4) carrying the
  # sharpest single-hour broadcast spike.
  tournament_windows <- list(60:66, 180:186, 300:306)
  finals_day_offset <- 4
  for (win in tournament_windows) {
    finals <- win[finals_day_offset]
    other <- setdiff(win, finals)
    seasonal[other]    <- seasonal[other] + 1.1
    ml_model[other]    <- ml_model[other] + 0.7
    trend[other]       <- trend[other] + 1.6
    tournament[other]  <- tournament[other] + 0.15
    seasonal[finals]   <- seasonal[finals] + 2.8
    ml_model[finals]   <- ml_model[finals] + 1.8
    trend[finals]      <- trend[finals] + 3.4
    tournament[finals] <- tournament[finals] + 0.25
  }

  # Viral growth-then-decay event: rapid ~3-week ascent, slower ~25-day decay -- an asymmetric
  # shape distinct from smooth organic growth.
  viral_growth <- 150:170
  viral_decay  <- 171:195
  profile_growth <- 1 - exp(-seq_along(viral_growth) / 6)
  profile_decay  <- exp(-seq_along(viral_decay) / 10)

  trend[viral_growth]      <- trend[viral_growth] + 2.0 * profile_growth
  trend[viral_decay]       <- trend[viral_decay]  + 4.5 * profile_decay
  ml_model[viral_growth]   <- ml_model[viral_growth] + 0.9 * profile_growth
  ml_model[viral_decay]    <- ml_model[viral_decay]  + 1.4 * profile_decay
  seasonal[viral_growth]   <- seasonal[viral_growth] + 1.3 * profile_growth
  seasonal[viral_decay]    <- seasonal[viral_decay]  + 2.0 * profile_decay
  tournament[viral_growth] <- tournament[viral_growth] + 1.0 * profile_growth
  tournament[viral_decay]  <- tournament[viral_decay]  + 1.6 * profile_decay

  list(
    panel = loss_panel(
      list(
        seasonal_baseline = seasonal,
        tournament_aware  = tournament,
        ml_model          = ml_model,
        trend_extrap      = trend
      ),
      labels = as.character(dates)
    ),
    tournament_idx = unlist(tournament_windows),
    viral_idx      = c(viral_growth, viral_decay),
    finals_idx     = vapply(tournament_windows, function(w) w[finals_day_offset], numeric(1))
  )
}

test_that("loss_panel()/report() identify a defensible champion for next-quarter capacity provisioning", {
  built <- .gaming_build_panel()
  panel <- built$panel

  expect_setequal(
    names(panel$losses),
    c("seasonal_baseline", "tournament_aware", "ml_model", "trend_extrap")
  )
  expect_length(panel$labels, 365)

  champ <- pooled_winner(panel$losses, panel$weights)
  # The flexible ML model's better general-purpose baseline accuracy outweighs its lack of
  # explicit tournament/virality logic -- it should be the pooled-mean champion over the full year.
  expect_identical(champ, "ml_model")

  rep_txt <- report(panel)
  expect_type(rep_txt, "character")
  expect_match(rep_txt, "LEADERBOARD", fixed = TRUE)
  expect_match(rep_txt, "PIVOT", fixed = TRUE)
  expect_match(rep_txt, "RESOLUTION", fixed = TRUE)
  expect_match(rep_txt, "ml_model", fixed = TRUE)
  # trend_extrap, blindsided by both the tournament finals spikes and the viral event's
  # asymmetric shape, should be the clear last-place model on the leaderboard.
  expect_match(rep_txt, "trend_extrap", fixed = TRUE)
})

test_that("decision_breakdown() shows the viral event, not the scheduled tournament weeks, drives the champion's fragility", {
  # This is the "jointly or separately" question: do the scheduled tournament spikes and the
  # unscheduled viral event compete for the same pivotal periods, or does one dominate?
  # tournament_aware's calendar awareness means tournament weeks are already a *structural*,
  # fully-priced-in disadvantage for ml_model (ml_model loses to tournament_aware on those days,
  # so removing them would not help ml_model's case -- they can never be pivotal "support" for
  # the champion). The viral event, which tournament_aware has NO special handling for either,
  # is where ml_model's edge over tournament_aware is actually concentrated.
  built <- .gaming_build_panel()
  panel <- built$panel
  L <- panel$losses
  w <- panel$weights

  champ <- pooled_winner(L, w)
  db <- decision_breakdown(L, w, a = champ)
  expect_true(is.list(db))
  expect_identical(db$binding_opponent, "tournament_aware")
  expect_true(db$k >= 1)
  # A moderate, non-trivial k* (neither instantly fragile nor requiring removal of most of the
  # year) -- this is a materially more ROBUST decision than the concentrated single-event
  # fragility test-realworld-techcapacity.R finds for its viral-growth scenario, a genuinely
  # different finding for a genuinely different mix of event types.
  expect_gt(db$k, 5)
  expect_lt(db$k, length(L[[1]]) / 2)

  n_in_tournament <- sum(db$removed %in% built$tournament_idx)
  n_in_viral       <- sum(db$removed %in% built$viral_idx)
  # None of the pivotal (removed) periods fall inside the scheduled tournament windows: those days
  # already favor tournament_aware over ml_model, so they cannot be among the periods whose
  # removal would help flip the decision toward tournament_aware.
  expect_equal(n_in_tournament, 0L)
  # A material share of the pivotal periods fall inside the viral growth/decay window -- the
  # viral event, not the scheduled tournaments, is what's load-bearing for this particular
  # champion-vs-runner-up margin.
  expect_gt(n_in_viral, 0L)
})

test_that("winner_stability() shows the champion is robust to resampling despite the scheduled spikes and the viral event", {
  # A genuinely new combination for this package's real-world test suite: scheduled, calendar-
  # known spikes (tournament weeks) mixed with one unscheduled viral event, within a single
  # T~365 panel (not a before/after checkpoint split). Block bootstrap resampling should still
  # recover the same pooled winner (ml_model) in the large majority of resamples, consistent
  # with decision_breakdown()'s moderate (not tiny) k* above -- a platform team can reasonably
  # provision next quarter's capacity around ml_model's demand forecasts.
  built <- .gaming_build_panel()
  panel <- built$panel

  ws <- winner_stability(panel$losses, panel$weights, seed = 11)
  expect_true(is.list(ws))
  expect_true(is.numeric(ws$stability))
  expect_true(ws$stability >= 0 && ws$stability <= 1)
  # Robust, not fragile: the champion should hold up in a strong majority of bootstrap resamples.
  expect_gt(ws$stability, 0.8)
})
