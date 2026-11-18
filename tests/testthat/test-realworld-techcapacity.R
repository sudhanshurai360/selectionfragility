# test-realworld-techcapacity.R — tech / cloud-infrastructure capacity-forecasting adoption
# scenario: a platform-engineering team forecasting daily API request volume over a ~16-week
# window, re-evaluating its model roster at two checkpoints.
#
# Real-world grounding for this scenario (not an abstract stress test):
#   - RAPID MODEL TURNOVER: production forecasting teams at traffic-heavy platforms routinely
#     swap models every few weeks as new approaches clear offline backtests (a classical
#     statistical model co-existing with, then being displaced by, an ML model is the normal
#     cadence -- this is the exact "model churn" scenario compare()'s own documentation calls
#     out as a precondition it does NOT relax: previous/current must share the same model set).
#   - VIRAL/EXPONENTIAL GROWTH EVENTS: a product launch or a viral social moment can push daily
#     request volume to 10-100x baseline for a few days -- a genuine, legitimate demand spike
#     (as opposed to a DDoS or a weather catastrophe), and a classic stress case for whichever
#     forecasting model lacks explicit changepoint/regime-shift handling.
#   - MULTIPLE CONCURRENT MODEL FAMILIES: a changepoint-aware model (Prophet-style), a classical
#     ARIMA-style baseline, a naive exponential-growth extrapolation, and a neural model (an
#     N-BEATS-style learner) commonly run side by side in a live model-selection pipeline.
#
# Two checkpoints are built below with a GENUINE model-set change (not just a new period count):
#   - checkpoint_before: {arima, prophet, exp_growth}, the roster BEFORE the viral event.
#   - checkpoint_after:  {arima, prophet, nbeats}, the roster AFTER: exp_growth was retired
#     (consistently the weakest performer, not worth the continued on-call/infra cost of keeping
#     it live) and nbeats was promoted into the live roster after promising offline backtests.
#     This checkpoint also contains the viral growth event itself (days 10-13 of a 20-day window),
#     during which arima and nbeats (trained at pre-event scale) badly mis-forecast, while
#     prophet's changepoint detection absorbs the shock far better.
# A third checkpoint (checkpoint_stable) reuses checkpoint_after's STABILIZED roster with no
# further churn, to confirm compare() across two checkpoints is not broken in general -- only the
# genuinely mismatched-model-set case is rejected.

.techcap_build_before <- function() {
  set.seed(101)
  n <- 20
  arima   <- pmax(1.00 + rnorm(n, 0, 0.07), 0.01)
  prophet <- pmax(0.95 + rnorm(n, 0, 0.09), 0.01)
  exp_growth <- pmax(1.15 + rnorm(n, 0, 0.10), 0.01)  # weakest; later retired from the live roster
  labels <- as.character(as.Date("2026-01-05") + 7 * (0:(n - 1)))  # weekly checkpoints
  loss_panel(
    list(arima = arima, prophet = prophet, exp_growth = exp_growth),
    labels = labels
  )
}

.techcap_build_after <- function() {
  set.seed(202)
  n <- 20
  arima   <- pmax(1.00 + rnorm(n, 0, 0.07), 0.01)
  prophet <- pmax(0.95 + rnorm(n, 0, 0.09), 0.01)
  nbeats  <- pmax(0.85 + rnorm(n, 0, 0.08), 0.01)   # newly promoted model, genuinely competitive
  # Viral growth event: a product-launch/viral-moment traffic surge hits for 4 consecutive
  # checkpoints (days 10-13), dwarfing normal variation (10-100x normal forecast error vs.
  # baseline ~1.0 losses above). arima and nbeats were trained/tuned at pre-event scale and blow
  # up; prophet's changepoint detection handles the regime shift far better.
  viral_idx <- 10:13
  arima[viral_idx]  <- c(4.2, 6.8, 5.1, 3.0)
  nbeats[viral_idx] <- c(3.8, 5.9, 4.4, 2.6)
  prophet[viral_idx] <- c(1.2, 1.6, 1.3, 1.1)
  labels <- as.character(as.Date("2026-01-05") + 7 * (20:(20 + n - 1)))
  loss_panel(
    list(arima = arima, prophet = prophet, nbeats = nbeats),
    labels = labels
  )
}

.techcap_build_stable <- function() {
  # Same, post-churn roster as checkpoint_after ({arima, prophet, nbeats}), one more evaluation
  # window later, with no further roster change and no further viral event -- the "quiet period"
  # a platform team expects most re-evaluations to look like.
  set.seed(303)
  n <- 20
  arima   <- pmax(1.00 + rnorm(n, 0, 0.07), 0.01)
  prophet <- pmax(0.95 + rnorm(n, 0, 0.09), 0.01)
  nbeats  <- pmax(0.85 + rnorm(n, 0, 0.08), 0.01)
  labels <- as.character(as.Date("2026-01-05") + 7 * (40:(40 + n - 1)))
  loss_panel(
    list(arima = arima, prophet = prophet, nbeats = nbeats),
    labels = labels
  )
}

test_that("compare() refuses a genuinely changed model roster across checkpoints, clearly and actionably", {
  # This is the realistic "we added/retired a model mid-comparison-window" business event --
  # distinct from a same-model-set rolling window. compare()'s own documented contract requires
  # previous/current to share the same model set (R/compare.R's setequal() guard); the roster
  # churn here (exp_growth retired, nbeats added between checkpoints) must be refused, not
  # silently mis-compared.
  before <- .techcap_build_before()
  after  <- .techcap_build_after()

  expect_setequal(names(before$losses), c("arima", "prophet", "exp_growth"))
  expect_setequal(names(after$losses),  c("arima", "prophet", "nbeats"))

  err <- tryCatch(compare(before, after), error = function(e) conditionMessage(e))
  expect_type(err, "character")
  # The error must be clearly worded (not a raw base-R crash) and name BOTH mismatched model
  # sets, so a platform engineer reading CI logs immediately sees which models disagree between
  # the two runs, rather than a generic "arguments differ" message.
  expect_match(err, "different model sets", fixed = TRUE)
  expect_match(err, "arima", fixed = TRUE)
  expect_match(err, "exp_growth", fixed = TRUE)
  expect_match(err, "nbeats", fixed = TRUE)
  expect_match(err, "prophet", fixed = TRUE)
})

test_that("compare() works normally once the roster has stabilized (contrast case for the churn rejection above)", {
  # Confirms the rejection above is specifically about the model SET differing, not some general
  # breakage from this domain's data shape (weekly checkpoints, Date-derived labels, a prior
  # viral event baked into the loss history). Two checkpoints sharing the SAME post-churn roster
  # must compare cleanly.
  after  <- .techcap_build_after()
  stable <- .techcap_build_stable()
  expect_setequal(names(after$losses), names(stable$losses))

  cr <- compare(after, stable)
  expect_s3_class(cr, "change_report")
  expect_true(cr$current_champion %in% c("arima", "prophet", "nbeats"))
  expect_true(is.numeric(cr$k_star))
  expect_true(cr$churn_base_rate >= 0 && cr$churn_base_rate <= 1)
})

test_that("a viral growth event concentrates the decision in a handful of pivotal periods, and report()/decision_breakdown() say so honestly", {
  # Within a single checkpoint (checkpoint_after), the viral event (days 10-13) is the mechanism
  # driving the comparison, just as a weather-catastrophe period would in a different domain.
  # This checks the same concentration diagnostics generalize to a DIFFERENT extreme-event
  # mechanism (viral/exponential growth, not catastrophe): report() and decision_breakdown()
  # should identify a small k* concentrated near the viral window, not a flat/uninformative read.
  after <- .techcap_build_after()
  L <- after$losses
  w <- after$weights
  labels <- after$labels
  n <- length(labels)

  champ <- pooled_winner(L, w)
  expect_identical(champ, "prophet")  # prophet's changepoint handling wins the pooled mean

  db <- decision_breakdown(L, w, a = champ)
  expect_true(is.list(db))
  expect_true(db$k >= 1)
  # k* should be small relative to T=20 -- a decision this concentrated is fragile, not robust,
  # exactly the signature a genuine extreme-event-driven comparison should produce.
  expect_lt(db$k, n / 2)
  # The pivotal periods removed by decision_breakdown() should include at least one day drawn
  # from (or immediately adjacent to) the viral event window (positions 10-13), since that is
  # where the champion's advantage is concentrated.
  viral_idx <- 10:13
  expect_true(any(db$removed %in% viral_idx))

  rep_txt <- report(after)
  expect_type(rep_txt, "character")
  expect_match(rep_txt, "PIVOT", fixed = TRUE)
  expect_match(rep_txt, "LEADERBOARD", fixed = TRUE)
  # report() must not claim an unconditional, unqualified "robust" verdict when the champion's
  # edge is this concentrated -- it should either certify significance explicitly or visibly
  # flag that identification/significance has NOT been established at this k*/sample size.
  expect_true(
    grepl("certif", rep_txt, ignore.case = TRUE) ||
      grepl("not identified", rep_txt, ignore.case = TRUE) ||
      grepl("does not certify", rep_txt, ignore.case = TRUE)
  )
})
