# test-realworld-mining.R
#
# REAL-WORLD USE CASE: a mining company's operations-planning team compares 4 competing
# models for forecasting MONTHLY ore output at a single mine over T=60 months (5 years):
#   - geo_baseline : a geological-model-based baseline (knows the ore body's structural
#                    level well, but has no visibility into short-notice outages)
#   - maint_aware  : an equipment-reliability/maintenance-schedule-aware model (also
#                    structurally sound, and specifically engineered to anticipate SOME
#                    near-term outage risk -- not clairvoyant, just less blindsided)
#   - ml_sensor    : an ML model on live sensor data (tightest residuals in normal
#                    operation, but has never "seen" a shutdown or a price-cycle regime
#                    change, so both discrete-event types blindside it harder)
#   - hist_trend   : a simple historical-trend extrapolation (noisiest, and structurally
#                    blind to regime shifts -- it keeps extrapolating the pre-shift level)
#
# This panel is built from two genuinely distinct real mining phenomena, deliberately kept
# separate so their effects on k* are not confounded:
#
#  1. An UNPLANNED SHUTDOWN (equipment failure / safety incident / geological surprise) --
#     a genuine SUPPLY-side physical disruption. Actual output craters in that month for a
#     reason no forecast made in advance can see coming; every model's forecast (made before
#     the event) misses it, by different amounts depending on how much outage-risk awareness
#     that model has. This is mechanically different from every other domain test file in
#     this package, which uses DEMAND-side shocks (a demand spike/collapse, a market panic, a
#     booking surge) -- here the loss spike originates in the physical/geological production
#     process itself, not in market behavior around a forecast.
#
#  2. A COMMODITY-PRICE-CYCLE-DRIVEN STRUCTURAL SHIFT (modeled on the well-documented
#     2014-2016 commodity price crash): from one month on, marginal/high-cost extraction
#     zones are taken offline, so the TRUE output level steps down permanently (~12%) --
#     a genuine regime change in the level being forecast, not a one-period shock. The
#     historical-trend model is hit hardest here because it keeps extrapolating the old,
#     pre-crash level for several months after the step.
#
# The central domain question this file asks: is the model-selection decision (which model
# the team should trust for next year's budget) driven by the rare, physically-caused
# shutdown month(s), or is it robust to them? decision_breakdown()'s k* and `removed` periods
# give a direct, falsifiable answer -- and the counterfactual-without-shutdown comparison
# below isolates the shutdown's specific causal contribution to the decision's fragility.

.make_mine_panel <- function(T = 60, seed = 42, shutdown_months = c(22, 23),
                              price_crash_month = 37, with_shutdown = TRUE) {
  set.seed(seed)
  true_level <- rep(1.00, T)
  # price-cycle-driven structural shift: marginal sites go offline from price_crash_month on
  true_level[price_crash_month:T] <- 0.88

  geo   <- true_level + stats::rnorm(T, 0, 0.04)
  maint <- true_level + stats::rnorm(T, 0, 0.035)
  ml    <- true_level + stats::rnorm(T, 0, 0.025)
  hist  <- true_level + stats::rnorm(T, 0, 0.06)

  # historical-trend model keeps extrapolating the pre-crash level for ~6 months post-crash
  drift <- numeric(T)
  if (price_crash_month <= T) {
    tail_len <- min(6, T - price_crash_month + 1)
    drift[price_crash_month:(price_crash_month + tail_len - 1)] <- seq(0.12, 0.02, length.out = tail_len)
  }
  hist <- hist + drift

  actual <- true_level + stats::rnorm(T, 0, 0.01)

  L <- list(geo_baseline = geo, maint_aware = maint, ml_sensor = ml, hist_trend = hist)

  # unplanned shutdown: a genuine SUPPLY-side shock -- actual output craters; forecasts made
  # in advance (all 4 models) miss it by construction, by different amounts per model.
  if (with_shutdown) {
    for (mo in shutdown_months) actual[mo] <- actual[mo] - 0.45
  }

  loss <- stats::setNames(vector("list", length(L)), names(L))
  miss_scale <- c(geo_baseline = 1.0, maint_aware = 0.55, ml_sensor = 1.3, hist_trend = 1.25)
  for (m in names(L)) {
    err <- abs(L[[m]] - actual)
    if (with_shutdown) {
      for (mo in shutdown_months) {
        set.seed(seed * 1000 + mo + match(m, names(L)))
        err[mo] <- err[mo] + abs(stats::rnorm(1, 0.45, 0.05)) * miss_scale[[m]]
      }
    }
    loss[[m]] <- pmax(err, 1e-4)
  }
  list(L = loss, labels = seq_len(T), shutdown_months = shutdown_months,
       price_crash_month = price_crash_month)
}

test_that("report() on the 5-year monthly mining panel runs end to end and surfaces a clear champion", {
  # Baseline sanity: a realistic T=60/K=4 monthly production panel should never crash
  # report(), and the four standard sections must all be present.
  p <- .make_mine_panel()
  panel <- loss_panel(p$L, labels = p$labels)
  # as.data.frame.loss_panel() is long-format (one row per model x period): K=4 models x
  # T=60 months = 240 rows.
  expect_equal(nrow(as.data.frame(panel)), 240)

  out <- report(panel, seed = 0)
  txt <- unclass(out)
  for (section in c("^VERDICT:", "LEADERBOARD", "RESOLUTION", "PIVOT")) {
    expect_match(txt, section)
  }
  # The maintenance-aware model (structurally sound AND somewhat outage-aware) should be the
  # pooled-mean champion -- it is strictly better-informed than every rival by construction.
  expect_match(txt, "maint_aware.*\\(champion\\)")
  expect_identical(pooled_winner(panel), "maint_aware")
})

test_that("the unplanned-shutdown month is EXACTLY the single period decision_breakdown() flags as pivotal (k*=1)", {
  # This is the core SUPPLY-side-fragility check: with a genuine physical/geological shutdown
  # in the panel, a single month's deletion should flip the pooled winner -- and that month
  # should be the shutdown month itself, not some unrelated period. This is the mining-specific
  # analogue of "which single bad month would overturn next year's budget model choice."
  p <- .make_mine_panel(shutdown_months = c(22, 23))
  panel <- loss_panel(p$L, labels = p$labels)

  db <- decision_breakdown(panel, return_ties = TRUE)
  expect_identical(db$k, 1L)
  expect_true(all(db$removed %in% p$shutdown_months))
  expect_identical(db$binding_opponent, "ml_sensor")

  # Sanity: decision_breakdown() must agree with resolution_report()'s own champion/rival read
  # (both read off the same pooled winner) even though their "binding" rival rules differ.
  rr <- resolution_report(panel$losses, panel$weights)
  expect_identical(rr$champion, "maint_aware")
})

test_that("removing the unplanned shutdown (holding the price-cycle shift fixed) makes the decision materially LESS fragile", {
  # Causal isolation: this is the distinguishing SUPPLY-vs-DEMAND-side mechanism check this
  # package has not exercised before. Strip out ONLY the unplanned-shutdown mechanism (keep
  # the commodity-price-cycle structural shift, which is a genuine regime change, not a
  # discrete supply shock) and confirm k* rises sharply -- i.e. the shutdown month(s), not the
  # price cycle, are what make this decision fragile in the WITH-shutdown panel. A fragility
  # diagnostic that didn't distinguish these two would be useless to an operations-planning
  # team trying to decide whether a maintenance outage risk, specifically, threatens their
  # model choice.
  p_with <- .make_mine_panel(with_shutdown = TRUE)
  p_without <- .make_mine_panel(with_shutdown = FALSE)

  panel_with <- loss_panel(p_with$L, labels = p_with$labels)
  panel_without <- loss_panel(p_without$L, labels = p_without$labels)

  db_with <- decision_breakdown(panel_with)
  db_without <- decision_breakdown(panel_without)

  expect_identical(db_with$k, 1L)
  # Without the shutdown, flipping the decision requires deleting several periods, not one --
  # a meaningfully more robust decision.
  expect_gt(db_without$k, db_with$k)
  expect_gte(db_without$k, 5L)
  # The shutdown month(s) are NOT even part of the without-shutdown panel's pivotal set (they
  # carry no special weight once the physical disruption is removed from the data).
  expect_false(any(db_without$removed %in% p_with$shutdown_months))
})

test_that("resolution_report()'s power check correctly flags this T=60 monthly panel as UNDERPOWERED to resolve the top two models", {
  # Domain-realistic finding: 5 years of monthly production data (T=60) is a SHORT panel by
  # the standards of statistical model comparison, even though it feels like "a lot of data"
  # to an operations team. The observed edge between the champion (maint_aware) and its
  # closest rival (ml_sensor) is small relative to what T=60 can reliably detect -- this test
  # locks in that resolution_report() surfaces that honestly (not identified / not resolved),
  # rather than overclaiming a confident winner off 5 years of monthly numbers. This is exactly
  # the "needs a defensible answer for budget planning" scenario: the defensible answer here is
  # "not yet resolved," not a false confident pick.
  p <- .make_mine_panel()
  panel <- loss_panel(p$L, labels = p$labels)
  rr <- resolution_report(panel$losses, panel$weights)

  expect_identical(rr$champion, "maint_aware")
  expect_identical(rr$binding_rival, "ml_sensor")
  expect_false(rr$resolved)
  expect_identical(rr$fragility_read, "undetermined")
  # The minimum detectable edge at 80% power must be several times the observed edge -- i.e.
  # genuinely underpowered, not a borderline "just barely not resolved" call.
  expect_gt(rr$mde, 3 * rr$observed_edge)
  expect_true(is.finite(rr$mde) && rr$mde > 0)

  out <- report(panel, seed = 0)
  txt <- unclass(out)
  expect_match(txt, "cannot determine at this sample size")
})

test_that("the mining panel sweep is stable across nearby shutdown timings and price-crash timings (no crash, consistent champion logic)", {
  # Repeated-application robustness, mirroring the other domain files' sweep test: a real
  # rollout compares this across many mines/commodities with different shutdown and
  # price-crash timing, not just one hand-tuned month. Every one must produce a complete,
  # internally consistent report().
  shutdown_starts <- c(5, 15, 30, 45, 55)
  crash_months <- c(20, 25, 40, 50, 58)
  problems <- character(0)

  for (i in seq_along(shutdown_starts)) {
    sm <- shutdown_starts[i]
    cm <- crash_months[i]
    p <- .make_mine_panel(seed = 100 + i, shutdown_months = c(sm, sm + 1), price_crash_month = cm)
    out <- tryCatch({
      panel <- loss_panel(p$L, labels = p$labels)
      withCallingHandlers(report(panel, seed = 0), warning = function(w) invokeRestart("muffleWarning"))
    }, error = function(e) e)

    if (inherits(out, "error")) {
      problems <- c(problems, sprintf("sweep %d: crashed -- %s", i, conditionMessage(out)))
      next
    }
    txt <- unclass(out)
    for (section in c("^VERDICT:", "LEADERBOARD", "RESOLUTION", "PIVOT")) {
      if (!grepl(section, txt)) {
        problems <- c(problems, sprintf("sweep %d: missing section matching '%s'", i, section))
      }
    }
    champ_line <- grep("\\(champion\\)", strsplit(txt, "\n")[[1]], value = TRUE)
    if (length(champ_line) != 1) {
      problems <- c(problems, sprintf("sweep %d: expected exactly one (champion) line, found %d", i, length(champ_line)))
    }
  }

  if (length(problems) > 0) {
    fail(paste("Problems found across the mining shutdown/price-crash timing sweep:\n",
               paste(problems, collapse = "\n")))
  } else {
    expect_length(problems, 0)
  }
})
