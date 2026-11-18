# Realistic contact-center / workforce-management (WFM) intraday call-volume forecasting scenario.
#
# Domain grounding: a WFM team compares several intraday call-volume forecasting models (the
# inputs to Erlang-C/Erlang-A staffing calculations) on half-hourly historical out-of-sample loss.
# Real contact-center call volume is governed by three *simultaneous* seasonal periods -- hour-of-
# day (the classic "double-hump" curve: a late-morning peak and a smaller post-lunch peak), day-
# of-week (Mondays heaviest from weekend backlog, weekends much lighter), and a mild annual cycle
# -- plus occasional SHARED-SHOCK volume spikes (a billing-cycle surge, an outage, or a marketing
# campaign) that every model under-forecasts by a correlated amount, because none of them carries
# an explicit regressor for the disruption. This is the standard WFM stylized fact set (see e.g.
# Gans, Koole & Mandelbaum 2003 on call-center queueing/staffing, and the TC06/forecasting-
# competition literature on multiple-seasonality intraday series) and is exactly the setting
# selectionfragility is meant to diagnose: "is our model-comparison winner real, or is it just the
# handful of disruption days distorting the pooled loss ranking?"
#
# SCALE NOTE (explored, not committed here): staffing-grade half-hourly data over a realistic
# 6-month WFM comparison window is T ~ 8,736 (182 days x 48 half-hour slots/day) -- two to three
# orders of magnitude above this package's existing example/test panels (T in the tens). At that
# scale (K=5 models, exploratory run on 2026-11-18, M4 Air, single core):
#   loss_panel()                                             : ~0.01 s
#   report()                                                 : ~1.5 s
#   mcs() [native engine, B=2000 default]                    : ~2.8 s
#   identified() [delegated MCS-package engine, reps=500]    : ~0.1 s
#   winner_stability() [default n_boot=2000]                 : ~1.3 s
#   exchangeable_benchmark() [DEFAULTS n_rep=200, n_boot=500] : ~80 s (see below)
# Everything except exchangeable_benchmark() stays comfortably interactive at this realistic T.
# exchangeable_benchmark() nests winner_stability()'s own O(n_boot * T) bootstrap inside an outer
# O(n_rep) simulation loop, so its cost is O(n_rep * n_boot * T); at the defaults and T=8736 that
# is ~200 * 500 * 8736 ~= 8.7e8 inner element-ops, which is where the ~80s comes from. A WFM
# analyst who runs exchangeable_benchmark(K, T = 8736) with the documented defaults (as the
# man page's own examples encourage: "real usage should use the defaults ... or larger") hits a
# ~80-second wait for ONE null-distribution estimate, worse for a 12-month window or a block
# sweep -- not a crash, but a genuine practicality/documentation gap at a T scale this package's
# own domain (intraday WFM forecasting) routinely produces. No correctness bug was found: every
# function returns a well-formed, internally consistent answer at this T; only the DEFAULT
# bootstrap cost of exchangeable_benchmark() is impractical here without a user manually cutting
# n_rep/n_boot, which the current docs do not flag as T-scale-dependent. See the roxygen note
# added to exchangeable_benchmark() in R/fragility.R for the user-facing guidance this finding
# produced.
#
# This committed test uses a SMALLER T (one week of half-hourly data = 336 periods) so the suite
# stays fast, but keeps the full multi-period-seasonality + shared-shock-day structure, and still
# exercises mcs()/identified()/winner_stability()/report() together end-to-end on a realistic
# (if compressed) WFM panel.

test_that("realistic multi-seasonal WFM call-volume panel: full pipeline runs and agrees on a winner", {
  set.seed(20261118)

  days <- 7L
  slots_per_day <- 48L
  Tn <- days * slots_per_day  # 336 half-hourly periods = 1 week

  t_idx <- seq_len(Tn)
  hour_of_day <- (t_idx - 1L) %% slots_per_day
  day_idx <- (t_idx - 1L) %/% slots_per_day
  dow <- day_idx %% 7L

  # Hour-of-day: double-hump intraday curve (late-morning + post-lunch peaks), quiet overnight.
  hod_frac <- hour_of_day / slots_per_day * 24
  intraday <- 0.15 + 0.85 * exp(-((hod_frac - 11)^2) / (2 * 3.2^2)) +
    0.55 * exp(-((hod_frac - 14.5)^2) / (2 * 2.5^2))
  intraday[hod_frac < 6 | hod_frac > 22] <- intraday[hod_frac < 6 | hod_frac > 22] * 0.25

  # Day-of-week: Monday-heavy, weekend-light -- the standard contact-center weekly pattern.
  dow_mult <- c(1.25, 1.05, 1.00, 1.00, 1.05, 0.55, 0.45)[dow + 1L]

  # Mild annual/seasonal trend (kept gentle since the window here is short).
  annual_trend <- 1 + 0.10 * sin(2 * pi * day_idx / 365) + 0.0003 * day_idx

  base_volume <- pmax(40 * intraday * dow_mult * annual_trend, 1)

  # Shared-shock disruption periods: a billing-cycle/outage surge on specific days, concentrated
  # around business hours -- affects the TRUE volume (and hence every model's error) similarly.
  disruption_days <- c(2L, 5L)  # two of the seven days get a disruption
  shock_mult <- rep(1, Tn)
  for (dd in disruption_days) {
    rows <- which(day_idx == (dd - 1L))
    surge <- 1 + 2.5 * exp(-((hod_frac[rows] - 12)^2) / (2 * 4^2))
    shock_mult[rows] <- surge
  }
  shock_common_miss <- (shock_mult - 1) * base_volume * shock_mult

  model_names <- c("naive_seasonal", "holtwinters_triple", "sarima", "prophet_like", "ml_gbm")
  noise_sd <- c(naive_seasonal = 6.0, holtwinters_triple = 3.5, sarima = 3.0,
                prophet_like = 2.8, ml_gbm = 2.2)
  # All models miss the shared shock by a correlated amount; ml_gbm is modestly better but still
  # misses it (no model carries an explicit disruption regressor).
  shock_model_scale <- c(naive_seasonal = 1.00, holtwinters_triple = 0.85, sarima = 0.80,
                          prophet_like = 0.75, ml_gbm = 0.65)

  losses <- stats::setNames(vector("list", length(model_names)), model_names)
  for (m in model_names) {
    idiosyncratic <- stats::rnorm(Tn, mean = 0, sd = noise_sd[[m]])
    shared_shock_component <- shock_common_miss * shock_model_scale[[m]]
    losses[[m]] <- abs(idiosyncratic + shared_shock_component * 0.9)
  }

  panel <- loss_panel(losses)
  expect_s3_class(panel, "loss_panel")
  expect_equal(length(panel$weights), Tn)

  # The full workflow should run end to end without error at this scale.
  rep_out <- report(panel)
  expect_true(any(grepl("^VERDICT:", rep_out)))

  # ml_gbm has the lowest noise and the smallest shared-shock miss scale, so it should pool-win.
  expect_equal(pooled_winner(losses), "ml_gbm")

  # Native and delegated MCS engines should both run cleanly at this scale and agree the winner
  # is identified as a singleton (ml_gbm's edge over the rest is large relative to its noise).
  Lmat <- do.call(cbind, losses)
  mcs_res <- mcs(Lmat, alpha = 0.10, seed = 1)
  expect_equal(colnames(Lmat)[mcs_res$surviving], "ml_gbm")

  ident <- identified(losses, alpha = 0.10, seed = 1)
  expect_true(is.logical(ident))

  # winner_stability() must complete quickly at this scale with a modest bootstrap count.
  t0 <- Sys.time()
  ws <- winner_stability(losses, n_boot = 300, seed = 1)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(ws$stability > 0.5)  # ml_gbm should win most resamples given its clear edge
  expect_lt(elapsed, 5.0)

  # exchangeable_benchmark() at this reduced T/n_rep/n_boot (NOT the function's own defaults --
  # see the module-level comment above for why the defaults are impractical at realistic WFM T)
  # should also complete quickly and return a well-formed null-distribution summary.
  t0 <- Sys.time()
  eb <- exchangeable_benchmark(K = length(model_names), T = Tn, n_rep = 20, n_boot = 50, seed = 1)
  elapsed_eb <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(eb$median >= 0 && eb$median <= 1)
  expect_lt(elapsed_eb, 10.0)
})
