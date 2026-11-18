# Realistic airline crew-scheduling / network-operations staffing forecast scenario.
#
# Domain grounding: an airline network-operations group compares several competing models for
# forecasting daily crew-demand/flight-disruption-driven staffing need. Unlike contact-center WFM
# (test-realworld-workforce.R; half-hourly, T ~ 8,700 over 6 months), airlines plan crew at DAILY
# granularity over MULTI-YEAR horizons -- a realistic comparison window is T ~ 1,000-1,500 (here:
# 4 years x 365 days = 1,460). The seasonality texture is also different in kind, not just scale:
#   - weekly: Friday/Sunday leisure+business overlap peaks, midweek trough (not the contact-center
#     Monday-heavy/weekend-light pattern);
#   - annual: a broad summer travel peak plus sharp Thanksgiving/Christmas spikes;
#   - a multi-month, regime-break-style demand COLLAPSE (modeled on the 2020 pandemic air-travel
#     collapse: a sharp multi-day drop followed by a slow ~400-day recovery), which is a genuinely
#     different shock SHAPE from either contact-center test's single/few-day shared-shock spikes;
#   - irregular, non-periodic, multi-day CASCADING disruption events (weather-driven ground stops,
#     mechanical cascading delays) -- unlike a single-day outage/billing-cycle spike, an airline
#     disruption's cost decays over several FOLLOWING days as crews and aircraft recover their
#     scheduled position, which induces short-lag positive autocorrelation in the loss series that
#     a single-day shock does not. This is exactly the pattern report()'s static autocorrelation
#     disclosure (see R/report.R, RESOLUTION section) exists to warn about, so this test checks
#     that disclosure is present and that the cascade structure does in fact produce detectable
#     lag-1 autocorrelation in the champion-vs-runner-up loss differential.
#
# RUNTIME-SCALING RE-VERIFICATION (exploratory, 2026-11-18, informs the roxygen note added to
# exchangeable_benchmark() in R/fragility.R, not asserted here as a hard number): the existing
# roxygen documents ~78s at K=5/T=8736 with the n_rep=200/n_boot=500 defaults, and claims the cost
# is O(n_rep * n_boot * T). At this scenario's T ~ 1,460 (about 1/6 of 8,736), a naive linear
# extrapolation predicts ~13s for one full-defaults call. Repeated direct measurement at T=1,460
# with the full defaults did NOT track that prediction reliably -- individual calls ranged from
# roughly the predicted ~13s up to 6x higher. Two causes were confirmed, neither a package bug:
# (1) `ps aux` during measurement showed this was a heavily shared/loaded machine running many
# OTHER concurrent R processes (other devtools::test() runs not started by this session), which
# inflates wall-clock time arbitrarily; and (2) even controlling for that with a clean multi-T
# min-of-repeats sweep (K=5, n_rep=20, n_boot=50 at T in {200,...,8736}), the per-T-unit rate was
# mildly DEcreasing as T grew (consistent with each of the n_rep replicates paying a fixed,
# T-independent setup cost -- seeding + drawing the null panel + entering winner_stability() --
# that is proportionally larger at small T and is not part of the function's documented O(T)
# inner-loop term). The O(n_rep * n_boot * T) complexity claim itself is still structurally
# correct (confirmed by code inspection: no hidden higher-order term in the block-resampling inner
# loop), but the single `T=8736` data point should not be used for precise linear extrapolation to
# other T -- see the roxygen addendum this finding produced. This test therefore keeps its own
# timing assertion generous rather than asserting any particular ratio against the documented
# figure, to avoid CI flakiness on shared/loaded CI runners.

test_that("realistic multi-year daily airline crew-staffing panel: cascading disruptions, pandemic-style collapse, full pipeline", {
  set.seed(20261118)

  years <- 4L
  Tn <- years * 365L  # 1,460 daily periods -- realistic airline crew-planning comparison window

  t_idx <- seq_len(Tn)
  doy <- (t_idx - 1L) %% 365L

  # Weekly pattern: Friday/Sunday peaks (leisure+business overlap), midweek trough -- the airline
  # network pattern, distinct from contact-center Monday-heavy/weekend-light.
  dow <- (t_idx - 1L) %% 7L
  dow_mult <- c(1.00, 0.92, 0.90, 0.95, 1.10, 1.05, 1.15)[dow + 1L]  # Mon..Sun

  # Annual seasonality: broad summer peak + sharp Thanksgiving/Christmas spikes.
  annual <- 1 + 0.25 * sin(2 * pi * (doy - 50) / 365) +
    0.10 * exp(-((doy - 359)^2) / (2 * 10^2)) +
    0.08 * exp(-((doy - 330)^2) / (2 * 7^2))

  base_demand <- pmax(100 * dow_mult * annual, 1)

  # Pandemic-style demand collapse: a sharp multi-day drop starting mid-year-1, followed by a slow
  # ~400-day recovery -- a genuine regime break, structurally different from a transient spike.
  pandemic_start <- 300L
  collapse_mult <- rep(1, Tn)
  collapse_idx <- which(t_idx >= pandemic_start & t_idx <= pandemic_start + 400L)
  rel <- t_idx[collapse_idx] - pandemic_start
  collapse_mult[collapse_idx] <- 0.15 + 0.85 * pmin(rel / 400, 1)^1.5
  base_demand <- base_demand * collapse_mult

  # Irregular, non-periodic, multi-day CASCADING disruptions: weather ground stops / mechanical
  # cascades. Each event's cost decays over 2-5 FOLLOWING days (crews/aircraft out of position) --
  # fundamentally different in shape from a single-day shared shock.
  n_events <- 150L
  event_days <- sample(seq_len(Tn), n_events)
  event_days <- event_days[!(event_days %in% collapse_idx)]
  cascade_effect <- rep(0, Tn)
  for (ev in event_days) {
    severity <- stats::runif(1, 0.6, 1.8)
    cascade_len <- sample(4:8, 1)
    for (d in 0:(cascade_len - 1L)) {
      day <- ev + d
      if (day <= Tn) cascade_effect[day] <- cascade_effect[day] + severity * exp(-d / 3.0)
    }
  }
  shock_common_miss <- cascade_effect * base_demand

  model_names <- c("naive_seasonal", "sarima", "ml_gbm", "network_opt_heuristic", "ensemble")
  noise_sd <- c(naive_seasonal = 9.0, sarima = 6.0, ml_gbm = 5.0,
                network_opt_heuristic = 5.5, ensemble = 4.0)
  # No model carries an explicit disruption regressor; ensemble is modestly better but still
  # misses the cascading-disruption shock by a correlated amount.
  shock_model_scale <- c(naive_seasonal = 1.00, sarima = 0.85, ml_gbm = 0.60,
                          network_opt_heuristic = 0.70, ensemble = 0.55)

  losses <- stats::setNames(vector("list", length(model_names)), model_names)
  for (m in model_names) {
    idiosyncratic <- stats::rnorm(Tn, mean = 0, sd = noise_sd[[m]])
    losses[[m]] <- abs(idiosyncratic + shock_common_miss * shock_model_scale[[m]])
  }

  panel <- loss_panel(losses)
  expect_s3_class(panel, "loss_panel")
  expect_equal(length(panel$weights), Tn)

  # Full workflow runs end to end at this scale/shape without error.
  rep_out <- report(panel)
  expect_true(any(grepl("^VERDICT:", rep_out)))

  # The autocorrelation disclosure must still print verbatim on a panel whose shock structure is
  # explicitly multi-day-correlated (the exact case the disclosure is meant to flag) -- it is a
  # static, always-printed line (not conditional on a runtime ACF check; see report.R's header
  # comment for why a dynamic check was rejected), so this also re-confirms it reads sensibly here.
  expect_true(any(grepl("RESOLUTION assumes no autocorrelation", rep_out)))

  # ensemble has the lowest noise and the smallest cascading-shock miss scale, so it should
  # pool-win.
  expect_equal(pooled_winner(losses), "ensemble")

  # The cascading (multi-day-decay) disruption structure should leave detectable short-lag
  # positive autocorrelation in the champion-vs-runner-up loss differential -- unlike a single-
  # day shared shock (which leaves lag-1 ACF near zero), a 2-5 day decaying cascade correlates
  # adjacent days' differentials. This is exactly the condition the RESOLUTION disclosure warns
  # about, so this assertion checks the disclosure's premise is realistic for this domain, not
  # merely that the disclosure string exists.
  means <- sapply(losses, mean)
  champ <- names(which.min(means))
  runner_up <- names(sort(means))[2]
  diff_series <- losses[[runner_up]] - losses[[champ]]
  acf_vals <- stats::acf(diff_series, lag.max = 5, plot = FALSE)$acf[, , 1]
  expect_gt(acf_vals[2], 0.05)  # lag-1 ACF (index 2; index 1 is lag-0 = 1 by construction)

  # Native and delegated MCS engines should both run cleanly at this scale.
  Lmat <- do.call(cbind, losses)
  mcs_res <- mcs(Lmat, alpha = 0.10, seed = 1)
  expect_true(is.list(mcs_res))
  expect_true(length(mcs_res$surviving) >= 1)

  ident <- identified(losses, alpha = 0.10, seed = 1)
  expect_true(is.logical(ident))

  # winner_stability() must complete quickly at this scale with a modest bootstrap count.
  t0 <- Sys.time()
  ws <- winner_stability(losses, n_boot = 300, seed = 1)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(ws$stability > 0.5)  # ensemble should win most resamples given its clear edge
  expect_lt(elapsed, 15.0)  # generous: avoid flakiness on shared/loaded CI runners

  # exchangeable_benchmark() at reduced n_rep/n_boot (NOT this function's own defaults -- see the
  # module-level comment above and the roxygen addendum in R/fragility.R for why the single
  # documented T=8736/~78s data point should not be linearly extrapolated to this T) should still
  # complete well within an interactive-exploration budget and return a well-formed result.
  t0 <- Sys.time()
  eb <- exchangeable_benchmark(K = length(model_names), T = Tn, n_rep = 20, n_boot = 50, seed = 1)
  elapsed_eb <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(is.list(eb))
  expect_true(eb$median >= 0 && eb$median <= 1)
  expect_true(eb$p05 <= eb$median && eb$median <= eb$p95)
  # Generous bound (not a precise scaling check -- see the header comment on why wall-clock
  # numbers at this T were too noisy on a shared machine to assert a tight ratio).
  expect_lt(elapsed_eb, 30.0)
})
