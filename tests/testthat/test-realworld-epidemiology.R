# REALISTIC DOMAIN TEST: an epidemiological forecast-hub scenario (think CDC FluSight / the
# COVID-19 Forecast Hub). Five competing models submit weekly 1-week-ahead case-count forecasts
# over ~2.5 years spanning three epidemiologically distinct waves:
#   - a classic sharp single-peak wave (SEIR-style growth then decay),
#   - a slow, broad plateau wave (gradual rise, long plateau, gradual decline), and
#   - an unusual double-peak wave (variant-driven resurgence -- the kind of multi-modal shape
#     that breaks a single-compartment mechanistic fit but is exactly what a flexible/ML model
#     can pick up).
# Troughs of low-transmission baseline activity separate the waves.
#
# This exercises three real, well-documented characteristics of epidemiological forecasting that
# are NOT covered by this package's other (more adversarial/synthetic) test files:
#   1. EXTREME, LEGITIMATE WITHIN-SERIES LOSS MAGNITUDE HETEROGENEITY: because case counts grow
#      and shrink exponentially, a model's absolute forecast error during a growth/decay week can
#      be two to three orders of magnitude larger than its error during a flat trough week --
#      cf. Cramer et al. (2022), "Evaluation of individual and ensemble probabilistic forecasts of
#      COVID-19 mortality in the US", PNAS: forecast-hub scoring error is heavily concentrated
#      around epidemic turning points, not spread evenly across the series. This is DIFFERENT from
#      the across-model magnitude-mismatch panels exercised elsewhere (e.g. resolution.R's
#      documented champion-mean-near-1e-12-floor caveat) -- here every model operates on the SAME
#      underlying count scale; the heterogeneity is WITHIN one series, driven by the epidemic
#      curve's own shape, and is a completely ordinary, non-adversarial feature of this domain.
#   2. NO SINGLE MODEL UNIFORMLY DOMINATES: a mechanistic/compartmental-style model is built to
#      win the classic single-peak wave (its home turf) but to systematically miss the second bump
#      of the double-peak wave; a flexible/ML-style model is noisier early but adapts to the
#      plateau and double-peak shapes; a naive persistence baseline is built to be worst exactly at
#      turning points (by construction, since "last week's value" is maximally wrong whenever the
#      series is changing fastest) -- the real reason every forecast hub carries a persistence/
#      baseline model, to demonstrate value added over doing nothing.
#   3. REPEATED, HUB-CADENCE COMPARISON: `compare()` is run once per week on a rolling trailing
#      window, exactly matching how a real hub (FluSight, the COVID-19 Forecast Hub) repeatedly
#      asks "is this week's apparent champion a durable pick, or just this window's lucky draw?"

test_that("multi-wave epidemiological panel has extreme, legitimate within-series loss magnitude heterogeneity", {
  set.seed(2024)
  T_weeks <- 130L

  baseline <- 50
  wave1 <- function(t) {
    peak <- 40000; growth_rate <- 0.35; decay_rate <- 0.22
    g <- peak * exp(growth_rate * (t - 16)) / (1 + exp(growth_rate * (t - 16)))
    d <- peak * exp(-decay_rate * pmax(t - 16, 0))
    ifelse(t <= 16, g, d)
  }
  wave2 <- function(t) {
    peak <- 15000
    rise <- peak / (1 + exp(-0.25 * (t - 10)))
    plateau_decay <- peak / (1 + exp(-0.25 * (10 - (t - 20))))
    pmin(rise, ifelse(t > 20, plateau_decay, peak))
  }
  wave3 <- function(t) {
    peak1 <- 9000; peak2 <- 13000
    bump1 <- peak1 * exp(-0.5 * ((t - 8) / 4)^2)
    bump2 <- peak2 * exp(-0.5 * ((t - 20) / 5)^2)
    bump1 + bump2
  }

  true_counts <- numeric(T_weeks)
  for (t in seq_len(T_weeks)) {
    true_counts[t] <- if (t <= 40) {
      baseline + wave1(t)
    } else if (t <= 55) {
      baseline + 5 * (t - 40)
    } else if (t <= 90) {
      baseline + wave2(t - 55)
    } else if (t <= 100) {
      baseline + 3 * (100 - t)
    } else {
      baseline + wave3(t - 100)
    }
  }
  true_counts <- pmax(true_counts, baseline * 0.5)

  # A real epidemic curve spans several orders of magnitude peak-to-trough -- this one spans
  # roughly 600x, well within realistic bounds for, e.g., a seasonal flu or COVID-19 wave.
  expect_gt(max(true_counts) / min(true_counts), 100)

  wave_id <- ifelse(seq_len(T_weeks) <= 40, "wave1_single_peak",
              ifelse(seq_len(T_weeks) <= 55, "trough1",
              ifelse(seq_len(T_weeks) <= 90, "wave2_plateau",
              ifelse(seq_len(T_weeks) <= 100, "trough2", "wave3_double_peak"))))
  rel_change <- c(0, abs(diff(true_counts)) / pmax(true_counts[-T_weeks], 1))
  growth_phase <- rel_change > 0.08

  mult_error <- function(n, sd, bias = 0) 1 + bias + stats::rnorm(n, 0, sd)
  n <- T_weeks
  make_forecast <- function(sd_by_wave, sd_growth_mult, bias_by_wave = NULL) {
    sd_vec <- numeric(n); bias_vec <- numeric(n)
    for (i in seq_len(n)) {
      w <- wave_id[i]
      sd_vec[i] <- sd_by_wave[[w]] * (if (growth_phase[i]) sd_growth_mult else 1)
      bias_vec[i] <- if (!is.null(bias_by_wave)) bias_by_wave[[w]] else 0
    }
    pmax(true_counts * mult_error(n, sd_vec, bias_vec), 1)
  }

  # seir_mech: wins the classic single-peak wave (its home turf); systematically UNDERSHOOTS the
  # second bump of the double-peak wave (a single-compartment fit chases the first bump, then
  # stays low -- a bias, not noise, exactly the known failure mode of fitting a unimodal
  # mechanistic model to a multi-modal resurgence).
  seir_mech <- make_forecast(
    sd_by_wave = list(wave1_single_peak = 0.04, trough1 = 0.05, wave2_plateau = 0.12,
                       trough2 = 0.05, wave3_double_peak = 0.10),
    sd_growth_mult = 1.3,
    bias_by_wave = list(wave1_single_peak = 0, trough1 = 0, wave2_plateau = -0.05,
                         trough2 = 0, wave3_double_peak = -0.30)
  )
  # ml_lstm: noisier/worse on the sharp early single-peak wave (overshoots, needs more signal),
  # but adapts well to the plateau and the double peak.
  ml_lstm <- make_forecast(
    sd_by_wave = list(wave1_single_peak = 0.15, trough1 = 0.07, wave2_plateau = 0.06,
                       trough2 = 0.06, wave3_double_peak = 0.07),
    sd_growth_mult = 1.6,
    bias_by_wave = list(wave1_single_peak = 0.08, trough1 = 0, wave2_plateau = 0,
                         trough2 = 0, wave3_double_peak = 0)
  )
  # arima: fine in flat/trough periods, poor at every wave's growth/decay transition (a linear
  # autoregressive model cannot capture exponential nonlinearity).
  arima <- make_forecast(
    sd_by_wave = list(wave1_single_peak = 0.10, trough1 = 0.03, wave2_plateau = 0.08,
                       trough2 = 0.03, wave3_double_peak = 0.09),
    sd_growth_mult = 2.2
  )
  # ensemble: consistently decent everywhere, never the single best on any one wave shape (the
  # realistic "hub ensemble" role).
  ensemble <- (seir_mech + ml_lstm + arima) / 3
  # naive_persistence: forecast = last week's count -- exactly right in a flat trough, maximally
  # wrong at every turning point by construction. The baseline every hub carries to demonstrate
  # value added over doing nothing.
  naive_persistence <- c(true_counts[1], true_counts[-n])

  models <- list(seir_mech = seir_mech, ml_lstm = ml_lstm, arima = arima,
                 ensemble = ensemble, naive_persistence = naive_persistence)
  # Loss = absolute forecast error in case-count units -- the realistic unit real hubs score in
  # (e.g. FluSight/COVID-19 Forecast Hub absolute-error and WIS scores), which is exactly why loss
  # magnitude tracks the exponential case-count scale instead of being normalized away.
  losses <- lapply(models, function(f) abs(f - true_counts))

  all_losses <- unlist(losses)
  nonzero <- all_losses[all_losses > 0]
  # The key realistic feature this test exists to exercise: growth/decay-phase losses are
  # routinely two-to-several orders of magnitude larger than trough-phase losses for the SAME
  # model, within the SAME series -- not an adversarial extreme, just an ordinary consequence of
  # exponential epidemic dynamics.
  expect_gt(max(nonzero) / min(nonzero), 100)
  for (m in names(losses)) {
    trough_mean <- mean(losses[[m]][!growth_phase])
    growth_mean <- mean(losses[[m]][growth_phase])
    expect_true(is.finite(trough_mean) && is.finite(growth_mean))
  }

  # --- Different models win different waves: a genuinely fragile/ambiguous selection scenario ---
  wave_levels <- c("wave1_single_peak", "wave2_plateau", "wave3_double_peak")
  wave_champions <- vapply(wave_levels, function(wl) {
    idx <- which(wave_id == wl)
    pooled_winner(lapply(losses, function(v) v[idx]))
  }, character(1))
  # No single model should dominate every wave shape -- that is the whole point of a fragile
  # selection scenario (contrast with a panel where one model is uniformly best).
  expect_gt(length(unique(wave_champions)), 1)
  expect_identical(unname(wave_champions["wave1_single_peak"]), "seir_mech")

  # --- Build the panel and run report() with this extreme within-series heterogeneity present ---
  panel <- loss_panel(losses, labels = seq_len(T_weeks))
  expect_s3_class(panel, "loss_panel")

  rep_txt <- report(panel)
  expect_s3_class(rep_txt, "selectionfragility_report")
  rep_str <- unclass(rep_txt)
  expect_true(grepl("VERDICT", rep_str, fixed = TRUE))
  expect_true(grepl("LEADERBOARD", rep_str, fixed = TRUE))
  expect_true(grepl("RESOLUTION", rep_str, fixed = TRUE))
  expect_true(grepl("PIVOT", rep_str, fixed = TRUE))

  # The within-series magnitude heterogeneity must not produce non-finite or nonsensical numbers
  # anywhere in the resolution pipeline (.res_scale()/.weighted_se() et al.) -- these percentages
  # should be finite and within a sane range for a same-scale (no cross-model scale mismatch)
  # panel, unlike the documented extreme-cross-model-scale-mismatch caveat elsewhere in this
  # package (resolution.R's champion-mean-near-1e-12-floor case), which does NOT apply here.
  rr <- resolution_report(panel$losses, panel$weights)
  expect_true(is.finite(rr$observed_edge))
  expect_true(is.finite(rr$mde))
  expect_true(is.finite(rr$significance_boundary))
  mcb <- mcb_bound(panel$losses, panel$weights)
  expect_true(is.finite(mcb))
  expect_true(abs(rr$observed_edge) < 10)   # a sane fraction-of-champion-mean, not 10,000%+
  expect_true(abs(mcb) < 10)

  f <- fragility(panel$losses, panel$weights)
  expect_true(is.finite(f$k_star))
  expect_true(is.finite(f$concentration))
  expect_false(isTRUE(f$degenerate_margin))  # a real, non-degenerate pooled margin

  # Domain-relevant fragility insight this scenario is built to surface: "periods won" (a model's
  # week-by-week win count) can disagree sharply with the pooled-mean champion -- exactly the kind
  # of selection fragility a hub coordinator needs flagged before retiring/crowning a model on
  # weekly win-count alone. seir_mech wins the single-peak wave outright but accumulates enough
  # growth-phase error elsewhere (undershooting wave 3's second peak) that the pooled mean and the
  # per-period plurality can disagree.
  summ <- summary(panel)
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_true(champ %in% summ$model)
  # The champion by pooled mean need not be the model with the most periods won -- this is the
  # central selection-fragility fact this package exists to diagnose.
  top_periods_won_model <- summ$model[which.max(summ$periods_won)]
  # This scenario is specifically built so the pooled-mean champion and the per-period plurality
  # winner diverge -- assert the disagreement directly rather than merely checking for it, so a
  # future change that accidentally makes them agree (and thus silently defeats the point of this
  # test) actually fails instead of being swallowed by a no-op `if` branch.
  expect_false(identical(top_periods_won_model, champ))
})

test_that("repeated weekly compare() over a rolling hub window behaves sensibly across ~100 sequential calls", {
  # Simulates the actual cadence of a real forecast hub: re-running the champion comparison every
  # week on a trailing window as new data arrives (cf. FluSight's weekly re-evaluation cycle).
  set.seed(2024)
  T_weeks <- 130L
  baseline <- 50
  wave1 <- function(t) {
    peak <- 40000; growth_rate <- 0.35; decay_rate <- 0.22
    g <- peak * exp(growth_rate * (t - 16)) / (1 + exp(growth_rate * (t - 16)))
    d <- peak * exp(-decay_rate * pmax(t - 16, 0))
    ifelse(t <= 16, g, d)
  }
  wave2 <- function(t) {
    peak <- 15000
    rise <- peak / (1 + exp(-0.25 * (t - 10)))
    plateau_decay <- peak / (1 + exp(-0.25 * (10 - (t - 20))))
    pmin(rise, ifelse(t > 20, plateau_decay, peak))
  }
  wave3 <- function(t) {
    peak1 <- 9000; peak2 <- 13000
    bump1 <- peak1 * exp(-0.5 * ((t - 8) / 4)^2)
    bump2 <- peak2 * exp(-0.5 * ((t - 20) / 5)^2)
    bump1 + bump2
  }
  true_counts <- numeric(T_weeks)
  for (t in seq_len(T_weeks)) {
    true_counts[t] <- if (t <= 40) baseline + wave1(t)
    else if (t <= 55) baseline + 5 * (t - 40)
    else if (t <= 90) baseline + wave2(t - 55)
    else if (t <= 100) baseline + 3 * (100 - t)
    else baseline + wave3(t - 100)
  }
  true_counts <- pmax(true_counts, baseline * 0.5)
  wave_id <- ifelse(seq_len(T_weeks) <= 40, "wave1_single_peak",
              ifelse(seq_len(T_weeks) <= 55, "trough1",
              ifelse(seq_len(T_weeks) <= 90, "wave2_plateau",
              ifelse(seq_len(T_weeks) <= 100, "trough2", "wave3_double_peak"))))
  rel_change <- c(0, abs(diff(true_counts)) / pmax(true_counts[-T_weeks], 1))
  growth_phase <- rel_change > 0.08
  mult_error <- function(n, sd, bias = 0) 1 + bias + stats::rnorm(n, 0, sd)
  n <- T_weeks
  make_forecast <- function(sd_by_wave, sd_growth_mult, bias_by_wave = NULL) {
    sd_vec <- numeric(n); bias_vec <- numeric(n)
    for (i in seq_len(n)) {
      w <- wave_id[i]
      sd_vec[i] <- sd_by_wave[[w]] * (if (growth_phase[i]) sd_growth_mult else 1)
      bias_vec[i] <- if (!is.null(bias_by_wave)) bias_by_wave[[w]] else 0
    }
    pmax(true_counts * mult_error(n, sd_vec, bias_vec), 1)
  }
  seir_mech <- make_forecast(
    sd_by_wave = list(wave1_single_peak = 0.04, trough1 = 0.05, wave2_plateau = 0.12,
                       trough2 = 0.05, wave3_double_peak = 0.10),
    sd_growth_mult = 1.3,
    bias_by_wave = list(wave1_single_peak = 0, trough1 = 0, wave2_plateau = -0.05,
                         trough2 = 0, wave3_double_peak = -0.30)
  )
  ml_lstm <- make_forecast(
    sd_by_wave = list(wave1_single_peak = 0.15, trough1 = 0.07, wave2_plateau = 0.06,
                       trough2 = 0.06, wave3_double_peak = 0.07),
    sd_growth_mult = 1.6,
    bias_by_wave = list(wave1_single_peak = 0.08, trough1 = 0, wave2_plateau = 0,
                         trough2 = 0, wave3_double_peak = 0)
  )
  arima <- make_forecast(
    sd_by_wave = list(wave1_single_peak = 0.10, trough1 = 0.03, wave2_plateau = 0.08,
                       trough2 = 0.03, wave3_double_peak = 0.09),
    sd_growth_mult = 2.2
  )
  ensemble <- (seir_mech + ml_lstm + arima) / 3
  naive_persistence <- c(true_counts[1], true_counts[-n])
  losses <- list(seir_mech = seir_mech, ml_lstm = ml_lstm, arima = arima,
                 ensemble = ensemble, naive_persistence = naive_persistence)
  losses <- lapply(losses, function(f) abs(f - true_counts))

  window <- 26L  # a ~6-month trailing window, a realistic hub re-evaluation horizon
  start_week <- window
  n_calls <- 0L
  n_champion_changes <- 0L
  n_act_true <- 0L
  champions <- character(0)

  for (end_wk in start_week:(T_weeks - 1L)) {
    prev_idx <- (end_wk - window + 1L):end_wk
    curr_idx <- (end_wk - window + 2L):(end_wk + 1L)
    # Real, stable per-period identity (absolute week number), not re-based positional labels --
    # required for compare()'s new-period attribution to work correctly on a rolling window (see
    # compare()'s own documented "New-period attribution requires real labels" section).
    prev_panel <- loss_panel(lapply(losses, function(v) v[prev_idx]), labels = prev_idx)
    curr_panel <- loss_panel(lapply(losses, function(v) v[curr_idx]), labels = curr_idx)

    cr <- compare(prev_panel, curr_panel, seed = end_wk)
    expect_s3_class(cr, "change_report")
    # compare() must never crash across a long sequence of calls on realistic, legitimately
    # extreme-magnitude-heterogeneous data, and must always attribute the rolling window's single
    # new period correctly (one new week added each step).
    expect_equal(cr$n_new_periods, 1L)
    expect_true(is.logical(cr$champion_changed) && !is.na(cr$champion_changed))
    expect_true(is.logical(cr$act) && !is.na(cr$act))
    expect_true(cr$churn_base_rate >= 0 && cr$churn_base_rate <= 1)

    n_calls <- n_calls + 1L
    if (cr$champion_changed) n_champion_changes <- n_champion_changes + 1L
    if (isTRUE(cr$act)) n_act_true <- n_act_true + 1L
    champions <- c(champions, cr$current_champion)
  }

  expect_equal(n_calls, length(start_week:(T_weeks - 1L)))
  expect_gte(n_calls, 100L)

  # Over a wave-structured, multi-year series, the apparent champion SHOULD change at least once
  # as the rolling window moves from one wave's territory into another's -- a hub that never sees
  # a champion change across 100+ weekly re-evaluations on data built with genuinely different
  # per-wave winners would indicate compare() is not sensitive to the underlying wave structure.
  expect_gt(n_champion_changes, 0L)

  # The per-wave champions established in the companion test (seir_mech on the single-peak wave,
  # ml_lstm/ensemble elsewhere) should actually show up as the ROLLING champion at some point --
  # i.e. the rolling comparison is tracking the real wave structure, not returning junk.
  expect_true("seir_mech" %in% champions)
  expect_true(any(c("ml_lstm", "ensemble", "arima") %in% champions))
})
