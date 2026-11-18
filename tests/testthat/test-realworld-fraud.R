# REALISTIC DOMAIN TEST: e-commerce transaction-fraud-rate forecasting at a payments company.
#
# BUSINESS CONTEXT: a payments company's risk team compares 4 competing weekly fraud-rate
# forecasting models -- rules_based (a hand-tuned velocity/blacklist rules engine, cheap and
# stable but cannot adapt to genuinely new patterns), classical_ml (a gradient-boosted-trees
# model on engineered features, retrained periodically), deep_learning (a neural model that is
# the live PRODUCTION detector actually screening transactions), and ensemble (a blend of all
# three) -- over T=104 weekly fraud-rate observations (~2 years). Risk management needs a
# defensible answer about which model to trust for next quarter's fraud-loss reserve and
# detection-threshold tuning, NOT just "which model has the lowest historical error."
#
# This domain has three real characteristics this package's other (more adversarial/synthetic
# or non-adversarial-wave) test files do not jointly exercise:
#
# 1. GENUINE RARE-EVENT, LOW-BASE-RATE SIGNAL: the true weekly fraud rate is baseline=0.30% --
#    well under 1%, realistic for card-not-present e-commerce fraud -- so almost all information
#    in the loss series is concentrated in a small number of elevated-fraud weeks, similar in
#    SHAPE to this package's other rare-event domains but on a genuinely sub-1% base rate.
#
# 2. PERIODIC "FRAUD WAVE" EVENTS: a new fraud ring/technique emerges three times across the two
#    years (weeks ~18, ~55, ~90 -- roughly one per "holiday-adjacent" high-volume season), each
#    producing a temporary multiplicative spike in the true fraud rate (8x/10x/14x baseline,
#    successive rings being more sophisticated) before detection catches up and the rate decays
#    back to baseline -- an epidemic-wave SHAPE, but adversarial, not biological, in cause.
#
# 3. ADVERSARIAL-EVOLVING SIGNAL (the genuinely distinct mechanism this test exists to add):
#    fraudsters actively adapt to whatever model is in PRODUCTION. deep_learning is the live
#    detector actually screening transactions, so it is the one fraud rings can probe and evade
#    over time -- rules_based and classical_ml are not deployed, so they are not specifically
#    targeted. deep_learning wins wave 1 outright (its decision boundary is still fresh and
#    unprobed), remains ahead on wave 2, but by wave 3 -- after two cycles of rings learning its
#    blind spots precisely BECAUSE it was deployed and won before -- it badly UNDERSHOOTS the
#    third wave and becomes the WORST model on that wave. This is a feedback loop driven by the
#    model's OWN deployment history, not a structural shape-mismatch (contrast with
#    test-realworld-epidemiology.R's seir_mech, which is bad at double-peak waves because of its
#    own fixed functional form, never because it was deployed) and not a weather/catastrophe-style
#    exogenous shock (contrast with test-realworld-weatherrisk.R's hurricane weeks, which hit
#    every model independent of deployment history).
#
# Loss = squared error in the forecasted fraud rate, in bps^2 ((forecast - true) * 1e4)^2 --
# the realistic "a rare large miss dominates the loss" unit a risk team would actually score in,
# directly analogous to this package's other rare-event domains.

set.seed(20261118L)
T_weeks <- 104L
baseline_rate <- 0.003  # 0.30% baseline e-commerce fraud rate -- genuinely sub-1%, rare-event

wave_centers <- c(18, 55, 90)
wave_peak_mult <- c(8, 10, 14)  # successive fraud rings are more sophisticated / damaging
wave_width <- 3

true_rate <- rep(baseline_rate, T_weeks)
wave_id <- rep("baseline", T_weeks)
for (i in seq_along(wave_centers)) {
  c_ <- wave_centers[i]
  for (t in seq_len(T_weeks)) {
    d <- abs(t - c_)
    if (d <= wave_width * 2) {
      bump <- wave_peak_mult[i] * baseline_rate * exp(-0.5 * (d / wave_width)^2)
      true_rate[t] <- true_rate[t] + bump
      if (bump > 0.15 * baseline_rate) wave_id[t] <- paste0("wave", i)
    }
  }
}

n <- T_weeks
mult_noise <- function(sd) 1 + stats::rnorm(n, 0, sd)

# rules_based: cannot adapt to NEW techniques at all (static rules) -- systematically
# undershoots every wave uniformly, regardless of wave number or deployment history.
rules_based <- true_rate * (1 + ifelse(wave_id == "baseline", 0, -0.55)) * mult_noise(0.10)

# classical_ml: moderate, roughly wave-number-independent adaptation -- retrained periodically,
# not the live detector, so not specifically targeted by adapting fraudsters.
classical_ml <- true_rate * (1 + ifelse(wave_id == "baseline", 0, -0.25)) * mult_noise(0.12)

# deep_learning: the PRODUCTION detector. Wins wave 1 (undershoot bias only -5%), still ahead on
# wave 2 (-15%), but by wave 3 -- after fraud rings have A/B-tested against and learned its
# specific blind spots precisely because it was deployed and won before -- it badly UNDERSHOOTS
# (+60% bias, i.e. forecasts far too LOW relative to the true elevated rate) and becomes the
# worst model specifically on the wave its own past success invited evasion against.
dl_wave_bias <- c(wave1 = -0.05, wave2 = -0.15, wave3 = 0.60)
deep_learning <- true_rate * (1 + ifelse(wave_id == "baseline", 0, dl_wave_bias[wave_id])) *
  mult_noise(ifelse(wave_id == "baseline", 0.08, 0.10))

# ensemble: blends all three -- never the single best on any one wave, but hedges the
# adversarial-targeting risk that concentrates on whichever single model is in production.
ensemble <- (rules_based + classical_ml + deep_learning) / 3

models <- lapply(
  list(rules_based = rules_based, classical_ml = classical_ml,
       deep_learning = deep_learning, ensemble = ensemble),
  function(v) pmax(v, 0)
)

# Loss in bps^2 -- rare, large misses during wave weeks dominate the loss signal, the realistic
# low-base-rate / rare-event characteristic this scenario is built around.
losses <- lapply(models, function(f) ((f - true_rate) * 1e4)^2)

test_that("fraud-rate panel is a genuine rare-event, wave-driven signal with realistic magnitude concentration", {
  all_losses <- unlist(losses)
  nz <- all_losses[all_losses > 0]
  # Wave-week losses are routinely orders of magnitude above baseline-week losses for the SAME
  # model -- the rare-event concentration this scenario exists to test, directly analogous to
  # this package's other rare-event domains but driven here by adversarial fraud dynamics.
  expect_gt(max(nz) / min(nz), 1e6)
  for (m in names(losses)) {
    wave_mean <- mean(losses[[m]][wave_id != "baseline"])
    base_mean <- mean(losses[[m]][wave_id == "baseline"])
    expect_true(is.finite(wave_mean) && is.finite(base_mean))
    expect_gt(wave_mean, base_mean)
  }
  # The baseline fraud rate is genuinely sub-1% -- the rare-event regime this test is grounded in.
  expect_lt(baseline_rate, 0.01)
})

test_that("adversarial adaptation: deep_learning wins the EARLY waves but loses the LATEST wave specifically because it is the deployed model", {
  wave_levels <- c("wave1", "wave2", "wave3")
  wave_champs <- vapply(wave_levels, function(wl) {
    idx <- which(wave_id == wl)
    pooled_winner(lapply(losses, function(v) v[idx]))
  }, character(1))

  # deep_learning (the production detector) is the best model on the first two waves...
  expect_identical(unname(wave_champs["wave1"]), "deep_learning")
  expect_identical(unname(wave_champs["wave2"]), "deep_learning")
  # ...but NOT on the third -- the adversarial-adaptation mechanism this test exists to exercise:
  # deep_learning's own deployment and past wins are what let fraud rings learn and evade it,
  # degrading ITS relative performance specifically, not a shared/exogenous shock to every model.
  expect_false(identical(unname(wave_champs["wave3"]), "deep_learning"))

  # No single model dominates every wave -- the core fragile-selection scenario this package
  # diagnoses (contrast with a panel where one model is uniformly best across all structure).
  expect_gt(length(unique(wave_champs)), 1)

  # Directly confirm the adversarial decay: deep_learning's wave3 mean loss must be its OWN worst
  # relative showing across the three waves (normalized by the true rate at each wave, since the
  # waves have different absolute magnitudes by construction).
  dl_wave_losses <- vapply(wave_levels, function(wl) {
    idx <- which(wave_id == wl)
    mean(losses$deep_learning[idx]) / mean(true_rate[idx])^2
  }, numeric(1))
  expect_equal(unname(which.max(dl_wave_losses)), 3L)
})

test_that("loss_panel()/report() run cleanly on the rare-event, wave-driven fraud panel", {
  panel <- loss_panel(losses, labels = seq_len(T_weeks))
  expect_s3_class(panel, "loss_panel")
  expect_equal(length(panel$labels), T_weeks)

  rpt <- report(panel, alpha = 0.10, seed = 1)
  expect_s3_class(rpt, "selectionfragility_report")
  rpt_str <- unclass(rpt)
  expect_true(grepl("VERDICT", rpt_str, fixed = TRUE))
  expect_true(grepl("LEADERBOARD", rpt_str, fixed = TRUE))
  expect_true(grepl("RESOLUTION", rpt_str, fixed = TRUE))
  expect_true(grepl("PIVOT", rpt_str, fixed = TRUE))

  # ensemble (the adversarial-targeting hedge) is the pooled-mean champion, and rules_based (the
  # model that cannot adapt to ANY new technique) is excluded from the MCS -- a sane, legible
  # result for this scenario's construction.
  expect_true(grepl("VERDICT: not identified \\(MCS size 3 of 4\\)", rpt_str))
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ensemble")
  expect_true(grepl("rules_based .*excluded", rpt_str))
})

test_that("decision_breakdown()/concentration_share(): the ranking is driven almost entirely by the LATEST fraud wave (where adversarial adaptation hits hardest), not spread evenly across waves", {
  panel <- loss_panel(losses, labels = seq_len(T_weeks))
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ensemble")

  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$binding_opponent, "classical_ml")
  # k* is tiny relative to T (4 of 104, ~3.8%) -- a textbook fragile-looking decision.
  expect_identical(db$k, 4L)
  removed <- sort(db$removed)

  wave3_idx <- which(wave_id == "wave3")
  # EVERY period decision_breakdown() needs to delete to flip ensemble-vs-classical_ml lies
  # inside the THIRD (latest, most-evaded) fraud wave specifically -- direct confirmation that
  # it is the adversarial-adaptation-driven wave, not wave1/wave2 or ordinary weeks, that drives
  # the model-selection decision for next quarter's reserve/threshold call.
  expect_true(all(removed %in% wave3_idx))

  wave1_idx <- which(wave_id == "wave1")
  wave2_idx <- which(wave_id == "wave2")
  expect_false(any(removed %in% wave1_idx))
  expect_false(any(removed %in% wave2_idx))

  cs <- concentration_share(panel)
  expect_true(is.finite(cs))
  # No single week dominates the margin (concentration_share stays modest) even though ALL of
  # k*'s removed periods sit inside one wave -- mirroring this package's documented
  # concentration_share()-vs-decision_breakdown() divergence in the weather-catastrophe domain
  # (test-realworld-weatherrisk.R), now reconfirmed under the adversarial-fraud-wave mechanism:
  # concentration_share() measures the SINGLE most influential period only, and under-states how
  # concentrated the decision really is in the wave as a whole.
  expect_lt(cs, 0.30)

  # Prop 2.2's zero-simulation certificate does not certify k*=4 as non-significant noise at
  # T=104 here (consistent with "not identified" above -- the champion's edge is genuinely
  # contestable, not dismissable as pure chance).
  cert <- prop22_certifies(db$k, T = T_weeks)
  expect_false(cert)
})

test_that("winner_stability()/exchangeable_benchmark() give a REASSURING read even though decision_breakdown() shows the decision hinges entirely on the latest adversarial fraud wave", {
  # This is the central decision-relevant cross-check requested for this domain: does the
  # champion's pooled-mean win survive random resampling (winner_stability, WITH replacement
  # over all T periods), or only the adversarially-chosen k*=4 periods identified above? Because
  # wave3's periods are scattered among 104 weeks, a random/block resample is unlikely to omit
  # all of them -- so winner_stability should NOT flag this panel as unusually fragile relative
  # to the exchangeable (no-true-winner) null, even though decision_breakdown()'s own adversarial
  # k* search (previous test) shows the ranking is almost entirely wave3-driven. This reproduces,
  # under the genuinely distinct adversarial-adaptation-wave mechanism, the SAME kind of
  # winner_stability-vs-decision_breakdown divergence this package's weather-catastrophe test
  # (test-realworld-weatherrisk.R) already documents for an exogenous-shock mechanism -- i.e. the
  # gap is not specific to one domain's cause of concentration, it recurs across mechanisms.
  panel <- loss_panel(losses, labels = seq_len(T_weeks))

  ws <- winner_stability(panel, seed = 1, n_boot = 2000)
  bench <- exchangeable_benchmark(K = 4, T = T_weeks, n_rep = 200, n_boot = 500, seed = 1)

  expect_true(is.finite(ws$stability))
  expect_true(is.finite(bench$p05))
  # Observed stability sits comfortably mid-range...
  expect_gt(ws$stability, 0.70)
  expect_lt(ws$stability, 0.90)
  # ...and, critically, ABOVE the pure-chance null's 5th percentile -- i.e. NOT flagged as
  # unusually fragile relative to chance alone, even though decision_breakdown()/
  # concentration_share() (previous test) show the decision is driven entirely by the handful of
  # periods inside the single most-recently-evaded fraud wave. Both readings are individually
  # correct; a risk analyst who checks only winner_stability()+exchangeable_benchmark() before
  # setting next quarter's reserve would get a falsely reassuring signal about how much the
  # ongoing adversarial arms race (not random noise) is actually driving the model ranking.
  expect_gt(ws$stability, bench$p05)
})

test_that("fragility(): pooled-mean champion and per-period plurality winner disagree -- the deployed model wins more individual weeks than the hedge that is actually the better long-run reserve/threshold choice", {
  panel <- loss_panel(losses, labels = seq_len(T_weeks))
  f <- fragility(panel$losses, panel$weights)

  expect_true(is.finite(f$k_star))
  expect_true(is.finite(f$concentration))
  expect_false(isTRUE(f$degenerate_margin))

  # ensemble is the pooled-mean champion (lowest average squared error)...
  expect_identical(f$pooled_winner, "ensemble")
  # ...but deep_learning -- the production model that wins both early waves outright before its
  # wave-3 adversarial-evasion collapse -- actually wins MORE individual weeks on a strict
  # per-period plurality basis. A risk team that picked "whichever model wins the most weeks"
  # would crown the model whose exposure to adversarial adaptation is actively growing, exactly
  # the selection-fragility trap this package exists to flag before a quarterly reserve/threshold
  # decision is locked in.
  expect_identical(f$per_period_winner, "deep_learning")
  expect_true(f$reversal)

  summ <- summary(panel)
  expect_identical(summ$model[which.min(summ$mean_loss)], "ensemble")
  expect_identical(summ$model[which.max(summ$periods_won)], "deep_learning")
})

test_that("resolution_report()/mcb_bound() stay finite and sane on this rare-event, wave-driven bps^2 loss scale", {
  panel <- loss_panel(losses, labels = seq_len(T_weeks))
  rr <- resolution_report(panel$losses, panel$weights)
  expect_true(is.finite(rr$observed_edge))
  expect_true(is.finite(rr$mde))
  expect_true(is.finite(rr$significance_boundary))
  mcb <- mcb_bound(panel$losses, panel$weights)
  expect_true(is.finite(mcb))
  # Sane fractional bounds, not a units/overflow artifact -- even though the underlying loss
  # scale (bps^2 on a sub-1% rate) is tiny in absolute terms, the RATIO-based resolution
  # statistics should stay on an ordinary, interpretable fractional scale.
  expect_true(abs(rr$observed_edge) < 10)
  expect_true(abs(mcb) < 10)
})
