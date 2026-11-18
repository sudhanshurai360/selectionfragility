# test-realworld-populationhealth.R
#
# A genuinely realistic CHRONIC-DISEASE / POPULATION-HEALTH-MANAGEMENT forecasting use case, as a
# lasting regression test and domain-grounding example -- distinct from this package's acute
# hospital-OPERATIONS example (test-realworld-healthcare.R: daily ED-visit volume, T=900, flu-
# surge-driven). This file is about the opposite end of the healthcare-forecasting spectrum: SLOW,
# demographically-driven chronic-disease cost trends, forecast at ANNUAL actuarial/risk-adjustment
# cadence with genuinely SMALL T, punctuated by a real drug-class-adoption structural break.
#
# THE USE CASE: a health plan's actuarial/population-health-management team compares 4 competing
# models for forecasting annual per-member-per-month (PMPM) chronic-disease-driven cost (diabetes,
# cardiovascular disease, COPD), over 12 years of annual data (T=12, 2013-2024) -- the realistic
# cadence for a capitation-rate-setting/risk-adjustment exercise, NOT a high-frequency operations
# problem:
#   1. historical_trend      -- naive trend extrapolation of the pre-existing aging-driven cost
#                                trend; the standard actuarial baseline method, blind to any new
#                                drug class
#   2. demographic_risk_adj  -- an HCC/demographic risk-adjustment model; captures the aging/
#                                prevalence composition shift well, but is ALSO blind to new
#                                therapies (it adjusts for WHO is covered, not WHAT they're on)
#   3. ml_claims_pharmacy    -- an ML model trained on claims/pharmacy (NDC code) data; picks up a
#                                new drug class's utilization surge quickly once it appears, but is
#                                noisier/more overfit-prone at this small T
#   4. drug_adoption_curve   -- explicitly models the new drug class's adoption S-curve; the
#                                sharpest tool for the structural break itself, but offers NO
#                                advantage over the naive baseline before the break occurs (see
#                                finding (C) below -- this is itself a realistic, useful property)
#
# THE STRUCTURAL BREAK: GLP-1 receptor agonists (semaglutide/tirzepatide) becoming standard-of-care
# for diabetes/obesity starting ~2021 is a real, well-documented, non-invented example of exactly
# this kind of break -- a new drug class driving an abrupt, measurable shift in chronic-disease-
# driven pharmacy cost trajectories (initially a pharmacy-cost step-up from a high list price,
# with downstream cardiometabolic-complication-cost offsets that were still emerging/debated in
# actual actuarial literature as of 2024-2025). This panel models the pharmacy-cost step-up (not
# the longer-run offset, which is out of scope for a 12-year window) starting in year index 9
# (2021) and ramping through 2024.
#
# WHY THIS IS A GENUINELY DIFFERENT DOMAIN SHAPE THAN THE ACUTE-CARE TEST, NOT JUST A RELABELING:
#   - SMALL T BY CONSTRUCTION, NOT BY CORNER-CUTTING: T=12/K=4 (T/K=3.0) is not a toy example
#     picked for speed -- it is the realistic annual cadence an actuarial/capitation-rate exercise
#     actually runs at. R/mcs.R's own small-T warning (added in an earlier audit round) explicitly
#     cites "T=12/K=4" as a freshly re-measured actuarial-reserving benchmark (~41%, not ~10%,
#     false-elimination rate under a true null) -- this test's domain and the package's own
#     already-measured worst case are the SAME point, not a coincidence to be papered over.
#   - PERSISTENT, SMOOTH BASELINE TREND, NOT A VOLATILE ONE: the pre-break baseline is a smooth
#     ~4.5%/year compounding aging/prevalence trend (CDC-documented multi-decade chronic-disease
#     prevalence drift), with only mild (sd=6 on a ~400-500 PMPM base) annual idiosyncratic noise --
#     unlike the acute-care test's day-to-day AR(1) autocorrelated noise, there is no recurring
#     seasonal cycle here at all; the dominant source of variation across the panel IS the one-time
#     structural break, not a repeating pattern.
#   - A STRUCTURAL BREAK THAT ARRIVES PARTWAY THROUGH THE SAMPLE, NOT A REPEATING SEASONAL EVENT:
#     this is the key contrast with the acute-care flu-surge panel. Flu season recurs every single
#     year of that panel; the GLP-1 break here happens ONCE, starting at a specific year and never
#     reverting, which is the actuarial/capitation-rate decision this team actually faces ("is the
#     break real and should next year's rate reflect it" vs. "which model best tracks a recurring
#     cycle").
#
# WHAT WE FOUND (documented here, not fixed in package code):
#
#  (A) report()/mcs() correctly fire the SMALL-T WARNING on this exact T=12/K=4 panel, with the
#      wording this package's own mcs.R already anchors to "actuarial-reserving" T=12/K=4 as its
#      measured worst case (~41% false-elimination under a true null). VERDICT correctly refuses
#      to call the panel "identified" (MCS size 3 of 4, demographic_risk_adj alone excluded), and
#      RESOLUTION correctly reports the observed edge as far short of the MDE at 80% power, so
#      report() prints "cannot determine at this sample size" -- exactly the conservative read a
#      capitation-rate-setting decision at T=12 needs, not a false "resolved."
#
#  (B) decision_breakdown()'s k* periods on this panel are NOT confined to the structural-break
#      window, in contrast to the acute-care test's fully-clustered flu-season finding. k*=3
#      (drug_adoption_curve vs. ml_claims_pharmacy), and the 3 named pivotal years are 2014, 2015,
#      and 2022 -- i.e. ONLY ONE of the three (2022) falls inside the 2021-2024 GLP-1 break window;
#      the other two predate the break entirely. This is a genuine, domain-informative contrast:
#      an actuarial analyst who assumed "any fragility here must be about the drug-class break"
#      (a natural assumption, since the break is this panel's most salient story) would be wrong --
#      most of this panel's k*-fragility traces to ordinary small-T noise in the pre-break years,
#      not to the break itself. Confirmed this is not an artifact of decision_breakdown() ignoring
#      the break: see (C) below, where the break does drive a different, clearly attributable
#      effect elsewhere in the same panel.
#
#  (C) A REALISTIC "NO FREE LUNCH BEFORE THE BREAK" PROPERTY, found via compare(): on a strictly
#      pre-break sub-panel (T=8, 2013-2020, before the GLP-1 adoption curve starts), the
#      drug_adoption_curve model's mean loss is EXACTLY TIED with historical_trend's (5.0717 each,
#      by construction -- drug_adoption_curve literally reduces to the same baseline before the
#      break fires). The pooled winner on that sub-panel is decided by this package's own
#      alphabetical tie-break (pooled_winner(), per its documented rule), not by any genuine
#      forecasting edge. Once the full 12-year panel (including the break) is evaluated, compare()
#      correctly reports champion_changed=FALSE (drug_adoption_curve remains champion both before
#      and after adding the break years) -- but the underlying reason is NOT "this model was always
#      best," it is "this model earns its advantage only once the break it is built to detect
#      actually happens." An actuarial team reading only compare()'s one-line champion-stability
#      verdict, without checking the LEADERBOARD mean-loss gap on the pre-break sub-panel the way
#      this test does, could mistakenly read "no champion change" as "this model was already adding
#      value before we had evidence of the break" -- it was not; it was tied with the dumbest
#      baseline in the panel until the break arrived. Not a bug: compare()'s champion-stability
#      check is answering exactly the question it says it answers (did the pooled winner change),
#      and it is correct that it did not. This is a documentation/interpretation finding, consistent
#      with this package's own stance elsewhere that a single summary field should not be read
#      without its surrounding context.

build_chronic_cost_panel <- function() {
  T <- 12L
  years <- 2013:2024

  # Aging/prevalence-driven baseline chronic-disease PMPM cost: smooth ~4.5%/year compounding
  # growth (CDC-documented multi-decade diabetes/CVD/COPD prevalence drift), NOT a volatile
  # process -- this is the "slow, demographically-driven, persistent/predictable" trend that
  # distinguishes population-health forecasting from acute-care demand forecasting.
  baseline_pmpm <- 420 * (1.045)^(0:(T - 1))

  # GLP-1 structural break: new drug class becomes standard-of-care for diabetes/obesity starting
  # 2021 (year index 9), driving a real, abrupt, one-time (non-reverting, non-seasonal) pharmacy-
  # cost step-up -- modeled as a ramp, not a single-year spike, since real adoption curves ramp.
  break_year_idx <- which(years == 2021)
  glp1_effect <- rep(0, T)
  glp1_effect[break_year_idx:T] <- c(35, 58, 72, 80)
  true_pmpm <- baseline_pmpm + glp1_effect

  # Mild annual idiosyncratic variation -- deliberately modest (sd=6 against a ~400-500 PMPM
  # base), since this is explicitly NOT an acute-care-style volatile process.
  set.seed(2026)
  true_pmpm_obs <- true_pmpm + rnorm(T, 0, 6)

  # Four models, each with a distinct, realistic blind spot (see file header).
  f_trend <- 420 * (1.045)^(0:(T - 1))                 # 1. naive trend, blind to the break
  f_demo <- baseline_pmpm * 1.01                        # 2. demographic risk-adj, also blind
  set.seed(99)
  f_ml <- true_pmpm + rnorm(T, 0, 10)                   # 3. ML on claims/pharmacy: catches the
  f_ml[1:(break_year_idx - 1)] <-                       #    break fast but noisier, esp. pre-break
    f_trend[1:(break_year_idx - 1)] + rnorm(break_year_idx - 1, 0, 10)
  f_adopt <- baseline_pmpm                              # 4. drug-adoption-curve model: reduces
  f_adopt[break_year_idx:T] <-                          #    exactly to baseline pre-break, tracks
    baseline_pmpm[break_year_idx:T] + c(30, 55, 75, 82) #    the ramp closely post-break

  L <- list(
    historical_trend     = abs(true_pmpm_obs - f_trend),
    demographic_risk_adj = abs(true_pmpm_obs - f_demo),
    ml_claims_pharmacy    = abs(true_pmpm_obs - f_ml),
    drug_adoption_curve   = abs(true_pmpm_obs - f_adopt)
  )
  list(L = L, years = years, break_year_idx = break_year_idx, break_years = years[break_year_idx:T])
}

test_that("realistic annual chronic-disease-cost panel builds cleanly at actuarial T=12/K=4 cadence", {
  built <- build_chronic_cost_panel()
  panel <- loss_panel(built$L, labels = built$years)

  expect_equal(length(panel$labels), 12L)
  expect_equal(sort(panel$models), sort(c(
    "historical_trend", "demographic_risk_adj", "ml_claims_pharmacy", "drug_adoption_curve"
  )))

  sm <- suppressWarnings(summary(panel))
  # The two break-aware models (ml_claims_pharmacy, drug_adoption_curve) should clearly
  # out-forecast the two break-blind models (historical_trend, demographic_risk_adj) on mean
  # absolute PMPM error -- the GLP-1-driven cost step-up is the dominant source of variation in
  # this panel once it arrives, so models that track it win decisively.
  break_aware_best <- min(sm$mean_loss[sm$model %in% c("ml_claims_pharmacy", "drug_adoption_curve")])
  break_blind_best <- min(sm$mean_loss[sm$model %in% c("historical_trend", "demographic_risk_adj")])
  expect_lt(break_aware_best, break_blind_best)
})

test_that(
  paste(
    "report() fires the small-T warning at this realistic T=12/K=4 actuarial cadence and",
    "correctly refuses to call the panel identified -- the defensible answer for a",
    "capitation-rate-setting decision at this sample size"
  ),
  {
    built <- build_chronic_cost_panel()
    panel <- loss_panel(built$L, labels = built$years)

    # Confirm the small-T warning fires, and specifically references T=12/K=4 -- this package's
    # own mcs.R already anchors its small-T threshold to a freshly re-measured "actuarial-
    # reserving" T=12/K=4 benchmark (~41% false-elimination under a true null), so this is not an
    # incidental warning: it is this package's own documented worst case for exactly this domain's
    # normal cadence.
    warn_msgs <- character(0)
    out <- withCallingHandlers(
      report(panel),
      warning = function(w) { warn_msgs <<- c(warn_msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    expect_true(any(grepl("T=12 periods for K=4 models", warn_msgs, fixed = TRUE)))
    expect_true(any(grepl("~41%", warn_msgs, fixed = TRUE)))
    # report() de-duplicates this warning across its two internal MCS call sites -- confirm it
    # does NOT print the identical warning twice (a real risk this package's own code comments
    # call out and specifically guard against).
    small_t_msgs <- warn_msgs[grepl("T=12 periods for K=4 models", warn_msgs, fixed = TRUE)]
    expect_equal(length(small_t_msgs), 1L)

    # VERDICT should correctly decline to call this identified at this sample size (demographic_
    # risk_adj is statistically indistinguishable from being in-or-out of the confidence set at
    # T=12), and RESOLUTION should correctly decline to call the edge powered.
    expect_match(out, "not identified")
    expect_match(out, "cannot determine at this sample size")

    rr <- suppressWarnings(resolution_report(panel))
    expect_false(rr$resolved)

    # This refusal should be stable across bootstrap seeds, not a coin flip -- important for an
    # annual capitation-rate exercise that will rerun this same comparison every single year.
    seeds_identified <- vapply(0:9, function(s) {
      suppressWarnings(isTRUE(identified(panel, alpha = 0.10, seed = s)))
    }, logical(1))
    expect_false(any(seeds_identified))
  }
)

test_that(
  paste(
    "decision_breakdown()'s k* pivotal years on this panel are NOT confined to the GLP-1",
    "structural-break window -- a genuine contrast with the acute-care test's fully-clustered",
    "flu-season finding, and a concrete warning against assuming 'any fragility here must be",
    "about the drug-class break' just because the break is this panel's most salient story"
  ),
  {
    built <- build_chronic_cost_panel()
    panel <- loss_panel(built$L, labels = built$years)

    champ <- pooled_winner(panel$losses, panel$weights)
    expect_equal(champ, "drug_adoption_curve")

    db <- decision_breakdown(panel$losses, panel$weights, a = champ)
    expect_equal(db$binding_opponent, "ml_claims_pharmacy")
    expect_gt(db$k, 0)
    expect_lte(db$k, 6)  # a small, human-checkable k* -- fragile in the ordinary sense too

    pivotal_years <- sort(built$years[db$removed])
    # The key finding: pivotal years are a MIX of pre-break and break-window years, not fully
    # inside the break window the way the acute-care test's flu-season pivotal days were fully
    # inside the flu window. At least one pivotal year predates the break entirely.
    pre_break_pivots <- pivotal_years[pivotal_years < min(built$break_years)]
    expect_gt(length(pre_break_pivots), 0)
    # ...and not ALL pivotal years are inside the break window either (i.e. this is genuinely
    # mixed, not just "mostly pre-break" relabeled).
    expect_lt(sum(pivotal_years %in% built$break_years), length(pivotal_years))
  }
)

test_that(
  paste(
    "compare(): the drug-adoption-curve model is EXACTLY TIED with the naive historical-trend",
    "baseline on a strictly pre-break sub-panel (it earns no genuine edge until the break it is",
    "built to detect actually happens), yet compare() correctly reports no champion change when",
    "the break years are added -- illustrating why a champion-stability verdict alone should not",
    "be read as 'this model was already adding value,' per finding (C) in this file's header"
  ),
  {
    built <- build_chronic_cost_panel()
    panel <- loss_panel(built$L, labels = built$years)

    # Strictly pre-break sub-panel: years 2013-2020, entirely before the GLP-1 ramp starts.
    pre_idx <- seq_len(built$break_year_idx - 1)
    expect_true(all(built$years[pre_idx] < min(built$break_years)))
    L_pre <- lapply(built$L, function(x) x[pre_idx])
    panel_pre <- loss_panel(L_pre, labels = built$years[pre_idx])

    sm_pre <- suppressWarnings(summary(panel_pre))
    trend_loss <- sm_pre$mean_loss[sm_pre$model == "historical_trend"]
    adopt_loss <- sm_pre$mean_loss[sm_pre$model == "drug_adoption_curve"]
    # Exactly tied (by construction: drug_adoption_curve reduces to the same baseline formula as
    # historical_trend before the break fires) -- confirms it has NO genuine forecasting edge yet.
    expect_equal(trend_loss, adopt_loss, tolerance = 1e-9)

    # Now compare the pre-break-only re-evaluation against the full post-break panel.
    cmp <- suppressWarnings(compare(panel_pre, panel))
    expect_s3_class(cmp, "change_report")
    expect_equal(cmp$previous_champion, "drug_adoption_curve")
    expect_equal(cmp$current_champion, "drug_adoption_curve")
    expect_false(cmp$champion_changed)
    expect_true(cmp$old_champion_still_in_mcs)
    # All 4 break-window years (2021-2024) are "new" relative to the strictly pre-break sub-panel.
    expect_equal(cmp$n_new_periods, length(built$break_years))
  }
)
