# REALISTIC DOMAIN TEST: corporate legal department / litigation finance settlement-exposure
# forecasting -- a genuinely new domain for this package, distinct from the insurance reserving
# test (aggregate loss development for an insurer's own balance sheet) and the weather-risk test
# (physically-grounded catastrophe losses). This is INDIVIDUAL CASE / PORTFOLIO LITIGATION
# outcome forecasting, where tail events are driven by jury unpredictability and legal-regime
# shocks, not any underlying physical process.
#
# BUSINESS CONTEXT: a corporate legal department (or a litigation finance firm pricing a
# portfolio) compares 4 competing models for forecasting AGGREGATE QUARTERLY SETTLEMENT EXPOSURE
# across a book of pending litigation:
#   - base_rate:        a historical-base-rate model (naive average of past settlement exposure
#                        by case type -- the weakest, most backward-looking of the four)
#   - case_regression:  a case-characteristic regression (venue, claim type, injury severity,
#                        plaintiff counsel track record, etc. -- the traditional actuarial-style
#                        litigation model)
#   - ml_model:         a gradient-boosted/ML model trained on case features (usually sharpest in
#                        "normal" quarters, but -- like the insurance test's ML reserving model --
#                        has no guaranteed graceful degradation on a genuinely novel legal-regime
#                        shock it has no training analog for)
#   - expert_judgment:  a legal-expert-judgment-anchored model (outside/in-house trial counsel's
#                        qualitative read on venue climate and verdict risk, blended with recent
#                        experience -- damps shocks the way BF's a priori ELR damps catastrophe
#                        years in P&C reserving, because experienced trial counsel partially
#                        anticipate regime shifts before they fully show up in the data)
#
# Performance is each model's ABSOLUTE RELATIVE ERROR vs. actual aggregate settlement exposure,
# by FISCAL QUARTER -- the native cadence of a litigation portfolio reserve review. T=40 (10
# years of quarterly history, 2016Q1-2025Q4) is realistic: GC offices and litigation finance
# underwriters routinely work with 8-12 years of portfolio history, not textbook T=30-100+ panels.
#
# REALISTIC HEAVY TAIL, MORE EXTREME THAN THE INSURANCE CASE: settlement/judgment exposure is
# famously right-skewed -- most pending cases settle for modest, fairly predictable amounts, but
# a small number of "bet the company" cases or nuclear jury verdicts produce enormous outcomes
# untethered from any physical loss process (unlike a hurricane or a pandemic). This panel uses a
# lognormal base error with CoV 0.45-0.65 (above the insurance test's 0.35-0.55) PLUS two explicit
# legal-regime-shift quarters:
#   - 2019Q3: a landmark appellate ruling that abruptly shifts the venue climate for an entire
#     class of pending litigation (plaintiff-friendly precedent sets off a wave of nuclear
#     verdicts) -- inflates error 3-11x, asymmetrically: ml_model (no training precedent for this
#     specific ruling) is hit WORST (11x); expert_judgment (trial counsel partially anticipated
#     the ruling once cert was granted) is damped MOST (3x).
#   - 2022Q1: a single "bet the company" nuclear verdict in a plaintiff-friendly venue --
#     inflates error 2-6x, asymmetrically again, but differently: by 2022Q1 ml_model had seen an
#     analogous venue pattern and copes far better (2x) than it did in 2019Q3, while
#     case_regression and base_rate (both backward-looking) are hit hardest (5.5-6x) -- a
#     believable, non-monotone catastrophe signature, not "one outlier on the champion."
#
# BUSINESS STAKES: the general counsel needs a DEFENSIBLE answer about which model to trust for
# next quarter's litigation reserve -- exactly the setting where "our model wins over 10 years"
# must be distinguished from "our model wins because of one or two landmark-verdict quarters."

test_that("realistic 10-year/40-quarter litigation-portfolio panel: report() flags the ML champion's edge as undetermined, not an artifact of a single nuclear-verdict quarter alone", {
  set.seed(8)
  T <- 40
  qtrs <- paste0(rep(2016:2025, each = 4), "Q", rep(1:4, 10))

  base_mean <- c(base_rate = 0.11, case_regression = 0.075, ml_model = 0.07, expert_judgment = 0.095)
  base_cv   <- c(base_rate = 0.5,  case_regression = 0.6,  ml_model = 0.65, expert_judgment = 0.45)

  losses <- as.data.frame(sapply(names(base_mean), function(m) {
    sigma2 <- log(1 + base_cv[m]^2)
    mu <- log(base_mean[m]) - sigma2 / 2
    stats::rlnorm(T, mu, sqrt(sigma2))
  }))
  rownames(losses) <- qtrs

  # Legal-regime-shift quarters (see header): asymmetric across models, as a real regime shift
  # would be -- not a single outlier dropped on the champion.
  losses["2019Q3", "base_rate"]       <- losses["2019Q3", "base_rate"] * 9
  losses["2019Q3", "case_regression"] <- losses["2019Q3", "case_regression"] * 7
  losses["2019Q3", "ml_model"]        <- losses["2019Q3", "ml_model"] * 11  # worst: no precedent
  losses["2019Q3", "expert_judgment"] <- losses["2019Q3", "expert_judgment"] * 3  # best-damped

  losses["2022Q1", "base_rate"]       <- losses["2022Q1", "base_rate"] * 6
  losses["2022Q1", "case_regression"] <- losses["2022Q1", "case_regression"] * 5.5
  losses["2022Q1", "ml_model"]        <- losses["2022Q1", "ml_model"] * 2    # copes better 2nd time
  losses["2022Q1", "expert_judgment"] <- losses["2022Q1", "expert_judgment"] * 4

  panel <- loss_panel(losses, labels = qtrs)

  # --- report() workflow ---------------------------------------------------------------
  rpt <- report(panel, alpha = 0.10, full = TRUE)

  # The ML model is the apparent champion (lowest mean absolute relative settlement-exposure
  # error over 10 years of quarterly data).
  expect_match(rpt, "ml_model +mean=0\\.0[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")
  # MCS does NOT single out ml_model as uniquely best -- case_regression also survives: a GC
  # reading only the leaderboard's top row would wrongly conclude the ML model is clearly better.
  expect_match(rpt, "VERDICT: not identified \\(MCS size 2 of 4\\)")
  # The most fragile pivot set names BOTH legal-regime-shift quarters, PLUS one ordinary quarter
  # (2025Q3) that is not a named regime shift at all -- k* answers "how many periods, adversarially
  # chosen, flip the pooled decision," not "how many landmark verdicts are there," and this panel
  # is a clean, realistic illustration of why those two numbers legitimately differ.
  expect_match(rpt, "k\\*=3 of 40 periods \\(2019Q3, 2022Q1, 2025Q3\\)")
  # RESOLUTION must give the GC the honest, non-hedged answer: the observed edge is not powered at
  # this realistic T=40 litigation-portfolio sample size -- "ml_model wins" is not something a GC
  # can certify from 10 years of quarterly history, extreme tail or not.
  expect_match(rpt, "cannot determine at this sample size")

  # --- decision_breakdown(): are the landmark-verdict quarters alone enough to flip the decision? ----
  L <- panel$losses; w <- panel$weights
  champ <- pooled_winner(L, w)
  expect_identical(champ, "ml_model")

  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$binding_opponent, "case_regression")
  expect_equal(db$k, 3L)
  removed_labels <- panel$labels[sort(db$removed)]
  expect_identical(removed_labels, c("2019Q3", "2022Q1", "2025Q3"))
  # Both named regime-shift quarters are among the removed set, but they are NOT the only ones --
  # confirming k* is not simply "the count of named catastrophes" even on a panel built around
  # exactly two of them.
  expect_true(all(c("2019Q3", "2022Q1") %in% removed_labels))

  # --- Prop 2.2's zero-simulation certificate: does it agree with the bootstrap MCS? -------------
  bd <- breakdown_number(L[["ml_model"]], L[["case_regression"]], w)
  expect_equal(bd$k, 3L)
  expect_true(prop22_certifies(bd$k, T = T))  # agrees: not statistically defensible from k*=3 of 40

  # --- resolution_report()/minimum_detectable_edge(): the power check the GC's outside counsel would want ---
  rr <- resolution_report(panel, alpha = 0.05, power = 0.80)
  expect_identical(rr$champion, "ml_model")
  expect_false(rr$resolved)
  expect_identical(rr$fragility_read, "undetermined")
  mde <- minimum_detectable_edge(panel, power = 0.80, alpha = 0.05)
  # The observed edge is well below (roughly half of) what T=40 quarters could reliably detect --
  # a GC citing "the ML model is better" on this basis alone is over-claiming.
  expect_lt(rr$observed_edge / mde, 0.6)

  # --- Leave-one-landmark-quarter-out: do EITHER regime-shift quarters alone flip the champion? ---
  # This is the concrete, GC-facing question: "is a single landmark verdict driving the whole
  # conclusion?" FINDING: neither 2019Q3 nor 2022Q1 alone -- nor both together -- flips the pooled
  # champion away from ml_model; it takes k*'s full adversarial removal set (both landmark quarters
  # PLUS one ordinary quarter) to actually flip the sign. The landmark-verdict quarters are
  # NECESSARY contributors to the margin (see net-margin-share check below) but not, by
  # themselves, SUFFICIENT to overturn the decision -- a more nuanced and more defensible finding
  # for a GC memo than "drop the bad quarter and the answer changes."
  mean_excl <- function(exclude) {
    vapply(L, function(col) {
      v <- col; names(v) <- qtrs
      mean(v[!(names(v) %in% exclude)])
    }, numeric(1))
  }
  expect_equal(names(which.min(mean_excl("2019Q3"))), "ml_model")
  expect_equal(names(which.min(mean_excl("2022Q1"))), "ml_model")
  expect_equal(names(which.min(mean_excl(c("2019Q3", "2022Q1")))), "ml_model")
})

test_that("concentration_share() under-states the landmark-verdict quarters' dominance of the NET margin (even more severe than the insurance/weather cases), while k*/decision_breakdown() correctly shows they are the dominant net driver", {
  # This is the already-known "concentration_share() under-states multi-event concentration"
  # limitation (see test-realworld-weatherrisk.R), but surfaced here in an EVEN MORE EXTREME form
  # appropriate to this domain's heavier tail: concentration_share() is documented to measure only
  # the SINGLE most influential period's share of the champion-vs-opponent margin. In this panel
  # the two legal-regime-shift quarters jointly account for the overwhelming majority of the NET
  # margin (the actual sign-determining force behind "ml_model beats case_regression") -- but
  # because that net margin is split across TWO comparably-sized landmark quarters (not one), and
  # because this panel's heavier tail produces much larger offsetting ordinary-quarter noise in
  # both directions, concentration_share()'s single-period, gross-dispersion-denominator design
  # reports a markedly smaller number. A GC reading ONLY concentration_share() here would
  # significantly underestimate how landmark-verdict-driven the decision really is.
  set.seed(8)
  T <- 40
  qtrs <- paste0(rep(2016:2025, each = 4), "Q", rep(1:4, 10))
  base_mean <- c(base_rate = 0.11, case_regression = 0.075, ml_model = 0.07, expert_judgment = 0.095)
  base_cv   <- c(base_rate = 0.5,  case_regression = 0.6,  ml_model = 0.65, expert_judgment = 0.45)
  losses <- as.data.frame(sapply(names(base_mean), function(m) {
    sigma2 <- log(1 + base_cv[m]^2)
    mu <- log(base_mean[m]) - sigma2 / 2
    stats::rlnorm(T, mu, sqrt(sigma2))
  }))
  rownames(losses) <- qtrs
  losses["2019Q3", "base_rate"]       <- losses["2019Q3", "base_rate"] * 9
  losses["2019Q3", "case_regression"] <- losses["2019Q3", "case_regression"] * 7
  losses["2019Q3", "ml_model"]        <- losses["2019Q3", "ml_model"] * 11
  losses["2019Q3", "expert_judgment"] <- losses["2019Q3", "expert_judgment"] * 3
  losses["2022Q1", "base_rate"]       <- losses["2022Q1", "base_rate"] * 6
  losses["2022Q1", "case_regression"] <- losses["2022Q1", "case_regression"] * 5.5
  losses["2022Q1", "ml_model"]        <- losses["2022Q1", "ml_model"] * 2
  losses["2022Q1", "expert_judgment"] <- losses["2022Q1", "expert_judgment"] * 4

  panel <- loss_panel(losses, labels = qtrs)
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ml_model")
  opp <- "case_regression"
  contrib <- panel$losses[[opp]] - panel$losses[[champ]]
  cat_idx <- which(panel$labels %in% c("2019Q3", "2022Q1"))

  # The two landmark-verdict quarters together account for the large majority (~85%) of the NET
  # margin by which ml_model beats case_regression -- confirmed directly, not just asserted.
  net_share <- sum(contrib[cat_idx]) / sum(contrib)
  expect_gt(net_share, 0.75)

  # ... yet concentration_share() -- scoped to the single largest period over gross |margin|
  # dispersion -- reports well under half that figure. This is the "under-states multi-event
  # concentration" limitation, reproduced in this new, more extreme domain: the gap between the
  # net-margin share (~85%) and concentration_share()'s own number is even wider here than in the
  # already-tested insurance/weather cases, consistent with this domain's heavier tail producing
  # larger offsetting ordinary-quarter noise.
  cs <- concentration_share(panel)
  expect_lt(cs, 0.30)
  expect_lt(cs, net_share / 2)

  # decision_breakdown()'s own margin accounting (via k*) is NOT fooled the same way: its removed
  # set names both landmark quarters among its 3 adversarially-chosen periods (test above), so a
  # GC who runs decision_breakdown() rather than stopping at concentration_share() alone gets the
  # correct, non-understated picture.
  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_true(all(c(which(panel$labels == "2019Q3"), which(panel$labels == "2022Q1")) %in% db$removed))
})

test_that("pivot_agreement() mostly tracks its `frac` knob on this realistic litigation panel too, reconfirming its documented limitation rather than certifying 2019Q3 as a 'real' feature of the data", {
  # DOCUMENTED LIMITATION (see ?pivot_agreement's own @section Limitation, and the weather-risk
  # test's reconfirmation): pivot_agreement() subsamples periods WITHOUT drawing new noise, so it
  # mostly tracks the `frac` knob rather than measuring the named pivotal period's true
  # robustness. Re-confirmed here on a litigation-forecasting panel with an even heavier tail: a
  # GC should NOT read a single pivot_agreement() number in isolation as "the landmark verdict is
  # a confirmed real driver" -- it must be read alongside decision_breakdown()/k* and RESOLUTION.
  set.seed(8)
  T <- 40
  qtrs <- paste0(rep(2016:2025, each = 4), "Q", rep(1:4, 10))
  base_mean <- c(base_rate = 0.11, case_regression = 0.075, ml_model = 0.07, expert_judgment = 0.095)
  base_cv   <- c(base_rate = 0.5,  case_regression = 0.6,  ml_model = 0.65, expert_judgment = 0.45)
  losses <- as.data.frame(sapply(names(base_mean), function(m) {
    sigma2 <- log(1 + base_cv[m]^2)
    mu <- log(base_mean[m]) - sigma2 / 2
    stats::rlnorm(T, mu, sqrt(sigma2))
  }))
  rownames(losses) <- qtrs
  losses["2019Q3", "base_rate"]       <- losses["2019Q3", "base_rate"] * 9
  losses["2019Q3", "case_regression"] <- losses["2019Q3", "case_regression"] * 7
  losses["2019Q3", "ml_model"]        <- losses["2019Q3", "ml_model"] * 11
  losses["2019Q3", "expert_judgment"] <- losses["2019Q3", "expert_judgment"] * 3
  losses["2022Q1", "base_rate"]       <- losses["2022Q1", "base_rate"] * 6
  losses["2022Q1", "case_regression"] <- losses["2022Q1", "case_regression"] * 5.5
  losses["2022Q1", "ml_model"]        <- losses["2022Q1", "ml_model"] * 2
  losses["2022Q1", "expert_judgment"] <- losses["2022Q1", "expert_judgment"] * 4
  panel <- loss_panel(losses, labels = qtrs)

  pa_50 <- pivot_agreement(panel, seed = 1, frac = 0.50)
  pa_80 <- pivot_agreement(panel, seed = 1, frac = 0.80)
  pa_95 <- pivot_agreement(panel, seed = 1, frac = 0.95)

  # All three move together with `frac` (monotonically non-decreasing), tracking the subsampling
  # knob rather than sitting at a frac-independent "true concentration" level.
  expect_true(pa_50 <= pa_80)
  expect_true(pa_80 <= pa_95)
  # None of them is close to 100%, even at frac=0.95 -- consistent with k* showing the pivotal
  # quarter's sign-flipping power depends on OTHER periods also being present (the 2025Q3 finding
  # above), not on 2019Q3 alone.
  expect_lt(pa_95, 1.0)
})
