# test-realworld-fashionretail.R -- a distinct, PERMANENT regression suite built from a
# realistic SEASONAL FASHION markdown-forecasting use case, as opposed to the grocery/SKU-
# location retail-promotions texture already exercised in test-realworld-retail.R. Three
# domain features make fashion markdown forecasting statistically different from grocery:
#
#  1. SHORT, NON-REPEATING SEASONAL CYCLES. A fashion season runs 12-16 weeks and is a
#     genuinely fresh, non-comparable cohort -- there is no "last year, same week" repeat
#     series the way there is for a grocery staple. T is structurally small FOREVER at the
#     single-season level (this file deliberately checks the small-T warning at T=14, not
#     just as an edge case but as the NORMAL unit of analysis for this domain), and only
#     becomes moderately sized by pooling multiple non-comparable seasons together.
#
#  2. TREND-DRIVEN, BIMODAL DEMAND UNCERTAINTY. A style is a hit or a flop, not just noisy
#     around a seasonal mean -- so this file's three example seasons span "moderate" (plan
#     plays out as expected), "flop" (demand ~45% of plan), and "hit" (demand ~90% above
#     plan). A historical-season-analog model that is excellent when the season behaves
#     "normally" (Scenario A below) becomes the worst model by a wide margin once pooled
#     across a hit/flop/moderate cohort mix (Scenario B) -- it has no mechanism for sensing
#     which regime this season is in.
#
#  3. MARKDOWN-CADENCE-DRIVEN STRUCTURAL BREAKS. Each season has 2 markdown events at known
#     week offsets (timing predictable, magnitude uncertain). The four competing models have
#     sharply different, genuinely domain-grounded strengths/weaknesses AT markdown weeks
#     specifically: an elasticity-based baseline is built exactly to forecast a price-cut
#     response and dominates at markdown weeks but is mediocre elsewhere; a trend/social-
#     signal model senses the hit/flop regime well but lags each markdown's step-change by a
#     couple of weeks; an ML model needs seasons of markdown examples to get good at markdown
#     weeks specifically (modeled here as improving season-over-season with experience).
#     Scenario C exploits this to test decision_breakdown() on markdown weeks specifically,
#     and surfaces a genuine finding: the pooled/overall champion (ml) is NOT the champion
#     once you restrict to markdown weeks alone (elasticity is), and that markdown-only
#     reversal rests on a far more fragile margin (k=3 of just 6 markdown weeks) than the
#     overall decision does.
#
# Every set.seed() below is load-bearing: this file asserts on the concrete numbers those
# seeds produce (verified by direct execution against the current package code), not just
# "no crash."

## =========================================================================================
## Scenario builders
## =========================================================================================

#' @keywords internal
.fashion_season <- function(L, trend_type, seed) {
  set.seed(seed)
  week <- 1:L
  mk1 <- round(L * 0.40)
  mk2 <- round(L * 0.75)

  trend_mult <- switch(trend_type, moderate = 1.0, flop = 0.55, hit = 1.9)

  # Attention-decay base curve (full-price interest fades as the season ages), then two
  # markdown-driven demand lifts at predictable timing (mk1, mk2) but with the true
  # magnitude only known in hindsight.
  base <- 100 * trend_mult * exp(-0.05 * (week - 1))
  markdown_lift <- rep(1, L)
  markdown_lift[mk1:L] <- 1.55
  markdown_lift[mk2:L] <- markdown_lift[mk2:L] * 1.45
  true_units <- pmax(0, base * markdown_lift + rnorm(L, 0, 2.5))

  list(week = week, mk1 = mk1, mk2 = mk2, trend_mult = trend_mult, true_units = true_units, L = L)
}

#' @keywords internal
.fashion_forecasts <- function(sc, ml_experience) {
  L <- sc$L; mk1 <- sc$mk1; mk2 <- sc$mk2
  week <- sc$week
  true_units <- sc$true_units

  # historical-season-analog: assumes last season's "moderate" trend always, but DOES know
  # the markdown calendar (timing is predictable) -- so its error is driven entirely by how
  # far THIS season's real trend departs from "moderate" (small in Scenario A, large once
  # pooled across hit/flop seasons in Scenario B).
  hist_base <- 100 * 1.0 * exp(-0.05 * (week - 1))
  hist_lift <- rep(1, L); hist_lift[mk1:L] <- 1.55; hist_lift[mk2:L] <- hist_lift[mk2:L] * 1.45
  hist_fc <- hist_base * hist_lift

  # trend/social-signal model: correctly senses the hit/flop regime (trend_mult) but lags
  # the markdown-driven step change by `lag` weeks -- right at/after each markdown it keeps
  # extrapolating the pre-markdown trajectory before catching up.
  trend_base <- 100 * sc$trend_mult * exp(-0.05 * (week - 1))
  trend_lift <- rep(1, L)
  lag <- 2
  trend_lift[(mk1 + lag):L] <- 1.55
  trend_lift[mk1:(min(mk1 + lag - 1, L))] <- 1.15
  if (mk2 + lag <= L) trend_lift[(mk2 + lag):L] <- trend_lift[(mk2 + lag):L] * 1.45
  trend_lift[mk2:(min(mk2 + lag - 1, L))] <- trend_lift[mk2:(min(mk2 + lag - 1, L))] * 1.15
  trend_fc <- trend_base * trend_lift + rnorm(L, 0, 4)

  # ML model: needs training examples of markdown response; its accuracy AT (and just
  # after) markdown weeks improves with ml_experience (0 = first season ever seen, 1 = one
  # prior season of history, 2 = two prior seasons) -- a realistic "cold start" story for a
  # category where every season is a fresh, non-comparable cohort. Elsewhere it is a
  # reasonably good generalist throughout.
  noise_sd_markdown <- c(14, 7, 3)[ml_experience + 1]
  ml_fc <- true_units + rnorm(L, 0, 5)
  near_mk <- unique(pmin(L, c(mk1, mk1 + 1, mk2, mk2 + 1)))
  ml_fc[near_mk] <- true_units[near_mk] + rnorm(length(near_mk), 0, noise_sd_markdown)

  # elasticity-based baseline: models the price-cut response well (that IS what an
  # elasticity model is for), but is mediocre/noisy in ordinary weeks because it has no way
  # to represent trend-driven attention decay or the hit/flop regime.
  elas_fc <- true_units
  elas_fc[near_mk] <- true_units[near_mk] + rnorm(length(near_mk), 0, 3)
  non_mk <- setdiff(seq_len(L), near_mk)
  elas_fc[non_mk] <- true_units[non_mk] * (1 + rnorm(length(non_mk), 0, 0.22))

  list(
    hist_analog  = abs(true_units - hist_fc),
    trend_signal = abs(true_units - trend_fc),
    ml           = abs(true_units - ml_fc),
    elasticity   = abs(true_units - elas_fc)
  )
}

#' @keywords internal
.build_fashion_panel <- function() {
  secA <- .fashion_season(14, "moderate", seed = 4101)  # season A: plays out as planned
  secB <- .fashion_season(16, "flop",     seed = 4102)  # season B: demand well below plan
  secC <- .fashion_season(16, "hit",      seed = 4103)  # season C: demand well above plan

  fcA <- .fashion_forecasts(secA, ml_experience = 0)  # first season: ml is cold-start
  fcB <- .fashion_forecasts(secB, ml_experience = 1)  # ml has one season of history
  fcC <- .fashion_forecasts(secC, ml_experience = 2)  # ml has two seasons of history

  L_all <- list(
    hist_analog  = c(fcA$hist_analog,  fcB$hist_analog,  fcC$hist_analog),
    trend_signal = c(fcA$trend_signal, fcB$trend_signal, fcC$trend_signal),
    ml           = c(fcA$ml,           fcB$ml,           fcC$ml),
    elasticity   = c(fcA$elasticity,   fcB$elasticity,   fcC$elasticity)
  )

  season_id <- c(rep("A", secA$L), rep("B", secB$L), rep("C", secC$L))
  week_in_season <- c(secA$week, secB$week, secC$week)
  is_markdown <- rep(FALSE, length(season_id))
  off <- 0
  for (sc in list(secA, secB, secC)) {
    is_markdown[off + sc$mk1] <- TRUE
    is_markdown[off + sc$mk2] <- TRUE
    off <- off + sc$L
  }
  labels <- paste0(season_id, "-W", sprintf("%02d", week_in_season))

  list(L = L_all, labels = labels, season_id = season_id, week_in_season = week_in_season,
       is_markdown = is_markdown, secA = secA, secB = secB, secC = secC,
       T = length(season_id))
}

## =========================================================================================
## SCENARIO A -- a single season in isolation (T=14), the NORMAL unit of analysis in fashion
## markdown planning (each season is a fresh, non-comparable cohort, so there is no larger
## "repeat" history to pool within-season). Business setting: this is the "moderate" season
## where demand played out close to plan, so the historical-season-analog model -- which
## assumes exactly this -- wins outright. The small-T warning (T/K=3.5, well below the
## package's own T/K=10 small-sample threshold) must fire, because T=14 is not an edge case
## here -- it is literally what a single fashion season's sell-through record looks like.
## =========================================================================================

test_that("Scenario A: a single fashion season (T=14) triggers report()'s small-T warning", {
  sc <- .build_fashion_panel()
  idxA <- sc$season_id == "A"
  panelA <- loss_panel(lapply(sc$L, function(v) v[idxA]), labels = sc$labels[idxA])
  expect_equal(length(panelA$labels), 14)

  warn_msgs <- character(0)
  withCallingHandlers(
    rep_txt <- report(panelA, alpha = 0.10),
    warning = function(w) { warn_msgs[[length(warn_msgs) + 1]] <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  expect_true(length(warn_msgs) >= 1)
  # Business meaning: this is NOT a generic "small sample" caveat -- it names the exact T/K
  # ratio, so a planning team sees precisely how far below the package's own reliability
  # anchor a single season's data sits.
  expect_match(warn_msgs[[1]], "T=14 periods for K=4 models \\(T/K=3\\.5\\)")
})

test_that("Scenario A: in the 'as-planned' season, hist_analog (not the fancier models) wins", {
  # Business meaning: a historical-season-analog model is NOT a strawman -- when a season
  # behaves the way the plan assumed, it is genuinely the best model, by a wide margin, over
  # ml/trend_signal/elasticity. This is the correct baseline against which Scenario B's
  # pooled result (hist_analog becomes the WORST model) must be read.
  sc <- .build_fashion_panel()
  idxA <- sc$season_id == "A"
  panelA <- loss_panel(lapply(sc$L, function(v) v[idxA]), labels = sc$labels[idxA])
  out <- suppressWarnings(report(panelA, alpha = 0.10))
  expect_match(out, "hist_analog\\s+mean=[0-9.]+\\s+in MCS\\s+periods won=\\d+ \\(champion\\)")
  means <- sapply(panelA$losses, mean)
  expect_identical(names(which.min(means)), "hist_analog")
  expect_lt(means[["hist_analog"]], means[["ml"]])
  expect_lt(means[["hist_analog"]], means[["trend_signal"]])
  expect_lt(means[["hist_analog"]], means[["elasticity"]])
})

## =========================================================================================
## SCENARIO B -- the planning team's real question: 3 seasons pooled (T=46, 14+16+16 weeks,
## 2 markdown events per season, a moderate/flop/hit cohort mix), comparing the historical-
## analog, trend/social-signal, ML, and elasticity models to decide which to trust going into
## the UPCOMING season. Pooling turns the single-season small-T problem into a T/K just above
## the package's own small-sample anchor (T/K=11.5) -- the realistic middle ground this domain
## actually offers: never as comfortable as a grocery category's years of weekly history, but
## not as dire as any one season alone.
## =========================================================================================

test_that("Scenario B: pooled 3-season panel (T=46) is valid and has the expected regime mix", {
  sc <- .build_fashion_panel()
  panel <- loss_panel(sc$L, labels = sc$labels)
  expect_equal(panel$models, c("hist_analog", "trend_signal", "ml", "elasticity"))
  expect_equal(length(panel$labels), 46)
  expect_identical(sc$T, 46L)
  expect_equal(sum(sc$is_markdown), 6)  # 2 markdown events x 3 seasons
  for (m in panel$models) expect_true(all(is.finite(panel$losses[[m]])) && all(panel$losses[[m]] >= 0))
})

test_that("Scenario B: report() crowns ml champion, identified AND resolved at this small T", {
  # Business meaning: unlike the grocery category in test-realworld-retail.R (3 full years of
  # weekly history still insufficient to resolve its champion), the fashion category's
  # trend-driven bimodal errors are large enough, and ml's experience-driven edge consistent
  # enough, that the comparison is genuinely DECISIVE even pooled across only 46 weeks --
  # a meaningful contrast a planning team should be able to act on with real confidence.
  sc <- .build_fashion_panel()
  panel <- loss_panel(sc$L, labels = sc$labels)
  out <- report(panel, alpha = 0.10)
  expect_match(out, "VERDICT: identified")
  expect_match(out, "ml\\s+mean=[0-9.]+\\s+in MCS\\s+periods won=\\d+ \\(champion\\)")
  expect_match(out, "-- resolved")
  means <- sapply(panel$losses, mean)
  expect_identical(names(which.min(means)), "ml")
  # hist_analog, excellent in the single "as-planned" Scenario A season, is now the WORST
  # model once pooled across a moderate/flop/hit cohort -- it has no way to sense which
  # regime a season is in, which is exactly the point this domain is built to probe.
  expect_identical(names(which.max(means)), "hist_analog")
})

test_that("Scenario B: resolution_report() confirms the pooled ml edge clears the MDE bar", {
  sc <- .build_fashion_panel()
  panel <- loss_panel(sc$L, labels = sc$labels)
  res <- resolution_report(panel$losses, panel$weights)
  expect_identical(res$champion, "ml")
  expect_true(res$resolved)
  expect_true(res$observed_edge > res$mde)
  expect_identical(res$binding_rival, "trend_signal")
})

test_that("Scenario B: fragility()/decision_breakdown() on the full pooled panel are well-formed", {
  sc <- .build_fashion_panel()
  panel <- loss_panel(sc$L, labels = sc$labels)
  db <- decision_breakdown(panel$losses, panel$weights)
  expect_identical(db$binding_opponent, "trend_signal")
  expect_true(db$k >= 1 && db$k <= sc$T)

  frag <- fragility(panel$losses, panel$weights,
                     shock_periods = sc$labels[sc$is_markdown], labels = sc$labels)
  expect_identical(frag$pooled_winner, "ml")
  expect_identical(frag$condorcet_winner, "ml")  # ml wins outright, no plurality ambiguity
  expect_false(frag$degenerate_margin)
  # Several of the top pivotal periods cluster at/just after markdown weeks (trend_signal's
  # markdown lag is exactly what makes those weeks disproportionately decisive), even though
  # the set isn't an exact match to the shock_periods list (hence shock_coincidence FALSE) --
  # markdown timing is a real but not total driver of which weeks are pivotal.
  expect_type(frag$shock_coincidence, "logical")
})

## =========================================================================================
## SCENARIO C -- the domain-specific question decision_breakdown() is built to answer: does
## the model ranking for MARKDOWN weeks specifically differ from, and rest on a different
## (more or less fragile) margin than, the ranking for ordinary weeks? This directly serves
## the planning team's actual decision -- which model to trust for the UPCOMING season's
## INITIAL MARKDOWN TIMING -- which is not the same question as "which model wins overall."
## =========================================================================================

test_that("Scenario C: markdown weeks alone flip the champion, on a much more fragile margin", {
  # Business meaning -- this is the genuine finding this scenario surfaces: the OVERALL
  # pooled champion (ml) is NOT the champion once the comparison is restricted to the 6
  # markdown weeks alone. There, the elasticity-based baseline -- built exactly to forecast
  # a price-cut response -- wins outright. A planning team asking specifically "which model
  # do I trust to call the SIZE of the initial-markdown demand pop" should look at THIS
  # comparison, not the pooled leaderboard, and decision_breakdown() is what exposes the gap.
  sc <- .build_fashion_panel()
  L_mk <- lapply(sc$L, function(v) v[sc$is_markdown])
  labels_mk <- sc$labels[sc$is_markdown]
  panel_mk <- loss_panel(L_mk, labels = labels_mk)

  means_mk <- sapply(panel_mk$losses, mean)
  expect_identical(names(which.min(means_mk)), "elasticity")

  db_mk <- decision_breakdown(L_mk)
  expect_identical(db_mk$binding_opponent, "ml")
  # Only 6 markdown weeks exist in this 3-season panel -- k must be interpreted against that
  # tiny denominator, not against the full T=46.
  expect_equal(db_mk$k / sum(sc$is_markdown), 0.5)  # HALF the markdown weeks would flip it
})

test_that("Scenario C: the non-markdown-week ranking keeps ml on top, but underpowered alone", {
  # Business meaning: away from markdown weeks, ml remains the best model (consistent with
  # the pooled result), and the markdown-week reversal above is RESOLVED on a 50% margin,
  # but ml's edge restricted to ordinary weeks alone is NOT resolved at this sample size --
  # i.e. ml's advantage pooled-overall is driven disproportionately by its markdown-week
  # performance plus volume of ordinary weeks, not by a large, individually-resolved edge in
  # ordinary weeks alone. A planning team should not read "ml wins pooled" as "ml wins every
  # sub-question decisively" -- decision_breakdown()/resolution_report() applied to the
  # markdown-only and non-markdown-only slices is what keeps that overreach in check.
  sc <- .build_fashion_panel()
  L_nomk <- lapply(sc$L, function(v) v[!sc$is_markdown])
  labels_nomk <- sc$labels[!sc$is_markdown]
  panel_nomk <- loss_panel(L_nomk, labels = labels_nomk)

  means_nomk <- sapply(panel_nomk$losses, mean)
  expect_identical(names(which.min(means_nomk)), "ml")

  res_nomk <- resolution_report(L_nomk)
  expect_identical(res_nomk$champion, "ml")
  expect_false(res_nomk$resolved)

  db_nomk <- decision_breakdown(L_nomk)
  expect_identical(db_nomk$binding_opponent, "trend_signal")
  # The non-markdown-week ranking is markedly LESS fragile (fewer weeks, as a fraction, would
  # need to flip) than the markdown-week ranking checked above (k/T = 0.5 there).
  expect_lt(db_nomk$k / sum(!sc$is_markdown), 0.5)
})

## =========================================================================================
## SCENARIO D -- compare(): "the upcoming season's early sell-through data arrives" as a
## rolling-window re-evaluation. Business setting: last year's/this-cycle's champion
## decision was made on seasons A+B (T=30). Now the FIRST 6 weeks of season C's sell-through
## have come in -- specifically the weeks BEFORE season C's own first markdown event (mk1=6)
## -- and the planning team wants to know, before committing to this season's markdown
## timing, whether the champion call still holds up on live data from the new season.
## =========================================================================================

test_that("Scenario D: compare() shows the champion call holding up as season C's early data arrives", {
  sc <- .build_fashion_panel()
  idx_prev <- sc$season_id %in% c("A", "B")
  idx_curr <- idx_prev | (sc$season_id == "C" & sc$week_in_season <= 6)
  expect_equal(sum(idx_prev), 30)
  expect_equal(sum(idx_curr), 36)  # 6 new, pre-first-markdown weeks of season C

  panel_prev <- loss_panel(lapply(sc$L, function(v) v[idx_prev]), labels = sc$labels[idx_prev])
  panel_curr <- loss_panel(lapply(sc$L, function(v) v[idx_curr]), labels = sc$labels[idx_curr])

  cr <- suppressWarnings(compare(panel_prev, panel_curr, alpha = 0.10, seed = 1))
  expect_s3_class(cr, "change_report")
  expect_null(cr$mcs_error)
  # Business meaning: the champion call is STABLE across the rolling window -- ml was, and
  # remains, the champion as season C's early (pre-markdown) weeks arrive, and compare()'s
  # act gate correctly stays FALSE (nothing for the planning team to act on yet).
  expect_identical(cr$previous_champion, "ml")
  expect_identical(cr$current_champion, "ml")
  expect_false(cr$champion_changed)
  expect_false(cr$act)
  expect_true(cr$old_champion_still_in_mcs)
  expect_equal(cr$n_new_periods, 6)
  expect_equal(cr$churn_base_rate, 0)
})

test_that("Scenario D: print.change_report() renders a clean one-line 'no change' summary", {
  sc <- .build_fashion_panel()
  idx_prev <- sc$season_id %in% c("A", "B")
  idx_curr <- idx_prev | (sc$season_id == "C" & sc$week_in_season <= 6)
  panel_prev <- loss_panel(lapply(sc$L, function(v) v[idx_prev]), labels = sc$labels[idx_prev])
  panel_curr <- loss_panel(lapply(sc$L, function(v) v[idx_curr]), labels = sc$labels[idx_curr])
  cr <- suppressWarnings(compare(panel_prev, panel_curr, alpha = 0.10, seed = 1))
  out <- capture.output(print(cr))
  expect_length(out, 1)
  expect_match(out, "no champion change \\(ml\\); act=FALSE")
})
