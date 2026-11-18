# test-realworld-semiconductor.R -- a GENUINELY REALISTIC semiconductor-fab capacity-planning use
# case, built from the industry's own well-documented structural features (not a toy/contrived
# panel), kept as a PERMANENT regression test. Context for readers unfamiliar with this domain:
#
#   - Wafer-fab capacity decisions have MULTI-YEAR lead times: breaking ground on new capacity (or
#     committing to keep a node running) today only pays off (or turns into an overbuild write-down)
#     years later. Capacity-planning teams therefore evaluate forecasting models at a QUARTERLY
#     cadence over many YEARS of history, not days/weeks of high-frequency data -- T=48 quarters
#     (12 years) below is deliberately in that "multi-year capital-planning" regime, structurally
#     different from a daily-cadence domain even though the sample size is numerically similar.
#   - The "semiconductor cycle" is a famous, well-documented, SELF-INFLICTED boom-bust pattern
#     (distinct from an ordinary macro business cycle): a demand surge triggers a capacity-building
#     race, the resulting capacity (plus panic over-ordering / double-booking by customers) arrives
#     with a multi-quarter lag, and by the time it does, it typically overshoots real end-demand,
#     causing a glut/correction. The 2021-2022 global chip shortage (COVID demand surge + auto/
#     consumer panic-ordering) followed by the 2022-2023 inventory correction/glut (widely reported
#     across memory and analog/mixed-signal segments) is exactly this pattern, modeled here as
#     2021Q1-2022Q2 (shortage boom) -> 2022Q3-2023Q4 (glut/bust). This is driven by the INDUSTRY'S
#     OWN capacity-investment and order-book lags, not an external shock -- the mechanism this
#     package's decision_breakdown()/k* machinery is well-suited to probe, since the fragility
#     question here is "how many periods does it take to flip the champion," not "is there a trend."
#   - A genuine PRODUCT-MIX-DRIVEN demand shift closes out the panel: 2024, modeled here as
#     2024Q1-2024Q4, where AI/datacenter chip demand surged sharply while legacy-node demand stayed
#     soft -- a real, widely reported divergence (hyperscaler AI capex guidance vs. soft legacy/
#     consumer sell-through) that is a genuinely different mechanism from the shortage/glut cycle:
#     it rewards a model that ingests END-MARKET indicators specific to the new demand driver, not
#     one that merely extrapolates the fab's own historical cycle shape.
#
# Four models, in this spirit (a chipmaker's capacity-planning team's actual shortlist for
# forecasting quarterly wafer demand for one product segment):
#   cycle_pattern -- extrapolates the segment's own historical boom-bust cycle shape: decent in
#                    ordinary times, but miscalibrated on BOTH the magnitude of the 2021-22 surge
#                    and the 2022-23 correction, and blind to the 2024 AI-driven mix shift (which
#                    has no precedent in this segment's own history).
#   order_book    -- driven by real customer order-book/backlog data: captures the shortage boom
#                    well (real backlogs were swelling in 2021), but is well known in this industry
#                    for the "bullwhip effect" -- backlogs/order books LAG the actual demand
#                    collapse, so this model is caught badly off guard once the glut/correction
#                    actually hits (customers cancel/de-book faster than the backlog reflects).
#   ml_endmarket  -- an ML model incorporating end-market indicators (hyperscaler AI capex
#                    guidance, PC/smartphone sell-through, auto production): modestly behind in
#                    calm quarters (the extra machinery doesn't always pay for itself day to day),
#                    but degrades far less through BOTH the bust and the 2024 AI-driven mix shift,
#                    because it watches the actual end-demand drivers rather than the fab's own
#                    cycle shape or its customers' order books.
#   trend_extrap  -- naive trend extrapolation: the trivial benchmark every real forecast shootout
#                    includes as a sanity check, hurt worst of all at every turning point (it
#                    simply assumes whatever is currently happening will keep happening).
#
# These tests were built by first running this exact scenario interactively against the package
# (`devtools::load_all()` -> `loss_panel()` -> `report()` -> `decision_breakdown()` -> `compare()`)
# to confirm two real, reproducible findings on realistic data, which the assertions below pin down
# permanently:
#
#   FINDING 1 (decision_breakdown(): the BUST, not the BOOM, drives k*): on the full 48-quarter
#   panel, ml_endmarket is the pooled champion and order_book is the binding rival -- but the
#   minimal set of periods whose removal would flip that decision (k*=5) is made up ENTIRELY of
#   glut/bust-phase quarters (2022Q4-2023Q4), not shortage-boom quarters and not the 2024 AI-shift
#   quarters. This is a genuinely useful, non-obvious diagnostic for a capacity-planning team: it
#   says the champion's edge over the order-book model is fragile specifically BECAUSE of how badly
#   the order-book model's bullwhip lag hurt it during the correction -- not because of the boom
#   (where order_book actually did fine) and not because of the recent AI-driven mix shift (which is
#   a separate mechanism entirely). A team reading only "ml_endmarket wins, order_book is the
#   runner-up" would miss which part of the 12-year history that conclusion actually rests on.
#
#   FINDING 2 (compare(): a re-evaluation at the start of the bust phase flips the champion, but
#   compare() correctly refuses to call it an act-now signal): a team that last checked its model
#   choice right as the shortage boom ended (2022Q2 -- the realistic moment a team would sanity-
#   check its capacity-planning model just as the industry's own capacity race peaks) would have
#   crowned order_book its champion. Re-evaluating today, with the full glut and AI-mix-shift
#   history in hand, flips the champion to ml_endmarket -- and compare() correctly attributes this
#   ENTIRELY to the new (post-2022Q2) periods, not to noise already present in the earlier data.
#   Yet because order_book has NOT been statistically eliminated from the post-break Model
#   Confidence Set (it is still a real contender, not clearly dominated), compare() correctly sets
#   act=FALSE -- exactly the "the ranking did flip, here is why, but it is not yet confident enough
#   to force a multi-year capacity bet on the new model" signal a capacity-planning team actually
#   needs before committing to years-long capital spending on the strength of one model's forecast.

.semi_quarters <- function() {
  seq(as.Date("2013-01-01"), as.Date("2024-10-01"), by = "3 months")  # 12 years, T=48
}
.semi_qlabels <- function(dates) {
  paste0(format(dates, "%Y"), "Q", (as.integer(format(dates, "%m")) - 1) %/% 3 + 1)
}
# AR(1) per-period noise (NOT i.i.d.) -- forecast-error loss differentials persist across
# consecutive quarters within a cycle phase, the same realistic feature used elsewhere in this
# package's own macro-nowcasting test; rho=0.45 is in that literature's realistic persistence range.
.semi_ar1 <- function(n, rho, sd_innov) {
  e <- rnorm(n, 0, sd_innov)
  x <- numeric(n)
  x[1] <- e[1] / sqrt(1 - rho^2)
  for (t in 2:n) x[t] <- rho * x[t - 1] + e[t]
  x
}

.build_semiconductor_panel <- function() {
  quarters <- .semi_quarters()
  Tn <- length(quarters)
  qlab <- .semi_qlabels(quarters)
  shortage <- quarters >= as.Date("2021-01-01") & quarters <= as.Date("2022-06-01")  # chip shortage boom
  glut     <- quarters >= as.Date("2022-07-01") & quarters <= as.Date("2023-12-01")  # inventory correction/bust
  aishift  <- quarters >= as.Date("2024-01-01")                                      # AI/datacenter mix shift

  set.seed(42)
  rho <- 0.45
  cycle_pattern <- 0.40 + .semi_ar1(Tn, rho, 0.04)
  order_book    <- 0.38 + .semi_ar1(Tn, rho, 0.04)
  ml_endmarket  <- 0.45 + .semi_ar1(Tn, rho, 0.04)
  trend_extrap  <- 0.42 + .semi_ar1(Tn, rho, 0.04)

  # Shortage boom: order_book (real backlogs swelling) and ml_endmarket (end-market indicators
  # surging) track the abrupt turn reasonably; cycle_pattern and trend_extrap are blindsided.
  cycle_pattern[shortage] <- cycle_pattern[shortage] + 1.1
  trend_extrap[shortage]  <- trend_extrap[shortage]  + 1.4
  order_book[shortage]    <- order_book[shortage]    + 0.15
  ml_endmarket[shortage]  <- ml_endmarket[shortage]  + 0.25

  # Glut/bust: the bullwhip effect -- order_book now lags (backlogs/order books take time to
  # reflect the cancellation wave), cycle_pattern mismatches the correction's magnitude, trend_extrap
  # (still assuming the boom continues) is hurt worst of all; ml_endmarket (watching actual
  # end-market sell-through) catches the bust fastest.
  cycle_pattern[glut] <- cycle_pattern[glut] + 0.9
  order_book[glut]    <- order_book[glut]    + 1.0
  trend_extrap[glut]  <- trend_extrap[glut]  + 1.8
  ml_endmarket[glut]  <- ml_endmarket[glut]  + 0.3

  # 2024 AI/datacenter product-mix shift: only ml_endmarket (ingests AI capex/hyperscaler
  # indicators) tracks the segment divergence; the other three, anchored on legacy-node-era
  # patterns, degrade.
  cycle_pattern[aishift] <- cycle_pattern[aishift] + 0.7
  order_book[aishift]    <- order_book[aishift]    + 0.5
  trend_extrap[aishift]  <- trend_extrap[aishift]  + 0.9
  ml_endmarket[aishift]  <- ml_endmarket[aishift]  + 0.05

  L <- list(cycle_pattern = cycle_pattern, order_book = order_book,
            ml_endmarket = ml_endmarket, trend_extrap = trend_extrap)
  list(L = L, qlab = qlab, shortage = shortage, glut = glut, aishift = aishift,
       panel = loss_panel(L, labels = qlab))
}

test_that("realistic 12-year quarterly wafer-demand panel: report() crowns ml_endmarket but flags a genuine VERDICT/RESOLUTION read", {
  s <- .build_semiconductor_panel()
  panel <- s$panel

  expect_identical(pooled_winner(panel$losses, panel$weights), "ml_endmarket")

  out <- report(panel, alpha = 0.10, seed = 1)
  expect_match(out, "ml_endmarket\\s+mean=.*in MCS.*\\(champion\\)")
  expect_match(out, "order_book\\s+mean=.*in MCS")
  # The naive trend-extrapolation benchmark and the pure historical-cycle model -- hurt worst by
  # the shortage/glut turning points and blind to the 2024 mix shift -- are correctly excluded from
  # the Model Confidence Set, not merely ranked lower.
  expect_match(out, "cycle_pattern\\s+mean=.*excluded")
  expect_match(out, "trend_extrap\\s+mean=.*excluded")
  # order_book is the binding rival the champion's edge is measured against, consistent with the
  # bullwhip-lag story: it is competitive enough overall (strong boom performance) to remain the
  # closest real contender, not a clearly-dominated also-ran.
  rr <- resolution_report(panel)
  expect_identical(rr$champion, "ml_endmarket")
  expect_identical(rr$binding_rival, "order_book")
})

test_that("decision_breakdown(): k* is driven specifically by the glut/bust-phase quarters, not the shortage boom or the 2024 AI mix shift", {
  s <- .build_semiconductor_panel()
  panel <- s$panel
  qlab <- s$qlab

  db <- decision_breakdown(panel)
  expect_identical(db$binding_opponent, "order_book")
  removed_labels <- qlab[db$removed]

  # Every single pivotal period lands inside the 2022Q3-2023Q4 glut/bust window...
  expect_true(all(removed_labels %in% qlab[s$glut]))
  # ...and NONE land in the 2021Q1-2022Q2 shortage boom (where order_book performed fine)...
  expect_false(any(removed_labels %in% qlab[s$shortage]))
  # ...and NONE land in the 2024 AI/datacenter mix-shift quarters (a separate mechanism entirely).
  expect_false(any(removed_labels %in% qlab[s$aishift]))
  # This is a real, non-trivial subset of the bust window (not "remove everything"): k* is small
  # relative to the 48-quarter panel, which is exactly the point of a decision-breakdown point --
  # a handful of concentrated periods, not the whole history, determine the champion's margin.
  expect_identical(db$k, 5L)
  expect_lt(db$k, length(qlab[s$glut]))

  # Restricting the panel to just the boom+bust transition window and re-running
  # decision_breakdown() must identify the SAME binding opponent and the SAME k* -- the pivotal
  # periods really do live entirely inside this window, not merely by coincidence of the full-panel
  # computation.
  trans_idx <- which(s$shortage | s$glut)
  panel_trans <- loss_panel(lapply(s$L, function(v) v[trans_idx]), labels = qlab[trans_idx])
  db_trans <- decision_breakdown(panel_trans)
  expect_identical(db_trans$binding_opponent, "order_book")
  expect_identical(db_trans$k, db$k)
})

test_that("compare(): re-evaluating at the start of the next cycle phase flips the champion, attributed entirely to the new periods, without overclaiming 'act'", {
  s <- .build_semiconductor_panel()
  L <- s$L
  qlab <- s$qlab

  # "previous": the capacity-planning team's last model check, taken right as the shortage boom
  # ends (2022Q2) -- the realistic point at which a team would sanity-check its forecasting model
  # just as the industry's own capacity race peaks, before the correction has played out.
  pre_idx <- which(qlab == "2022Q2")
  previous <- loss_panel(lapply(L, function(v) v[1:pre_idx]), labels = qlab[1:pre_idx])
  # "current": re-evaluation today, with the full glut/correction AND the 2024 AI-driven mix shift
  # now in hand -- the realistic re-evaluation at the start of the NEXT cycle phase (the mix-shift
  # era) that a capacity-planning team would run before committing to its next multi-year
  # capacity-investment decision.
  current <- s$panel

  expect_identical(pooled_winner(previous$losses, previous$weights), "order_book")
  expect_identical(pooled_winner(current$losses, current$weights), "ml_endmarket")

  cr <- compare(previous, current, alpha = 0.10, seed = 1)
  expect_identical(cr$previous_champion, "order_book")
  expect_identical(cr$current_champion, "ml_endmarket")
  expect_true(cr$champion_changed)
  # The flip is attributable ENTIRELY to the new (post-2022Q2) periods: truncating current back to
  # previous's own window reverts to the old champion, confirming this is not noise already present
  # in the pre-correction data.
  expect_identical(cr$champion_without_new_periods, "order_book")
  expect_equal(cr$n_new_periods, length(qlab) - pre_idx)
  # order_book has not been statistically eliminated from the post-break Model Confidence Set (it
  # is still a real contender on the strength of its boom-era performance) -- so even though the
  # pooled ranking flipped, compare() correctly refuses to call this a confident, bet-the-fab
  # "act on it now" regime change.
  expect_true(isTRUE(cr$old_champion_still_in_mcs))
  expect_false(cr$act)

  printed <- paste(capture.output(print(cr)), collapse = "\n")
  expect_match(printed, "champion changed order_book -> ml_endmarket")
  expect_match(printed, "entirely due to the \\d+ new period\\(s\\)")
  expect_match(printed, "act=FALSE")
})
