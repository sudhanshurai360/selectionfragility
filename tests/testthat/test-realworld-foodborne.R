# REALISTIC DOMAIN TEST: weekly foodborne-illness surveillance (think CDC FoodNet / PulseNet).
#
# BUSINESS CONTEXT: a public-health surveillance team compares 4 competing models for
# forecasting weekly case counts of a single reportable pathogen, to decide which model to trust
# for next quarter's surveillance resource allocation (lab capacity, interview staffing, recall
# follow-up):
#   - hist_baseline:  a historical-baseline model -- long-run average weekly case count, blind
#                     to any outbreak/supply-chain information (the "do nothing smart" baseline
#                     every surveillance program carries, same role as a persistence/climatology
#                     baseline in other forecast-hub domains).
#   - supply_chain:   a supply-chain-signal-aware model that gets an early, partial read during
#                     an active outbreak week itself (e.g. distributor/shipment anomaly signals)
#                     but has no special skill outside outbreak weeks.
#   - ml_recall:      an ML model that incorporates retail-recall data; its signal kicks in during
#                     an outbreak week AND carries one week of lag/memory after a recall is first
#                     flagged, so it tracks both the up-swing and the decay tail better than
#                     supply_chain.
#   - trend_extrap:   a simple recent-trend (last-2-week) extrapolation model -- the forecasting
#                     equivalent of a naive persistence baseline, and for exactly the same
#                     structural reason it is bad at turning points in epidemic-curve work: it is
#                     maximally wrong exactly when the series is changing fastest, i.e. at
#                     outbreak onset and at the recall-driven resolution.
#
# WHY THIS IS A GENUINELY DIFFERENT SHOCK SHAPE FROM THIS PACKAGE'S OTHER EPIDEMIOLOGY FIXTURE
# (test-realworld-epidemiology.R, a seasonal COVID/flu forecast-hub scenario):
#   A seasonal respiratory wave ramps up and down over many weeks (SEIR-style growth/decay,
#   10-20+ week waves) and recurs on a predictable calendar (flu season, COVID waves). A
#   point-source foodborne outbreak (e.g. a contaminated romaine-lettuce or ground-beef lot) is
#   the opposite shock shape: NO seasonal predictability at all, a SHARP, SUDDEN onset tied to
#   when a specific contaminated product reaches consumers, and a FAST resolution once the source
#   is identified, traced, and the product recalled (CDC outbreak investigations typically
#   resolve within a small number of weeks of the recall). This fixture's per-outbreak shape
#   (3-4 week rise to peak, exponential decay back to baseline within another ~3 weeks) reflects
#   that: each outbreak occupies only ~6-7 of 156 weeks, the outbreaks are isolated point events
#   scattered across 3 years with long baseline troughs between them, and there is no recurring
#   calendar signal a model could exploit the way a flu/COVID model can exploit "winter is
#   coming." Surveillance case counts are also WEEKLY (CDC FoodNet reporting cadence), not daily.
#
# WHAT THIS CHECKS (cross-checked against the SAME already-documented "concentration_share()
# understates multi-event concentration" limitation found in a THIRD, unrelated generating
# mechanism -- catastrophe-bond/weather-risk claims in test-realworld-weatherrisk.R and rare
# binary failures in test-realworld-manufacturing.R -- confirming the limitation is a property of
# the STATISTIC, not of any one domain's shock mechanism):
#   1. The sparse, bursty outbreak weeks -- not the long stretches of flat endemic-baseline
#      weeks -- are what decision_breakdown()'s k* actually removes to flip the champion. This is
#      the point-source analogue of the seasonal-epidemiology fixture's "loss is concentrated at
#      turning points" finding, but here the "turning points" are entire, countable, named
#      outbreak events rather than phases of one continuous wave.
#   2. concentration_share() (restricted by design to the single most influential period) again
#      understates the TRUE multi-event dispersion share (computed directly from the champion-
#      vs-binding-opponent margin contributions, summed over all outbreak weeks) -- reconfirming
#      the limitation generalizes to a sparse point-source regime, not just continuous-hazard
#      catastrophe curves.
#   3. Pathogen case volume/outbreak frequency materially changes HOW MUCH of the (always sparse)
#      outbreak-week set k* needs to remove: a low-baseline, low-frequency pathogen (Listeria,
#      2 outbreaks/156wk) requires removing a much larger SHARE of its own (smaller) outbreak-week
#      set than a higher-frequency pathogen (Salmonella, 4 outbreaks/156wk) -- a genuine,
#      domain-relevant comparative finding, not an artifact of the fixture.

test_that("Salmonella weekly panel: k* removes ONLY sparse point-source outbreak weeks, and the champion/VERDICT disagreement is real (not a bug)", {
  build_salmonella_panel <- function(seed = 20260926L) {
    set.seed(seed)
    T_weeks <- 156L  # 3 years of weekly FoodNet-style surveillance
    baseline_rate <- 8  # typical weekly reported Salmonella cases for this catchment

    true_counts <- rep(baseline_rate, T_weeks) + stats::rpois(T_weeks, 1.5)

    # 4 point-source outbreaks of varying severity, each a sharp 3-4 week rise + exponential
    # decay back to baseline within another ~3 weeks -- NOT a seasonal wave shape.
    outbreaks <- list(
      list(start = 22,  peak = 3, height = 60, decay = 0.55),  # severe (e.g. romaine lettuce)
      list(start = 61,  peak = 2, height = 25, decay = 0.6),   # moderate (e.g. sprouts)
      list(start = 95,  peak = 2, height = 40, decay = 0.5),   # severe (e.g. ground beef)
      list(start = 130, peak = 1, height = 15, decay = 0.7)    # mild, short (e.g. egg product)
    )

    outbreak_flag <- rep(FALSE, T_weeks)
    outbreak_week_idx <- integer(0)
    for (ob in outbreaks) {
      for (d in 0:6) {
        wk <- ob$start + d
        if (wk > T_weeks) next
        bump <- if (d <= ob$peak) {
          ob$height * (d + 1) / (ob$peak + 1)
        } else {
          ob$height * exp(-ob$decay * (d - ob$peak))
        }
        if (bump > 1) {
          true_counts[wk] <- true_counts[wk] + bump
          outbreak_flag[wk] <- TRUE
          outbreak_week_idx <- c(outbreak_week_idx, wk)
        }
      }
    }
    outbreak_week_idx <- sort(unique(outbreak_week_idx))
    true_counts <- round(pmax(true_counts, 0))

    n <- T_weeks
    hist_baseline <- supply_chain <- ml_recall <- trend_extrap <- rep(NA_real_, n)
    for (t in seq_len(n)) {
      hist_baseline[t] <- baseline_rate + stats::rnorm(1, 0, 1)
      in_ob <- outbreak_flag[t]
      supply_chain[t] <- if (in_ob) {
        true_counts[t] * stats::runif(1, 0.55, 0.85) + stats::rnorm(1, 0, 2)
      } else {
        baseline_rate + stats::rnorm(1, 0, 1.2)
      }
      lag_signal <- if (t > 1) outbreak_flag[t - 1] else FALSE
      ml_recall[t] <- if (in_ob || lag_signal) {
        true_counts[t] * stats::runif(1, 0.7, 0.95) + stats::rnorm(1, 0, 1.5)
      } else {
        baseline_rate + stats::rnorm(1, 0, 1)
      }
      # naive recent-trend extrapolation -- maximally wrong at outbreak onset/resolution, exactly
      # like a persistence baseline at an epidemic turning point.
      trend_extrap[t] <- if (t <= 2) baseline_rate else 2 * true_counts[t - 1] - true_counts[t - 2]
    }

    sq_loss <- function(pred) pmin((pred - true_counts)^2, 1e6)
    L <- list(
      hist_baseline = sq_loss(hist_baseline),
      supply_chain  = sq_loss(supply_chain),
      ml_recall     = sq_loss(ml_recall),
      trend_extrap  = sq_loss(trend_extrap)
    )
    list(
      panel = loss_panel(L, labels = paste0("W", seq_len(n))),
      outbreak_week_idx = outbreak_week_idx,
      true_counts = true_counts
    )
  }

  b <- build_salmonella_panel()
  panel <- b$panel

  # Sanity on the generating mechanism itself: outbreaks are a SMALL minority of weeks (sparse),
  # scattered across the 3-year window with long baseline troughs between them (point-source, not
  # a continuous seasonal wave) -- this is what makes "which weeks does k* remove" an interesting
  # question at all.
  expect_lt(length(b$outbreak_week_idx), 0.2 * length(panel$losses[[1]]))
  expect_gt(length(b$outbreak_week_idx), 15)

  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ml_recall")

  # VERDICT and RESOLUTION legitimately disagree on this panel: the champion's edge over its
  # single binding rival is powered/resolved, but the MCS bootstrap cannot simultaneously rule out
  # a THIRD model (trend_extrap) as statistically indistinguishable from the champion -- exactly
  # the documented "VERDICT and RESOLUTION answer different questions" case, reconfirmed here on
  # realistic point-source surveillance data rather than a contrived panel.
  rpt <- report(panel, seed = 1)
  expect_match(rpt, "VERDICT: not identified \\(MCS size 3 of 4\\)")
  expect_match(rpt, "ml_recall\\s+mean=[0-9.]+\\s+in MCS\\s+periods won=\\d+ \\(champion\\)")
  expect_match(rpt, "resolved")

  # --- the central point-source finding: k* removes ONLY sparse outbreak weeks -----------------
  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$k, 6L)
  expect_identical(db$binding_opponent, "supply_chain")
  removed <- sort(db$removed)
  # Every single period k* removes is a genuine point-source outbreak week -- on this panel the
  # fewest-periods-to-flip set lands ENTIRELY inside the sparse outbreak-week set, with zero
  # ordinary endemic-baseline weeks needed. This is the foodborne-specific, point-source version
  # of "loss is concentrated at turning points": here there is no continuous wave to speak of, so
  # the turning points ARE the whole outbreak-week set, discretely.
  expect_true(all(removed %in% b$outbreak_week_idx))
  expect_identical(length(removed), 6L)

  # --- concentration_share() again understates TRUE multi-event dispersion -----------------------
  # (cross-check against the SAME already-documented limitation confirmed in two unrelated
  # mechanisms elsewhere in this suite: catastrophe-bond claims and rare binary manufacturing
  # failures -- here in a sparse, point-source surveillance-count mechanism.)
  cs <- concentration_share(panel)
  opp <- db$binding_opponent
  la <- panel$losses[[champ]]; lb <- panel$losses[[opp]]
  contrib <- lb - la
  true_multi_event_share <- sum(abs(contrib[b$outbreak_week_idx])) / sum(abs(contrib))

  # The single largest outbreak week accounts for a modest share of the gross margin dispersion...
  expect_lt(cs, 0.40)
  # ...even though ALL FOUR outbreaks together (26 weeks) account for the clear majority of that
  # same gross dispersion -- confirming concentration_share()'s single-period scope (not a broken
  # denominator) is what produces the smaller number, exactly the already-documented pattern.
  expect_gt(true_multi_event_share, 0.60)
  expect_lt(cs / true_multi_event_share, 0.6)

  # pivot_agreement() is present and in [0,1]; not re-litigating its own documented frac-tracking
  # weakness here (already covered by the weather-risk fixture), just confirming it runs cleanly
  # on this domain's sparser event structure.
  pa <- pivot_agreement(panel, seed = 1)
  expect_true(pa >= 0 && pa <= 1)
})

test_that("Listeria weekly panel: a rarer, low-volume pathogen's sparser outbreak set is CONSUMED A LARGER SHARE OF to flip the decision than Salmonella's denser one", {
  # Listeria is a genuinely different pathogen profile from Salmonella for the SAME surveillance
  # team: much lower sporadic weekly baseline (a handful of cases, not ~8/week), and far fewer
  # outbreak events in the same 3-year window -- but each one is large relative to its own
  # baseline (Listeria outbreaks tend to be high-severity/high-case-fatality events tied to a
  # narrow set of ready-to-eat product categories, e.g. deli meat or soft cheese).
  build_listeria_panel <- function(seed = 90125L) {
    set.seed(seed)
    T_weeks <- 156L
    baseline_rate <- 1.2

    true_counts <- rep(baseline_rate, T_weeks) + stats::rpois(T_weeks, 0.3)

    outbreaks <- list(
      list(start = 50,  peak = 2, height = 18, decay = 0.45),  # e.g. deli meat
      list(start = 120, peak = 1, height = 10, decay = 0.6)    # e.g. soft cheese
    )
    outbreak_flag <- rep(FALSE, T_weeks)
    outbreak_week_idx <- integer(0)
    for (ob in outbreaks) {
      for (d in 0:6) {
        wk <- ob$start + d
        if (wk > T_weeks) next
        bump <- if (d <= ob$peak) {
          ob$height * (d + 1) / (ob$peak + 1)
        } else {
          ob$height * exp(-ob$decay * (d - ob$peak))
        }
        if (bump > 0.5) {
          true_counts[wk] <- true_counts[wk] + bump
          outbreak_flag[wk] <- TRUE
          outbreak_week_idx <- c(outbreak_week_idx, wk)
        }
      }
    }
    outbreak_week_idx <- sort(unique(outbreak_week_idx))
    true_counts <- round(pmax(true_counts, 0))

    n <- T_weeks
    hist_baseline <- supply_chain <- ml_recall <- trend_extrap <- rep(NA_real_, n)
    for (t in seq_len(n)) {
      hist_baseline[t] <- baseline_rate + stats::rnorm(1, 0, 0.4)
      in_ob <- outbreak_flag[t]
      supply_chain[t] <- if (in_ob) {
        true_counts[t] * stats::runif(1, 0.55, 0.85) + stats::rnorm(1, 0, 1)
      } else {
        baseline_rate + stats::rnorm(1, 0, 0.5)
      }
      lag_signal <- if (t > 1) outbreak_flag[t - 1] else FALSE
      ml_recall[t] <- if (in_ob || lag_signal) {
        true_counts[t] * stats::runif(1, 0.7, 0.95) + stats::rnorm(1, 0, 0.8)
      } else {
        baseline_rate + stats::rnorm(1, 0, 0.4)
      }
      trend_extrap[t] <- if (t <= 2) baseline_rate else 2 * true_counts[t - 1] - true_counts[t - 2]
    }

    sq_loss <- function(pred) pmin((pred - true_counts)^2, 1e6)
    L <- list(
      hist_baseline = sq_loss(hist_baseline),
      supply_chain  = sq_loss(supply_chain),
      ml_recall     = sq_loss(ml_recall),
      trend_extrap  = sq_loss(trend_extrap)
    )
    list(panel = loss_panel(L, labels = paste0("W", seq_len(n))), outbreak_week_idx = outbreak_week_idx)
  }

  b <- build_listeria_panel()
  panel <- b$panel

  # Only 2 point-source outbreaks (vs. Salmonella's 4) and a much smaller resulting outbreak-week
  # footprint -- a materially sparser event structure for this lower-frequency pathogen.
  expect_identical(length(b$outbreak_week_idx), 13L)

  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ml_recall")

  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$k, 11L)
  expect_identical(db$binding_opponent, "hist_baseline")
  removed <- sort(db$removed)
  expect_true(all(removed %in% b$outbreak_week_idx))

  # The comparative, domain-relevant finding: Listeria's sparser outbreak set is consumed a much
  # LARGER share of (11 of its own 13 outbreak weeks, ~85%) to flip the decision than Salmonella's
  # denser one was (6 of its own 26 outbreak weeks, ~23%, from the prior test). A surveillance team
  # relying on a single rule of thumb ("k* as a fraction of outbreak weeks tells you how robust the
  # pick is") would be misled across pathogens with different outbreak frequencies -- the same k*-
  # vs-event-count mapping this package's weather-risk fixture already showed is panel-specific,
  # now reconfirmed as ALSO frequency-specific within the same forecasting task and the same four
  # candidate models.
  frac_consumed <- length(removed) / length(b$outbreak_week_idx)
  expect_gt(frac_consumed, 0.7)

  cs <- concentration_share(panel)
  expect_true(is.finite(cs) && cs > 0 && cs < 1)

  rpt <- report(panel, seed = 1)
  expect_match(rpt, "VERDICT: identified \\(MCS size 1 of 4\\)")
  expect_match(rpt, "k\\*=11 of 156 periods")
})
