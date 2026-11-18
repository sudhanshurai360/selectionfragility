# REALISTIC DOMAIN TEST: parametric storm-severity / precipitation insurance underwriting.
#
# BUSINESS CONTEXT: a parametric weather-insurance underwriter compares 4 competing
# precipitation/storm-severity forecasting models -- climatology (the seasonal-normal
# persistence baseline), ar1_regional (a simple regional autoregressive statistical model),
# ensemble_nwp (a physics-based numerical-weather-prediction ensemble), and ml_nowcast (a
# machine-learning nowcasting model trained mainly on common-case patterns) -- and needs to know
# whether its "ensemble_nwp is the best model" conclusion is statistically solid or an artifact
# of a handful of catastrophic storm weeks.
#
# CADENCE, JUSTIFIED: weekly re-scoring during the NOAA-defined Atlantic hurricane season (early
# June - late November, ~22 weeks) across 9 seasons (2016-2024) gives T = 198 periods. Weekly
# resolution is the real granularity underwriters re-score at during active season (they need to
# see WHICH week drove losses, not just an annual number); multiple seasons are needed to see
# genuine year-to-year, ENSO-driven variability in storm activity. This lands exactly in the
# package's own documented sweet spot -- "a few hundred periods, a handful of which are
# catastrophic" -- not an adversarial construction.
#
# EXTREME EVENT CONCENTRATION (realistic, not adversarial): most weeks are unremarkable --
# small, symmetric forecast-error noise shared by all 4 models (no persistent skill difference
# in ordinary weather). Five weeks are genuine historical-magnitude catastrophes -- Hurricane
# Harvey (2017), Hurricane Michael (2018), Hurricane Laura (2020), Hurricane Ida (2021), and
# Hurricane Ian (2022) -- where loss differentials are an order of magnitude larger than normal.
# This is THE textbook case for parametric catastrophe insurance: Atlantic hurricane losses are
# famously fat-tailed, with a small number of named storms producing the overwhelming majority
# of insured loss in any given decade (well documented in catastrophe-modeling practice, e.g.
# AIR/RMS/Verisk cat models and NOAA's own billion-dollar-disaster accounting).
#
# ENSO / YEAR-TO-YEAR CLIMATE VARIABILITY (real domain structure, deliberately imperfect): La
# Nina seasons reduce tropical-Atlantic vertical wind shear, historically correlating with more
# and more-intense hurricanes (Gray 1984; Camargo & Sobel 2005). 2017, 2020, 2021, and 2022 were
# La Nina (or La-Nina-leaning) seasons, consistent with 4 of the 5 catastrophes landing there.
# Hurricane Michael (2018) is kept as a genuine exception -- a major storm during a weak-El-Nino
# season -- because real catastrophe history is not a mechanically clean ENSO signal.
#
# MODEL ASYMMETRY (realistic, not a toy "one model has a bad day"): ensemble_nwp, being
# physics-based, resolves genuinely unprecedented storm dynamics reasonably well even in a
# catastrophe week (small loss, ~1.0-1.6 vs. an ordinary-week scale of ~1.0). climatology,
# ar1_regional, and ml_nowcast are all trained on/anchored to typical conditions and all miss
# badly on a true tail event (losses ~10-15, an order of magnitude above normal) -- this mirrors
# the real, well-documented "generalization gap" of empirical/statistical and ML weather models
# relative to physics-based NWP on out-of-distribution, record-magnitude extremes.

weeks_per_season <- 22L
years <- 2016:2024
T <- length(years) * weeks_per_season  # 198
labels <- unlist(lapply(years, function(y) paste0(y, "-W", sprintf("%02d", 1:weeks_per_season))))
models <- c("climatology", "ar1_regional", "ensemble_nwp", "ml_nowcast")

idx_of <- function(year, week) (which(years == year) - 1L) * weeks_per_season + week

# The 5 catastrophe weeks and the real storms they represent.
catastrophes <- list(
  list(year = 2017, week = 12, name = "Hurricane Harvey (2017)"),
  list(year = 2018, week = 18, name = "Hurricane Michael (2018)"),
  list(year = 2020, week = 13, name = "Hurricane Laura (2020)"),
  list(year = 2021, week = 13, name = "Hurricane Ida (2021)"),
  list(year = 2022, week = 17, name = "Hurricane Ian (2022)")
)
cat_idx <- sort(vapply(catastrophes, function(cc) idx_of(cc$year, cc$week), numeric(1)))

build_weather_panel <- function(seed = 20261118L) {
  set.seed(seed)
  # Ordinary weeks: small, roughly symmetric, NO persistent skill difference between models --
  # the realistic "quiet weather" case where all 4 approaches are in the same ballpark.
  L <- stats::setNames(
    lapply(models, function(m) pmax(stats::rnorm(T, mean = 1.0, sd = 0.05), 0.1)),
    models
  )
  for (cc in catastrophes) {
    i <- idx_of(cc$year, cc$week)
    L$climatology[i]  <- stats::runif(1, 11, 14)
    L$ar1_regional[i] <- stats::runif(1, 10, 13)
    L$ensemble_nwp[i] <- stats::runif(1, 1.0, 1.6)   # physics captures the extreme reasonably
    L$ml_nowcast[i]   <- stats::runif(1, 12, 15)
  }
  loss_panel(L, labels = labels)
}

test_that("realistic 9-season Atlantic-hurricane weather-risk panel: report() names ensemble_nwp champion and k* correctly implicates the catastrophe weeks", {
  panel <- build_weather_panel()

  rpt <- report(panel, alpha = 0.10, seed = 1)

  # ensemble_nwp is the pooled champion and survives alone in the MCS: the physics-based model's
  # edge over the other 3 is NOT just a coin flip -- it is identified.
  expect_match(rpt, "VERDICT: identified \\(MCS size 1 of 4\\)")
  expect_match(rpt, "ensemble_nwp +mean=1\\.0[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")

  # --- decision_breakdown(): is the decision genuinely driven by the catastrophe weeks? --------
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ensemble_nwp")

  db <- decision_breakdown(panel, a = champ, return_ties = TRUE)
  expect_identical(db$binding_opponent, "ar1_regional")
  # k* is small relative to T (9 of 198 periods, ~4.5%) -- exactly the fragile-looking decision
  # this package is built to flag -- but it is NOT exactly equal to the 5 named catastrophes: it
  # takes those 5 PLUS 4 additional, ordinary, small-margin weeks to fully flip the sign of the
  # pooled margin. This is a genuine, non-bug finding, not an artifact of this test's construction
  # (see the margin-accounting test below): once you strip out the catastrophes, the residual
  # margin between ensemble_nwp and ar1_regional is small but still (by chance, since ordinary
  # weeks carry no true skill difference) slightly positive, so a few more of the largest ordinary
  # weeks must also go before the decision actually flips. decision_breakdown()'s k* answers "how
  # many periods, adversarially chosen, does it take to flip the decision" -- not "how many
  # catastrophes are there" -- and this panel is a clean illustration of why those two numbers can
  # legitimately differ even when catastrophes are overwhelmingly the dominant driver.
  expect_identical(db$k, 9L)
  removed <- sort(db$removed)
  # All 5 named catastrophe weeks are among the 9 removed periods.
  expect_true(all(cat_idx %in% removed))
  # The catastrophes are a strict, large-majority subset of what k* removes.
  expect_equal(sum(removed %in% cat_idx), 5L)

  # --- margin accounting: catastrophes dominate the NET margin almost completely --------------
  la <- panel$losses[[champ]]; lb <- panel$losses[["ar1_regional"]]
  contrib <- lb - la
  # The 5 catastrophe weeks alone account for essentially the entire net margin by which
  # ensemble_nwp beats ar1_regional -- this is the "decision hinges on a handful of catastrophic
  # periods" property the task is built around, confirmed directly rather than just asserted.
  expect_gt(sum(contrib[cat_idx]) / sum(contrib), 0.95)

  # --- Prop 2.2 certificate: does NOT certify this k* as a mere artifact of noise --------------
  # k*=9 is too large for Proposition 2.2's zero-simulation bound to certify "non-significant" at
  # T=198 (the threshold there is k<=3) -- consistent with the MCS bootstrap's "identified" read:
  # the champion's edge, though catastrophe-driven, is not dismissable as pure chance.
  cert <- prop22_certifies(db$k, T = T)
  expect_false(cert)
})

test_that("concentration_share() under-states multi-event concentration relative to decision_breakdown()'s own margin accounting (realistic finding, not a bug)", {
  # concentration_share() is explicitly documented and designed to measure the SINGLE most
  # influential period's share of the champion-vs-opponent margin (see ?concentration_share:
  # "Share of the champion's margin concentrated in the single most influential period"). In this
  # REALISTIC multi-catastrophe scenario (5 distinct hurricanes, not one mega-storm), that design
  # choice means concentration_share() reports a comparatively modest number even though the
  # DECISION is, as shown above, almost entirely catastrophe-driven -- because no single storm
  # dominates the other four. This is a genuine, domain-relevant characterization check: a user
  # who reads ONLY concentration_share() in a multi-catastrophe book of business could come away
  # thinking the decision is much less concentrated than decision_breakdown()'s own k*/margin
  # accounting (above) shows it actually is.
  panel <- build_weather_panel()
  champ <- pooled_winner(panel$losses, panel$weights)
  la <- panel$losses[[champ]]; lb <- panel$losses[["ar1_regional"]]
  contrib <- lb - la

  cs <- concentration_share(panel)
  # The single largest week (Hurricane Ian, 2022) accounts for only a modest fraction of the
  # margin's gross dispersion ...
  expect_lt(cs, 0.30)
  # ... even though ALL FIVE catastrophe weeks together account for the large majority of that
  # same gross dispersion (sum of |contribution|) -- confirming concentration_share()'s
  # single-period scope, not a broken denominator, is what produces the smaller number.
  expect_gt(sum(abs(contrib[cat_idx])) / sum(abs(contrib)), 0.75)
})

test_that("pivot_agreement() reconfirms its documented frac-tracking weakness on this realistic multi-catastrophe panel, and is even less informative here than on a single-shock panel", {
  # DOCUMENTED, PRE-EXISTING LIMITATION (see ?pivot_agreement's own @section Limitation): this
  # statistic is designed to answer "is the named pivotal period a real feature of the data," but
  # because it subsamples WITHOUT drawing new noise, it mostly just tracks the `frac` knob
  # regardless of the panel's true concentration. This test checks whether that documented
  # weakness is ALSO visible on a realistic, non-adversarial weather-catastrophe panel (it is),
  # and additionally surfaces a MORE SEVERE version of it that is specific to having SEVERAL
  # comparably-sized catastrophes (the realistic case here) rather than one dominant shock: when a
  # subsample excludes enough catastrophe weeks, pivot_agreement()'s internal
  # champion-vs-opponent pair can itself flip (a different model becomes champion, or a different
  # rival becomes "the" binding opponent), so even subsamples that DO retain the full-sample's
  # named pivotal week frequently disagree with it, because the comparison itself has changed.
  panel <- build_weather_panel()

  pa_80 <- pivot_agreement(panel, seed = 1, frac = 0.80)
  pa_50 <- pivot_agreement(panel, seed = 1, frac = 0.50)
  pa_95 <- pivot_agreement(panel, seed = 1, frac = 0.95)

  # All three move together with `frac`, tracking the subsampling knob rather than sitting at a
  # frac-independent "true concentration" level -- the core, already-documented limitation,
  # reconfirmed on realistic data.
  expect_true(pa_50 < pa_80)
  expect_true(pa_80 < pa_95)

  # At frac=0.8, agreement (~0.75) is close to (a bit below) the naive "inclusion probability"
  # baseline of 0.8 one would get from a single dominant shock.
  expect_equal(pa_80, 0.755, tolerance = 0.05)

  # At frac=0.5, the naive inclusion-probability baseline predicts ~0.5 (n_sub/T is exactly 0.5),
  # but the realistic multi-catastrophe structure here pulls the observed agreement MATERIALLY
  # below that baseline -- confirming the champion/opponent-identity-flip mechanism described
  # above, which does not arise in a single-dominant-shock panel (where inclusion of the top
  # period is both necessary AND sufficient for agreement). This is a genuine elaboration of the
  # documented limitation, specific to this realistic domain scenario, reported for human review
  # rather than treated as a bug: pivot_agreement() is working exactly as designed, but its
  # already-weak signal is even weaker in the realistic multi-catastrophe case than in the
  # single-shock examples used in its own documentation.
  expect_lt(pa_50, 0.40)
})

test_that("winner_stability()/exchangeable_benchmark() give a REASSURING read that decision_breakdown()/concentration_share() contradict -- both are correct, and a user relying on only one could be misled", {
  # This is the central decision-relevant question from the task: "would a different set of
  # catastrophes in a counterfactual history have picked a different model?" winner_stability()
  # answers via block-bootstrap RESAMPLING WITH REPLACEMENT over all T periods. Because there are
  # 5 catastrophe weeks scattered across 198 periods, it is numerically very unlikely for a
  # bootstrap resample to omit ALL FIVE simultaneously -- so the champion (ensemble_nwp) wins
  # essentially every resample, and winner_stability reports maximal, not reduced, stability.
  #
  # decision_breakdown()'s k* instead asks the ADVERSARIAL question: what is the FEWEST periods
  # that, if deleted, flip the decision? That number (9 of 198) is small, and nearly all of its
  # content is the 5 catastrophes (prior tests). These are different, both legitimate questions
  # ("how fragile is this to RANDOM missing data" vs. "how fragile is this to a SPECIFIC small
  # subset of periods"), and this panel is a realistic case where they point in different
  # directions. report()'s own PIVOT section (which surfaces k*/concentration_share/
  # pivot_agreement but NOT winner_stability/exchangeable_benchmark) gives the catastrophe-aware
  # answer; a user who separately calls winner_stability()+exchangeable_benchmark() -- exactly
  # the workflow this package's own documentation recommends as a "complementary... fragility
  # read" -- would see a falsely reassuring signal for this realistic book of business. Reported
  # for human review: not a bug, since both statistics are individually correct, but a real gap
  # between what report() shows and what the complementary bootstrap functions show on exactly
  # the scenario (multi-catastrophe concentration) they are meant to help with.
  panel <- build_weather_panel()

  ws <- winner_stability(panel, seed = 1, n_boot = 2000)
  bench <- exchangeable_benchmark(K = length(models), T = T, n_rep = 200, n_boot = 500, seed = 1)

  # winner_stability reports the champion surviving (essentially) every resample ...
  expect_gt(ws$stability, 0.99)
  # ... which is ABOVE the pure-chance null's 5th percentile -- i.e. NOT flagged as unusually
  # fragile relative to chance alone, even though decision_breakdown()/concentration_share()
  # (prior tests) show the decision is almost entirely dependent on 5 specific catastrophe weeks.
  expect_gt(ws$stability, bench$p05)
})
