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

# =============================================================================================
# SECOND, DEEPER PASS (2026-11-18) -- extends the round above, does not re-litigate it.
#
# The prior round's central finding (winner_stability()/exchangeable_benchmark() give a
# reassuring read that decision_breakdown()/concentration_share() contradict) is ACCEPTED and
# UNCHANGED here. This pass checks three NEW things the prior round did not measure, now that
# the package has also grown (a) a static small-T warning in mcs()/identified() (T/K<10), (b) an
# always-printed autocorrelation disclosure line in report(), and (c) a dedup fix for warnings
# that used to double-fire from report()'s two internal MCS call sites.
# =============================================================================================

# --- NEW SCENARIO for check (1): a realistic SHORT-HISTORY, ANNUAL-CADENCE cat panel -----------
# Many real catastrophe-bond / cat-reinsurance programs reset and re-score ANNUALLY (one
# aggregate seasonal loss-ratio observation per year), not weekly -- and the "usable" history for
# catastrophe modeling is itself short (insurers/modelers generally trust detailed hurricane-loss
# history back to roughly the early 2000s, not multiple centuries). A 24-year annual panel
# (2001-2024, K=4 models) gives T/K = 6.0 -- BELOW this package's own newly-added small-T
# threshold (T/K<10) -- while ALSO being a genuine multi-catastrophe-concentrated panel (4 of the
# 24 years are historically catastrophic seasons: 2005/Katrina, 2008/Ike, 2017/Harvey,
# 2021/Ida). This combination (small-T AND multi-catastrophe-concentrated) is realistic, not
# adversarial: cat-risk data is routinely both.
years_annual <- 2001:2024
T_annual <- length(years_annual)  # 24; T/K = 24/4 = 6.0 < the package's T/K=10 small-T threshold
cat_years_annual <- c(2005, 2008, 2017, 2021)

build_annual_smallT_panel <- function(seed = 42L) {
  set.seed(seed)
  L <- stats::setNames(
    lapply(models, function(m) pmax(stats::rnorm(T_annual, mean = 1.0, sd = 0.05), 0.1)),
    models
  )
  for (y in cat_years_annual) {
    i <- which(years_annual == y)
    L$climatology[i]  <- stats::runif(1, 11, 14)
    L$ar1_regional[i] <- stats::runif(1, 10, 13)
    L$ensemble_nwp[i] <- stats::runif(1, 1.0, 1.6)
    L$ml_nowcast[i]   <- stats::runif(1, 12, 15)
  }
  loss_panel(L, labels = as.character(years_annual))
}

test_that("CHECK (1), NEW: the small-T warning and the already-documented winner_stability-vs-decision_breakdown divergence COEXIST sensibly (not overwhelmingly/contradictorily) on a realistic short-history annual cat panel", {
  panel <- build_annual_smallT_panel()

  # report() fires the small-T warning exactly once (dedup holds even in this NEW small-T +
  # multi-catastrophe combination, not just in the single-mechanism cases the dedup fix targeted).
  warns <- testthat::capture_warnings(rpt <- report(panel, alpha = 0.10, seed = 1))
  expect_length(warns, 1)
  expect_match(warns, "T=24 periods for K=4 models \\(T/K=6\\.0\\).*small-sample regime")

  # The small-T caveat (a WARNING, side-channel) and the printed report body are cleanly
  # SEPARATED -- the small-T language never bleeds into the printed VERDICT/PIVOT text itself, so
  # a reader of just the printed report is not confronted with two overlapping disclosures mashed
  # into one paragraph. This is the comprehensibility question the task asks: the two caveats
  # layer (warning channel vs. print-body channel) rather than collide.
  expect_false(grepl("small-sample", rpt))
  expect_match(rpt, "VERDICT: identified \\(MCS size 1 of 4\\)")
  # PIVOT correctly names all 4 catastrophe years as the k* set -- and on this SHORTER, annual
  # panel k* lands EXACTLY on the named catastrophes (a cleaner mapping than the weekly panel
  # above, where k*=9 took the 5 catastrophes plus 4 ordinary weeks). This is itself a new,
  # genuine (non-bug) observation: the gap between "k*" and "count of named catastrophes" that the
  # prior round flagged is panel-specific, not a fixed property of this statistic.
  expect_match(rpt, "k\\*=4 of 24 periods \\(2005, 2008, 2017, 2021\\)")
  # The always-on autocorrelation disclosure is present exactly once, and not duplicated by the
  # small-T warning's own text (the two caveats are about different things -- MCS false-elimination
  # rate vs. the RESOLUTION SE's independence assumption -- and say so without overlapping wording).
  expect_equal(lengths(gregexpr("assumes no autocorrelation", rpt)), 1L)

  # Now reconfirm, on THIS small-T panel, that the already-documented reassuring-vs-concentrated
  # divergence from the prior round still holds -- i.e. the small-T warning does not somehow
  # replace or resolve it, it just adds an orthogonal caveat about VERDICT reliability.
  champ <- pooled_winner(panel$losses, panel$weights)
  expect_identical(champ, "ensemble_nwp")
  db <- suppressWarnings(decision_breakdown(panel, a = champ, return_ties = TRUE))
  expect_identical(db$k, 4L)
  expect_identical(sort(db$removed), sort(vapply(cat_years_annual, function(y) which(years_annual == y), integer(1))))

  ws <- suppressWarnings(winner_stability(panel, seed = 1, n_boot = 2000))
  bench <- suppressWarnings(exchangeable_benchmark(K = length(models), T = T_annual, n_rep = 200, n_boot = 500, seed = 1))
  # The reassuring bootstrap read survives small-T too: the champion wins (essentially) every
  # resample, well above the pure-chance null's 5th percentile -- the SAME divergence from
  # decision_breakdown()'s own k*=4/24 concentrated read as in the weekly panel above. A
  # practitioner relying on winner_stability() alone would get a falsely reassuring signal here
  # on top of (not instead of) the separate small-T VERDICT caveat -- the two signals are
  # comprehensible together (they answer different questions) but a user would need to know to
  # check BOTH decision_breakdown() and winner_stability(), since neither alone tells the full
  # story and report() only surfaces the decision_breakdown()-side one.
  expect_gt(ws$stability, 0.99)
  expect_gt(ws$stability, bench$p05)
})

# --- NEW SCENARIO for check (2): an 8-10 catastrophe, longer multi-decade panel ----------------
# A 16-season (2009-2024), weekly-cadence panel gives T = 352 (comfortably in the "large T, small
# catastrophic fraction" regime the task asks for -- T/K = 88, nowhere near the small-T
# threshold) with 9 distinct, named historical Atlantic hurricanes spread across the window
# (Irene 2011, Sandy 2012, Matthew 2016, Harvey 2017, Michael 2018, Dorian 2019, Laura 2020, Ida
# 2021, Ian 2022) -- nearly double the prior round's 5-event panel, to see whether
# concentration_share()'s already-confirmed "understates multi-event concentration" limitation
# gets proportionally WORSE as the catastrophe count grows, which the prior round (fixed at
# exactly 5 events) could not measure.
years9 <- 2009:2024
weeks_per_season9 <- weeks_per_season
T9 <- length(years9) * weeks_per_season9  # 352
labels9 <- unlist(lapply(years9, function(y) paste0(y, "-W", sprintf("%02d", 1:weeks_per_season9))))
idx_of9 <- function(year, week) as.integer((which(years9 == year) - 1L) * weeks_per_season9 + week)

catastrophes9 <- list(
  list(year = 2011, week = 14, name = "Hurricane Irene (2011)"),
  list(year = 2012, week = 20, name = "Hurricane Sandy (2012)"),
  list(year = 2016, week = 15, name = "Hurricane Matthew (2016)"),
  list(year = 2017, week = 12, name = "Hurricane Harvey (2017)"),
  list(year = 2018, week = 18, name = "Hurricane Michael (2018)"),
  list(year = 2019, week = 13, name = "Hurricane Dorian (2019)"),
  list(year = 2020, week = 13, name = "Hurricane Laura (2020)"),
  list(year = 2021, week = 13, name = "Hurricane Ida (2021)"),
  list(year = 2022, week = 17, name = "Hurricane Ian (2022)")
)
cat_idx9 <- sort(vapply(catastrophes9, function(cc) idx_of9(cc$year, cc$week), integer(1)))

build_weather_panel9 <- function(seed = 20261118L) {
  set.seed(seed)
  L <- stats::setNames(
    lapply(models, function(m) pmax(stats::rnorm(T9, mean = 1.0, sd = 0.05), 0.1)),
    models
  )
  for (cc in catastrophes9) {
    i <- idx_of9(cc$year, cc$week)
    L$climatology[i]  <- stats::runif(1, 11, 14)
    L$ar1_regional[i] <- stats::runif(1, 10, 13)
    L$ensemble_nwp[i] <- stats::runif(1, 1.0, 1.6)
    L$ml_nowcast[i]   <- stats::runif(1, 12, 15)
  }
  loss_panel(L, labels = labels9)
}

test_that("CHECK (2), NEW: concentration_share()'s already-confirmed understatement of multi-event concentration gets WORSE, proportionally, as the catastrophe count grows from 5 to 9", {
  panel9 <- build_weather_panel9()
  champ9 <- pooled_winner(panel9$losses, panel9$weights)
  expect_identical(champ9, "ensemble_nwp")

  db9 <- suppressWarnings(decision_breakdown(panel9, a = champ9, return_ties = TRUE))
  # On this longer, 9-catastrophe panel, k* lands EXACTLY on the 9 named catastrophes -- a
  # cleaner, 1-to-1 mapping than the prior round's 5-event weekly panel (which needed 9 periods,
  # 5 catastrophes + 4 ordinary weeks, to flip the decision). Both are legitimate: k* answers an
  # adversarial-removal question, not "how many named catastrophes are there," and here they
  # happen to coincide exactly.
  expect_identical(db9$k, 9L)
  expect_identical(sort(db9$removed), cat_idx9)

  la9 <- panel9$losses[[champ9]]; lb9 <- panel9$losses[[db9$binding_opponent]]
  contrib9 <- lb9 - la9
  cs9 <- concentration_share(panel9)
  multi_ratio9 <- sum(abs(contrib9[cat_idx9])) / sum(abs(contrib9))

  # Same qualitative pattern as the prior round's 5-event panel: concentration_share() (looking
  # only at the single largest period) reports a modest number, while the 9 catastrophes TOGETHER
  # account for the large majority of gross margin dispersion.
  expect_lt(cs9, 0.15)
  expect_gt(multi_ratio9, 0.75)

  # --- THE NEW MEASUREMENT: does the understatement get worse as catastrophe count grows? ------
  # Recompute the SAME ratio -- concentration_share() / (true multi-event dispersion share) -- on
  # the prior round's own 5-event panel (reusing its exact `build_weather_panel()`/`cat_idx`/
  # champion-vs-ar1_regional construction, unchanged) for a like-for-like comparison.
  panel5 <- build_weather_panel()
  champ5 <- pooled_winner(panel5$losses, panel5$weights)
  la5 <- panel5$losses[[champ5]]; lb5 <- panel5$losses[["ar1_regional"]]
  contrib5 <- lb5 - la5
  cs5 <- concentration_share(panel5)
  multi_ratio5 <- sum(abs(contrib5[cat_idx])) / sum(abs(contrib5))

  ratio5 <- cs5 / multi_ratio5  # ~0.22: how much of the TRUE multi-event concentration a reader
  ratio9 <- cs9 / multi_ratio9  # ~0.12: sees via concentration_share() alone, at 5 vs. 9 events

  # Going from 5 to 9 roughly-comparable catastrophes, concentration_share() captures a SMALLER
  # fraction of the true multi-event concentration, not the same fraction -- i.e. the
  # already-known understatement is not a fixed-size blind spot, it widens as the book of
  # business accumulates more distinct catastrophe types. This is new information the prior
  # round's single fixed-N=5 construction could not have revealed, and it matters practically:
  # the MORE historically catastrophe-exposed a cat program's loss history is, the LESS
  # representative concentration_share() becomes of its true concentration.
  expect_lt(ratio9, ratio5)
  expect_lt(ratio9, 0.75 * ratio5)  # not just smaller -- at least a 25% relative widening of the gap
})

test_that("CHECK (3), NEW: report()'s warning-dedup fix correctly handles both new panels (no literal duplicate warning text, no duplicated disclosure lines)", {
  # The small-T + multi-catastrophe annual panel: dedup already checked above (exactly 1 warning
  # fires even though .run_mcs_arch() is called from two internal sites in report()). Reconfirmed
  # here with an explicit no-duplicates assertion in the dedup-focused idiom.
  warns_annual <- testthat::capture_warnings(report(build_annual_smallT_panel(), alpha = 0.10, seed = 1))
  expect_equal(length(warns_annual), length(unique(warns_annual)))
  expect_length(warns_annual, 1)

  # The 9-catastrophe, T=352 panel is NOT small-T (T/K=88), so no small-T warning is expected at
  # all here -- but report() still runs two internal MCS call sites, so this is a genuine,
  # different check of the same dedup mechanism: whatever warnings (if any) DO fire are not
  # duplicated word-for-word.
  warns9 <- testthat::capture_warnings(rpt9 <- report(build_weather_panel9(), alpha = 0.10, seed = 1))
  expect_equal(length(warns9), length(unique(warns9)))

  # And the always-printed autocorrelation disclosure in the report BODY (a separate mechanism
  # from the warning dedup -- it isn't a `warning()` at all, it's unconditionally appended text)
  # is itself not duplicated within one report() call on this new, larger panel.
  expect_equal(lengths(gregexpr("assumes no autocorrelation", rpt9)), 1L)
})
