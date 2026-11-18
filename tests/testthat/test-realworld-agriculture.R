# REALISTIC DOMAIN TEST: county-level corn-yield forecast model selection for a commodity
# trading desk / crop-insurance actuary.
#
# BUSINESS CONTEXT: a trading desk (or a crop-insurance underwriter pricing area-yield
# products) compares 4 competing yield-forecasting models used to anticipate county/regional
# corn yield ahead of harvest: a weather-regression model (cheap, no mechanistic drought-stress
# representation), a satellite/remote-sensing (NDVI-based) model (reacts to vegetative stress
# in near-real-time, though with some revisit-cadence lag), an agronomic process-based crop
# simulation model (DSSAT/APSIM-style mechanistic physiology, strong on "normal" weather but
# can be mis-specified under genuinely extreme/novel anomaly inputs), and a ML ensemble
# blending weather+satellite+soil features (best raw accuracy once enough training data has
# accumulated, i.e. subject to a genuine technological regime shift over the sample).
#
# GENUINELY-PERMANENT SMALL T: crop yield is observed once per growing season. 25 years
# (2000-2024) of annual county-level forecast-error history is a REALISTIC, even generous,
# back-test window for this domain -- not a stress edge case. Unlike a transient small-T
# problem, "collect more data" is not a realistic remedy within a human career span: T will
# grow by at most ~1 observation per year forever. T=25, K=4 gives T/K=6.25, below this
# package's own small-sample warning threshold (T/K<10), confirmed below to fire.
#
# SHARED SHOCKS (weather-driven, correlated across ALL models): three real, recent US
# Midwest/Corn-Belt extreme-weather years fall inside this window and are modeled as a shared,
# common-direction shock hitting every model's forecast error simultaneously, on top of each
# model's own idiosyncratic skill at handling it:
#   2002 -- regional drought (moderate shared shock)
#   2012 -- severe, widespread Midwest drought (worst since 1988) -- the largest shared shock
#   2019 -- historic wet spring / record prevent-plant flooding across the Corn Belt
# This is the textbook "every model tends to miss in the same direction in an extreme weather
# year" mechanism: a common additive term hits weather_reg/satellite/agronomic/ml_ensemble
# alike, with model-specific skill layered on top (satellite handles stress detection best;
# agronomic simulation is specifically mis-specified under the most extreme anomaly inputs;
# weather_reg and ml_ensemble are hit hardest of all).
#
# REGIME SHIFT: the ML ensemble is deliberately built with a structural break at 2012 (a
# genuine agronomic/technological change point -- more satellite/soil-sensor training data,
# precision-ag adoption) -- mediocre skill in the first 12 years, better after, consistent with
# the "new variety/practice shift" mechanism named in this domain's brief.
#
# BUSINESS STAKES: a trading desk or actuary choosing next season's forecasting model on the
# strength of a 25-year back-test needs to know whether that choice is a real, durable skill
# edge or an artifact of 2-3 extreme weather years inside an otherwise-small sample.

test_that("realistic 25-year, 4-model corn-yield panel: small-sample warning fires and VERDICT/RESOLUTION correctly refuse to overclaim", {
  years <- 2000:2024
  T <- length(years)
  shock_years <- c(2002, 2012, 2019)
  shock_idx <- which(years %in% shock_years)

  set.seed(7)
  shared_shock <- numeric(T)
  shared_shock[shock_idx] <- c(1.6, 3.0, 2.0)  # common weather-driven miss, same direction, all models

  idio <- function(sd) rnorm(T, 0, sd)

  weather_reg_skill <- rep(1.2, T)              # consistently mediocre, no drought mechanism
  satellite_skill    <- rep(0.0, T)
  agronomic_skill    <- rep(0.0, T)
  ml_skill           <- c(rep(1.0, 12), rep(-0.3, T - 12))  # regime shift at 2012

  satellite_skill[shock_idx]    <- c(0.3, 0.4, 0.3)   # NDVI catches stress fast -> small extra miss
  agronomic_skill[shock_idx]    <- c(1.3, 2.0, 1.1)   # simulation mis-specified under the anomaly
  weather_reg_skill[shock_idx]  <- weather_reg_skill[shock_idx] + c(1.0, 1.8, 1.2)
  ml_skill[shock_idx]           <- ml_skill[shock_idx] + c(0.6, 0.9, 0.7)

  base <- 8.0
  L <- list(
    weather_reg = pmax(0.1, base + shared_shock + weather_reg_skill + idio(0.5)),
    satellite   = pmax(0.1, base + shared_shock + satellite_skill   + idio(0.5)),
    agronomic   = pmax(0.1, base + shared_shock + agronomic_skill   + idio(0.5)),
    ml_ensemble = pmax(0.1, base + shared_shock + ml_skill          + idio(0.5))
  )
  panel <- loss_panel(L, labels = years)

  # --- small-T warning: T/K=6.25 is below the package's own T/K<10 threshold, and this is a
  # PERMANENT feature of annual crop-yield data, not a transient one. The warning should fire,
  # and it should NOT prescribe "collect more data" as a remedy (not realistic for this domain) --
  # it should instead say to interpret the result accordingly. Confirm both.
  expect_warning(
    report(panel),
    "T=25 periods for K=4 models \\(T/K=6\\.2\\)"
  )
  w_msg <- tryCatch({ report(panel); NA_character_ }, warning = function(w) conditionMessage(w))
  expect_false(grepl("collect more data", w_msg, ignore.case = TRUE))
  expect_match(w_msg, "small-T panels are supported")
  expect_match(w_msg, "interpret the result accordingly")

  rpt <- withCallingHandlers(report(panel), warning = function(w) invokeRestart("muffleWarning"))

  # satellite is the apparent champion by pooled mean, but with only K=4 and heavy shock-year
  # noise, the MCS does not uniquely identify it (ties with agronomic and ml_ensemble) --
  # exactly the honest, un-hedged signal this package exists to give a trading desk.
  expect_match(rpt, "VERDICT: not identified \\(MCS size 3 of 4\\)")
  expect_match(rpt, "satellite +mean=8\\.3[0-9]+ +in MCS +periods won=[0-9]+ \\(champion\\)")
  expect_match(rpt, "weather_reg +mean=9\\.8[0-9]+ +excluded")

  # RESOLUTION: the observed pooled-mean edge is nowhere near what 25 years of once-a-year
  # observations can reliably detect at 80% power -- the trading desk's "our model wins" read
  # is NOT something this sample size can actually certify.
  expect_match(rpt, "cannot determine at this sample size")

  # PIVOT: k* is small relative to T (a handful of periods flips the pooled winner) -- the
  # champion's apparent edge is fragile, not durable, at this sample size.
  expect_match(rpt, "k\\*=4 of 25 periods")
  expect_match(rpt, "fragility read UNDETERMINED")
})

test_that("decision_breakdown()/prop22_certifies(): the drought/flood years are IMPLICATED in k* but do not fully explain it -- an honest, not a toy-clean, finding", {
  years <- 2000:2024
  T <- length(years)
  shock_years <- c(2002, 2012, 2019)
  shock_idx <- which(years %in% shock_years)

  set.seed(7)
  shared_shock <- numeric(T)
  shared_shock[shock_idx] <- c(1.6, 3.0, 2.0)
  idio <- function(sd) rnorm(T, 0, sd)

  weather_reg_skill <- rep(1.2, T)
  satellite_skill    <- rep(0.0, T)
  agronomic_skill    <- rep(0.0, T)
  ml_skill           <- c(rep(1.0, 12), rep(-0.3, T - 12))

  satellite_skill[shock_idx]    <- c(0.3, 0.4, 0.3)
  agronomic_skill[shock_idx]    <- c(1.3, 2.0, 1.1)
  weather_reg_skill[shock_idx]  <- weather_reg_skill[shock_idx] + c(1.0, 1.8, 1.2)
  ml_skill[shock_idx]           <- ml_skill[shock_idx] + c(0.6, 0.9, 0.7)

  base <- 8.0
  L <- list(
    weather_reg = pmax(0.1, base + shared_shock + weather_reg_skill + idio(0.5)),
    satellite   = pmax(0.1, base + shared_shock + satellite_skill   + idio(0.5)),
    agronomic   = pmax(0.1, base + shared_shock + agronomic_skill   + idio(0.5)),
    ml_ensemble = pmax(0.1, base + shared_shock + ml_skill          + idio(0.5))
  )
  panel <- loss_panel(L, labels = years)

  db <- suppressWarnings(decision_breakdown(panel, return_ties = TRUE))
  expect_equal(db$k, 4L)
  expect_equal(db$binding_opponent, "agronomic")

  removed_labels <- panel$labels[db$removed]
  # 2012 (the worst drought) and 2019 (the flood year) are both in the minimal flipping set --
  # the two biggest named shared shocks DO drive a meaningful share of k*.
  expect_true(2012 %in% removed_labels)
  expect_true(2019 %in% removed_labels)
  # But the set is NOT purely the 3 named shock years: 2002 is absent and two ordinary years
  # (2003, 2018) are present instead. This is the honest, domain-realistic finding -- shared
  # weather shocks are IMPLICATED in this champion's fragility, not the full story. A trading
  # desk that assumed "it's just the drought years" would be only half right.
  expect_false(2002 %in% removed_labels)
  expect_true(any(!(removed_labels %in% shock_years)))

  # Prop 2.2's zero-simulation certificate: k*=4 at T=25 does NOT certify non-significance --
  # i.e. the package's fast check agrees with VERDICT/RESOLUTION that this is a live, unresolved
  # question, not a free pass either way.
  cert <- prop22_certifies(db$k, T = T)
  expect_identical(cert, FALSE)

  # Confirms k* is doing real work: the pooled winner does NOT simply flip once you throw out
  # all 3 named shock years outright (that specific 3-period subset is not db$removed's set) --
  # it takes db$removed's exact minimal combination, underscoring that the fragility is a
  # genuine small-T combinatorial fact about this panel, not just "drop the bad weather years."
  keep <- !(years %in% shock_years)
  panel_noshock <- loss_panel(lapply(L, function(x) x[keep]), labels = years[keep])
  expect_identical(pooled_winner(panel), pooled_winner(panel_noshock))
})

test_that("resolution_report(): R1/R2 both correctly refuse to call this 25-year panel resolved", {
  years <- 2000:2024
  T <- length(years)
  shock_years <- c(2002, 2012, 2019)
  shock_idx <- which(years %in% shock_years)

  set.seed(7)
  shared_shock <- numeric(T)
  shared_shock[shock_idx] <- c(1.6, 3.0, 2.0)
  idio <- function(sd) rnorm(T, 0, sd)

  weather_reg_skill <- rep(1.2, T)
  satellite_skill    <- rep(0.0, T)
  agronomic_skill    <- rep(0.0, T)
  ml_skill           <- c(rep(1.0, 12), rep(-0.3, T - 12))

  satellite_skill[shock_idx]    <- c(0.3, 0.4, 0.3)
  agronomic_skill[shock_idx]    <- c(1.3, 2.0, 1.1)
  weather_reg_skill[shock_idx]  <- weather_reg_skill[shock_idx] + c(1.0, 1.8, 1.2)
  ml_skill[shock_idx]           <- ml_skill[shock_idx] + c(0.6, 0.9, 0.7)

  base <- 8.0
  L <- list(
    weather_reg = pmax(0.1, base + shared_shock + weather_reg_skill + idio(0.5)),
    satellite   = pmax(0.1, base + shared_shock + satellite_skill   + idio(0.5)),
    agronomic   = pmax(0.1, base + shared_shock + agronomic_skill   + idio(0.5)),
    ml_ensemble = pmax(0.1, base + shared_shock + ml_skill          + idio(0.5))
  )
  panel <- loss_panel(L, labels = years)

  rr <- suppressWarnings(resolution_report(panel, alpha = 0.05, power = 0.80))
  expect_identical(rr$champion, "satellite")
  expect_false(rr$resolved)              # R1 refuses: edge not powered at T=25
  expect_false(rr$identified)            # R2 refuses: MCS ties satellite with 2 rivals
  expect_identical(rr$fragility_read, "undetermined")
  expect_identical(rr$binding_rival, "agronomic")

  # The observed edge is roughly half the minimum detectable edge at 80% power -- a trading
  # desk reading only the pooled mean ("satellite wins by X%") would be overclaiming relative
  # to what 25 once-a-year observations can actually support.
  expect_lt(rr$observed_edge / rr$mde, 0.6)
})

test_that("mcs()/identified(): the small-sample warning threshold (T/K<10) fires at this domain's realistic T/K=6.25, between the package's own documented T=30/K=3 (~18%) and T=12/K=4 (~41%) false-elimination anchors", {
  # DOCUMENTED ANCHORS (mcs.R / identify.R roxygen, confirmed by this package's prior
  # domain-review rounds on other industries): under a TRUE complete null (all K models i.i.d.,
  # equal mean, zero autocorrelation), measured false-elimination rate at alpha=0.10 is ~18% at
  # T=30/K=3 (T/K=10) and ~41% at T=12/K=4 (T/K=3).
  #
  # THIS TEST asks the question specific to this domain's REALISTIC, PERMANENT cadence: T=25
  # annual observations, K=4 competing models (T/K=6.25), a point strictly between the two
  # documented anchors. The warning must fire (confirmed), and the false-elimination rate should
  # land between the two anchors, consistent with a monotonic T/K relationship rather than an
  # unrelated, surprising value at this new point. Loose threshold (directional regression
  # guard, not a tight pin on a bootstrap-seeded statistic) -- measured here at ~27% over 150
  # replications, comfortably between 18% and 41% and well above the nominal alpha=0.10.
  K <- 4; T <- 25; alpha <- 0.10; n_rep <- 150

  expect_warning(
    mcs(matrix(rnorm(T * K, 1, 0.3), nrow = T, ncol = K), alpha = alpha, seed = 0),
    "T=25 periods for K=4 models \\(T/K=6\\.2\\)"
  )

  false_elim <- 0L
  for (i in seq_len(n_rep)) {
    set.seed(i)
    Lmat <- matrix(rnorm(T * K, mean = 1, sd = 0.3), nrow = T, ncol = K)
    m <- suppressWarnings(mcs(Lmat, alpha = alpha, seed = i))
    if (length(m$surviving) < K) false_elim <- false_elim + 1L
  }
  rate <- false_elim / n_rep

  expect_gt(rate, 0.15)   # above nominal alpha and above a trivial-noise floor
  expect_lt(rate, 0.45)   # at or below the worse T=12/K=4 anchor, not a runaway outlier
})
