# test-realworld-healthcare.R
#
# A genuinely realistic HEALTHCARE-OPERATIONS forecasting use case, as a lasting regression test
# and domain-grounding example -- distinct from this package's other examples (macro/ARIMA-style
# loss panels), and distinct from population-level epidemiological case-count forecasting.
#
# THE USE CASE: a hospital operations-analytics team compares 4 competing models that forecast
# daily ED (emergency department) visit volume, over ~2.5 years of daily data (T=900, within the
# realistic 730-1000 day range for a multi-year hospital operations dataset):
#   1. dow_holiday_baseline -- a day-of-week/holiday-adjusted climatology, blind to flu and weather
#   2. weather_linked       -- adds extreme-heat/cold adjustment, cruder on dow/holiday, blind to flu
#   3. flu_surveillance     -- tracks the annual flu-season surge (with realistic surveillance
#                              reporting lag), cruder on dow/holiday, blind to weather
#   4. ml_ensemble          -- blends all three signals well on average, but adds its own small
#                              idiosyncratic noise (a realistic "best-on-average, less robust
#                              period-to-period" property of ensembles)
# The decision this team is making -- which model to trust for NEXT flu season's ED staffing plan
# -- has direct patient-safety consequences if wrong (understaffing an ED during a flu surge) and
# direct cost consequences either way. This is exactly the kind of "high stakes / small margin for
# error" decision the package's own README singles out as the reason report()'s RESOLUTION section
# exists: "which model wins" is the wrong question on its own; "how confident should we be, given
# the stakes" is the right one.
#
# WHY THIS IS A REALISTIC HEALTHCARE PANEL, NOT A GENERIC ONE (grounded in well-documented
# hospital-operations/ED-crowding domain facts, not invented structure):
#   - STRONG DAY-OF-WEEK EFFECT: ED visit volume is reliably higher on Mondays (weekend backlog +
#     reduced weekend primary-care access) and at weekends, and lower midweek -- the classic
#     Monday-peak pattern documented in ED-crowding/capacity-planning literature (e.g. ACEP/IHI
#     capacity guidance). This is a structurally different kind of "day-of-week effect" than
#     retail's promotional calendar.
#   - HOLIDAY EFFECTS THAT MOVE TWO VOLUME STREAMS IN OPPOSITE DIRECTIONS: major holidays (New
#     Year's, July 4th, Thanksgiving, Christmas) show elevated ED utilization (injury,
#     alcohol-related, delayed care-seeking around closures) while elective-procedure volume
#     collapses (surgical schedules are blocked out) -- unlike a retail holiday, which is a single
#     demand spike in one direction.
#   - RECURRING ANNUAL FLU-SEASON SURGE: a smooth, repeating winter bump (here: a Gaussian bump
#     centered near day 45 of each 365-day year, SD~30 days, ~45% peak amplitude), modeling the
#     well-documented annual ED/inpatient flu-RSV-pneumonia surge hospitals plan staffing around
#     every single year -- a MILD, REGULAR seasonal pattern, deliberately NOT framed as a novel
#     multi-wave epidemic shock.
#   - AUTOCORRELATED FORECAST ERRORS DURING THE FLU WINDOW: day-to-day forecast error is simulated
#     as AR(1) with much higher persistence (rho=0.65) during the flu season than outside it
#     (rho=0.15) -- successive days of a surge genuinely share unmodeled local shocks (a
#     within-surge reporting lag, a local outbreak cluster), which is exactly the kind of
#     correlated-error-cluster this package's own resolution.R ALREADY documents as a live,
#     undisclosed-by-default risk to the R1 "resolved" gate (see .weighted_se()'s comment:
#     "Forecast loss differentials are canonically autocorrelated in practice... minimum_detectable
#     _edge()/resolution_report() can report more confidence... than the data actually supports").
#
# WHAT WE FOUND (documented here, not fixed in package code -- see rationale below):
#
#  (A) On the primary 4-model panel below (equal weights), report() and resolution_report() behave
#      CORRECTLY and CONSERVATIVELY: flu_surveillance narrowly beats ml_ensemble on pooled mean
#      loss (10.76 vs 10.90, a ~1.3% edge) but the observed edge (1.3%) falls far short of the
#      minimum_detectable_edge() at 80% power (~8.0%), so resolution_report()$resolved is FALSE and
#      report()'s RESOLUTION line correctly prints "cannot determine at this sample size" rather
#      than any confident-sounding verdict. VERDICT also says "not identified" (MCS size 2 of 4),
#      stably so across seeds 0-10 (not a borderline, seed-flippable call). This is exactly the
#      answer a hospital analyst needs before staffing next flu season off a 1.3% average-MAE edge.
#
#  (B) A SHARPER, HEALTHCARE-SPECIFIC READING OF PIVOT's k*: decision_breakdown() finds k*=4 --
#      only 4 of the 900 days, if removed, flip the pooled winner from flu_surveillance to
#      ml_ensemble. We checked WHICH 4 days: all four (day-of-year 49, 48, 67, 69, across the two
#      flu seasons in the panel) fall inside the annual flu-season window. "k*=4 of 900 periods" on
#      its own reads as "4 independent bad days would need to flip," but in this recurring-seasonal
#      domain those 4 days are NOT 4 independent draws -- they are clustered instances of the very
#      recurring phenomenon (the flu surge) the staffing decision is ABOUT, consistent with the
#      autocorrelation caveat above. A hospital analyst reading only the PIVOT line's headline
#      k*/T ratio (4/900 = 0.44%, sounds re-assuringly tiny) without also reading the named periods
#      themselves could come away more confident than warranted that the pick is robust. This
#      package already prints the named periods (report()'s PIVOT paragraph, k<=5 branch) -- the
#      finding here is that doing so, and specifically checking whether they cluster, is important
#      and worth deliberately doing in a recurring-seasonal domain like this one, not that the
#      printed k*/T ratio is wrong.
#
#  (C) A CONFIRMED, DOMAIN-REALISTIC INSTANCE OF AN ALREADY-DOCUMENTED GAP (see resolution_report()
#      case below): weighting periods by a REALISTIC per-day ED patient-observation count (which,
#      because flu season has more ED visits, is naturally higher during the flu window -- NOT an
#      adversarially engineered weight) produces weights with CV~0.12 that positively correlate
#      with the per-period loss differential's volatility (r~0.38). resolution_report()'s own
#      documented false-confidence guard (resolution.R, @section Assumption) only warns when
#      CV(w) > 0.3 AND that correlation > 0.2 -- our realistic hospital weight pattern clears the
#      correlation threshold but NOT the CV threshold, so NO warning fires even though the
#      "resolved" read it accompanies sits in a weight/volatility-correlation regime the package's
#      own code comments already measure as carrying an ~8.1% false-confidence rate ("...8.1% in
#      the unremarkable seasonal case" -- resolution.R, lines documenting the re-verification).
#      This is NOT a new bug: it is the package authors' own already-disclosed, deliberately-scoped
#      limitation (the CV>0.3 threshold is explicitly calibrated against a documented false-positive
#      rate on the warning itself, 13-38% depending on T), and this test does not change that
#      threshold. It is flagged here because this test's realistic hospital weighting lands exactly
#      in that documented gap, which is a useful, concrete, domain-grounded illustration of it --
#      see the SUGGESTION note in the test below.
#
#  (D) UX/COMMUNICATION QUESTION (not a code bug, flagged per the task's instructions): report()'s
#      plain "resolved" / "cannot determine at this sample size" wording is well-calibrated on
#      panel (A) above -- it does NOT overclaim. But because a hospital ED-staffing decision is
#      higher-stakes (patient-safety-adjacent) than a typical business forecast-selection decision,
#      a non-statistician operations manager reading a BARE "-- resolved" line (as happens in
#      scenario (C)) has no way to know, from report()'s own text alone, that the underlying power
#      calculation assumes i.i.d. loss differentials and weight-homogeneous variance -- two
#      assumptions this package's own comments say are commonly violated by realistic seasonal/
#      observation-count weighting (exactly this domain) without the built-in guard necessarily
#      firing. SUGGESTION (not a decision, not applied here): the package's documentation
#      (resolution_report()'s help page already carries the relevant @section Assumption text) could
#      additionally note, as a worked example, that high-stakes/patient-safety-adjacent callers may
#      want to treat "resolved" as a necessary-but-not-sufficient signal and additionally inspect
#      mcb_bound()'s wider simultaneous bound and pivot_agreement()'s seed-stability check before
#      acting on a single "resolved" read -- i.e. read "resolved" as "not yet shown to be
#      under-powered" rather than "confirmed with x% confidence," which is the more conservative of
#      the two readings the plain English could support. This is left as a suggestion for the
#      package maintainer to adopt or reject, per the task's instructions not to unilaterally change
#      report()'s printed text/logic for a single domain's interpretation needs.

build_er_panel <- function() {
  set.seed(20261118)

  T <- 900L  # ~2.5 years of daily ED volume data
  day <- 0:(T - 1)
  dow <- day %% 7

  # Day-of-week multiplier: Monday peak, midweek trough, weekend lift (index 1=Mon..7=Sun).
  dow_mult <- c(1.00, 1.12, 0.94, 0.90, 0.92, 0.97, 1.08)[dow + 1]

  # Holiday calendar: 5 major US holidays/year, ED spike + elective collapse.
  holiday_offsets <- c(0, 184, 270, 329, 358)
  holiday_days <- unique(as.vector(outer(holiday_offsets, 0:3, function(o, y) o + y * 365)))
  holiday_days <- holiday_days[holiday_days < T]
  is_holiday <- rep(FALSE, T); is_holiday[holiday_days + 1] <- TRUE
  holiday_ed_mult <- ifelse(is_holiday, 1.30, 1.00)

  # Recurring annual flu-season surge: smooth Gaussian bump, ~45% peak amplitude, centered near
  # day-of-year 45 (mid-January), SD~30 days -- a mild, REGULAR seasonal pattern recurring every
  # year, not a novel multi-wave epidemic shock.
  day_in_year <- day %% 365
  flu_center <- 45
  d_wrap <- pmin(abs(day_in_year - flu_center), 365 - abs(day_in_year - flu_center))
  flu_mult <- 1 + 0.45 * exp(-(d_wrap^2) / (2 * 30^2))
  in_flu_window <- flu_mult > 1.05

  # Sporadic extreme-weather events (not annual-periodic), the weather-linked model's specialty.
  set.seed(777)
  weather_centers <- sample(day, 18)
  weather_mult <- rep(1.0, T)
  for (c in weather_centers) {
    idx <- max(0, c - 1):min(T - 1, c + 1)
    weather_mult[idx + 1] <- weather_mult[idx + 1] * 1.25
  }

  baseline_volume <- 180
  true_volume <- baseline_volume * dow_mult * holiday_ed_mult * flu_mult * weather_mult

  # AR(1) observation noise, MORE PERSISTENT during the flu window -- the correlated-error-cluster
  # structure this package's resolution.R autocorrelation caveat is specifically about.
  set.seed(2026)
  rho <- ifelse(in_flu_window, 0.65, 0.15)
  eps <- numeric(T)
  eps[1] <- rnorm(1, 0, 6)
  for (t in 2:T) eps[t] <- rho[t] * eps[t - 1] + rnorm(1, 0, 6 * sqrt(1 - rho[t]^2))
  observed_volume <- true_volume + eps

  # Four models, each with a distinct, realistic blind spot (see file header).
  f_baseline <- baseline_volume * dow_mult * holiday_ed_mult
  f_weather <- baseline_volume * (0.5 + 0.5 * dow_mult) * holiday_ed_mult * weather_mult
  flu_signal_lag <- c(rep(1, 3), flu_mult[1:(T - 3)])  # ~3-day surveillance reporting lag
  f_flu <- baseline_volume * (0.5 + 0.5 * dow_mult) * holiday_ed_mult * flu_signal_lag
  set.seed(55)
  f_ensemble <- baseline_volume * dow_mult * holiday_ed_mult * (0.3 + 0.7 * flu_mult) *
    (0.5 + 0.5 * weather_mult) + rnorm(T, 0, 4)

  L <- list(
    dow_holiday_baseline = abs(observed_volume - f_baseline),
    weather_linked        = abs(observed_volume - f_weather),
    flu_surveillance      = abs(observed_volume - f_flu),
    ml_ensemble            = abs(observed_volume - f_ensemble)
  )
  list(L = L, day_in_year = day_in_year, in_flu_window = in_flu_window)
}

test_that("realistic ED-forecasting panel builds cleanly and has the expected leaderboard shape", {
  built <- build_er_panel()
  panel <- loss_panel(built$L)

  expect_equal(length(panel$labels), 900L)
  expect_equal(sort(panel$models), sort(c(
    "dow_holiday_baseline", "weather_linked", "flu_surveillance", "ml_ensemble"
  )))

  sm <- summary(panel)
  # The two flu/surveillance-aware models (flu_surveillance, ml_ensemble) should clearly
  # out-forecast the two blind-to-flu models (dow_holiday_baseline, weather_linked) on mean
  # absolute error -- the flu surge is the dominant source of volume variation in this panel,
  # so a model that tracks it wins decisively over one that doesn't.
  flu_aware_best <- min(sm$mean_loss[sm$model %in% c("flu_surveillance", "ml_ensemble")])
  flu_blind_best <- min(sm$mean_loss[sm$model %in% c("dow_holiday_baseline", "weather_linked")])
  expect_lt(flu_aware_best, flu_blind_best)
})

test_that(
  paste(
    "report()/resolution_report() correctly refuse to call the flu_surveillance-vs-ml_ensemble",
    "contest resolved at this sample size -- the realistic decision a hospital would face before",
    "staffing next flu season off only ~2.5 years of data"
  ),
  {
    built <- build_er_panel()
    panel <- loss_panel(built$L)

    rr <- resolution_report(panel)
    # flu_surveillance is the pooled-mean champion, narrowly ahead of ml_ensemble.
    expect_equal(rr$champion, "flu_surveillance")
    expect_equal(rr$binding_rival, "ml_ensemble")

    # STAKES-SENSITIVITY CHECK: the observed edge is small (around 1%) relative to the minimum
    # detectable edge at a conventional 80% power target -- i.e. this package correctly says the
    # data cannot yet support confidently staffing next flu season around flu_surveillance over
    # ml_ensemble. This is the central "resolved means something specific, don't over-read it"
    # assertion this test exists to pin: for a patient-safety-adjacent decision, UNDER-powered
    # (resolved == FALSE) is exactly what a careful hospital analyst should see here, and it is
    # exactly what the package reports -- a case where the plain-English verdict and the
    # underlying power calculation agree, with no gap for a non-statistician to be misled by.
    expect_false(rr$resolved)
    expect_true(rr$observed_edge < rr$mde)
    expect_true(rr$observed_edge < 0.03)   # observed edge is a couple of percent at most
    expect_true(rr$mde > 0.05)             # would need a much bigger edge to be detectable here

    out <- report(panel)
    expect_match(out, "cannot determine at this sample size")
    expect_match(out, "fragility read UNDETERMINED")

    # VERDICT (R2/MCS) should likewise refuse to call this identified, and -- important for a
    # recurring annual decision like flu-season staffing, where the same borderline panel shape
    # recurs every year -- this refusal should be STABLE across bootstrap seeds, not a coin flip
    # that happens to land on "not identified" for seed=0 alone. A seed-unstable "not identified"
    # would itself be a kind of fragility worth knowing about before trusting next year's rerun.
    seeds_identified <- vapply(0:9, function(s) isTRUE(identified(panel, alpha = 0.10, seed = s)),
                                logical(1))
    expect_false(any(seeds_identified))
  }
)

test_that(
  paste(
    "PIVOT's k* pivotal periods, in this recurring-seasonal panel, cluster inside the annual",
    "flu-season window -- illustrating the autocorrelation caveat concretely rather than",
    "abstractly: the '4 periods' that would flip the champion are not 4 independent draws, they",
    "are clustered instances of the very recurring phenomenon (flu season) the staffing decision",
    "is about, which is exactly the correlated-error-cluster risk resolution.R's own",
    "autocorrelation caveat warns about for domains like this one"
  ),
  {
    built <- build_er_panel()
    panel <- loss_panel(built$L)

    champ <- pooled_winner(panel$losses, panel$weights)
    db <- decision_breakdown(panel$losses, panel$weights, a = champ)
    expect_equal(champ, "flu_surveillance")
    expect_gt(db$k, 0)
    expect_lte(db$k, 10)  # a small, human-checkable k* (fragile in the ordinary sense too)

    pivotal_days <- sort(db$removed)
    pivotal_in_flu <- built$in_flu_window[pivotal_days + 1]
    # Every single pivotal period falls inside the flu-season window -- not a scattering of
    # independent "bad days." A reader who only sees report()'s headline "k*=k of 900 periods"
    # ratio, without separately checking whether the named periods cluster, could read this
    # fragility as smaller/more random than it actually is in a recurring-seasonal domain.
    expect_true(all(pivotal_in_flu))
  }
)

test_that(
  paste(
    "a realistic ED-observation-count weighting (naturally higher during flu season, since flu",
    "season has more ED visits to observe) lands in the package's own already-documented",
    "false-confidence gap: CV(weights) clears the correlation condition but not the CV>0.3",
    "threshold, so resolution_report()'s weight-heterogeneity warning does not fire even though",
    "the accompanying 'resolved' verdict sits in a regime the package's own code comments",
    "already measure as carrying a non-trivial (~8%) false-confidence rate for an 'unremarkable",
    "seasonal' weight pattern. This is a KNOWN, documented, deliberately-scoped package limitation",
    "(resolution.R's own @section Assumption) -- this test does not change the guard's threshold,",
    "it pins the realistic healthcare instance of the already-disclosed gap as a lasting,",
    "concrete domain example (see this file's header, finding (C), for the full discussion and a",
    "flagged documentation SUGGESTION, not a code change)"
  ),
  {
    set.seed(31)
    T <- 900L
    day <- 0:(T - 1)
    day_in_year <- day %% 365
    flu_center <- 45
    d_wrap <- pmin(abs(day_in_year - flu_center), 365 - abs(day_in_year - flu_center))
    flu_mult <- 1 + 0.45 * exp(-(d_wrap^2) / (2 * 30^2))
    in_flu <- flu_mult > 1.05

    # Realistic per-day ED-patient observation-count weights: baseline ~150 observed cases/day,
    # rising toward ~210/day at flu peak -- i.e. derived directly from the SAME flu curve as the
    # domain simulation above, not an adversarially engineered weight vector.
    weights <- round(150 + 60 * (flu_mult - 1) / 0.45)

    champ <- rnorm(T, 10, 2)
    # The rival's extra error is concentrated (higher-variance) specifically during the flu
    # window -- again a realistic property (forecast noise is more volatile exactly when demand
    # is surging), not an engineered pathology.
    rival_extra_noise <- ifelse(in_flu, rnorm(T, 0, 8), rnorm(T, 0, 1))
    rival <- champ + 1.5 + rival_extra_noise
    L <- list(champ = abs(champ), rival = abs(rival))

    cv_w <- stats::sd(weights) / mean(weights)
    diff_ <- abs(rival) - abs(champ)
    resid2 <- (diff_ - mean(diff_))^2
    wv_cor <- suppressWarnings(stats::cor(weights, resid2))

    # Confirms the panel lands exactly in the documented gap: correlation condition satisfied,
    # CV condition NOT satisfied (a realistic flu-season weight swing is much milder than the
    # guard's CV>0.3 threshold was calibrated against).
    expect_gt(wv_cor, 0.2)
    expect_lt(cv_w, 0.3)

    rr <- withCallingHandlers(
      resolution_report(L, w = weights),
      warning = function(w) {
        fail(sprintf(
          "resolution_report() warned (\"%s\") on this realistic hospital weighting -- if the ",
          "weight-heterogeneity guard's threshold has been tightened since this test was written, ",
          "update this test's expectations (and re-read finding (C) in this file's header) rather ",
          "than treating this failure as evidence of a regression."
        ))
      }
    )
    expect_true(rr$resolved)
    expect_gt(rr$observed_edge, rr$significance_boundary)
  }
)
