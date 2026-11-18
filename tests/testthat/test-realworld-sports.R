# test-realworld-sports.R — professional basketball analytics scenario: comparing competing
# single-game point-spread/win-probability forecasting models across a FIXED, bounded season
# structure, with a genuine mid-season roster shock and a multi-season rolling re-evaluation.
#
# Real-world grounding for this scenario (distinct from every other domain tested in this
# package so far):
#   - GENUINE, BOUNDED SEASON STRUCTURE: an NBA season is exactly 82 games -- a fixed, known T
#     per season, unlike open-ended calendar time (daily/weekly ticks with no natural stopping
#     point). Three full seasons give T = 246, built from real season-boundary period labels
#     ("2021-G1", ..., "2021-G82", "2022-G1", ...) rather than an abstract index.
#   - MID-SEASON ROSTER SHOCK: a significant trade/injury event partway through season 2 (game
#     40 of 82, global period 122) causes an ABRUPT, PERSISTENT shift in the team's true
#     performance distribution -- the team the models are forecasting changed, not the models
#     themselves (distinct from tech-capacity's model-churn scenario, where the MODEL roster
#     changes; here the roster changes under a fixed set of models). A short chaotic
#     adjustment window (6 games) follows the event itself, then a new, persistent performance
#     level holds for the remainder of season 2 and all of season 3.
#   - FOUR COMPETING MODELS: a simple Elo-rating model (slow to adapt, no mechanism to ingest
#     roster news), an advanced-statistics regression model (moderately slow to adapt), a
#     Vegas-line-anchored baseline (the market re-prices with a lag but is not instantaneous),
#     and an ML model using player-tracking data (recalibrates fastest because it directly
#     observes the new lineup's on-court behavior) -- the realistic reason ml_tracking should
#     win the post-trade era even though it is not clearly best pre-trade.
#   - MULTI-SEASON COMPARISON + ROLLING RE-EVALUATION: a single season's T=82 is thin for this
#     diagnostic, so the base panel spans three seasons; the front office then re-evaluates at
#     the start of season 4 with a realistic rolling window (seasons 2-4, i.e. season 1 drops
#     off and season 4 is appended) that itself still spans the original trade event.
#
# What this file checks (the open questions in the brief):
#   1. loss_panel()/pooled_winner()/decision_breakdown() handle the season-boundary labeling
#      scheme correctly -- NOT confused by the boundary itself, and will correctly REJECT a
#      naive labeling convention that reuses "G1".."G82" every season (not globally unique).
#   2. decision_breakdown()'s k* concentrates on the roster-change-driven (post-trade) periods,
#      not spuriously on the season-boundary games themselves.
#   3. report() produces a coherent, non-crashing one-screen verdict on this 246-period, 4-model
#      panel.
#   4. compare() correctly handles the front office's rolling-window re-evaluation at the start
#      of the next season, spanning the original trade event, including correct new-period
#      attribution (season 4 is 82 genuinely new periods).

.sports_n_season <- 82L
.sports_n_seasons <- 3L
.sports_T <- .sports_n_season * .sports_n_seasons  # 246
.sports_trade_idx <- .sports_n_season * 1L + 40L   # mid season-2, game 40 -> global period 122

.sports_build_labels <- function(seasons = 2021:2023, n_season = .sports_n_season) {
  s <- rep(seasons, each = n_season)
  g <- rep(seq_len(n_season), times = length(seasons))
  paste0(s, "-G", g)
}

# Builds the base 3-season, 4-model panel with a mid-season-2 trade/injury shock (global period
# 122 = "2022-G40"): a short chaotic adjustment window (6 games, everyone's forecast error
# spikes), then a persistent new performance level for the rest of season 2 and all of season 3.
# ml_tracking (player-tracking data) adapts fastest and degrades the least post-trade; elo,
# advstats, and vegas all carry a lasting, larger penalty because they lack a fast mechanism to
# ingest the roster change.
.sports_build_base_panel <- function(seed = 42) {
  set.seed(seed)
  T <- .sports_T
  labels <- .sports_build_labels()
  trade_idx <- .sports_trade_idx

  elo         <- pmax(8.5 + rnorm(T, 0, 1.5), 0.1)
  advstats    <- pmax(7.5 + rnorm(T, 0, 1.3), 0.1)
  ml_tracking <- pmax(7.0 + rnorm(T, 0, 1.4), 0.1)
  vegas       <- pmax(6.5 + rnorm(T, 0, 1.2), 0.1)

  shock_idx <- trade_idx:(trade_idx + 5L)
  elo[shock_idx]         <- elo[shock_idx]         + runif(length(shock_idx), 8, 14)
  advstats[shock_idx]    <- advstats[shock_idx]    + runif(length(shock_idx), 6, 11)
  vegas[shock_idx]       <- vegas[shock_idx]       + runif(length(shock_idx), 5, 9)
  ml_tracking[shock_idx] <- ml_tracking[shock_idx] + runif(length(shock_idx), 2, 4)

  post_idx <- (trade_idx + 6L):T
  elo[post_idx]         <- elo[post_idx]         + 5.0
  vegas[post_idx]       <- vegas[post_idx]       + 3.0
  advstats[post_idx]    <- advstats[post_idx]    + 4.0
  ml_tracking[post_idx] <- ml_tracking[post_idx] + 0.1

  L <- list(elo = elo, advstats = advstats, ml_tracking = ml_tracking, vegas = vegas)
  loss_panel(L, labels = labels)
}

test_that("a season-boundary labeling scheme that reuses per-season game numbers is correctly rejected", {
  # A tempting but WRONG labeling convention: label every season's games "G1".."G82" again,
  # relying on positional order alone to disambiguate seasons. loss_panel() requires globally
  # unique labels (its one identity key for compare()'s new-period attribution), so this must be
  # rejected with a clear, actionable message -- not silently accepted with season 2's "G1"
  # colliding with season 1's "G1".
  set.seed(1)
  n <- .sports_T
  L <- list(a = rnorm(n, 1, 0.1), b = rnorm(n, 1.05, 0.1))
  bad_labels <- rep(paste0("G", seq_len(.sports_n_season)), .sports_n_seasons)

  err <- tryCatch(loss_panel(L, labels = bad_labels), error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "duplicate", fixed = TRUE)
  # The correct convention -- a season-qualified label ("2021-G1", not bare "G1") -- must work.
  good_labels <- .sports_build_labels()
  expect_length(unique(good_labels), n)
  expect_silent(loss_panel(L, labels = good_labels))
})

test_that("the base 3-season panel builds cleanly with real season-boundary labels", {
  panel <- .sports_build_base_panel()
  expect_s3_class(panel, "loss_panel")
  expect_identical(length(panel$labels), .sports_T)
  expect_identical(panel$labels[1], "2021-G1")
  expect_identical(panel$labels[.sports_n_season], "2021-G82")
  expect_identical(panel$labels[.sports_n_season + 1L], "2022-G1")
  expect_identical(panel$labels[.sports_trade_idx], "2022-G40")
  expect_identical(panel$labels[.sports_T], "2023-G82")
  expect_setequal(names(panel$losses), c("elo", "advstats", "ml_tracking", "vegas"))
})

test_that("decision_breakdown()'s k* attributes the decision to the roster-change era, not to the season boundaries themselves", {
  # The front office's real question: is the model choice driven by the genuine roster-change
  # mechanism (post-trade era), or by some artifact of how the panel happens to be chunked into
  # seasons? decision_breakdown() has no concept of "season" at all -- it only sees a flat list
  # of period labels and losses -- so this checks that the diagnostic's purely statistical
  # concentration measure nonetheless lines up with the real-world mechanism, and does NOT
  # spuriously fixate on the four season-boundary games.
  panel <- .sports_build_base_panel()
  champ <- pooled_winner(panel)
  # ml_tracking's faster adaptation to the roster change should make it the pooled champion,
  # even though it is not dominant in every single pre-trade period.
  expect_identical(champ, "ml_tracking")

  db <- decision_breakdown(panel$losses, panel$weights, a = champ)
  expect_true(db$k >= 1)
  expect_true(db$k < .sports_T)

  # The large majority of the periods decision_breakdown() must remove to flip the decision
  # should fall in the post-trade era (global period >= trade_idx) -- i.e. the diagnostic is
  # correctly attributing fragility to the roster-change mechanism, not to noise spread evenly
  # across the whole 3-season panel.
  frac_post_trade <- mean(db$removed >= .sports_trade_idx)
  expect_gt(frac_post_trade, 0.75)

  # The season-1/2 boundary (last game of season 1, first game of season 2: positions 82, 83)
  # PREDATES the trade event (period 122) and carries no special meaning of its own -- it is an
  # ordinary pre-trade pair of games. If decision_breakdown() were confusing "a new season
  # starts here" with "something structurally important happened here," both of these would be
  # disproportionately likely to show up among the removed periods. (The season-2/3 boundary,
  # positions 164-165, is deliberately NOT checked the same way: it falls inside the genuinely
  # fragile post-trade era, so its games are expected to be ordinary members of that era's
  # removed set, not evidence of boundary-specific confusion.)
  pre_trade_boundary <- c(.sports_n_season, .sports_n_season + 1L)
  n_pre_trade_boundary_in_removed <- sum(db$removed %in% pre_trade_boundary)
  expect_lte(n_pre_trade_boundary_in_removed, 1L)
})

test_that("report() produces a coherent one-screen verdict on the 3-season, 4-model sports panel", {
  panel <- .sports_build_base_panel()
  rep_txt <- report(panel)
  expect_s3_class(rep_txt, "selectionfragility_report")
  expect_match(rep_txt, "VERDICT", fixed = TRUE)
  expect_match(rep_txt, "LEADERBOARD", fixed = TRUE)
  expect_match(rep_txt, "RESOLUTION", fixed = TRUE)
  expect_match(rep_txt, "PIVOT", fixed = TRUE)
  # ml_tracking should show up as the champion in the leaderboard text.
  expect_match(rep_txt, "ml_tracking.*\\(champion\\)", perl = TRUE)
  # The post-trade advantage is large and persistent enough across two full seasons' worth of
  # periods that this panel should be a genuinely RESOLVED, identified decision -- the front
  # office gets a real answer, not an "undetermined" one, which is the whole point of running
  # three seasons instead of one.
  expect_match(rep_txt, "VERDICT: identified", fixed = TRUE)
  expect_match(rep_txt, "resolved", fixed = TRUE)
})

test_that("compare() handles the front office's rolling multi-season re-evaluation at the start of next season, spanning the trade event", {
  # The front office's real workflow: going into season 4, re-run the comparison on a rolling
  # window that drops the oldest season (2021) and appends the newest (2024) -- a same-length
  # (T=246) rolling window that still spans the original mid-season-2 trade event, exactly the
  # scenario compare()'s own documentation (R/compare.R) flags as needing real, non-positional
  # labels to attribute correctly (a purely positional panel would report n_new_periods=0 even
  # though season 2021 really did drop out and season 2024 really did come in).
  previous <- .sports_build_base_panel()
  L_prev <- previous$losses
  n_season <- .sports_n_season
  T <- .sports_T

  set.seed(99)
  season4_games <- seq_len(n_season)
  elo4      <- pmax(8.5 + rnorm(n_season, 0, 1.5), 0.1) + 5.0
  advstats4 <- pmax(7.5 + rnorm(n_season, 0, 1.3), 0.1) + 4.0
  ml4       <- pmax(7.0 + rnorm(n_season, 0, 1.4), 0.1) + 0.1
  vegas4    <- pmax(6.5 + rnorm(n_season, 0, 1.2), 0.1) + 3.0
  labels4   <- paste0("2024-G", season4_games)

  idx_season2plus <- (n_season + 1L):T  # drop season 2021, keep seasons 2022-2023
  model_names <- names(L_prev)
  new_vals <- list(elo = elo4, advstats = advstats4, ml_tracking = ml4, vegas = vegas4)
  L_curr <- stats::setNames(
    lapply(model_names, function(m) c(as.numeric(L_prev[[m]])[idx_season2plus], new_vals[[m]])),
    model_names
  )
  labels_curr <- c(previous$labels[idx_season2plus], labels4)
  current <- loss_panel(L_curr, labels = labels_curr)
  expect_identical(length(current$labels), T)  # same-length rolling window: T stays 246

  cr <- compare(previous, current)
  expect_s3_class(cr, "change_report")

  # Real, season-qualified labels let compare() correctly attribute the rolling window: season
  # 2021 (82 periods) dropped out, season 2024 (82 periods) came in -- net count of genuinely NEW
  # periods (present in current, absent from previous) is 82, exactly season 4's length.
  expect_identical(cr$n_new_periods, n_season)

  # ml_tracking's post-trade advantage persists into season 4 (same construction as the trade
  # era), so the champion should be unchanged by this rolling re-evaluation, and that should be
  # true whether or not the new season is included -- i.e. the front office's answer ("keep
  # trusting ml_tracking") does not depend on the brand-new season's data alone.
  expect_identical(cr$current_champion, "ml_tracking")
  expect_false(cr$champion_changed)
  expect_identical(cr$champion_without_new_periods, "ml_tracking")
  expect_false(cr$act)  # nothing actionable: no champion change for a promotion/alerting gate
})

test_that("decision_breakdown() does not systematically fixate on season-boundary games in a no-trade control panel", {
  # Control check: with NO roster-change mechanism at all (two models with stable, overlapping
  # distributions across the same 3-season/season-boundary-labeled structure), the periods
  # decision_breakdown() removes to flip the decision should not land on the season-boundary
  # games any more often than their 4-in-246 share of the panel would predict by chance alone.
  n_season <- .sports_n_season
  T <- .sports_T
  labels <- .sports_build_labels()
  boundary_positions <- c(n_season, n_season + 1L, 2L * n_season, 2L * n_season + 1L)

  set.seed(7)
  n_draws <- 25L
  boundary_hits <- 0L
  for (i in seq_len(n_draws)) {
    a <- rnorm(T, 7, 1.2)
    b <- rnorm(T, 7.3, 1.2)
    p <- loss_panel(list(a = a, b = b), labels = labels)
    champ <- pooled_winner(p)
    db <- decision_breakdown(p$losses, p$weights, a = champ)
    if (length(db$removed) > 0 && any(db$removed %in% boundary_positions)) boundary_hits <- boundary_hits + 1L
  }
  # A generous upper bound: boundary games hitting on every single draw would indicate a real
  # bias toward season-boundary positions; well under that confirms no such artifact.
  expect_lt(boundary_hits / n_draws, 0.6)
})
