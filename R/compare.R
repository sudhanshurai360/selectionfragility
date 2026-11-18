# compare.R — compare(previous, current), the run-over-run champion-change diagnostic.
# Faithful port of compare.py. `change_report()` builds an S3 object of class "change_report"
# (the R analogue of Python's ChangeReport); `.act` is the single field a CI/pipeline promotion
# step should gate on.

#' @keywords internal
.new_change_report <- function(previous_champion, current_champion, champion_changed,
                                champion_without_new_periods, n_new_periods,
                                old_champion_still_in_mcs, k_star, churn_base_rate, act,
                                mcs_error = NULL) {
  structure(list(
    previous_champion = previous_champion,
    current_champion = current_champion,
    champion_changed = champion_changed,
    champion_without_new_periods = champion_without_new_periods,
    n_new_periods = n_new_periods,
    old_champion_still_in_mcs = old_champion_still_in_mcs,
    k_star = k_star,
    churn_base_rate = churn_base_rate,
    act = act,
    mcs_error = mcs_error
  ), class = "change_report")
}

#' @keywords internal
.escape_champion_name <- function(name) {
  # STRICT-AUDIT FIX (2026-11-18): sibling bug to report.R's LEADERBOARD control-character fix
  # (same commit). print.change_report() interpolates previous_champion/current_champion into
  # its one-line sprintf() summary with a bare %s, with no escaping at all -- unlike
  # LEADERBOARD's per-model rows, there isn't even a collision-triggered escaping path here, so
  # ANY model name containing a literal newline/carriage-return (or other C0/DEL control
  # character) sails straight through unescaped. Confirmed directly: a model named
  # "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE_INJECTED_LINE\nact=TRUE" used as the
  # previous_champion printed as three separate lines, with the middle line rendering
  # byte-for-byte like a genuine VERDICT section header and a genuine "act=TRUE" -- a reader of
  # this one-line summary has no way to tell a fabricated line from this function's real prose.
  # loss_panel()'s own name validation (panel.R) only rejects empty/duplicate names, not control
  # characters, so this is directly reachable the same way the LEADERBOARD bug was. Fixed with
  # the SAME escaping approach already established in report.R: encodeString(..., quote = "'")
  # whenever the name contains a control character, so a champion name alone can never inject
  # extra "lines" into the printed summary.
  if (is.null(name) || length(name) != 1 || is.na(name)) return(name)
  if (grepl("[\\x00-\\x1f\\x7f]", name, perl = TRUE)) encodeString(name, quote = "'") else name
}

#' @export
print.change_report <- function(x, ...) {
  prev_disp <- .escape_champion_name(x$previous_champion)
  curr_disp <- .escape_champion_name(x$current_champion)
  if (!x$champion_changed) {
    head <- sprintf("no champion change (%s); act=FALSE", curr_disp)
  } else if (is.null(x$champion_without_new_periods)) {
    head <- sprintf(
      "champion changed %s -> %s; current shares no periods with previous -- cannot attribute the change; act=%s",
      prev_disp, curr_disp, x$act
    )
  } else if (identical(x$champion_without_new_periods, x$previous_champion)) {
    head <- sprintf(
      "champion changed %s -> %s, entirely due to the %d new period(s) (removing them reverts to the old champion); act=%s",
      prev_disp, curr_disp, x$n_new_periods, x$act
    )
  } else {
    head <- sprintf("champion changed %s -> %s; act=%s", prev_disp, curr_disp, x$act)
  }
  mcs_desc <- if (!is.null(x$mcs_error)) {
    sprintf("MCS undetermined (%s)", x$mcs_error)
  } else if (isTRUE(x$old_champion_still_in_mcs)) {
    "still in the MCS"
  } else {
    "left the MCS"
  }
  cat(sprintf("change_report(%s, old champion %s, k*=%s, churn_base_rate=%.3f)\n",
              head, mcs_desc, x$k_star, x$churn_base_rate))
  invisible(x)
}

#' @keywords internal
.churn_base_rate <- function(L, w, n_perm = 400, seed = 0) {
  # STRICT-AUDIT FIX (2026-11-18): n_perm never got the overflow-safe coerce-and-check treatment
  # its siblings n_boot/n_rep/B all received elsewhere in the package (including n_rep, fixed in
  # this exact round). The old `if (n_perm <= 0)` let NA/Inf/character through to crash with a
  # raw, unworded error, and -- worse -- let a FRACTIONAL n_perm (e.g. 7.9) through silently: the
  # loop below truncated via seq_len(n_perm) to floor(n_perm) real iterations, but the final
  # churn_base_rate divided by the ORIGINAL untruncated n_perm, mismatching numerator and
  # denominator and returning a confidently wrong statistic with no warning. Reassigning n_perm
  # to its coerced integer value HERE (reused below by both the loop and the final division) fixes
  # the mismatch outright; a fractional n_perm is still accepted and truncated, matching the
  # established convention n_boot/B/block already use, but numerator and denominator can no
  # longer disagree.
  n_perm_in <- n_perm; n_perm <- suppressWarnings(as.integer(n_perm))
  if (length(n_perm) != 1 || is.na(n_perm) || n_perm <= 0) {
    stop(sprintf("n_perm must be a single positive integer; got %s.",
                  paste(format(n_perm_in), collapse = ", ")), call. = FALSE)
  }
  # STRICT-AUDIT FIX (2026-11-18): seed was the one parameter in this function passed straight
  # into set.seed() with no local guard, unlike n_perm just above (same fix commit) -- a bad seed
  # (NA/non-scalar/out-of-range) reached a raw, unworded "supplied seed is not a valid integer"
  # error from base R's set.seed() itself. Worse than the n_perm case: compare() wraps its call
  # into .run_mcs_arch() in a tryCatch that degrades gracefully into mcs_error, but NOT this call,
  # so the raw error aborted the whole compare() call rather than failing with this package's own
  # worded style.
  seed_in <- seed; seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1 || is.na(seed) || seed < 0) {
    stop(sprintf(".churn_base_rate(): seed must be a single non-negative integer; got %s.",
                  paste(format(seed_in), collapse = ", ")), call. = FALSE)
  }
  models <- sort(names(L), method = "radix")
  M <- do.call(cbind, lapply(models, function(m) as.numeric(L[[m]])))  # T x K
  colnames(M) <- models
  obs_champ <- pooled_winner(stats::setNames(lapply(models, function(m) M[, m]), models), w)

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)

  T <- nrow(M)
  mean_w <- mean(w)
  changes <- 0L
  for (i in seq_len(n_perm)) {
    t_star <- sample.int(T, 1)
    new_period <- sample(M[t_star, ])  # same values as a real period, random model assignment
    Mp <- rbind(M, new_period)
    wp <- c(w, mean_w)
    perm_champ <- pooled_winner(stats::setNames(lapply(models, function(m) Mp[, m]), models), wp)
    if (perm_champ != obs_champ) changes <- changes + 1L
  }
  changes / n_perm
}

#' Compare two loss_panels from consecutive runs
#'
#' Reports whether the pooled champion changed between two consecutive evaluation runs, and
#' whether that change looks like signal or noise.
#'
#' @param previous A `loss_panel` from the earlier run.
#' @param current A `loss_panel` from the later run.
#' @section New-period attribution requires real labels: `n_new_periods`/`champion_without_new_periods`
#'   detect a rolling window by comparing the two panels' period LABELS -- if both panels were built
#'   with `loss_panel()`'s default positional labels (no `labels=` passed, the common case for a bare
#'   list/matrix), positional labels carry no cross-panel identity, so a same-length rolling window
#'   (periods really did drop out and new ones came in, but T stayed the same) is honestly reported as
#'   `n_new_periods=0`/`champion_without_new_periods=NULL` ("cannot attribute"), even though periods
#'   really did change. Pass `labels=` explicitly to `loss_panel()` for both panels if you need
#'   accurate new-period attribution on a same-length rolling window.
#'
#'   STRICT-AUDIT FIX (2026-11-18): that remedy does NOT cover every real-world labeling
#'   convention -- it only fixes POSITIONAL labels. A real, non-positional labeling scheme where
#'   the SAME label strings are reused every run (e.g. a pipeline that numbers "slot 1..24 in
#'   the trailing window," not calendar date) has `labels_are_positional == FALSE` yet suffers
#'   the identical defect, because `compare()` matches periods by label-STRING identity: if both
#'   panels use `labels = paste0("period", 1:24)` again, a genuine rolling window (oldest period
#'   dropped, one new period appended) is still reported as `n_new_periods=0`, because the label
#'   SETS are identical even though the underlying periods are not. If your labeling convention
#'   re-numbers slots rather than carrying a stable per-period identity (a calendar date, a
#'   period-since-epoch counter), `compare()`'s new-period attribution will be silently wrong
#'   regardless of whether you pass `labels=`.
#' @param alpha Significance level passed to the MCS check.
#' @param n_perm Number of permutations for the churn base rate.
#' @param seed RNG seed.
#' @return An object of class `change_report`. `old_champion_still_in_mcs` is `NULL` (not `NA`)
#'   when the internal MCS check errors -- see `?selectionfragility`'s "Sentinel conventions"
#'   section.
#' @seealso [loss_panel()] to build the `previous`/`current` panels; [mcs()]/[model_confidence_set()]
#'   for the Model Confidence Set check this uses to assess whether the old champion really left.
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' years <- 2015:2024
#' previous <- loss_panel(L, labels = years)
#'
#' # A new evaluation period (2025) arrives -- add one new value to EVERY model's
#' # vector, since all models in a panel must share the same periods.
#' L_new_period <- list(ar1 = c(L$ar1, 1.00), nbeats = c(L$nbeats, 0.50), ets = c(L$ets, 1.20))
#' current <- loss_panel(L_new_period, labels = c(years, 2025))
#'
#' cr <- compare(previous, current)
#' cr$act
#' # FALSE here: cr$champion_changed is FALSE (ar1 is still the pooled champion after the new
#' # period), so there is nothing actionable -- a promotion/alerting step should do nothing.
#' # cr$act only becomes TRUE when the champion changed AND the old champion has demonstrably
#' # left the Model Confidence Set (not just lost the pooled mean by noise) -- that is the one
#' # boolean a CI/pipeline promotion step should gate a real action on.
#' @export
compare <- function(previous, current, alpha = 0.10, n_perm = 400, seed = 0) {
  for (p in list(previous = previous, current = current)) {
    if (!inherits(p, "loss_panel")) {
      stop(paste0(
        "compare() expects two loss_panel objects; build one first with loss_panel(...)."
      ), call. = FALSE)
    }
  }
  .validate_alpha(alpha)
  # STRICT-AUDIT FIX (2026-11-18): this guard used to compare the CACHED previous$models/
  # current$models fields -- the same "trusts a pre-validated object even if it's since been
  # mutated" blind spot already found and fixed in print.loss_panel(), as.data.frame.loss_panel(),
  # and summary.loss_panel() (panel.R; all three moved to live names(x$losses) specifically
  # because $models can silently disagree with a panel's actual losses after a direct field edit).
  # compare() was the one call site that still trusted the stale cache: confirmed directly
  # reachable -- mutating `previous$losses <- previous$losses[c("ar1","nbeats")]` after
  # construction (dropping "ets") leaves `previous$models` at its original, now-stale 3-model
  # value, so this check silently passed and compare() went on to report a plausible-looking
  # champion comparison between a 2-model previous panel and a 3-model current panel with no error
  # at all -- exactly the mismatched-model-set comparison this guard exists to prevent. Reading
  # the model sets live from names(previous$losses)/names(current$losses) (what pooled_winner()
  # and every other downstream call in this function actually iterate over) closes the gap the
  # same way the panel.R methods already did.
  if (!setequal(names(previous$losses), names(current$losses))) {
    # LOCALE-AUDIT FIX (2026-11-18): bare sort() on model names here used R's default LC_COLLATE
    # locale collation -- confirmed to print the SAME two model sets in a DIFFERENT order in this
    # error message depending on the session's locale (e.g. C orders "Beta_model" < "Zeta_model" <
    # "alpha_model", en_US/tr_TR order "alpha_model" < "Beta_model" < "Zeta_model"). Doesn't change
    # whether the error fires, but is exactly the kind of non-reproducible, locale-dependent output
    # this package's radix-sort hardening elsewhere exists to eliminate. method="radix" throughout.
    stop(sprintf(
      "previous and current panels have different model sets -- cannot compare mismatched models: %s vs %s.",
      paste(sort(names(previous$losses), method = "radix"), collapse = ", "),
      paste(sort(names(current$losses), method = "radix"), collapse = ", ")
    ), call. = FALSE)
  }
  prev_L <- previous$losses; prev_w <- previous$weights
  curr_L <- current$losses; curr_w <- current$weights
  .validate_losses(curr_L, curr_w)

  prev_champ <- pooled_winner(prev_L, prev_w)
  curr_champ <- pooled_winner(curr_L, curr_w)
  champion_changed <- prev_champ != curr_champ

  curr_labels <- current$labels
  T_curr <- length(curr_labels)

  if (isTRUE(previous$labels_are_positional) || isTRUE(current$labels_are_positional)) {
    T_prev <- length(previous$labels)
    if (T_curr < T_prev) {
      stop(sprintf(
        paste0(
          "current has fewer periods (%d) than previous (%d) -- compare() expects current to be ",
          "the LATER run, a superset of previous's periods plus any new ones. This usually means ",
          "the two arguments were passed in the wrong order (try swapping them: ",
          "compare(previous = current, current = previous))."
        ), T_curr, T_prev
      ), call. = FALSE)
    }
    n_new <- max(0, T_curr - T_prev)
    if (n_new == 0) {
      champion_without_new_periods <- NULL
    } else {
      keep <- seq_len(T_curr - n_new)
      L_trunc <- lapply(curr_L, function(v) as.numeric(v)[keep])
      w_trunc <- as.numeric(curr_w)[keep]
      champion_without_new_periods <- pooled_winner(L_trunc, w_trunc)
    }
  } else {
    prev_label_set <- as.character(previous$labels)
    curr_label_set <- as.character(curr_labels)
    missing_old <- setdiff(prev_label_set, curr_label_set)
    is_new <- !(curr_label_set %in% prev_label_set)
    n_new <- sum(is_new)
    # Reversed-argument guard, deliberately NARROW: fire ONLY when current introduces no new
    # periods yet is missing some of previous's (i.e. current's labels are a strict subset of
    # previous's -- current is unambiguously the EARLIER/smaller run, so the arguments were
    # swapped). A broader "current dropped any old label" guard would wrongly reject two cases the
    # Python original supports and documents by name: a rolling window (drop the oldest period,
    # append a new one at the same T -- here n_new > 0, so this guard stays silent and the overlap
    # is compared), and a fully disjoint current (n_new == T_curr, handled below as "cannot
    # attribute"). Python has no guard here at all and relies on honest reporting; this guard adds
    # only the append-case reversed-order protection Python lacks, without regressing those two.
    if (n_new == 0 && length(missing_old) > 0) {
      stop(sprintf(
        paste0(
          "current's periods are a strict subset of previous's (missing: %s) and current adds no ",
          "new period -- compare() expects current to be the LATER run (retaining previous's ",
          "periods, plus any new ones). This usually means the two arguments were passed in the ",
          "wrong order (try swapping them: compare(previous = current, current = previous))."
        ), paste(missing_old, collapse = ", ")
      ), call. = FALSE)
    }
    if (n_new == 0) {
      champion_without_new_periods <- curr_champ
    } else if (n_new == T_curr) {
      champion_without_new_periods <- NULL
    } else {
      keep <- which(!is_new)
      L_trunc <- lapply(curr_L, function(v) as.numeric(v)[keep])
      w_trunc <- as.numeric(curr_w)[keep]
      champion_without_new_periods <- pooled_winner(L_trunc, w_trunc)
    }
  }

  mcs_error <- NULL
  old_champion_still_in_mcs <- NULL
  mcs_result <- tryCatch(.run_mcs_arch(curr_L, alpha = alpha, seed = seed, w = curr_w), error = function(e) e)
  if (inherits(mcs_result, "error")) {
    old_champion_still_in_mcs <- NULL
    mcs_error <- conditionMessage(mcs_result)
  } else {
    old_champion_still_in_mcs <- prev_champ %in% mcs_result
  }

  db_curr <- decision_breakdown(curr_L, curr_w, a = curr_champ)
  k_curr <- db_curr$k

  churn <- .churn_base_rate(curr_L, curr_w, n_perm = n_perm, seed = seed)

  act <- isTRUE(champion_changed && isFALSE(old_champion_still_in_mcs))

  .new_change_report(
    previous_champion = prev_champ,
    current_champion = curr_champ,
    champion_changed = champion_changed,
    champion_without_new_periods = champion_without_new_periods,
    n_new_periods = n_new,
    old_champion_still_in_mcs = old_champion_still_in_mcs,
    k_star = k_curr,
    churn_base_rate = churn,
    act = act,
    mcs_error = mcs_error
  )
}
