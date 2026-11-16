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

#' @export
print.change_report <- function(x, ...) {
  if (!x$champion_changed) {
    head <- sprintf("no champion change (%s); act=FALSE", x$current_champion)
  } else if (is.null(x$champion_without_new_periods)) {
    head <- sprintf(
      "champion changed %s -> %s; current shares no periods with previous -- cannot attribute the change; act=%s",
      x$previous_champion, x$current_champion, x$act
    )
  } else if (identical(x$champion_without_new_periods, x$previous_champion)) {
    head <- sprintf(
      "champion changed %s -> %s, entirely due to the %d new period(s) (removing them reverts to the old champion); act=%s",
      x$previous_champion, x$current_champion, x$n_new_periods, x$act
    )
  } else {
    head <- sprintf("champion changed %s -> %s; act=%s", x$previous_champion, x$current_champion, x$act)
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
  if (n_perm <= 0) stop(sprintf("n_perm must be a positive integer; got %s.", n_perm), call. = FALSE)
  models <- sort(names(L))
  M <- do.call(cbind, lapply(models, function(m) as.numeric(L[[m]])))  # T x K
  colnames(M) <- models
  obs_champ <- pooled_winner(stats::setNames(lapply(models, function(m) M[, m]), models), w)

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)

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
#' @param alpha Significance level passed to the MCS check.
#' @param n_perm Number of permutations for the churn base rate.
#' @param seed RNG seed.
#' @return An object of class `change_report`.
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
  if (!setequal(previous$models, current$models)) {
    stop(sprintf(
      "previous and current panels have different model sets -- cannot compare mismatched models: %s vs %s.",
      paste(sort(previous$models), collapse = ", "), paste(sort(current$models), collapse = ", ")
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
    is_new <- !(as.character(curr_labels) %in% prev_label_set)
    n_new <- sum(is_new)
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
