# report.R — the one-screen report. Faithful port of report.py. Prose lives here, never in the
# returned value (it returns a single formatted character string, matching the Python original's
# "\n".join(lines) return type -- call cat() on it, or just print it, for display).

#' @keywords internal
.fmt_label <- function(lbl) {
  if (inherits(lbl, "Date") || inherits(lbl, "POSIXt")) {
    tryCatch(format(lbl, "%Y-%m"), error = function(e) as.character(lbl))
  } else {
    as.character(lbl)
  }
}

#' @keywords internal
.disambiguate_labels <- function(labels_subset) {
  base <- vapply(labels_subset, .fmt_label, character(1))
  if (length(unique(base)) == length(base)) return(base)
  finer <- vapply(seq_along(labels_subset), function(i) {
    lbl <- labels_subset[[i]]
    if (inherits(lbl, "Date") || inherits(lbl, "POSIXt")) {
      out <- tryCatch(format(lbl, "%Y-%m-%d"), error = function(e) NA_character_)
      if (!is.na(out)) return(out)
    }
    base[i]
  }, character(1))
  if (length(unique(finer)) == length(finer)) return(finer)
  counts <- table(finer)
  seen <- stats::setNames(rep(0L, length(finer)), NULL)
  out <- character(length(finer))
  seen_map <- list()
  for (i in seq_along(finer)) {
    txt <- finer[i]
    seen_map[[txt]] <- (seen_map[[txt]] %||% 0L) + 1L
    out[i] <- if (counts[[txt]] == 1) txt else sprintf("%s (#%d)", txt, seen_map[[txt]])
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
.fmt_pct <- function(x) {
  if (is.null(x) || is.na(x) || !is.finite(x)) return("n/a")
  sprintf("%.1f%%", x * 100)
}

#' @keywords internal
.strict_win_counts <- function(L) {
  models <- sort(names(L))
  M <- do.call(rbind, lapply(models, function(m) as.numeric(L[[m]])))
  pmin <- apply(M, 2, min)
  is_min <- sweep(M, 2, pmin, `==`)
  n_min <- colSums(is_min)
  unique_min <- is_min & matrix(n_min == 1, nrow = nrow(M), ncol = ncol(M), byrow = TRUE)
  wins <- rowSums(unique_min)
  stats::setNames(as.integer(wins), models)
}

#' One-screen summary of a loss_panel
#'
#' VERDICT (identification), LEADERBOARD (mean loss / in-MCS / periods won), RESOLUTION (observed
#' edge vs MDE, MCB bound), PIVOT (k*, named periods or largest contributor, concentration_share,
#' pivot_agreement).
#'
#' @param panel A `loss_panel` object (not a raw list -- this function reads `$weights`/`$labels`).
#' @param alpha Significance level for the Model Confidence Set.
#' @param full Accepted for forward compatibility; unused (the default view is the compact one).
#' @return A character string of class `selectionfragility_report` (the report text) with a
#'   `print` method, so both `report(panel)` (auto-printed at the console) and
#'   `print(report(panel))` display the formatted report; `cat(report(panel))` also works.
#' @seealso [loss_panel()] to build `panel`; [fragility()] for the full set of diagnostic fields
#'   in list form rather than formatted text; [compare()] for a run-over-run version of this check.
#' @export
report <- function(panel, alpha = 0.10, full = FALSE) {
  if (!inherits(panel, "loss_panel")) {
    stop(sprintf("report() expects a loss_panel, got %s. Build one first with loss_panel(...).", class(panel)[1]), call. = FALSE)
  }
  L <- panel$losses
  w <- panel$weights
  labels <- panel$labels
  models <- names(L)
  K <- length(models)
  T <- length(labels)

  mcs_error <- NULL
  survivors <- character(0); msize <- NULL; ident <- NULL
  mcs_res <- tryCatch(.run_mcs_arch(L, alpha = alpha, w = w), error = function(e) e)
  if (inherits(mcs_res, "error")) {
    ident <- NULL; msize <- NULL; survivors <- character(0)
    mcs_error <- conditionMessage(mcs_res)
  } else {
    survivors <- mcs_res
    msize <- length(survivors)
    ident <- isTRUE(msize == 1L)
  }
  means <- stats::setNames(sapply(models, function(m) stats::weighted.mean(as.numeric(L[[m]]), w)), models)
  wins <- .strict_win_counts(L)
  champ <- pooled_winner(L, w)

  res <- tryCatch(
    resolution_report(L, w, alpha = 0.05, power = 0.80, mcs_alpha = alpha),
    error = function(e) list(resolved = FALSE, observed_edge = NaN, mde = NaN,
                              significance_boundary = NaN, binding_rival = NULL,
                              identified = NULL, fragility_read = "undetermined")
  )

  lines <- character(0)
  if (is.null(mcs_error)) {
    lines <- c(lines, sprintf("VERDICT: %s (MCS size %d of %d)", if (ident) "identified" else "not identified", msize, K))
    if (!isTRUE(res$resolved)) {
      lines <- c(lines, "  (see RESOLUTION below -- identification alone does not mean the edge is",
                 "   powered at this sample size)")
    }
  } else {
    lines <- c(lines, sprintf("VERDICT: not available for this panel -- %s", mcs_error))
  }
  lines <- c(lines, "")
  lines <- c(lines, "LEADERBOARD")
  ordered <- models[order(means[models])]
  padded <- stats::setNames(sprintf("%-15s", ordered), ordered)
  collisions <- ordered[sapply(ordered, function(m) sum(padded == padded[m]) > 1)]
  # Quoted + escaped (not a bare sprintf("'%s'", ...)) so a model name that itself contains a
  # quote character does not render as an ambiguous run of unescaped quotes once wrapped --
  # verified directly: two colliding names "ar1's" / "ar1's " used to print as `'ar1's'` /
  # `'ar1's '`, i.e. exactly the kind of indistinguishable-looking duplicate this disambiguation
  # branch exists to prevent. encodeString(..., quote="'") backslash-escapes the quote/backslash
  # the same way R's own quoting of strings does, so the wrapped name is always unambiguous.
  disp <- stats::setNames(
    ifelse(ordered %in% collisions, encodeString(ordered, quote = "'"), ordered),
    ordered
  )
  for (mm in ordered) {
    flag <- if (!is.null(mcs_error)) "n/a" else if (mm %in% survivors) "in MCS" else "excluded"
    marker <- if (mm == champ) " (champion)" else ""
    lines <- c(lines, sprintf("  %-15s mean=%.4f  %-9s  periods won=%d%s", disp[mm], means[mm], flag, wins[mm], marker))
  }
  lines <- c(lines, "")
  lines <- c(lines, "RESOLUTION")
  edge_txt <- sprintf("observed edge %s vs MDE@80%% power %s", .fmt_pct(res$observed_edge), .fmt_pct(res$mde))
  if (isTRUE(res$resolved) && !is.null(res$observed_edge) && is.finite(res$observed_edge) && res$observed_edge <= 0) {
    lines <- c(lines, sprintf("  %s -- resolved: no edge exists (the champion ties its binding rival exactly)", edge_txt))
  } else if (isTRUE(res$resolved)) {
    lines <- c(lines, sprintf("  %s -- resolved", edge_txt))
  } else {
    lines <- c(lines, sprintf("  %s -- cannot determine at this sample size", edge_txt))
  }
  bound_res <- tryCatch(mcb_bound(L, w), error = function(e) e)
  if (inherits(bound_res, "error")) {
    lines <- c(lines, sprintf("  MCB bound: not available -- %s", conditionMessage(bound_res)))
  } else {
    lines <- c(lines, sprintf("  MCB bound: the champion's true edge over the binding rival may be as large as %s, at 95%% confidence", .fmt_pct(bound_res)))
  }
  lines <- c(lines, "")
  lines <- c(lines, "PIVOT")
  db <- decision_breakdown(L, w, a = champ)
  k <- db$k; opp <- db$binding_opponent; removed <- db$removed
  if (k == 0 || is.null(opp)) {
    lines <- c(lines, "  k*=0 -- no strict pooled winner (an exact or effective tie); nothing is pivotal")
  } else {
    if (k <= 5) {
      resp <- paste(.disambiguate_labels(labels[sort(removed)]), collapse = ", ")
      period_txt <- sprintf("k*=%d of %d periods (%s)", k, T, resp)
    } else {
      la <- as.numeric(L[[champ]]); lb <- as.numeric(L[[opp]])
      c_ <- w * (lb - la)
      top_i <- which.max(c_)
      period_txt <- sprintf("k*=%d of %d periods (largest single contributor: %s)", k, T, .fmt_label(labels[[top_i]]))
    }
    if (identical(res$fragility_read, "undetermined")) {
      reason <- if (!is.null(mcs_error)) {
        "MCS unavailable for this panel (see VERDICT above)"
      } else if (!isTRUE(ident)) {
        "not identified (MCS size > 1)"
      } else {
        "observed edge below MDE at this sample size"
      }
      lines <- c(lines, sprintf("  %s -- fragility read UNDETERMINED (%s); see VERDICT/RESOLUTION above before treating this as evidence either way", period_txt, reason))
    } else {
      cert <- prop22_certifies(k, T)
      cert_txt <- if (isTRUE(cert)) {
        "certifies non-significance at the 5% level (Prop. 2.2, no simulation)"
      } else if (isFALSE(cert)) {
        "does not certify non-significance at the 5% level"
      } else {
        "no constraint from Proposition 2.2 at this k*"
      }
      lines <- c(lines, sprintf("  %s -- %s", period_txt, cert_txt))
      cs <- concentration_share(L, w)
      pa <- pivot_agreement(L, w, seed = 0)
      lines <- c(lines, sprintf("  concentration_share: %s of the margin comes from the single largest-contribution period", .fmt_pct(cs)))
      if (!is.na(pa)) {
        lines <- c(lines, sprintf("  pivot_agreement: %s of subsamples name the same pivotal period", .fmt_pct(pa)))
      }
    }
  }
  structure(paste(lines, collapse = "\n"), class = "selectionfragility_report")
}

#' @export
print.selectionfragility_report <- function(x, ...) {
  cat(unclass(x), "\n", sep = "")
  invisible(x)
}
