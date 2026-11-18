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

# STRICT-AUDIT FIX (2026-11-18): factored out of LEADERBOARD's own disambiguation block so the
# SAME control-character escaping it introduced for model names also covers PIVOT's printed
# period labels and compare.R's printed champion names -- both were found to have the identical
# injection gap LEADERBOARD's fix closed (a model/label/champion name containing a literal
# newline/CR/tab/DEL/ANSI-escape-sequence character prints verbatim into the one-screen report,
# indistinguishable from real prose, or alters a real terminal's rendering via an embedded ANSI
# escape). Centralizing avoids a 4th copy of this exact regex drifting out of sync like the
# K/T sibling-gap pattern already documented elsewhere in this package.
#' @keywords internal
.has_control_char <- function(x) grepl("[\\x00-\\x1f\\x7f]", x, perl = TRUE)

#' @keywords internal
.escape_control_chars <- function(x) {
  ifelse(.has_control_char(x), encodeString(x, quote = "'"), x)
}

#' @keywords internal
.fmt_pct <- function(x) {
  if (is.null(x) || is.na(x) || !is.finite(x)) return("n/a")
  sprintf("%.1f%%", x * 100)
}

#' @keywords internal
.strict_win_counts <- function(L) {
  models <- sort(names(L), method = "radix")
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
#' @param seed RNG seed for every bootstrap this function runs internally: the Model Confidence
#'   Set bootstrap behind VERDICT/`fragility_read`, and the `pivot_agreement()` subsample
#'   bootstrap behind PIVOT's `pivot_agreement:` line. On a genuinely borderline panel the
#'   identified/not-identified verdict can be seed-sensitive (see `?mcs`'s "Adversarial use"
#'   section for a quantified example: identification can swing ~37-42 percentage points across
#'   seed=1..300 on one borderline panel), and `pivot_agreement()`'s own percentage is similarly
#'   seed-sensitive on a small/borderline panel. Previously silently pinned to `seed=0` with no
#'   way to check; pass a few different seeds yourself near a borderline alpha if this matters
#'   for your use case.
#' @return A character string of class `selectionfragility_report` (the report text) with a
#'   `print` method, so both `report(panel)` (auto-printed at the console) and
#'   `print(report(panel))` display the formatted report; `cat(report(panel))` also works.
#' @seealso [loss_panel()] to build `panel`; [fragility()] for the full set of diagnostic fields
#'   in list form rather than formatted text; [compare()] for a run-over-run version of this check.
#' @examples
#' # Ten years of losses for three models: ar1 is steady, nbeats wins nine of the ten years
#' # but one shock-year loss (2.60) is large enough that ar1 wins on the pooled mean anyway,
#' # ets is uniformly worse.
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' report(panel)   # auto-prints; print(report(panel))/cat(report(panel)) also work
#' @export
report <- function(panel, alpha = 0.10, full = FALSE, seed = 0) {
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
  mcs_res <- tryCatch(.run_mcs_arch(L, alpha = alpha, w = w, seed = seed), error = function(e) e)
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
    resolution_report(L, w, alpha = 0.05, power = 0.80, mcs_alpha = alpha, mcs_seed = seed),
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
    } else if (!isTRUE(ident)) {
      # STRICT-AUDIT FIX (2026-11-18): the opposite-direction caveat was missing. VERDICT (R2,
      # the MCS bootstrap) and RESOLUTION (R1, a power check against the binding rival alone)
      # answer different questions and can legitimately disagree -- "not identified" + "resolved"
      # on the same panel is not a contradiction, but a one-screen reader seeing only the top two
      # sections (this function is advertised as exactly that) could easily misread "resolved" as
      # "the edge over the rival is real and the decision is settled," missing that the MCS
      # bootstrap still can't rule out some OTHER rival not involved in RESOLUTION's calculation.
      lines <- c(lines, "  (see RESOLUTION below -- a powered edge over the binding rival alone does",
                 "   not mean the MCS bootstrap can rule out every other model; VERDICT and",
                 "   RESOLUTION answer different questions and can legitimately disagree)")
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
  # STRICT-AUDIT FIX (2026-11-18): a model name containing a literal newline/carriage-return (or
  # any other C0/DEL control character) was embedded into LEADERBOARD's one-row-per-model text
  # completely unescaped unless it ALSO happened to collide with another name after padding --
  # loss_panel()'s own name validation (panel.R) only rejects empty/duplicate names, not control
  # characters, so this was directly reachable. Confirmed directly: a model named
  # "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE\nb" printed as three separate lines
  # inside LEADERBOARD, with the middle line rendering byte-for-byte like a genuine VERDICT
  # section header -- a one-screen reader (this function's own advertised audience) has no way to
  # tell that line apart from this function's real prose. This is a strictly worse failure mode
  # than the quote-collision bug the comment below already guards: it doesn't need a second,
  # colliding model name to trigger, and it breaks the one-line-per-model invariant the whole
  # table depends on rather than just looking ambiguous. Escape any name containing a control
  # character through the SAME encodeString() quoting already used for padding collisions, so a
  # model name alone can never inject extra "lines" (or anything that reads as a section header)
  # into the printed report.
  has_control_char <- .has_control_char(ordered)
  needs_escape <- ordered %in% collisions | has_control_char
  # Quoted + escaped (not a bare sprintf("'%s'", ...)) so a model name that itself contains a
  # quote character does not render as an ambiguous run of unescaped quotes once wrapped --
  # verified directly: two colliding names "ar1's" / "ar1's " used to print as `'ar1's'` /
  # `'ar1's '`, i.e. exactly the kind of indistinguishable-looking duplicate this disambiguation
  # branch exists to prevent. encodeString(..., quote="'") backslash-escapes the quote/backslash
  # the same way R's own quoting of strings does, so the wrapped name is always unambiguous.
  disp <- stats::setNames(
    ifelse(needs_escape, encodeString(ordered, quote = "'"), ordered),
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
  # NOTE ON MIXED TIE-BREAK RULES IN THIS PARAGRAPH: `db$binding_opponent` below (and the k*/removed-
  # periods text built from it) uses decision_breakdown()'s alphabetically-first tie-break; the
  # concentration_share()/pivot_agreement() calls further down in this same paragraph use a DIFFERENT,
  # largest-margin tie-break internally (pivot.R's .champ_opp_contributions()) for the same "which
  # opponent" question. On a panel with a genuine k*-tie (common -- ~38% of random panels with a
  # strict pooled winner have one), this paragraph can name two different opponents across its own
  # lines. This is deliberate, not a drift bug: see decision_breakdown()'s @section Limitation
  # (fragility.R) for why each rule is correct for its own call site and why unifying them would
  # reintroduce a rename-dependence bug the largest-margin rule exists to prevent. RESOLUTION above
  # uses a THIRD, independent rule (resolution.R's .binding_rival(), closest pooled-mean distance) --
  # also documented there, also not unified with either of these.
  db <- decision_breakdown(L, w, a = champ)
  k <- db$k; opp <- db$binding_opponent; removed <- db$removed
  if (k == 0 || is.null(opp)) {
    lines <- c(lines, "  k*=0 -- no strict pooled winner (an exact or effective tie); nothing is pivotal")
  } else {
    if (k <= 5) {
      # STRICT-AUDIT FIX (2026-11-18): same injection class as LEADERBOARD's model-name fix
      # above, reached through period LABELS instead of model names -- loss_panel()'s own label
      # validation (panel.R) rejects NA/duplicate labels but not control characters, so a label
      # containing a literal newline/CR/tab/DEL/ANSI-escape character printed completely
      # unescaped into PIVOT's one-line period_txt. Confirmed directly: a 10-period panel with
      # labels[10] <- "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE\nb" made the pivotal
      # period print a fake VERDICT line inside PIVOT, byte-for-byte indistinguishable from a
      # real section header -- the exact failure mode the LEADERBOARD fix was written to close,
      # just through the other printed identifier this function has. Escaped through the same
      # .escape_control_chars() helper (factored out of that fix) rather than a second copy.
      resp <- paste(.escape_control_chars(.disambiguate_labels(labels[sort(removed)])), collapse = ", ")
      period_txt <- sprintf("k*=%d of %d periods (%s)", k, T, resp)
    } else {
      la <- as.numeric(L[[champ]]); lb <- as.numeric(L[[opp]])
      c_ <- w * (lb - la)
      top_i <- which.max(c_)
      # Same fix as the k<=5 branch above, for the single-label case (k>5, "largest single
      # contributor" instead of a comma-joined list): an ANSI-escape-sequence label here is
      # arguably the worse variant of the two, since cat()'ing the report to a real terminal lets
      # the escape sequence change color/cursor position rather than just printing a fake prose
      # line -- confirmed directly with a label containing "\x1b[31m...\x1b[0m".
      period_txt <- sprintf("k*=%d of %d periods (largest single contributor: %s)", k, T, .escape_control_chars(.fmt_label(labels[[top_i]])))
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
      # prop22_certifies() is only valid for a k* computed from an UNWEIGHTED breakdown_number()
      # (see prop22.R's own scope note) -- but `k` here came from decision_breakdown(L, w, ...)
      # using the panel's REAL, possibly non-uniform weights. Calling prop22_certifies(k, T) on
      # that weighted k could in principle silently produce a confident "certifies non-significance"
      # line that is not actually certified once weighting is correctly excluded from the Prop 2.2
      # statistic (measured at 6.75% of simulated weighted panels). This branch is reached only when
      # fragility_read != "undetermined" (i.e. "resolved"), which already requires identified() to
      # have succeeded, which already requires .weights_are_uniform(w) (.validate_mcs_weights2()
      # rejects non-uniform weights upstream in .run_mcs_arch) -- so `!.weights_are_uniform(w)` below
      # is provably always FALSE here today. It is kept as a defensive, currently-inert guard in case
      # a future change relaxes that upstream validation; it does not currently execute.
      cert_txt <- if (!.weights_are_uniform(w)) {
        "no constraint from Proposition 2.2 (panel has non-uniform weights)"
      } else {
        cert <- prop22_certifies(k, T)
        if (isTRUE(cert)) {
          "certifies non-significance at the 5% level (Prop. 2.2, no simulation)"
        } else if (isFALSE(cert)) {
          "does not certify non-significance at the 5% level"
        } else {
          "no constraint from Proposition 2.2 at this k*"
        }
      }
      lines <- c(lines, sprintf("  %s -- %s", period_txt, cert_txt))
      if (isTRUE(cert)) {
        # STRICT-AUDIT FIX (2026-11-18): this line can print "certifies non-significance"
        # immediately below RESOLUTION's "resolved" with no text connecting the two, which reads
        # as a flat self-contradiction. It isn't one: RESOLUTION's R1 check (resolution.R) uses
        # the SAMPLE variance (ddof=1) and a one-sided Student-t critical value, while Prop 2.2's
        # certificate (prop22.R) is an unconditional, distribution-free bound keyed to the
        # POPULATION variance (ddof=0) and a fixed two-sided z -- two different, independently
        # valid tests that use different variance conventions and different critical values, so
        # they can and sometimes do disagree on the identical rival pair. Neither is wrong.
        lines <- c(lines, paste0(
          "    (not a contradiction -- RESOLUTION's \"resolved\" and this certificate use ",
          "different variance conventions and critical values; see ?prop22_certifies)"
        ))
      }
      cs <- concentration_share(L, w)
      # Threaded from report()'s own seed= (STRICT-AUDIT FIX, 2026-11-18): this used to be
      # hardcoded to seed=0 regardless of what the caller passed to report(), which silently
      # defeated the seed-sensitivity check seed= exists for -- a caller following this file's
      # own @param seed advice ("pass a few different seeds yourself") would see the PIVOT
      # line's percentage never move, since pivot_agreement() is just as RNG-dependent (a
      # 200-rep bootstrap) as the MCS identification check that seed= does correctly control.
      pa <- pivot_agreement(L, w, seed = seed)
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
