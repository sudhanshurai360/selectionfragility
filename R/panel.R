#' @importFrom stats setNames
NULL

# loss_panel — the single validated entry point for every public function in this package.
#
# Every other function in this package consumes a loss_panel (or its raw $losses/$weights,
# which are the SAME validated data). The bare constructor is not exported -- go through
# loss_panel() so a caller cannot skip validation by accident. This mirrors the Python
# package's `LossPanel.from_losses()` contract (see panel.py); `from_forecasts()` (long
# forecast-vs-actual frames with metric computation and auto-inference) is NOT ported in
# this release -- build a wide loss matrix/data.frame/list yourself and pass it to
# `loss_panel()`.

.MAX_ABS_LOSS <- 1e100
.LARGE_K_WARN_THRESHOLD <- 100

#' @keywords internal
.looks_like_metadata_column <- function(arr, T) {
  arr <- as.numeric(arr)
  if (T < 4) return(FALSE)
  # NA/NaN/Inf short-circuit: Python's NaN == x is False (not NaN), so np.all(...) resolves
  # cleanly to FALSE and the heuristic falls through harmlessly; R's NA == x is NA, which
  # breaks all()/if() with a raw "missing value where TRUE/FALSE needed" error instead of
  # letting the real, better-worded NaN/Inf check downstream report it. Match the Python
  # behavior (never crash here) by deferring entirely to that downstream check.
  if (anyNA(arr) || any(!is.finite(arr))) return(FALSE)
  if (all(arr == arr[1])) return(FALSE)
  diffs <- diff(arr)
  if (length(diffs) > 0 && all(diffs == diffs[1]) && diffs[1] != 0) return(TRUE)
  max_p <- min(10, T %/% 2)
  if (max_p >= 2) {
    for (p in 2:max_p) {
      reps_needed <- ceiling(T / p)
      tiled <- rep(arr[1:p], reps_needed)[1:T]
      # EXACT equality, matching Python's np.array_equal -- NOT all.equal(). all.equal() allows a
      # default relative tolerance (~1.5e-8), so a genuine, non-periodic loss column that merely
      # comes CLOSE to tiling a short pattern (floating-point noise on an otherwise-periodic real
      # signal, or plain coincidence) was being wrongly flagged as metadata and rejected -- a real
      # loss column the Python oracle accepts outright, since array_equal requires bit-for-bit
      # equality. Verified directly against the Python oracle: a period-3 pattern with one value
      # perturbed by 1e-9 is accepted (False) in Python but was wrongly rejected (TRUE) here.
      if (T %/% p >= 2 && length(arr) == length(tiled) && all(arr == tiled)) return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
.check_metadata_columns <- function(models, T) {
  if (T <= 0) return(invisible(NULL))
  for (name in names(models)) {
    if (.looks_like_metadata_column(models[[name]], T)) {
      stop(sprintf(
        paste0(
          "column '%s' looks like a categorical/metadata column (an exact arithmetic sequence or ",
          "a short repeating pattern across %d periods), not a per-period loss -- a stray metadata ",
          "column must never be silently crowned a model. Drop it, or build the panel from an ",
          "explicit named list of the real model columns only. (If this is placeholder/toy data ",
          "you typed by hand, e.g. 1:5 or 2:6, that's exactly the kind of exact arithmetic ",
          "sequence this check exists to catch -- add a little noise, e.g. 1:5 + rnorm(5, 0, 0.01), ",
          "to try the package with fake numbers.)"
        ), name, T
      ), call. = FALSE)
    }
  }
  invisible(NULL)
}

#' @keywords internal
.check_no_name_collision <- function(raw_names) {
  dup <- unique(raw_names[duplicated(raw_names)])
  if (length(dup) > 0) {
    stop(sprintf(
      "duplicate model/column name(s) %s -- rename before building a loss_panel.",
      paste(sprintf("'%s'", dup), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' @keywords internal
.validate_weights <- function(w, T) {
  if (is.character(w)) {
    stop(sprintf(
      "weights must be a numeric vector of %d values (one per period), not a string; got %s.",
      T, paste(w, collapse = ", ")
    ), call. = FALSE)
  }
  if (is.factor(w)) {
    # A factor is not numeric (is.numeric(factor) is FALSE) and not character (is.character(factor)
    # is also FALSE), so it slips past the string guard above and straight into as.numeric(), which
    # on a factor returns the INTEGER LEVEL CODES, not the labels -- e.g. factor(c("10","20","30"))
    # silently becomes c(1, 2, 3). This is the same class of silent-wrong-answer bug as `NA == x`:
    # a value that looks numeric to a careless call but silently is not. Reject explicitly, by the
    # same policy as the string guard above, rather than let it corrupt weights with no warning.
    stop(sprintf(
      "weights must be a numeric vector of %d values (one per period), not a factor; got levels %s. ",
      T, paste(levels(w), collapse = ", ")
    ), call. = FALSE)
  }
  if (is.logical(w)) {
    # STRICT-AUDIT FIX (2026-11-18): every model-column constructor in this file (.from_list,
    # .from_data_frame, and .from_matrix as its own dedicated fix) rejects a logical column with
    # this exact rationale -- "a flag/indicator column... silently coerced TRUE/FALSE -> 1/0...
    # crowned a model with no warning." .validate_weights() carries the identical risk (and its
    # own factor guard above already frames factor-weights as "the same class of silent-wrong-
    # answer bug") but was never given the sibling is.logical() guard -- a logical weights vector
    # silently became a 0/1 numeric weight vector with zero warning.
    stop(sprintf(
      "weights must be a numeric vector of %d values (one per period), not logical (TRUE/FALSE); got %s.",
      T, paste(w, collapse = ", ")
    ), call. = FALSE)
  }
  w <- suppressWarnings(as.numeric(w))
  if (length(w) != T) {
    stop(sprintf("weights must have length %d (one per period); got length %d.", T, length(w)), call. = FALSE)
  }
  if (!all(is.finite(w))) {
    stop("weights must not contain NA/NaN/Inf values.", call. = FALSE)
  }
  if (any(w < 0)) {
    stop("weights must not contain negative values -- weights are observation counts, not signed contributions.", call. = FALSE)
  }
  if (all(w == 0)) {
    stop("weights are all zero; at least one period must carry positive weight.", call. = FALSE)
  }
  if (any(w > .MAX_ABS_LOSS)) {
    # STRICT-AUDIT FIX (2026-11-18): unlike losses, which have had this exact cap since the first
    # hardening round, weights had none -- each individual weight was only checked for finite/
    # non-negative, so a weight that is, by itself, "valid" (e.g. 1e308) could still make sum(w)
    # overflow to Inf inside stats::weighted.mean(), producing Inf/Inf = NaN for every model's
    # mean loss. pooled_winner() then fed that into sort(), whose default na.last=NA silently
    # drops all-NaN entries, returning NA_character_ as the "champion" with NO error or warning
    # -- which then cascaded into wildly misattributed downstream errors (e.g. decision_breakdown()
    # blaming a "length mismatch" and resolution_report() reporting "T=0" on a real T=2 panel,
    # because L[[NA_character_]] silently returns NULL rather than erroring).
    stop(sprintf(
      paste0("weights must not exceed %.0e in magnitude -- a weight this large can make ",
             "sum(weights) overflow to Inf and silently corrupt every weighted statistic downstream."),
      .MAX_ABS_LOSS
    ), call. = FALSE)
  }
  w
}

#' @keywords internal
.check_K_vs_T <- function(K, T) {
  if (K > T) {
    warning(sprintf(
      paste0(
        "K=%d models but T=%d periods (K > T) -- this looks like it might be a transposed frame ",
        "(models and periods swapped); did you mean to pass the data transposed?"
      ), K, T
    ), call. = FALSE)
  } else if (K > .LARGE_K_WARN_THRESHOLD) {
    warning(sprintf(
      paste0(
        "K=%d models -- the native mcs()/model_confidence_set() engine's cost grows steeply with ",
        "model count (measured super-cubic in K: its pairwise elimination is O(K^3 * B)); the ",
        "MCS::MCSprocedure()-backed report()/identified()/mcs_size() path is cheaper (~quadratic) ",
        "but still grows. This is not an error -- large model sweeps are supported -- but expect it ",
        "to be slow; if you don't need per-model MCS membership, consider narrowing to a candidate ",
        "shortlist first. THIS IS ONLY A WARNING, NOT A HARD CAP: neither K nor T has an enforced ",
        "ceiling anywhere in this package (unlike B, which ",
        "does -- see mcs.R's .MAX_BOOTSTRAP_B). If this ",
        "package is ever called from a server/API process on untrusted caller-supplied K/T/B, that ",
        "caller can trivially turn a single call into a minutes-to-hours-long computation with ",
        "nothing stronger than this warning to stop them -- validate/cap K and T yourself at your ",
        "own service boundary if that's your deployment shape."
      ), K
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' @keywords internal
.new_loss_panel <- function(losses, weights, labels, labels_are_positional) {
  structure(
    list(
      losses = losses,
      weights = weights,
      labels = labels,
      labels_are_positional = labels_are_positional,
      models = names(losses)
    ),
    class = "loss_panel"
  )
}

#' Build a validated loss panel
#'
#' The single validated entry point for every other function in this package. Accepts a named
#' list of per-period loss vectors, a periods-by-models numeric matrix, or a wide data.frame
#' (one column per model, row names or an explicit `labels=` giving the period labels).
#'
#' @param data A named list of numeric vectors (one per model, equal length), a numeric matrix
#'   (rows = periods, columns = models), or a data.frame (wide, one column per model).
#' @param weights Optional numeric vector of per-period weights (e.g. observation counts).
#'   Defaults to equal weights.
#' @param labels Optional vector of period labels, one per period. If omitted, inferred from
#'   list names / data.frame row names (or positional integers if neither is available).
#' @return An object of class `loss_panel`.
#' @seealso [report()] for the full staged diagnostic pipeline on a built panel; [mcs_size()]/
#'   [identified()] and [mcs()]/[model_confidence_set()] for identification; [compare()] for
#'   comparing two panels from consecutive runs.
#' @export
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#'
#' # The same data also works as a matrix (rows = periods, columns = models) or a data.frame,
#' # with optional weights (e.g. observation counts per period) and labels:
#' M <- cbind(ar1 = L$ar1, nbeats = L$nbeats, ets = L$ets)
#' w <- c(4, 4, 4, 4, 4, 4, 4, 4, 1, 4)  # the shock period (2.60) carries less weight
#' panel2 <- loss_panel(M, weights = w, labels = 2015:2024)
#' panel2$weights
loss_panel <- function(data, weights = NULL, labels = NULL) {
  if (is.matrix(data)) {
    out <- .from_matrix(data)
  } else if (is.data.frame(data)) {
    out <- .from_data_frame(data)
  } else if (is.list(data)) {
    out <- .from_list(data)
  } else {
    stop(sprintf(
      paste0(
        "loss_panel() does not know how to read a %s; pass a named list of per-model vectors, ",
        "a matrix (periods x models), or a wide data.frame."
      ), class(data)[1]
    ), call. = FALSE)
  }
  loss_dict <- out$losses
  inferred_labels <- out$labels
  labels_are_positional <- out$labels_are_positional

  T <- length(loss_dict[[1]])
  if (T == 0) stop("panel has zero periods.", call. = FALSE)
  if (T == 1) stop("panel has only 1 period; need at least 2 periods to compare models.", call. = FALSE)
  if (length(loss_dict) < 2) {
    stop(sprintf("need at least 2 models to compare a selection decision; got %d.", length(loss_dict)), call. = FALSE)
  }

  for (m in names(loss_dict)) {
    v <- loss_dict[[m]]
    if (length(v) != T) {
      stop(sprintf(
        "all models must share the same number of periods; '%s' has %d, expected %d (from the first model).",
        m, length(v), T
      ), call. = FALSE)
    }
    if (!all(is.finite(v))) {
      stop(sprintf(
        paste0(
          "model '%s' has NA/NaN/Inf loss values; drop or impute those periods before building a ",
          "loss_panel -- a single NA silently poisons the pooled mean and can crown a spurious winner."
        ), m
      ), call. = FALSE)
    }
    if (any(abs(v) > .MAX_ABS_LOSS)) {
      stop(sprintf(
        paste0(
          "model '%s' has a loss magnitude above %.0e, far beyond any real per-period loss metric -- ",
          "arithmetic at this scale silently overflows double precision downstream and produces ",
          "meaningless results rather than a clear error. Check for a units/scaling bug upstream."
        ), m, .MAX_ABS_LOSS
      ), call. = FALSE)
    }
  }

  .check_K_vs_T(length(loss_dict), T)

  if (!is.null(labels)) {
    final_labels <- labels
    if (length(final_labels) != T) {
      stop(sprintf(
        "labels has length %d, expected %d (one per period, matching the loss vectors).",
        length(final_labels), T
      ), call. = FALSE)
    }
    str_labels <- as.character(final_labels)
    if (anyNA(str_labels)) {
      # STRICT-AUDIT FIX (2026-11-18): an NA/null label used to pass silently as long as it
      # wasn't duplicated -- but labels are compare()'s one identity key for telling a new
      # period from an old one, and an NA label is unique by %in%-matching semantics (NA %in% x
      # is always FALSE), so a single NA defeats compare()'s reversed-argument safety guard:
      # that period gets miscounted as "new" rather than "missing," n_new becomes nonzero, and
      # the guard's n_new==0 precondition is never met -- compare() then confidently prints a
      # fully-formed but causally backwards change_report instead of erroring on the swapped
      # arguments. Reachable through the one file-I/O boundary too: load_loss_panel() accepted a
      # JSON "labels" array containing null without complaint. Reject outright here instead.
      stop("labels must not contain NA/NULL values -- every period needs a real identity for compare() to match on.", call. = FALSE)
    }
    if (length(unique(str_labels)) != T) {
      # LOCALE-AUDIT FIX (2026-11-18): bare sort() on duplicate labels used LC_COLLATE locale
      # collation -- confirmed to list the same duplicate set in a different order (and, with
      # more than 5 distinct duplicates, to truncate to a DIFFERENT head(dupes, 5) subset) across
      # locales. Doesn't change whether the error fires, but same non-reproducibility class this
      # package's radix-sort hardening elsewhere exists to eliminate. method="radix" throughout.
      dupes <- sort(unique(str_labels[duplicated(str_labels)]), method = "radix")
      stop(sprintf(
        paste0(
          "labels contains duplicate value(s) %s -- labels must uniquely identify each period, ",
          "since compare() relies on label identity (as text) to tell a new period from an old one."
        ), paste(utils::head(dupes, 5), collapse = ", ")
      ), call. = FALSE)
    }
    labels_are_positional <- FALSE
  } else {
    final_labels <- inferred_labels
  }

  w <- if (!is.null(weights)) .validate_weights(weights, T) else rep(1, T)
  .new_loss_panel(loss_dict, w, final_labels, labels_are_positional)
}

#' @keywords internal
.from_list <- function(data) {
  if (length(data) == 0) stop("empty list passed to loss_panel(); need at least 2 models.", call. = FALSE)
  nm <- names(data)
  # STRICT-AUDIT FIX (2026-11-18): the original `any(nm == "")` crashes with a raw, unworded
  # "missing value where TRUE/FALSE needed" the moment any name is NA (not ""): `nm == ""`
  # yields NA for that element, any(c(FALSE, NA)) is itself NA (not FALSE -- any() can't rule
  # out a hidden TRUE), so `is.null(nm) || NA` is NA and if(NA) aborts outright, bypassing this
  # file's entire stop()-with-a-clear-message policy. Check anyNA() first (it short-circuits the
  # `||` before `nm == ""` ever sees an NA) so both an NA name and an empty-string name get the
  # same clear, deliberate rejection instead of one of them crashing past it.
  if (is.null(nm) || anyNA(nm) || any(nm == "")) {
    stop("loss_panel() needs a NAMED list (one name per model); got an unnamed, NA-named, or empty-named list.", call. = FALSE)
  }
  .check_no_name_collision(nm)
  for (k in nm) {
    v <- data[[k]]
    if (is.logical(v)) {
      stop(sprintf(
        "model '%s' is logical, not a per-period numeric loss -- looks like a flag/indicator column, not a model.",
        k
      ), call. = FALSE)
    }
    if (!is.numeric(v) || length(v) == 0) {
      stop(sprintf("model '%s' must be a non-empty numeric vector of per-period losses.", k), call. = FALSE)
    }
  }
  models <- stats::setNames(lapply(data, as.numeric), nm)
  T <- length(models[[1]])
  .check_metadata_columns(models, T)
  list(losses = models, labels = seq_len(T) - 1L, labels_are_positional = TRUE)
}

#' @keywords internal
.from_matrix <- function(arr) {
  if (length(dim(arr)) != 2) stop("matrix input to loss_panel() must be 2-D (periods x models).", call. = FALSE)
  if (is.logical(arr)) {
    # Matches the is.logical rejection the list and data.frame constructors already apply: a
    # logical matrix otherwise coerces TRUE/FALSE -> 1/0 silently and gets treated as 0/1 "losses",
    # so an indicator/flag matrix would be crowned a set of models with no warning on this one path.
    stop(paste0(
      "loss_panel() got a logical matrix -- this looks like a flag/indicator matrix, not per-period ",
      "losses. If these really are 0/1 losses, convert with `arr * 1` or `matrix(as.numeric(arr), ...)` ",
      "first so the intent is explicit."
    ), call. = FALSE)
  }
  T <- nrow(arr); K <- ncol(arr)
  if (K == 0) {
    stop(sprintf("matrix has zero model columns (dim %d x %d); need at least 2 models.", T, K), call. = FALSE)
  }
  cn <- colnames(arr)
  nm <- if (!is.null(cn)) cn else paste0("m", seq_len(K) - 1L)
  if (!is.null(cn)) {
    # STRICT-AUDIT FIX (2026-11-18): an NA or "" column name used to slip past here with no
    # explicit check at all and get "rejected" only by accident downstream, through R's own
    # `[[`-by-name quirk: `list[[NA_character_]]` and `list[["" ]]` both silently return NULL
    # even when a same-named element exists (confirmed directly: `setNames(list(1:6,7:12),
    # c("","b"))[[""]]` is NULL, not 1:6). That NULL then surfaced as a wrong, confusing error
    # from a LATER, unrelated check ("all models must share the same number of periods; ''
    # has 0, expected 6") rather than this file's own clear, deliberate "needs a real name"
    # wording that the list-path sibling check already gives. Still rejects the input (so no
    # empty/NA-named model was ever silently built), but reject it with the same deliberate,
    # worded guard as `.from_list()`, not a coincidence of lookup semantics.
    if (anyNA(nm) || any(nm == "")) {
      stop(paste0(
        "loss_panel() got a matrix with an NA or empty-string column name -- every model needs ",
        "a real, non-empty name. Set colnames() explicitly, or pass the matrix with no colnames ",
        "at all to get auto-generated 'm0', 'm1', ... names."
      ), call. = FALSE)
    }
    .check_no_name_collision(nm)
  }
  models <- stats::setNames(lapply(seq_len(K), function(i) as.numeric(arr[, i])), nm)
  .check_metadata_columns(models, T)
  list(losses = models, labels = seq_len(T) - 1L, labels_are_positional = TRUE)
}

#' @keywords internal
.from_data_frame <- function(df) {
  cn0 <- colnames(df)
  # STRICT-AUDIT FIX (2026-11-18): same accidental-rejection gap as .from_matrix() -- an NA or
  # "" column name (reachable via `colnames(df) <- c(NA, "b")` or `check.names = FALSE`) had no
  # explicit check here either, and was "caught" only because `df[[""]]`/`df[[NA_character_]]`
  # silently return NULL even for an existing same-named column, surfacing as a wrong, confusing
  # downstream error ("column '' is not numeric (class NULL)") instead of this file's own clear,
  # deliberate wording. Reject explicitly and early, matching .from_list()/.from_matrix().
  if (anyNA(cn0) || any(cn0 == "")) {
    stop(paste0(
      "loss_panel() got a data.frame with an NA or empty-string column name -- every model needs ",
      "a real, non-empty name. Rename the column(s) first (e.g. via colnames(df) <- ... or by not ",
      "passing check.names = FALSE with a blank name)."
    ), call. = FALSE)
  }
  if (anyDuplicated(cn0)) {
    stop("duplicate column names in the data.frame -- rename before building a loss_panel.", call. = FALSE)
  }
  rn <- rownames(df)
  has_real_rownames <- !is.null(rn) && !identical(rn, as.character(seq_len(nrow(df))))
  if (has_real_rownames && anyDuplicated(rn)) {
    stop(paste0(
      "duplicate row names -- cannot silently keep both rows as independent periods; ",
      "deduplicate or aggregate first."
    ), call. = FALSE)
  }
  .check_no_name_collision(colnames(df))
  T <- nrow(df)
  models <- list()
  for (col in colnames(df)) {
    s <- df[[col]]
    if (is.logical(s)) {
      stop(sprintf(
        "column '%s' is logical, not a per-period numeric loss -- looks like a flag/indicator column, not a model.",
        col
      ), call. = FALSE)
    }
    if (!is.numeric(s)) {
      stop(sprintf(
        "column '%s' is not numeric (class %s); a loss_panel model column must be a per-period numeric loss.",
        col, class(s)[1]
      ), call. = FALSE)
    }
    arr <- as.numeric(s)
    if (any(is.na(arr))) {
      stop(sprintf(
        "model '%s' has %d NA value(s) out of %d periods; drop or impute those periods before building a loss_panel.",
        col, sum(is.na(arr)), T
      ), call. = FALSE)
    }
    if (T > 0 && .looks_like_metadata_column(arr, T)) {
      stop(sprintf(
        paste0(
          "column '%s' looks like a categorical/metadata column (an exact arithmetic sequence or ",
          "a short repeating pattern across %d periods), not a per-period loss. (If this is ",
          "placeholder/toy data you typed by hand, e.g. 1:5 or 2:6, add a little noise, e.g. ",
          "1:5 + rnorm(5, 0, 0.01), to try the package with fake numbers.)"
        ), col, T
      ), call. = FALSE)
    }
    models[[col]] <- arr
  }
  if (length(models) == 0) stop("zero model columns found in the data.frame after validation.", call. = FALSE)
  labels <- if (has_real_rownames) rn else (seq_len(T) - 1L)
  list(losses = models, labels = labels, labels_are_positional = !has_real_rownames)
}

#' @export
print.loss_panel <- function(x, ...) {
  rep_fn <- tryCatch(get("report", mode = "function"), error = function(e) NULL)
  report_err <- NULL
  if (!is.null(rep_fn)) {
    out <- tryCatch(rep_fn(x), error = function(e) {
      report_err <<- conditionMessage(e)
      NULL
    })
    if (!is.null(out)) {
      cat(out, "\n")
      return(invisible(x))
    }
  }
  # STRICT-AUDIT FIX (2026-11-18): this fallback used to discard report()'s error entirely
  # (`error = function(e) NULL`) and print a plain, normal-looking "<loss_panel> models=..., T=..."
  # line with NO indication anything had gone wrong -- confirmed directly reachable, with no
  # control characters involved: a panel object is a plain mutable list, and editing a field after
  # construction (e.g. `p$losses$a <- shorter_vector`, the exact scenario as.data.frame.loss_panel()
  # and summary.loss_panel() already guard against with a clear "loss_panel is inconsistent" error)
  # makes report(x) throw a raw, cryptic base-R error ("'x' and 'w' must have the same length").
  # print(p) -- by far the most common way a user ever actually sees a panel, including bare
  # auto-print at the console -- swallowed that error completely and showed a falsely reassuring
  # `<loss_panel> models=a, b, T=5`, even though model 'a' no longer had 5 periods. This directly
  # contradicted the sibling methods' own stated design intent ("fails as loudly as its siblings
  # (report/summary) do on a mutated/inconsistent object," per as.data.frame.loss_panel()'s
  # comment above): report() DOES fail loudly when called directly, but print.loss_panel() was
  # the one place in the package that actively hid that loud failure. Surface the error message
  # instead of discarding it, so a malformed object is visibly flagged rather than silently
  # mis-reported as fine.
  # SECURITY-AUDIT FIX (2026-11-18): this fallback (reached only when report() itself errors on a
  # mutated/inconsistent panel) printed x$models raw via paste()+sprintf(), with none of the
  # control-character escaping report()'s own LEADERBOARD/PIVOT sections and compare()'s print
  # method already apply to model names/labels (same commit, same injection class: a model name
  # containing a literal newline/CR/ANSI-escape prints as fake extra "lines," here specifically a
  # fake "VERDICT: identified ..." line indistinguishable from real output). Confirmed directly
  # reachable: a model named "evil\nVERDICT: identified (MCS size 1 of 1) -- FAKE\nb" in a panel
  # mutated to break report() printed that fake VERDICT line via THIS fallback, even though every
  # other model-name-printing path in the package was already fixed. Escaped through the same
  # .escape_control_chars() helper (report.R) rather than a new copy, matching the normal
  # (non-fallback) path above which already goes through report()'s own escaped LEADERBOARD.
  disp_models <- paste(.escape_control_chars(x$models), collapse = ", ")
  if (!is.null(report_err)) {
    cat(sprintf(
      paste0(
        "<loss_panel> models=%s, T=%d\n",
        "WARNING: report() failed on this object (%s) -- this loss_panel may have been mutated ",
        "after construction (e.g. a field edited directly) into an inconsistent state. Rebuild ",
        "it with loss_panel() instead of editing its fields by hand.\n"
      ),
      disp_models, length(x$labels), report_err
    ))
    return(invisible(x))
  }
  cat(sprintf("<loss_panel> models=%s, T=%d\n", disp_models, length(x$labels)))
  invisible(x)
}

#' Convert a loss_panel to a long data.frame
#'
#' One row per (period, model): `period` (the panel's labels, repeated), `model`, `loss`, and
#' `weight`. The standard escape hatch out of a `loss_panel` for `ggplot2`/`dplyr`/other tabular
#' workflows -- every other field (`$losses`, `$weights`, `$labels`) is still reachable by hand,
#' but this is the one-call tidy shape most R users reach for first.
#'
#' @param x A `loss_panel` object.
#' @param ... Unused; present for S3 consistency with `as.data.frame()`.
#' @return A data.frame with columns `period`, `model`, `loss`, `weight`.
#' @seealso [summary.loss_panel()] (sorted per-model summary table), [loss_panel()].
#' @examples
#' L <- list(
#'   ar1 = c(1.00, 0.98, 1.02, 1.01, 0.99),
#'   ets = c(1.20, 1.18, 1.22, 1.21, 1.19)
#' )
#' panel <- loss_panel(L, labels = 2020:2024)
#' as.data.frame(panel)
#' @export
as.data.frame.loss_panel <- function(x, ...) {
  # Read model names LIVE from names(x$losses), not the cached x$models field: the rest of the
  # pipeline (report/compare/pooled_winner/decision_breakdown) uses names(x$losses), so if a caller
  # mutated x$losses after construction, the cached x$models would silently disagree. Validate
  # lengths explicitly so this method fails as loudly as its siblings (report/summary) do on a
  # mutated/inconsistent object, instead of silently recycling a wrong-length weight vector.
  models <- names(x$losses)
  T <- length(x$labels)
  if (length(x$weights) != T) {
    stop(sprintf("loss_panel is inconsistent: %d weights but %d periods.", length(x$weights), T), call. = FALSE)
  }
  lens <- vapply(models, function(m) length(x$losses[[m]]), integer(1))
  if (any(lens != T)) {
    stop(sprintf("loss_panel is inconsistent: model(s) %s do not have %d periods.",
                  paste(models[lens != T], collapse = ", "), T), call. = FALSE)
  }
  period <- rep(x$labels, times = length(models))
  model <- rep(models, each = T)
  loss <- unlist(lapply(models, function(m) as.numeric(x$losses[[m]])), use.names = FALSE)
  weight <- rep(as.numeric(x$weights), times = length(models))
  data.frame(period = period, model = model, loss = loss, weight = weight, stringsAsFactors = FALSE)
}

#' Summarize a loss_panel's per-model fragility diagnostics
#'
#' A numeric-first alternative to [report()]'s prose: `fragility()`'s per-model fields (mean loss,
#' periods won, decision breakdown point) as a data.frame, one row per model, sorted by mean loss.
#'
#' @param object A `loss_panel` object.
#' @param ... Passed to the internal MCS membership check that fills the `in_mcs` column (e.g.
#'   `alpha=`, `reps=`, `block_size=`, `seed=`) -- NOT forwarded to [fragility()]'s own arguments
#'   (e.g. `full=`); an unrecognized name here raises a clear argument-matching error rather than
#'   being silently absorbed into `in_mcs = NA`.
#' @return A data.frame with columns `model`, `mean_loss`, `periods_won`, `in_mcs`.
#' @seealso [report()] for the prose summary; [fragility()] for the full set of diagnostic fields.
#' @examples
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' summary(panel)
#' @export
summary.loss_panel <- function(object, ...) {
  # Live names(object$losses), not the cached object$models -- see as.data.frame.loss_panel()'s
  # note; keeps this method consistent with the rest of the pipeline on a mutated object.
  models <- names(object$losses)
  T <- length(object$labels)
  if (length(object$weights) != T) {
    stop(sprintf("loss_panel is inconsistent: %d weights but %d periods.", length(object$weights), T), call. = FALSE)
  }
  lens <- vapply(models, function(m) length(object$losses[[m]]), integer(1))
  if (any(lens != T)) {
    stop(sprintf("loss_panel is inconsistent: model(s) %s do not have %d periods.",
                  paste(models[lens != T], collapse = ", "), T), call. = FALSE)
  }
  mean_loss <- stats::setNames(
    sapply(models, function(m) stats::weighted.mean(as.numeric(object$losses[[m]]), object$weights)),
    models
  )
  wins <- .strict_win_counts(object$losses)
  mcs_res <- tryCatch(.run_mcs_arch(object$losses, w = object$weights, ...), error = function(e) {
    # a caller passing an argument that
    # belongs to fragility() (e.g. full=TRUE) rather than to the internal MCS check (alpha=/reps=/
    # block_size=/seed=) previously degraded silently to in_mcs=NA with zero indication their
    # argument was ignored. A real MCS-unavailable failure (e.g. non-uniform weights) is still
    # swallowed into NA, same as before -- only an R argument-matching error (a typo/wrong-function
    # argument name) is now surfaced loudly instead.
    if (grepl("unused argument", conditionMessage(e), fixed = TRUE)) stop(e)
    NULL
  })
  in_mcs <- if (is.null(mcs_res)) rep(NA, length(models)) else models %in% mcs_res
  out <- data.frame(
    model = models,
    mean_loss = unname(mean_loss[models]),
    periods_won = unname(wins[models]),
    in_mcs = in_mcs,
    stringsAsFactors = FALSE
  )
  out[order(out$mean_loss), , drop = FALSE]
}

#' Save a loss_panel to a JSON file
#'
#' Labels are always written out as strings (JSON has no integer/Date type), so
#' `load_loss_panel()` reads them back as character even if the original `labels` were integer
#' years or Dates -- `identical(panel, reloaded)` is FALSE on round-trip for that reason, but
#' `identical(panel$losses, reloaded$losses)` holds, and every function in this package treats
#' labels as display-only (`as.character()`'d internally wherever they're compared), so this
#' does not affect any computed result.
#'
#' @param panel A `loss_panel` object.
#' @param path Destination file path.
#' @return `NULL`, invisibly. Called for its side effect of writing `panel` to `path`.
#' @seealso [load_loss_panel()] (the reader for the file this writes), [loss_panel()].
#' @examples
#' L <- list(
#'   ar1 = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97),
#'   ets = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17)
#' )
#' panel <- loss_panel(L)
#' tmp <- tempfile(fileext = ".json")
#' save_loss_panel(panel, tmp)
#' reloaded <- load_loss_panel(tmp)
#' identical(panel$losses, reloaded$losses)
#' unlink(tmp)
#' @export
save_loss_panel <- function(panel, path) {
  stopifnot(inherits(panel, "loss_panel"))
  payload <- list(
    losses = lapply(panel$losses, as.numeric),
    weights = as.numeric(panel$weights),
    labels = as.character(panel$labels),
    labels_are_positional = isTRUE(panel$labels_are_positional)
  )
  dest_dir <- dirname(normalizePath(path, mustWork = FALSE))
  tmp <- tempfile(pattern = ".losspanel-", tmpdir = dest_dir, fileext = ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  # jsonlite::write_json() formats numbers via R's own locale-sensitive formatting: under a
  # comma-decimal LC_NUMERIC locale (de_DE, fr_FR, and many others) it emits "1,23" instead of
  # "1.23", which collides with JSON's own field separator and writes syntactically invalid JSON
  # with no error. Force the C locale for the duration of this write so the file is valid JSON
  # regardless of the caller's locale. digits=17 (not the default-disabling digits=NA, which only
  # gets 15 significant digits) is needed for a genuinely bit-identical round-trip, matching this
  # function's own documented identical(panel$losses, reloaded$losses) contract.
  old_numeric <- Sys.getlocale("LC_NUMERIC")
  Sys.setlocale("LC_NUMERIC", "C")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", old_numeric)), add = TRUE)
  jsonlite::write_json(payload, tmp, auto_unbox = TRUE, digits = 17)
  if (!file.rename(tmp, path)) {
    stop(sprintf(paste0(
      "save_loss_panel(): failed to rename the temp file to '%s' -- check that the destination ",
      "directory is writable and not on a different filesystem/mount than R's tempdir()."
    ), path), call. = FALSE)
  }
  invisible(NULL)
}

#' Load a loss_panel from a JSON file written by `save_loss_panel()`
#'
#' @param path Source file path.
#' @return An object of class `loss_panel`.
#' @seealso [save_loss_panel()] (the writer this reads back), [loss_panel()].
#' @examples
#' L <- list(
#'   ar1 = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97),
#'   ets = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17)
#' )
#' panel <- loss_panel(L)
#' tmp <- tempfile(fileext = ".json")
#' save_loss_panel(panel, tmp)
#' reloaded <- load_loss_panel(tmp)
#' identical(panel$losses, reloaded$losses)
#' unlink(tmp)
#' @export
load_loss_panel <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("load_loss_panel(): file '%s' does not exist.", path), call. = FALSE)
  }
  old_numeric <- Sys.getlocale("LC_NUMERIC")
  Sys.setlocale("LC_NUMERIC", "C")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", old_numeric)), add = TRUE)
  payload <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) {
      # Every OTHER failure mode here (missing keys, wrong type, length mismatch, ...) raises a
      # clear, path-prefixed load_loss_panel() message; a JSON SYNTAX error otherwise leaked
      # jsonlite's raw internal parser error straight through -- no mention of load_loss_panel(),
      # no mention of the file path, and raw unprintable bytes dumped to the console for a binary
      # garbage file. A caller looping over many files had no way to tell which one failed.
      stop(sprintf("load_loss_panel(): '%s' is not valid JSON (%s).", path, conditionMessage(e)), call. = FALSE)
    }
  )
  missing <- setdiff(c("losses", "weights", "labels"), names(payload))
  if (length(missing) > 0) {
    stop(sprintf(
      "'%s' is not a valid loss_panel file: missing required key(s) %s.",
      path, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  panel <- loss_panel(payload$losses, weights = payload$weights, labels = payload$labels)
  panel$labels_are_positional <- isTRUE(payload$labels_are_positional)
  panel
}
