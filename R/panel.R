#' @importFrom stats setNames
NULL

# loss_panel — the single validated entry point for every public function in this package.
#
# Mirrors Python's LossPanel.from_losses() (panel.py). The bare constructor is not exported, so
# callers cannot skip validation. from_forecasts() is not ported: build a wide loss
# matrix/data.frame/list and pass it to loss_panel().

.MAX_ABS_LOSS <- 1e100
.LARGE_K_WARN_THRESHOLD <- 100

#' @keywords internal
.looks_like_metadata_column <- function(arr, T) {
  arr <- as.numeric(arr)
  if (T < 4) return(FALSE)
  # Leave NA/Inf to the worded check downstream (R's NA == x would crash all() here).
  if (anyNA(arr) || any(!is.finite(arr))) return(FALSE)
  if (all(arr == arr[1])) return(FALSE)
  diffs <- diff(arr)
  if (length(diffs) > 0 && all(diffs == diffs[1]) && diffs[1] != 0) return(TRUE)
  max_p <- min(10, T %/% 2)
  if (max_p >= 2) {
    for (p in 2:max_p) {
      reps_needed <- ceiling(T / p)
      tiled <- rep(arr[1:p], reps_needed)[1:T]
      # Exact equality, as Python's np.array_equal; all.equal()'s tolerance would flag real losses.
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
    # as.numeric() on a factor returns level codes, not values.
    stop(sprintf(
      "weights must be a numeric vector of %d values (one per period), not a factor; got levels %s. ",
      T, paste(levels(w), collapse = ", ")
    ), call. = FALSE)
  }
  if (is.logical(w)) {
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
    # Huge weights can overflow sum(w) to Inf and turn every weighted mean into NaN.
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
      # compare() matches periods by label; an NA label would be miscounted as a new period.
      stop("labels must not contain NA/NULL values -- every period needs a real identity for compare() to match on.", call. = FALSE)
    }
    if (length(unique(str_labels)) != T) {
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
  # anyNA() first: `nm == ""` is NA for an NA name and would crash the if().
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
    # list[[""]] and list[[NA]] return NULL even when such an element exists, so a blank name
    # would surface later as a confusing length error. Reject it here.
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
  # Same blank-name problem as .from_matrix(): df[[""]] returns NULL.
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
  # report() failed (usually a panel edited after construction): say so rather than print a
  # normal-looking summary, and escape model names so they cannot inject fake output lines.
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
  # Use live names(x$losses), not the cached x$models, and fail loudly on an edited, inconsistent
  # panel rather than recycle a wrong-length weight vector.
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
#' # Small panel; expect the MCS small-sample warning from the in_mcs check.
#' L <- list(
#'   ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
#'   nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
#'   ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
#' )
#' panel <- loss_panel(L)
#' summary(panel)
#' @export
summary.loss_panel <- function(object, ...) {
  # Same live-names and consistency checks as as.data.frame.loss_panel().
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
    # A misspelled or wrong-function argument (e.g. full=TRUE) is an error; a genuine MCS failure
    # (e.g. non-uniform weights) becomes in_mcs = NA.
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
  # Force the C locale: under a comma-decimal locale (de_DE, fr_FR) jsonlite writes "1,23", which
  # is invalid JSON. digits = 17 gives a bit-identical round-trip.
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
      # Name the file in JSON syntax errors (jsonlite's message omits it), so a loop over many
      # files shows which one failed.
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
