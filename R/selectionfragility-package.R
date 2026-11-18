#' selectionfragility: Decision Breakdown Point and Selection-Fragility Diagnostics
#'
#' A staged pipeline for forecast-model-selection diagnostics, from a validated loss panel
#' through identification ([mcs()]/[mcs_size()]), the decision breakdown point ([decision_breakdown()]),
#' resolution/power checks ([resolution_report()]), significance certification ([prop22_certifies()]),
#' pivot/concentration analysis ([pivot_agreement()]), and run-over-run comparison ([compare()]).
#' Start at [loss_panel()] and [report()], or read `vignette("selectionfragility")` for a guided tour.
#'
#' @section Running this package's bootstrap functions in parallel:
#' [mcs()], [winner_stability()], and [exchangeable_benchmark()] all require an explicit `seed=`
#' rather than drawing one implicitly, which makes them safe to call from
#' `parallel::mclapply()`/`future`/`foreach` workers -- but only if each worker is given a
#' *different* seed. `parallel::mclapply()` on non-Windows forks the current process, and a fork
#' gives every child an independent, non-interfering copy of both R's global options (confirmed:
#' `options(selectionfragility.elimination = ...)` set inside one forked worker is invisible to
#' sibling workers and to the parent) and `.Random.seed` -- so there is no cross-worker state
#' leakage from this package. The one real risk is a *user* mistake, not a package bug: calling
#' e.g. `parallel::mclapply(1:4, function(i) winner_stability(L, seed=99))` with the SAME literal
#' seed in every worker deterministically reproduces the identical bootstrap in all four, silently
#' giving you four copies of one result dressed up as four independent ones. Vary the seed per
#' worker instead, e.g. `parallel::mclapply(seeds, function(s) winner_stability(L, seed=s))` with
#' `seeds <- 1:4` (or any panel-specific offset), so each worker's bootstrap draws are genuinely
#' independent.
#'
#' @section Sentinel conventions for "cannot determine": this package uses two different
#'   sentinels for "this diagnostic could not be determined," deliberately, not by accident.
#'   `NA` marks a DEGENERATE-BUT-COMPUTED result: the comparison actually ran, but the specific
#'   question doesn't meaningfully apply to this input (e.g. [fragility()]'s `fragile` field when
#'   no `benchmark` was supplied, [prop22_certifies()] when `k<=0`/`k>=T`, or
#'   [concentration_share()]/[pivot_agreement()] in their own degenerate cases) -- these live
#'   inside a scalar/typed-vector return where `NULL` would not be type-stable. `NULL` marks a
#'   diagnostic whose underlying computation never completed at all: a caught error inside a
#'   list-returning function, most commonly [identified()] erroring inside
#'   [resolution_report()]'s R2 check or [compare()]'s internal MCS call (e.g.
#'   `resolution_report()$identified`, `compare()$old_champion_still_in_mcs`). For
#'   `resolution_report()$identified` specifically, `NULL` additionally mirrors the Python
#'   original's `None` for the identical caught-error case -- a deliberate cross-language parity
#'   choice, not just an internal convention. Don't write code that treats `NA` and `NULL` as
#'   interchangeable across this package's return values; check each field's own documentation
#'   for which sentinel it actually uses.
#'
#' @keywords internal
"_PACKAGE"
