#' selectionfragility: Decision Breakdown Point and Selection-Fragility Diagnostics
#'
#' A staged pipeline for forecast-model-selection diagnostics, from a validated loss panel
#' through identification ([mcs()]/[mcs_size()]), the decision breakdown point ([decision_breakdown()]),
#' resolution/power checks ([resolution_report()]), significance certification ([prop22_certifies()]),
#' pivot/concentration analysis ([pivot_agreement()]), and run-over-run comparison ([compare()]).
#' Start at [loss_panel()] and [report()], or read `vignette("selectionfragility")` for a guided tour.
#'
#' @section Running this package's bootstrap functions in parallel:
#' [mcs()], [winner_stability()], and [exchangeable_benchmark()] all take a `seed=` argument
#' (default `0`), reseed R's RNG from it, and restore the caller's `.Random.seed` on exit, so
#' their results neither depend on nor disturb the session's RNG state. They are safe to call from
#' `parallel::mclapply()`/`future`/`foreach` workers, provided each worker gets a *different*
#' seed: the same literal seed in every worker reproduces one bootstrap N times. Use e.g.
#' `parallel::mclapply(1:4, function(s) winner_stability(L, seed = s))`.
#'
#' @section Sentinel conventions for "cannot determine":
#'   `NA` marks a result that was computed but where the question does not apply to this input
#'   (e.g. [fragility()]'s `fragile` with no `benchmark`, [prop22_certifies()] when `k<=0` or
#'   `k>=T`); it appears inside scalar or typed-vector returns. `NULL` marks a diagnostic whose
#'   computation did not complete -- a caught error inside a list-returning function, e.g.
#'   `resolution_report()$identified` or `compare()$old_champion_still_in_mcs` (matching Python's
#'   `None`). Check each field's documentation for which sentinel it uses.
#'
#' @keywords internal
"_PACKAGE"
