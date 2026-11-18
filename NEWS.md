# selectionfragility 0.1.0

Initial release: a native R port of the Python package
[`selection-fragility`](https://github.com/sudhanshurai360/selection-fragility) v1.0.4.

* 9 modules ported: `loss_panel`/panel validation, `fragility` (pooled winner, decision breakdown
  point k*, winner stability, exchangeable benchmark, surprise concentration, selection-as-election
  diagnostics), `mcs`/`model_confidence_set` (native Model Confidence Set engine), `identify`
  (independent cross-check engine delegating to `MCS::MCSprocedure()`), `resolution` (minimum
  detectable edge, significance boundary, MCB bound, selection regret, resolution report with R1/R2
  refusal rules), `prop22` (zero-simulation Proposition 2.2 certificate), `pivot` (concentration
  share, pivot agreement), `report` (one-screen summary), `compare` (run-over-run champion-change
  diagnostic).
* 1102 tests (`testthat`, edition 3; grew from an initial 187 across several hardening rounds --
  this count will keep moving as hardening continues; check `devtools::test()`'s own output rather
  than treating this number as authoritative), including a dedicated cross-language
  oracle-comparison suite verified against the Python package's live behavior, not just its
  documented contract.
* `R CMD check --as-cran`: 0 errors, 0 warnings given a complete LaTeX toolchain and a current
  HTML Tidy on the checking machine (both independently confirmed); a machine missing either
  reports environment-only PDF/HTML-manual ERROR/WARNING/NOTE lines that are not package defects.
  Passes R-hub's cross-platform check (Linux, macOS, Windows, R-devel).
* Known, documented differences from the Python original: `loss_panel()` does not yet port
  `from_forecasts()` (build a wide loss matrix/data.frame/list yourself); RNG draws are not
  bit-for-bit identical across languages (statistically equivalent, not byte-identical);
  `resolution_report()` in the R port can `warning()` when weights are non-uniform and positively
  correlate with the per-period differential's volatility while reporting `resolved=TRUE` -- the
  Python original documents the identical understatement risk in comments but never raises a
  warning for it, so this is an R-only, currently undisclosed-until-now behavioral difference.
  Confirmed by direct live-behavioral diffing against Python v1.0.4 (2026-11-18): the R port is
  currently STRICTER than Python on three input-validation points added in this round's hardening
  and never mirrored upstream -- (a) `loss_panel()`'s weight-magnitude cap (R rejects a weight
  above 1e100; Python's `_validate_weights()` has no such cap, and the identical silent-corruption
  failure this R guard was built to prevent -- `sum(weights)` overflowing to `Inf`, producing a
  `NaN` mean, then a misleading unrelated crash elsewhere -- was reproduced directly in live
  Python); (b) `loss_panel()` rejecting a logical (`TRUE`/`FALSE`) weights vector, where Python
  silently coerces it to 0/1; (c) `loss_panel()` rejecting an empty-string model/column name,
  where Python's `LossPanel.from_losses()` accepts it and it flows cleanly through `report()`.
  These are currently one-directional (R stricter, not wrong): a caller relying on Python's more
  permissive behavior for any of these three inputs will get an R-side error instead.
* Pre-release hardening (multiple rounds of independent adversarial/CRAN-reviewer/adopter review):
  quantified a seed-shopping vulnerability in the MCS/identification layer (see `?mcs`,
  "Adversarial use" section) and added `report()`'s `seed=` argument so a caller can check it;
  added defensive guards around the `MCS::MCSprocedure()` cross-check call and `save_loss_panel()`'s
  atomic write; pinned a minimum `MCS` version; found and fixed a fresh-session `.Random.seed`
  leak in 6 internal bootstrap wrappers (`pivot_agreement`, `mcs`, `model_confidence_set`,
  `winner_stability`, `exchangeable_benchmark`, `compare`), including a second, more subtle leak
  window where `set.seed()` ran one statement before `on.exit()`'s restore was registered (closed
  by reordering, not by re-deriving the restore logic itself); `report()`'s `seed=` now also
  threads into the PIVOT section's `pivot_agreement()` call, which was previously hardcoded to
  `seed=0` regardless of what the caller passed; closed several "fixed one parameter, missed an
  identical sibling" validation gaps found by repeated strict full-history regression audits
  (`exchangeable_benchmark()`'s `T`, `mcs()`'s `elimination`, `identify()`'s internal `seed`,
  `compare()`'s `n_perm` -- the last of which could silently return a wrong statistic under a
  fractional value, not just crash -- and `loss_panel()`'s weights accepting a logical vector).
  A further round found and closed: `breakdown_number()`'s greedy elimination loop used naive
  float64 subtraction while its starting total used compensated summation, letting rounding
  error cross the stop threshold one removal too early in rare near-tied panels (understating
  k*, confirmed via 50-digit decimal arithmetic and a brute-force adversarial sweep); three more
  RNG-consuming functions (`winner_stability`, `exchangeable_benchmark`, `pivot_agreement`) were
  missing the same seed-validation guard added elsewhere, plus a theoretical (currently
  unreachable at default settings) seed-collision in `exchangeable_benchmark()`'s loop
  arithmetic, both now guarded; an output-injection bug where a model name containing control
  characters (e.g. an embedded newline) could print a fake section header into `report()`'s
  LEADERBOARD, now escaped; `mcs()`'s advertised small-T calibration was independently confirmed,
  via direct comparison against live Python and the external `MCS::MCSprocedure()` engine, to be
  an inherent property of the Hansen-Lunde-Nason procedure rather than an R-side defect (three
  independent implementations show the same pattern); `pivot_agreement()`'s weak discriminative
  power was likewise confirmed shared with the Python original, not an R-specific flaw; and the
  three different "which opponent" tie-break rules across `decision_breakdown()`/
  `concentration_share()`/`resolution_report()` were confirmed, from the Python oracle's own
  documented rationale, to be intentionally different by design rather than an inconsistency to
  unify.
