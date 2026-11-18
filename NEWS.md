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
* 1032 tests (`testthat`, edition 3; grew from an initial 187 across several hardening rounds --
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
