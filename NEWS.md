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
* 187 tests (`testthat`, edition 3), including a dedicated cross-language oracle-comparison suite
  verified against the Python package's live behavior, not just its documented contract.
* `R CMD check --as-cran`: 0 errors, 0 warnings; passes R-hub's cross-platform check (Linux, macOS,
  Windows, R-devel).
* Known, documented differences from the Python original: `loss_panel()` does not yet port
  `from_forecasts()` (build a wide loss matrix/data.frame/list yourself); RNG draws are not
  bit-for-bit identical across languages (statistically equivalent, not byte-identical).
* Pre-release hardening (multiple rounds of independent adversarial/CRAN-reviewer/adopter review):
  quantified a seed-shopping vulnerability in the MCS/identification layer (see `?mcs`,
  "Adversarial use" section) and added `report()`'s `seed=` argument so a caller can check it;
  added defensive guards around the `MCS::MCSprocedure()` cross-check call and `save_loss_panel()`'s
  atomic write; pinned a minimum `MCS` version.
