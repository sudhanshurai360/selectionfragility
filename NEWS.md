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
* 1146 tests (`testthat`, edition 3; grew from an initial 187 across several hardening rounds --
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
  A second round of live-behavioral diffing (2026-11-18) found five more, in both directions:
  `identify()`/`mcs_size()`'s `block_size=0` (iid resampling) is accepted by R but rejected by
  Python's equivalent entry point; conversely R enforces a `reps>=2` floor Python's equivalent
  lacks (Python silently accepts `reps=1`). `winner_stability()`'s degenerate-pair `warning()`
  (added this round) and its seed validation (also added this round, which now fires even on a
  degenerate pair and rejects `seed=NULL`) are both R-only -- Python's equivalent stays silent on
  the degenerate case and accepts `seed=None`/any seed value on that path without validation.
  `.fsum()` (R, this package's permutation-invariant summation for opponent tie-breaking) is NOT
  provably exact the way Python's `math.fsum()` is -- confirmed to return a catastrophically
  wrong answer (0 instead of 10) on a contrived mixed-sign input at this package's own 1e100
  magnitude cap, while still remaining permutation-invariant (the one guarantee the tie-break
  logic actually depends on); not fixed, since this needs near-cap-magnitude near-exact
  cancellation unlikely on realistic forecast-loss data, and reimplementing Shewchuk's algorithm
  correctly is a nontrivial separate undertaking. `compare()`'s reversed-argument detection guard
  was independently re-confirmed (by execution, not just reading R's own code comment) to be
  R-only; Python's `compare()` silently returns a plausible-looking but causally backwards result
  on swapped arguments.
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
  unify. A final strict-audit round the same day closed several more gaps: `mcs()`'s own `seed=`
  and `compare()`'s internal `.churn_base_rate()` `seed=` had no coercion/validation guard at all
  -- not merely missing from the three RNG functions listed above -- unlike `identify()`'s
  internal `seed` (already listed above as fixed); `per_period_winner()`/`decision_breakdown()`'s
  shared `return_ties` parameter and `decision_breakdown()`'s own `a=` (explicit-champion)
  parameter had no input validation either, the latter previously mis-blaming a length-mismatch
  error on `breakdown_number()` instead of naming the real problem (an unrecognized model name);
  `winner_stability()`'s `circular` parameter was unguarded, and its degenerate-pair short-circuit
  now `warning()`s that the returned `freq` is a hardcoded uniform placeholder, not a real
  bootstrap win frequency; `fragility.R`'s own internal weight validator (used by every raw-list
  call, not just `loss_panel()`) was mirrored to reject character/factor/logical weight vectors
  and cap weight magnitude at 1e100, matching `panel.R`'s already-hardened `.validate_weights()`;
  `loss_panel()` now rejects an NA/NULL period label outright (previously it passed silently
  unless duplicated, which could defeat `compare()`'s new-vs-missing-period safety check); `mcs()`
  now rejects a data.frame with non-numeric columns instead of silently coercing or crashing raw;
  `.is_degenerate_pair()`, `breakdown_number()`'s starting margin, and `fragility()`'s per-opponent
  margins now use the same permutation-order-invariant `.fsum()` already used in pivot.R, closing
  a sibling instance of the identical rename/reorder-dependence bug; `report()` now prints an
  explicit "these answer different questions" caveat when VERDICT says "not identified" but
  RESOLUTION says "resolved" (previously only the opposite direction was flagged), and a short
  explanatory line when Proposition 2.2's certificate and RESOLUTION's own "resolved" read
  disagree (different variance conventions, not a contradiction). Two further findings, confirmed
  but left unfixed as shared/oracle-pinned or out-of-scope for a formula change this close to
  submission: `mcs()`'s bootstrap is measurably over-sized even under a true null with zero
  autocorrelation (~18% false-elimination at alpha=0.10, T=30, K=3, vs. the nominal ~10%; ~9% at
  alpha=0.05 vs. the nominal 5%) -- additive to, and separate from, the already-documented AR(1)
  effect (see `?mcs`); and `minimum_detectable_edge()`/`significance_boundary()`/`mcb_bound()`/
  `resolution_report()`'s champion-mean normalization has a hard, unsmoothed floor at 1e-12 that a
  champion mean merely small relative to its own volatility (not just an extreme cross-model scale
  mismatch) can land near, where a sub-measurement-precision input change can swing the reported
  percentage by 10+ orders of magnitude (see their own `@section Assumption`).
  A final re-verification pass the same day, specifically targeting the fixes above rather than
  trusting them, found and closed several more: `exchangeable_benchmark()`'s seed-collision guard
  threshold was off by one (erred safe but rejected a legitimate `n_rep`), and separately a
  "valid" seed near `.Machine$integer.max` could overflow the function's own `seed + r` loop
  arithmetic back into the exact raw, unworded error the seed-guard round was meant to eliminate
  -- both now fixed; the control-character/ANSI-escape output-injection fix applied to `report()`'s
  LEADERBOARD had the identical gap in PIVOT's printed period labels and in `compare()`'s
  `print.change_report()` (a model name with an embedded newline could inject a fake section
  header into either), both now closed with the same escaping helper; `loss_panel()`'s empty-name
  rejection turned out to be deliberate on only one of its three construction paths (list) and
  accidental on the other two (matrix/data.frame, via a base-R `[[` lookup quirk) -- and the one
  "deliberate" path had its own latent bug, crashing with a raw, unworded error on a genuine `NA`
  model name instead of this package's own `stop()`-with-message convention; all three paths now
  share one deliberate, uniform guard. A fourth confirmed-but-left-undocumented limitation in
  resolution.R was found: `mcb_bound()`'s own stated "never tighter than the observed edge"
  guarantee can be violated under extreme loss magnitude combined with weight concentration and
  near-zero true variance, confirmed bit-for-bit identical in the live Python oracle (same
  deliberately bit-for-bit-matched formula) -- documented with a regression test that pins the
  current (known-broken) behavior, not fixed, for the same shared-formula reason as the other
  `resolution.R` limitations above.
