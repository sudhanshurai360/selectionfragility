# selectionfragility

**Is your "best" forecasting model a real choice, or a fragile artifact of a few periods?**

`selectionfragility` provides two complementary, decision-oriented diagnostics for forecast
**model selection**:

- **Model Confidence Set (MCS)** — which models are statistically *indistinguishable* from the
  best (Hansen, Lunde & Nason 2011). A set of size 1 means the best model is **point-identified**;
  a larger set means selection is **not identified**.
- **Decision breakdown point (`k*`)** — the *fewest evaluation periods whose deletion flips the
  pooled winner*, a model-selection application of the robust-statistics breakdown-point concept
  (Hampel 1971) to a selection decision rather than an estimator. A small `k*` means the crowned
  model rests on a handful of periods and should not be trusted as a stable choice.

This is the native R port of the Python package
[`selection-fragility`](https://github.com/sudhanshurai360/selection-fragility) (PyPI:
`pip install selection-fragility`) — same method, same validated numerics, independently verified
against it. See that repo for the accompanying paper and the full evaluation card.

## Install

```r
# From CRAN, once published:
install.packages("selectionfragility")

# Right now, from GitHub:
# install.packages("remotes")
remotes::install_github("sudhanshurai360/selectionfragility")
```

## Quick start

```r
library(selectionfragility)

L <- list(
  ar1    = c(1.00, 0.98, 1.02, 1.01, 0.99, 1.03, 0.97, 1.00, 1.02, 0.98),
  nbeats = c(0.90, 0.88, 0.92, 0.91, 0.89, 0.93, 0.87, 0.90, 2.60, 0.88),
  ets    = c(1.20, 1.18, 1.22, 1.21, 1.19, 1.23, 1.17, 1.20, 1.22, 1.18)
)

panel <- loss_panel(L)
report(panel)
```

Notice `ar1` is crowned the pooled-mean champion even though `nbeats` wins 9 of the 10 periods
outright — one bad period (a loss of 2.60) is enough to flip the pooled winner. That gap between
"wins almost every period" and "loses on the pooled mean" is exactly what this package's
diagnostics are built to surface: the MCS tells you whether that gap is statistically real or just
noise, and `k*` tells you how many periods' removal it would take to flip the verdict back.

If the Model Confidence Set comes back larger than 1 model, or `k*` is small (as here), don't
report a single champion as settled — report the full MCS membership, note the small `k*`
explicitly, and either collect more evaluation periods or treat the two models as practically
tied until you have more data. `report()`'s R1/R2 refusal rules (its resolved-edge check and its
Model Confidence Set check — see `?resolution_report`) already withhold a confident `k*` read
when the data can't support one; take that refusal as the answer, not an obstacle.

See `?selectionfragility` for a one-page map of every function in the staged pipeline, and
`vignette("selectionfragility")` for the full walkthrough — installation, interpreting the
report, the staged API, weights, CI-gating with `compare()`, and the honest-reporting caveats
(what `k*`/`winner_stability`/the MCS will and will not tell you — including why a result computed
on autocorrelated forecast losses, the normal case in time series, needs extra care; see that
section before trusting a close "resolved" call).

## Relationship to the Python package

Function names and signatures are near-identical 1:1 with the Python package
(`loss_panel()` ≈ `LossPanel.from_losses()`, `report()`, `decision_breakdown()`, `mcs_size()`,
`identified()`, `compare()`, `pooled_winner()`), and the one-screen `report()` output is
structured identically in both languages. The following do **not** carry over:

- **No `from_forecasts()` equivalent yet** — this port only takes an already-computed loss panel.
- **Bootstrap-based numbers are not bit-for-bit identical across languages** — R's RNG and
  NumPy's RNG are different generators, so `winner_stability()`, the MCS bootstrap p-value, and
  `compare()`'s churn rate will not match exactly on "the same" seed. Compare conclusions, not
  decimals.
- **`resolution_report()` can emit an R-only warning the Python original never raises** — when
  weights are non-uniform and positively correlate with the per-period differential's volatility
  while reporting `resolved=TRUE`, the R port `warning()`s (see `?resolution_report`'s
  "Assumption" section); the Python original's `_weighted_se` documents the same understatement
  risk in comments but does not act on it. A script meant to behave identically across both
  languages may see this warning fire in R with nothing analogous in Python under the same inputs.
- **R is currently stricter than Python v1.0.4 on three input-validation points**, confirmed by
  direct live-behavioral diffing (2026-11-18), all added in this R port's own hardening and not
  yet mirrored upstream: (1) `loss_panel()` caps weight magnitude at 1e100 — Python's
  `_validate_weights()` has no such cap, and the exact silent-corruption failure this guard
  prevents (a huge weight overflowing `sum(weights)` to `Inf`, producing a `NaN` mean, then an
  unrelated, misleading crash) was reproduced directly in live Python; (2) `loss_panel()` rejects
  a logical (`TRUE`/`FALSE`) weights vector — Python silently coerces it to 0/1; (3) `loss_panel()`
  rejects an empty-string model/column name — Python accepts it and it flows cleanly through
  `report()`. These are one-directional: Python is more permissive, not differently-behaved, on
  all three, so an R caller never sees Python's result silently replaced by a different one — they
  get a clear R-side error instead where Python would have proceeded.
- **Five more confirmed divergences from a second round of live-behavioral diffing (2026-11-18)**:
  `identify()`/`mcs_size()`'s `block_size=0` (iid resampling) works in R but errors in Python's
  equivalent; conversely R requires `reps>=2` where Python's equivalent silently accepts `reps=1`.
  `winner_stability()`'s new degenerate-pair warning and new seed validation are both R-only —
  Python's equivalent stays silent on a degenerate pair and accepts any seed (including `None`)
  on that code path. `.fsum()` (R's permutation-invariant tie-break summation) is not provably
  exact the way Python's `math.fsum()` is — confirmed to return a catastrophically wrong answer
  on a contrived input at this package's own weight/loss magnitude cap, though it remains
  permutation-invariant even then (see `?concentration_share`). `compare()`'s reversed-argument
  guard is R-only, confirmed by execution — Python silently returns a plausible but backwards
  result on swapped arguments.
- **`save_loss_panel()`/`load_loss_panel()` round-trip losslessly with Python's `LossPanel.save()`/
  `.load()`**, confirmed by direct cross-language execution (2026-11-18): a 5-model, 24-period
  panel with non-uniform weights and real date-like labels, built and saved in one language,
  loads in the other with `losses`/`weights` bit-identical to double precision and `labels`
  identical as text. Both sides write/read the same JSON schema (`losses`/`weights`/`labels`/
  `labels_are_positional`) regardless of key order. R's locale-safety fix (forcing `LC_NUMERIC="C"`
  around the write/read) has no Python counterpart because none is needed — Python's `json` module
  formats floats independent of the active locale (verified directly under `de_DE.UTF-8`: a
  Python-saved file is still dot-decimal, and R reads it correctly even with R itself run under
  that same locale). Non-ASCII model names/labels round-trip cleanly too, even though the two
  languages pick different valid JSON encodings for them (R's `jsonlite` writes literal UTF-8
  bytes; Python's `json` defaults to `\uXXXX`-escaping — both are standard JSON that the other
  side's reader decodes correctly). The two newer R-only `loss_panel()` guards documented above
  were checked specifically at this file boundary with real Python-produced files: a weight of
  `1e200` saved by Python's `.save()` is correctly rejected by `load_loss_panel()` on read
  (`"weights must not exceed 1e+100 in magnitude"`); a `None` label is accepted by Python's
  `LossPanel.from_losses()` at construction time but `.save()` always stringifies labels
  (`str(label)`), so it reaches the JSON file as the literal string `"None"`, not a JSON `null` —
  R's NA/NULL-label rejection can never actually be reached through Python's real `.save()`, only
  through a hand-crafted file.

- **`report()`'s own structure matches across languages; three of its newest caveats/fixes do
  not** — confirmed by direct execution (2026-11-18), not just reading the source. Both languages'
  `report()` print the identical four sections, same names, same order (VERDICT, LEADERBOARD,
  RESOLUTION, PIVOT), on every constructed panel tried. But: (1) R's VERDICT prints a "these answer
  different questions" caveat when "not identified" and "resolved" co-occur, and PIVOT prints a
  "not a contradiction" line when Prop. 2.2's certificate and RESOLUTION's "resolved" disagree —
  Python's `report()` has neither, confirmed on a live panel where Python prints "not identified"
  with no caveat at all even though RESOLUTION says "resolved" on the same panel. (2) R escapes
  control characters/ANSI escapes in model names and period labels before printing them into
  LEADERBOARD/PIVOT; Python's original has the identical unfixed vulnerability — a model name with
  an embedded newline (reproduced directly) prints as a literal fake `VERDICT:` line inside
  Python's LEADERBOARD, indistinguishable from a real section header, where R prints one
  safely-quoted row. (3) R's `report(seed=...)` threads into the PIVOT section's
  `pivot_agreement()` bootstrap; Python's `report()` (as of upstream commit `5be875b`, which does
  have a `seed=` parameter) does not — it is hardcoded to `seed=0` inside that one internal call,
  confirmed by running the same panel at `seed=0..4`: R's printed `pivot_agreement:` percentage
  moves across seeds, Python's does not. These are the same three fixes made to this R port this
  round (see `NEWS.md`), now confirmed to be genuine R-only enhancements rather than things Python
  already had.

Cite either implementation; they compute the same quantities.

## Relationship to existing CRAN packages

`identified()`/`mcs_size()` can run on the CRAN [`MCS`](https://cran.r-project.org/package=MCS)
package's own `MCSprocedure()` engine, cross-checked against this package's independent native
re-implementation (`mcs()`/`model_confidence_set()`) — MCS membership by itself is not new here,
`MCS` already does that; what this package adds on top is the decision breakdown point (k*),
resolution/power diagnostics, selection-as-election diagnostics, and `compare()`.

`mcb_bound()` is **not** [`tsutils`](https://cran.r-project.org/package=tsutils)'s `nemenyi()`
rank-based "MCB plot" (the Koning et al. 2005-style simultaneous-CI-on-ranks tool familiar from
the M3/M4 forecasting-competition literature). It bounds the loss-level edge directly — a
Hsu-style multiple-comparisons-with-the-best bound on the mean loss differential, not on average
ranks. Use `tsutils` if you want the classic rank/Nemenyi MCB plot across many series; use
`mcb_bound()` if you want a bound on the champion's actual loss-level edge over its binding rival.

This package does not implement or wrap the Diebold-Mariano test (`forecast::dm.test()`,
`multDM`, or similar) and is not a drop-in substitute for it. DM tests pairwise predictive-accuracy
equality between exactly two models on one comparison; this package answers the different,
*selection*-oriented question of how fragile the champion is once you are choosing among two or
more candidates — the Model Confidence Set already generalizes a pairwise DM-style test to K ≥ 2
models, and `k*`/`compare()` go further to ask how many periods or how much re-evaluation it would
take to flip that choice. If you have exactly two models and only want a pairwise equal-accuracy
test, `dm.test()` is the simpler, right tool; reach for `selectionfragility` once you are
*selecting* a winner from several candidates and want to know how stable that selection is, not
just whether two models differ.

## No plotting

`selectionfragility` ships no plotting methods — no `plot.loss_panel()`, no MCS-elimination
diagram, no bootstrap-distribution histogram. Every diagnostic here returns a number, a
data.frame (`as.data.frame.loss_panel()`, `summary.loss_panel()`), or `report()`'s printed text;
visualizing any of it (per-period losses, the `winner_stability()` bootstrap draws, etc.) is
ordinary `ggplot2`-on-a-data-frame work once you have `as.data.frame(panel)`, and this package
deliberately does not duplicate that. If you want a ready-made comparison plot across many series,
see `tsutils::nemenyi()`'s MCB plot above.

## License

MIT — see [LICENSE.md](LICENSE.md).
