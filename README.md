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
tied until you have more data. `report()`'s R1/R2 refusal rules already withhold a confident
`k*` read when the data can't support one; take that refusal as the answer, not an obstacle.

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
structured identically in both languages. Three things do **not** carry over:

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

## License

MIT — see [LICENSE.md](LICENSE.md).
