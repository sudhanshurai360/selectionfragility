# selectionfragility

**Is your "best" forecasting model a real choice, or a fragile artifact of a few periods?**

`selectionfragility` provides two complementary, decision-oriented diagnostics for forecast
**model selection**:

- **Model Confidence Set (MCS)** — which models are statistically *indistinguishable* from the
  best (Hansen, Lunde & Nason 2011). A set of size 1 means the best model is **point-identified**;
  larger means selection is **not identified**.
- **Decision breakdown point (`k*`)** — the *fewest evaluation periods whose deletion flips the
  pooled winner*. A small `k*` means the crowned model rests on a handful of periods and should
  not be trusted as a stable choice.

This is the native R port of the Python package
[`selection-fragility`](https://github.com/sudhanshurai360/selection-fragility) (PyPI:
`pip install selection-fragility`) — same method, same validated numerics, independently verified
against it. See that repo for the accompanying paper and the full evaluation card.

> **Status: pre-CRAN release.** The core port (9 modules), its `testthat` suite (157 tests), and a
> getting-started vignette are complete and independently audited module-by-module against the
> Python package's own test suite and live output, and pass `R CMD check --as-cran` cleanly
> (0 errors, 0 warnings, 0 notes). CRAN submission is in progress; until then, install from
> GitHub (below).

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

See `vignette("selectionfragility")` for the full walkthrough — installation, interpreting the
report, the staged API, weights, CI-gating with `compare()`, and the honest-reporting caveats
(what `k*`/`winner_stability`/the MCS will and will not tell you).

## Relationship to the Python package

Function names and signatures are near-identical 1:1 with the Python package
(`loss_panel()` ≈ `LossPanel.from_losses()`, `report()`, `decision_breakdown()`, `mcs_size()`,
`identified()`, `compare()`, ...), and the one-screen `report()` output is structured identically
in both languages. Two things do **not** carry over:

- **No `from_forecasts()` equivalent yet** — this port only takes an already-computed loss panel.
- **Bootstrap-based numbers are not bit-for-bit identical across languages** — R's RNG and
  NumPy's RNG are different generators, so `winner_stability()`, the MCS bootstrap p-value, and
  `compare()`'s churn rate will not match exactly on "the same" seed. Compare conclusions, not
  decimals.

Cite either implementation; they compute the same quantities.

## License

MIT — see [LICENSE.md](LICENSE.md).
