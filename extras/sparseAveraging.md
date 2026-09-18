# Sparse coefficient averaging

The comparison runner accepts two additional, experimental baselines:

- `SparseLocalAvgLasso`: sample-size-weighted local Cyclops lasso coefficients,
  followed by soft thresholding of slopes. The intercept is never thresholded.
- `DebiasedLocalAvgLasso`: subtract a local approximate inverse-Hessian times
  mean negative-log-likelihood gradient before averaging and thresholding.

These are sparse aggregation estimators, **not pooled logistic-lasso solvers**.
`LocalAvgLasso` is unchanged. Failed local fits are omitted and their weights
renormalized, as in that baseline; model artifacts record the contributing sites.
Debiasing failures stop the new method rather than silently falling back.

## Running

Add `SparseLocalAvgLasso,DebiasedLocalAvgLasso` to `--methods` in the usual
`extras/runComparisonMatrix.R` invocation. Existing resume handling applies.
After installing this branch and restarting R, an existing production argument
list can be reused without changing its tasks, population settings or folds:

```r
source("extras/runComparisonMatrix.R")
newArgs <- args
newArgs[["methods"]] <- "SparseLocalAvgLasso,DebiasedLocalAvgLasso"
newArgs[["resume"]] <- "true"
newArgs[["save-models"]] <- "true"
runComparison(newArgs)
```

This appends the new methods to the existing results directory; it does not
reconstruct artifacts for previously completed methods. Keeping model saving
enabled preserves thresholds, exact coefficients and debiasing diagnostics.

No threshold needs to be specified:

- Local models use the existing Cyclops auto-CV settings.
- An additional leave-one-training-site-out validation selects the aggregation
  threshold. Each split refits preprocessing and local models using only its
  training sites. The outer held-out database is never used for selection.
- By default, maximize equal-site mean AUC. Exact ties select the smallest
  threshold; any non-finite validation score fails explicitly.
- The default threshold grid is `0,0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5,1`.
  Override it with `--local-average-thresholds=...` (zero is always included),
  or use `--local-average-metric=logLoss` to minimize mean validation log loss.
- `--local-average-threshold=0.05` fixes the threshold and skips threshold CV.
  A fixed zero threshold reproduces ordinary averaging for `SparseLocalAvgLasso`.
  It does not undo debiasing in `DebiasedLocalAvgLasso`.

Thresholds are in shared coefficient units, before optional baseline
normalization (age is already divided by 100 by matrix construction). They are
not Cyclops prior variances or pooled-likelihood lambda values. Results record
`aggregationThreshold`; artifacts contain the full threshold CV trace,
unthresholded aggregate, original local models, corrections, gradients, and
curvature diagnostics. `summarizeCoefficients.R` includes both methods.

## Debiasing specification and limits

Based on the local debias/average/threshold construction and generalized-loss
weighted nodewise regressions in [Lee et al., 2017](https://jmlr.org/papers/v18/16-002.html).
This is a local-precision, unequal-sample-weighted adaptation, not a replication
of their shared-precision distributed algorithm or a claim of their statistical
guarantees under heterogeneous databases.

For each site, compute `H = X' W X / n`, with stable logistic curvature
`W = plogis(eta) * plogis(-eta)`. Standardize its positive diagonal to one,
then fit nodewise Gaussian lasso using glmnet. A small synthetic design with the
same Gram matrix avoids repeated patient-row passes. Each inverse row is formed
using residual second moment plus nodewise lambda times the L1 coefficient norm,
then transformed back to the original coefficient units. The debiasing sign is
`beta - Theta %*% gradient` for negative log likelihood.

Nodewise lambda is `multiplier * sqrt(log(max(p, 2)) / n)`, default multiplier 1;
override with `--local-debias-multiplier`. This choice is heuristic, not a
confidence-bound calibration. Zero-curvature columns receive no correction and
are listed in diagnostics. The intercept participates in inverse curvature but
is unpenalized in local model fitting and aggregate thresholding.

This implementation allocates a dense local Hessian and is intended for modest
feature sets. It rejects more than 512 matrix columns before allocation;
`--local-debias-max-features` explicitly overrides this guard. It is not yet a
scalable full-OMOP-feature implementation.

## Communication

Relative to ordinary coefficient averaging, with local fitting/tuning otherwise
held fixed:

| Step | Additional communication |
|---|---|
| Soft-thresholding at a fixed threshold | None: the server already has the averaged coefficients. |
| Local debiasing at a fixed nodewise multiplier | No additional round is inherently needed: each site sends a corrected coefficient vector instead of its original vector. The Hessian and nodewise regressions stay local. |
| Selecting the threshold (the default) | Yes: inner training-site fits must be combined, then candidates evaluated at each inner validation site and scores returned. |

With four outer-training sites, threshold CV performs four inner site splits
and twelve additional local fits per new method. This is a computation count,
not a message count. All thresholds can be evaluated from one aggregate vector
and a threshold grid per validation site, so separate exchanges for every
threshold are not inherently necessary. Inner preprocessing may also require
additional training-site summaries.

The current baseline runner simulates these operations locally and does not
instrument protocol messages or payload sizes. Accordingly, `messages` and
`numbers` are NA, not zero. Do not describe the default tuned workflow as having
the same end-to-end communication as fixed-threshold one-shot averaging.

## Local real-data benchmark

From the package root, after installing this branch:

```r
source("extras/benchmarkSparseAveraging.R")
benchmarkSparseAveraging(list(
  "cache-root" = "../main/results/localDualAvgDefaults20260917",
  "output" = "results/sparseAveraging",
  "workers" = "1"
))
```

This uses cached real age/sex/phenotype matrices: four simulated training sites
and one untouched validation site per task. It compares both new baselines to
ordinary averaging and pooled Cyclops. All use Cyclops auto-CV; new thresholds
are tuned only on training sites. Local fits are reused across averages to avoid
duplicated work. Reported method time includes their fitting and threshold CV.
Outputs include held-out AUC/log loss, exact nonzeros, and training objective/KKT
at the pooled model's selected lambda as a **reference diagnostic**, not the
objective optimized by an averaging estimator. One fold of simulated sites
does not establish performance across heterogeneous real databases.
