# FederatedLearning

FederatedLearning is an R package for running simulated federated patient-level prediction experiments on OMOP Common Data Model data prepared with OHDSI PatientLevelPrediction (PLP). It provides helpers for loading PLP data on worker processes, aligning covariates across site-like data splits, fitting several federated logistic-regression methods, and evaluating the resulting model.

The package is currently a research prototype. The primary execution model is a single coordinating R session controlling multiple local worker processes. See [PRIVACY.md](PRIVACY.md) for the current execution and privacy positioning.

## Installation

Install from GitHub, pinned to a commit so results can be traced to the code:

```r
install.packages("remotes")
remotes::install_github("mi-erasmusmc/FederatedLearning@<commit>")
# or, from a local checkout:
remotes::install_local(".")
```

The package depends on OHDSI/PLP tooling and compiles C++ code, so the local R environment needs a working compiler toolchain (Rtools on Windows). The pooled and local lasso baselines also need Cyclops.

## Input Layout

Most workflows expect one PLP data folder per simulated client/site, typically created with `PatientLevelPrediction::savePlpData()`:

```text
data/
  taskA/
    site1/
      plpData files...
    site2/
      plpData files...
    site3/
      plpData files...
```

Each client folder should contain compatible PLP covariate data for the same prediction task. The package aligns features by `covariateId` using either a union or intersection map (`runComparison()` always intersects across the training sites). This is necessary but not a full guarantee that each split used identical covariate-generation settings.

## Comparing Methods

`runComparison()` is the main entry point. For each site in `holdouts` it trains every method on the other sites, evaluates on the held-out site, and writes results and models to the output directory.

```r
library(FederatedLearning)

pop <- PatientLevelPrediction::createStudyPopulationSettings(
  riskWindowStart = 1, riskWindowEnd = 365,
  removeSubjectsWithPriorOutcome = TRUE, priorOutcomeLookback = 99999,
  requireTimeAtRisk = FALSE
)
task <- createTaskSettings("taskA", targetId = 1234, outcomeId = 5678,
  populationSettings = pop)
sites <- c(site1 = "data/taskA/site1", site2 = "data/taskA/site2",
  site3 = "data/taskA/site3")
features <- list(allCovariates = createFeatureSettings(minFraction = 0.001))

methods <- list(
  createMethodSettings("ADAP2"),
  createMethodSettings("LocalAvgLasso"),
  createMethodSettings("PooledLasso")
)

runComparison(task, sites, features, methods, "results/taskA", holdouts = names(sites))
results <- readComparisonResults("results/taskA")
```

The four settings functions:

- `createTaskSettings()`: name, target and outcome cohort ids, and PLP population settings.
- `createFeatureSettings()`: which covariates enter the model (`covariateIds`, `analysisIds`, `excludeCovariateIds`), logical-OR `combinations` of binary covariates, and `minFraction`, a shared training-only filter that drops covariates present in fewer than that fraction of outer-training entries (default 0.001; `NULL` disables it). Pass a named list to compare several.
- `createMethodSettings(method, ...)`: a method and its options. Options are the comparison runner's flags in camelCase; an unknown name stops with the list of valid ones. A vector value defines a grid of configurations.
- `createRunSettings()`: how the run executes, not what it estimates (`mirai`, `hosts`, `verbose`, `saveModels`, `rerunErrors`).

The output directory holds:

- `comparison_results.csv`: one row per method, held-out site and feature setting, with AUC, log loss, calibration (in the large, intercept, slope, ICI), nonzero predictors, selected penalty, communication, run time, peak memory and fit diagnostics; a filled `error` column marks a failed fit;
- `models/`: saved models, read with `loadModel()` and scored with `predictModel()`;
- `preprocessing/`: the shared feature-filter audit, with per-predictor training counts (keep private);
- `run_manifest.csv` and `manifest/`: one row per call with the framework and study commits, R, the BLAS, a settings hash and the full settings.

Calling `runComparison()` again on the same directory resumes and skips finished combinations, and warns when the code or settings differ from the earlier run. Use a new directory when a setting changes.

### ADAP Options

ADAP1 and ADAP2 take these options in `createMethodSettings()`. The defaults are the settings we recommend.

| Option | Default | Meaning |
| --- | --- | --- |
| `adapInitBackend` | `"glmnet_sparse"` | Local starting lasso: glmnet on the sparse site matrix; `"glmnet"` uses a dense matrix when it fits (same fit, slower), `"localCyclops"` the coefficient-averaging baseline's local models. |
| `adapInitFolds` | `3` | CV folds for the local starting lasso. |
| `adapInitStratify` | `TRUE` | Stratify those folds by outcome. |
| `adapEigenDiagnostics` | `"final"` | Eigenvalue diagnostics (each O(p^3)) only for the final fit and the server's correction; `"all"` at every fit, `"none"` never. The fitted model is the same. Variants whose fit uses the eigenvalues default to `"all"`. |
| `leadRank` | `1` | Lead site by training size: 1 is the largest. |
| `innerFolds` | `3` | Outcome-stratified CV folds for the penalty search at the lead site. |
| `adapLambdaSelectionMetric` | `"deviance"` | Penalty selection score: `"deviance"` or `"auc"`. |
| `lambdaSearchMaxEvals` | `25` | Most penalties evaluated by the search. |
| `cvSeed` | `42` | Seed for the CV folds. |

ADAP2 stores a dense p-by-p Hessian per site, so memory grows with the square of the number of predictors: roughly 10 GB in total at 5,000 predictors and 30 GB at 10,000 on four sites.

## Low-Level API

`fitFederated()` fits one method on an existing worker cluster, without the outer folds, baselines or result files. This example assumes `clientPaths` already point to PLP data folders. With `mirai = FALSE` and `clientHosts = "localhost"`, the clients are simulated by local PSOCK worker processes.

```r
library(FederatedLearning)
library(PatientLevelPrediction)

clientPaths <- c(
  "data/taskA/site1",
  "data/taskA/site2",
  "data/taskA/site3"
)
clientHosts <- rep("localhost", length(clientPaths))

popSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  riskWindowStart = 1,
  riskWindowEnd = 365,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = FALSE
)

cl <- clusterInit(clientHosts, clientPaths, mirai = FALSE)
on.exit(parallel::stopCluster(cl), add = TRUE)

clusterLoadData(cl, clientPaths, popSettings)

fit <- fitFederated(
  cl = cl,
  algorithm = "DualAvg",
  config = list(
    intercept = TRUE,
    rounds = 100,
    k = 5,
    etaClient = 0.01,
    etaServer = 1,
    lambda = 0.001,
    mapType = "union",
    featureSet = "ageSex",
    epsilon = 1e-6
  )
)

metrics <- clusterEvaluateModel(cl, fit$w)
metrics
```

## Preparing External PLP Data

The `extras/fetchTaskData.R` helper can fetch ATLAS cohort JSON, generate cohorts, extract PLP data, and save the expected folder layout. It follows the standard OHDSI split between shareable study settings, local execution settings, and private data-source connection settings:

```bash
cp extras/fetch_study_template.yml extras/fetch_study.yml
cp extras/fetch_execution_template.yml extras/fetch_execution.yml
cp extras/fetch_data_sources_template.yml private/fetch_data_sources.yml
```

Commit only generic study settings when appropriate. Keep real data-source schemas, JDBC details, and environment-variable names in private ignored files. Keep passwords and tokens in environment variables, for example:

```bash
export ATLAS_BASE_URL="https://atlas.example.org/WebAPI"
export DB_CLUSTER_PASSWORD="..."
export JDBC_DRIVER_PATH="/path/to/jdbc/drivers"
```

Run extraction:

```bash
Rscript extras/fetchTaskData.R \
  --study=extras/fetch_study.yml \
  --execution=extras/fetch_execution.yml \
  --data-sources=private/fetch_data_sources.yml \
  --overwrite=false
```

The script builds standard OHDSI objects internally: `DatabaseConnector::createConnectionDetails()`, `PatientLevelPrediction::createDatabaseDetails()`, `PatientLevelPrediction::createStudyPopulationSettings()`, `FeatureExtraction` covariate settings, and `CohortGenerator` cohort tables. DatabaseConnector fields can be supplied directly, through `*Env` keys, or through a `connectionStringTemplate` in the private data-source config.

## Comparison Runner

`extras/runComparisonMatrix.R` is the command-line form of the same runner that `runComparison()` calls. It runs methods across tasks, feature sets, and leave-one-client-out folds:

```bash
Rscript extras/runComparisonMatrix.R \
  --data-root=data \
  --tasks=taskA,taskB \
  --feature-sets=ageSex,ageSexPhenotypes \
  --methods=DualAvg,ODAL,ADAP,ADAP1,ADAPDiag \
  --client-ids=site1,site2,site3 \
  --folds=1:3 \
  --result-directory=results/comparisonMatrix
```

Outputs include per-fold metrics, summary metrics by method, diagnostics, and lambda paths where available.

### Matched Feature Filtering

`--federated-preprocess=baseline` opts federated methods into the baseline rarity
and near-constant-column filters. The script's default remains `none`;
`runComparison()` applies the shared filter to every method unless
`createFeatureSettings(minFraction = NULL)`. Filtering uses
summaries from the outer-training sites only, with one common feature map for
inner tuning, final fitting and held-out scoring. This matches the baseline's
existing outer-training preprocessing scope, not preprocessing refitted inside
each inner split. Solvers and penalty selection are unchanged.

The default minimum frequency is `floor(0.001 * trainingRows)`. The option named
`baseline-preprocess-remove-redundancy` checks near-zero variance, not correlation
or duplicate columns. Matched filtering requires baseline preprocessing enabled
and `--baseline-preprocess-normalize=false`. Age keeps its existing age/100 scale.

To check an existing comparison, use the installed package:

```r
audit <- FederatedLearning::auditMatchedPreprocessing("results/originalComparison")
```

This restores the settings, task-specific horizons and training paths from saved
`PooledLasso` models. It does not need the original command or runner script,
load held-out data, fit models, or modify the original results. Review
`preprocessing-audit/summary.csv` inside the old result directory. The companion
`features.csv` lists the filtering decisions. Missing models and other fold-level
errors are recorded in the summary, so completed audits are not lost.

Optional `tasks` and `folds` arguments restrict the audit. If the data have moved,
pass `dataRoot` pointing to the directory containing `task/clientId` folders.
Saved models are required. A `matched` result confirms the feature mask and
training row counts, not that every underlying data value is unchanged.
`requiresRefit=false` means filtering changed no columns, not a general guarantee
that other settings are unchanged.

After reviewing the audit, run a small comparison pilot in a **new** result
directory with `--federated-preprocess=baseline`,
`--baseline-preprocess-normalize=false`, and
`--preprocessing-reference-directory=results/originalComparison`.
Keep the original horizons, seeds, penalties and solver settings. The reference
check stops fitting on a mismatch. Test ODAL2 first, then a penalized surrogate
and DualAvg with tuning. Gate the wider rerun on correct masks, coefficient
mapping and saved-model rescoring, not on improved AUC.

The runner also supports `--preprocessing-audit-only=true` for auditing a new
configuration without fitting. Without a reference directory this reports
`not_requested`, not confirmation of parity with an earlier run.

Per-fold CSV/RDS audits retain nonzero counts, moments, both removal flags, the
original and retained maps, and a retained-ID fingerprint. Federated model
artifacts store this under `featureFiltering`, distinct from the baseline's
matrix-transform `preprocessing` object. Saved coefficients use the retained map
and are not filtered a second time. Different preprocessing settings cannot be
resumed in the same result directory.

`preprocessMessages` and `preprocessNumbers` record the new client summary replies
and their numeric payloads, separate from existing fit communication fields.
They do not include feature-map transport, worker setup, or protocol overhead.
`preprocessElapsedSeconds` includes matrix creation and summarization. This setup
is shared across methods in a task-fold but reported for each fitted method, so
do not sum it across methods as measured total wall time.

## Algorithms

Registered algorithm names are passed to `fitFederated(algorithm = ...)`.

| Algorithm | Description | Typical communication |
| --- | --- | --- |
| `DualAvg` | C++ implementation of distributed dual averaging for L1-regularized logistic regression. | Server broadcasts dual state; clients return dual-state deltas. |
| `DualAvgCpp` | Explicit alias for the C++ dual-averaging implementation. | Same high-level payload as `DualAvg`. |
| `DualAvgR` | R reference implementation used for parity testing. | Same high-level payload as `DualAvg`. |
| `ODAL`, `ODAL2` | Unpenalized second-order surrogate, fitted with `optim` (BFGS by default). | Local coefficients, gradients, full Hessians, final lead-site coefficients. |
| `ODAL1` | Unpenalized first-order surrogate. | Local coefficients, gradients, final lead-site coefficients. |
| `ADAP`, `ADAP2` | Full-Hessian ADAP surrogate with quadratic coordinate descent and additional numerical acceptance checks. | Local lasso estimates, gradients, full Hessians, lead-site surrogate fit. |
| `ADAP_PDA` | Legacy full-Hessian correction with simultaneous diagonal proximal updates, not the pinned upstream PDA coordinate-descent solver. | Local estimates, gradients, full Hessians, lead-site surrogate fit. |
| `ADAP1` | First-order ADAP-style variant without Hessian transmission. | Local estimates, gradients, lead-site surrogate fit. |
| `ADAPDiag` | Diagonal-correction extension, retaining full lead-site curvature by default. | Local estimates, gradients, Hessian diagonals, lead-site surrogate fit. |
| `Prox-ADAP`, `C-ADAP`, `MaxConv-ADAP` | Experimental ADAP surrogates (proximal, convexified and maximum-convexity variants) for method development. | As `ADAP2`. |

`runComparison()` also accepts these centralised and local lasso baselines, fitted with Cyclops:

| Baseline | Description |
| --- | --- |
| `PooledLasso` | Lasso on the pooled training rows; penalty chosen by holding out each training database (`pooledTuning = "rows"` uses CV over pooled rows). |
| `LocalAvgLasso` | Average of the local lassos' coefficients, weighted by training size (`localAverageWeighting = "equalClient"` for equal weights). |
| `SparseLocalAvgLasso`, `DebiasedLocalAvgLasso` | Coefficient averaging with a tuned sparsity threshold, without or with local debiasing. |
| `LocalEnsembleLasso` | Average of the local models' predicted probabilities. |
| `BiggestSiteLasso` | The largest training site's local lasso. |
| `LocalBestLasso` | The local lasso that scores best at the other training sites. |
| `LocalSiteLasso` | Each training site's local lasso, evaluated separately. |

Several PDA-style methods finish in a small number of phases rather than many communication rounds. Dual averaging usually uses many more rounds but smaller per-round payloads.

These implementations do not reproduce every upstream initialization, tuning or
acceptance rule. See the [surrogate fidelity audit](extras/surrogateFidelity.md)
for the pinned reference, actual method dispatch and cross-validation boundaries.

## Feature Sets

The built-in feature filters are:

| `featureSet` | Meaning |
| --- | --- |
| `all` | All covariates in the global map. |
| `ageSex` | Hard-coded age/sex covariate IDs currently used by local experiments. |
| `phenotypes` | Covariates with phenotype analysis ID currently set to `49`. |
| `ageSexPhenotypes` | Union of the age/sex and phenotype filters. |

These names apply to `fitFederated()` and the runner script; `runComparison()` selects covariates with `createFeatureSettings()` instead. You can also pass explicit `covariateIds` or `analysisIds` in `config`. The current feature filters are intentionally simple and should be validated against the covariate settings used to generate PLP data.

## Execution Model

The usual setup is simulated federation on one machine: one coordinating R process starts one worker process per client folder.

`clusterInit(clientHosts, clientPaths, mirai = FALSE)` uses `parallelly::makeClusterPSOCK()` and passes `clientHosts` as PSOCK workers. The examples use `localhost`. PSOCK can be configured for remote workers, but this package does not currently provide a production remote-federation setup.

`mirai = TRUE` currently creates a `mirai` cluster with `n = length(clientHosts)`. Treat this as process-based execution unless you have separately configured and validated remote `mirai` daemons.

Worker processes store loaded PLP data and matrices in worker global state. `clusterLoadData()` clears prior package worker state before loading new PLP data, and `clusterCreateMatrices()` replaces any previous model matrix before creating a new one. Use `clusterClearState(cl)` when reusing a cluster across unrelated experiments or after a failed run.

## What Is Shared

Depending on the method and helper functions used, the server may receive:

- covariate reference metadata used to build the global feature map;
- sample counts and outcome counts from diagnostics;
- local coefficients;
- gradients and Hessians or Hessian diagonals;
- dual-averaging state deltas;
- lead-site fitted coefficients and tuning metrics.

These payloads are exchanged inside the simulated federation between the coordinating process and worker processes. See [PRIVACY.md](PRIVACY.md).

## Current Limitations

- The package is documented as simulated federated learning, not as a production multi-site deployment.
- `clientFrac` is present in some APIs but the core training loop currently evaluates all clients each round.
- Aggregation is weighted by sample size by default, with equal-client weighting as an option; method-specific behavior should still be checked before interpreting results as pooled empirical-risk minimization.
- Feature compatibility across sites is not fully enforced beyond covariate ID mapping.
- Worker state is global within each worker process and should be treated as session state.

These limitations are tracked as GitHub issues and should be addressed before presenting the package as a reusable external federated-learning framework.

## Validation

Run local tests:

```bash
Rscript -e 'testthat::test_dir("tests/testthat", reporter = "summary")'
```

Run an R package check:

```bash
R CMD build .
R CMD check --no-manual --no-build-vignettes FederatedLearning_*.tar.gz
```
