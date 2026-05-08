# FederatedLearning

FederatedLearning is an R package for running simulated federated patient-level prediction experiments on OMOP Common Data Model data prepared with OHDSI PatientLevelPrediction (PLP). It provides helpers for loading PLP data on worker processes, aligning covariates across site-like data splits, fitting several federated logistic-regression methods, and evaluating the resulting model.

The package is currently a research prototype. The primary execution model is a single coordinating R session controlling multiple local worker processes. See [PRIVACY.md](PRIVACY.md) for the current execution and privacy positioning.

## Installation

Install the package from a local checkout:

```r
install.packages("remotes")
remotes::install_local(".")
```

The package depends on OHDSI/PLP tooling and compiles C++ code, so the local R environment needs a working compiler toolchain and the required OHDSI packages installed.

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

Each client folder should contain compatible PLP covariate data for the same prediction task. The package aligns features by `covariateId` using either a union or intersection map. This is necessary but not a full guarantee that each split used identical covariate-generation settings.

## Minimal PLP Example

This example assumes `clientPaths` already point to PLP data folders. With `mirai = FALSE` and `clientHosts = "localhost"`, the clients are simulated by local PSOCK worker processes.

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

`extras/runComparisonMatrix.R` runs methods across tasks, feature sets, and leave-one-client-out folds:

```bash
Rscript extras/runComparisonMatrix.R \
  --data-root=data \
  --tasks=taskA,taskB \
  --feature-sets=ageSex,ageSexPhenotypes \
  --methods=DualAvg,ODAL,ADAP_PDA,ADAP1,ADAPDiag \
  --client-ids=site1,site2,site3 \
  --folds=1:3 \
  --result-directory=results/comparisonMatrix
```

Outputs include per-fold metrics, summary metrics by method, diagnostics, and lambda paths where available.

## Algorithms

Registered algorithm names are passed to `fitFederated(algorithm = ...)`.

| Algorithm | Description | Typical communication |
| --- | --- | --- |
| `DualAvg` | R implementation of distributed dual averaging for L1-regularized logistic regression. | Server broadcasts dual state; clients return dual-state deltas. |
| `DualAvgCpp` | C++ implementation of the same dual-averaging idea. | Same high-level payload as `DualAvg`. |
| `FastDualAvg` | Accelerated/variant dual-averaging implementation. | Server broadcasts aggregate states; clients return gradient-like state summaries. |
| `ODAL` | One-shot distributed approximation using local fits, gradients, Hessians, and a lead-site surrogate solve. | Local coefficients, gradients, full Hessians, final lead-site coefficients. |
| `ADAP` | Earlier ADAP-style surrogate implementation. | Local lasso estimates and derivative summaries. |
| `ADAP_PDA` | ADAP implementation aligned closely with the `pda` package full-Hessian method. | Local estimates, gradients, full Hessians, lead-site surrogate fit. |
| `ADAP1` | First-order ADAP-style variant without Hessian transmission. | Local estimates, gradients, lead-site surrogate fit. |
| `ADAPDiag` | Reduced ADAP variant using diagonal Hessian information when configured. | Local estimates, gradients, optional Hessian diagonals, lead-site surrogate fit. |
| `ADAP2` | Experimental ADAP surrogate workflow with optional full or diagonal Hessian mode and lambda tuning helpers. | Local lasso estimates, gradients, Hessian summaries, lead-site CV/fit payloads. |

Several PDA-style methods finish in a small number of phases rather than many communication rounds. Dual averaging usually uses many more rounds but smaller per-round payloads.

## Feature Sets

The built-in feature filters are:

| `featureSet` | Meaning |
| --- | --- |
| `all` | All covariates in the global map. |
| `ageSex` | Hard-coded age/sex covariate IDs currently used by local experiments. |
| `phenotypes` | Covariates with phenotype analysis ID currently set to `49`. |
| `ageSexPhenotypes` | Union of the age/sex and phenotype filters. |

You can also pass explicit `covariateIds` or `analysisIds` in `config`. The current feature filters are intentionally simple and should be validated against the covariate settings used to generate PLP data.

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
- Some aggregation behavior is equal-client rather than sample-size weighted; method-specific behavior should be checked before interpreting results as pooled empirical-risk minimization.
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
