# Surrogate Fidelity and Cross Validation

Audit of the implementation at framework commit `9ad4526`, compared with
Penncil/pda commit `ee90de152f6886bcba1404cce9cde11cb31f3777`, the reference
pinned in CI. This document distinguishes mathematical component parity from
complete workflow equivalence. It does not change fitting behavior or certify
historical results from other package revisions.

## Method Names and Solvers

| Registered name | Actual fitter | Relationship to the pinned reference |
| --- | --- | --- |
| `ADAP`, `ADAP2` | `.fitPdaAdapSurrogate` | Full second-order ADAP objective and repeatedly rebuilt quadratic subproblems. Current CD, tuning and acceptance policies differ from upstream. |
| `ADAP1` | `.fitPdaAdapFirstOrderSurrogate` | First-order objective, using the same quadratic-solver infrastructure. The pinned `ADAP.estimate` implements the full second-order case, not this variant. |
| `ADAPDiag` | `.fitPdaAdapRemoteDiagSurrogate` | Our diagonal-correction extension, with full lead-site Hessian in each quadratic subproblem. Not an upstream diagonal implementation. |
| `ADAP_PDA` | `.fitPdaAdapPdaProx` | Legacy simultaneous diagonal-metric proximal update. Despite its name, this is **not** the pinned upstream coordinate-descent solver. |
| `ADAPDiag` with `adapDiagStyle = "pda"` | `.fitPdaAdapPdaProx(useFull = FALSE)` | Legacy diagonal update, not the default diagonal-correction fitter. |
| `ODAL`, `ODAL2` | `.clientUpdateODAL`, `optim` | Unpenalized second-order surrogate with numerical-gradient BFGS by default, as in the upstream estimator when configured for BFGS. Initialization and acceptance differ. |
| `ODAL1` | `.clientUpdateODAL`, `optim` | First-order version, omitting the quadratic correction. |

`ADAPDiag` defaults to `adapDiagStyle = "localFullRemoteDiag"`.
The default `ADAP` and `ADAP2` registrations select the same exact-surrogate
implementation. Here "exact" refers to the specified surrogate, not the pooled
logistic objective. `C-ADAP`, `Prox-ADAP` and `MaxConv-ADAP` deliberately modify
the surrogate and are not claims of upstream reproduction.

The legacy proximal fitter computes a full gradient once per iteration, divides
each coordinate by a diagonal curvature and soft-thresholds the slopes. It does
not refresh the gradient after each coordinate. A full correction matrix can
enter that gradient without making the update cyclic coordinate descent.

## Initialization and Scaling

The relevant implementation is in [`R/FL-ADAP.R`](../R/FL-ADAP.R) and
[`R/ODAL.R`](../R/ODAL.R). The upstream definitions are
[`ADAP.R`](https://github.com/Penncil/pda/blob/ee90de152f6886bcba1404cce9cde11cb31f3777/R/ADAP.R),
[`ODAL.R`](https://github.com/Penncil/pda/blob/ee90de152f6886bcba1404cce9cde11cb31f3777/R/ODAL.R)
and `pdaSync` in
[`pda.R`](https://github.com/Penncil/pda/blob/ee90de152f6886bcba1404cce9cde11cb31f3777/R/pda.R).

- **ADAP local fits:** both implementations use binomial `cv.glmnet` lasso and
  `lambda.min`, with glmnet's default ten folds. Upstream leaves standardization
  at glmnet's default `TRUE`. Ours defaults to `config$standardize = FALSE`.
  Matched-standardization parity is tested, not default-setting equivalence.
  Standardization during penalized fitting can change the selected model even
  though returned coefficients are on the input scale.
- **ADAP expansion:** both form a sample-size-weighted average of local
  coefficients. All sites evaluate derivatives at this common point. Our lead
  is the largest training site unless `leadIndex` is supplied. Upstream takes
  an explicitly designated `lead_site`. Local initialization uses the ambient
  RNG. `cvSeed` controls our later surrogate CV, not that earlier glmnet fit.
- **ODAL expansion:** ours also averages local coefficients by sample size.
  Upstream `pdaSync` supports coordinate-wise inverse-variance weighting
  (`init_method = "meta"`), coordinate-wise median, or the lead estimate.
  These are not generally the same expansion point. Our `odalInit = "pda"`
  means local unpenalized `glm` without fallback, not upstream meta weighting.
- **ODAL fallback:** optional `odalInit = "ridgeFallback"` or
  `odalRidgeFallback = TRUE` uses ridge glmnet after a local glm error or
  nonfinite coefficient. A convergence warning with finite coefficients alone
  does not trigger it. Default ridge lambda is `1e-8`, with standardization
  defaulting to `TRUE`. This changes initialization, not the unpenalized final
  surrogate. Upstream has no corresponding fallback here.
- **Loss scale:** our site derivatives use mean negative log likelihood.
  Sample-size weighting gives pooled mean derivatives. Upstream ODAL uses
  log-likelihood derivatives and then negates its objective. Tests account for
  that sign. ADAP lambda multiplies the unpenalized-intercept L1 penalty on the
  mean-loss scale, not the Cyclops prior-variance scale.
- **Preprocessing:** feature alignment, filtering and age scaling occur outside
  these surrogate fitters. PDA receives the supplied matrix and does not
  reproduce the OMOP pipeline. Component parity requires identical columns,
  order, values and intercept treatment. The comparison runner does not expose
  a general `standardize` flag for ADAP initialization.

## Lead Site Cross Validation

Let `b` be the fixed averaged initialization, `g` and `H` the sample-size-weighted
global mean derivatives there, and `N` the total outer-training sample size.
For a lead validation subset of size `nV`, the default exact path uses:

```text
gTrain = (N * g - nV * gValidation(b)) / (N - nV)
HTrain = (N * H - nV * HValidation(b)) / (N - nV)
```

The lead likelihood excludes the validation rows. The first-order path applies
only the gradient formula, and the diagonal path uses Hessian diagonals.
This matches the subtraction in the pinned upstream ADAP CV code.

The option name is misleading: **`lambdaCvGlobalAdjustment = "leaveValOut"`
matches this upstream subtraction. `"pda"` keeps the full global summaries
unchanged.** It is not the upstream CV convention. The legacy `ADAP_PDA` path
always keeps the full summaries and does not consume this option.

Neither the default exact path nor upstream refits local initialization within
each lead fold. The expansion point, full-lead starting estimate and lambda
range use all outer-training lead outcomes. Thus this is validation conditional
on an outcome-dependent fixed expansion, not fully refitted inner CV. Removing
validation derivatives does not remove their influence on that expansion.
This is distinct from exposing the outer held-out database, which is not needed
by any of these fitting steps.

### Search and Warm Starts

| Choice | Pinned upstream ADAP | Current exact ADAP, ADAP1 and default ADAPDiag |
| --- | --- | --- |
| Surrogate folds | Five shuffled row groups, with rounded chunk sizes | Five by default, shuffled balanced row labels, `cvSeed = 42` by default. Neither stratified nor person-grouped. |
| Lambda range | 100 log-spaced values from maximum to minimum, maximum includes intercept score | 100 by default, excludes unpenalized intercept from maximum. First-order and diagonal paths use their corresponding quadratic. Minimum ratio is `0.02` if `n < p`, otherwise `1e-4`. |
| Search | Entire descending grid, subject to solver failures | Direct fitter defaults to grid. `runComparisonMatrix.R` defaults to bounded log-lambda search (`"optimize"`), budget 25, tolerance `log(1.5)`. |
| Initial CV model | First quadratic built at the full-lead estimate, first inner CD starts at zero | Starts at full-lead estimate and builds its quadratic there |
| Warm starts | Previous descending-path solution within the same fold | Nearest successfully fitted stronger/equal lambda in the same fold, otherwise full-lead estimate. No transfer between folds. |
| Final start | Full-lead estimate | Full-lead estimate, not the CV fitted coefficients |
| Score | Sum of fold mean negative log likelihoods, up to common factor | Mean of fold mean negative log likelihoods (`"deviance"`), or optional AUC |
| Failure/selection | Inner failure stops the remaining path in that fold | A candidate requires valid fits in every fold. Score ties within `1e-8` favor stronger lambda. |

`"deviance"` currently means **mean log loss**, not twice summed log loss.
Since 20 October 2026, current CV pools log loss over held-out observations,
so unequal-sized folds count by their size. The upstream criterion gives each
fold equal weight when every fold has a score. AUC selection still averages
fold AUCs, which cannot be pooled.

The automatic search descends from the upper lambda bound, then refines near the
best evaluated value. It does not guarantee a global optimum or reproduce
Cyclops' search. A failed candidate can end the initial downward exploration.
CV uses all lead rows by default (`lambdaCvMaxRows = Inf`). An explicit finite
cap changes the tuning data and must be recorded.

The legacy `ADAP_PDA`/diagonal-`pda` path instead uses a gradient-based lambda
maximum, grid search only, and starts each fold and the final fit at the averaged
expansion. Successful solutions warm-start subsequent lambdas within a fold.
The runner can record `lambdaSearch = "optimize"` for these methods even though
they ignore it. They also ignore the exact path's CV row cap and adjustment
options. A saved configuration is not proof those options took effect.

## Stopping and Acceptance

Upstream CD takes unbounded cyclic coordinate minimizers, with 100 inner sweeps
and a `1e-4` change tolerance on the smooth quadratic. Its outer loop allows
100 iterations and tests smooth-quadratic change at `1e-5`. Those change tests
omit the L1 contribution. It does not pre-reject negative correction eigenvalues.

Current exact paths bound coordinate steps and check descent of the penalized
quadratic. Outer termination uses coefficient change, with a final approximate
KKT check. Public defaults are 500 CV outer iterations, at least 1,000 final
outer iterations, 100 inner sweeps and `tol = 1e-5`. The default KKT tolerance
is `1e-4`, also used to classify near-zero coefficients. Magnitude checks default
to `1e4` for coefficients and linear predictors. These checks are not an exact
optimality certificate. ADAP2 rejects negative correction eigenvalues beyond
its numerical tolerance, and default ADAPDiag rejects negative diagonal
corrections. These acceptance rules differ from upstream.

Quadratic descent does **not** guarantee descent of the actual nonlinear
surrogate. Matched-objective experiments in
[`compareSurrogateSolvers.md`](compareSurrogateSolvers.md) reproduce this problem
with both current and upstream CD. A full-objective safeguard would be a
separate numerical change, not evidence of parity.

The legacy proximal path floors diagonal update curvature at `1e-4`, stops on
maximum coefficient change (`1e-6` by default), and allows 1,000 iterations.
It lacks the exact path's KKT/negative-correction acceptance checks. Its final
caller returns coefficients even when the iteration limit is reached.

ODAL uses numerical-gradient `optim`, starting at the averaged initialization,
with default BFGS and `maxit = 1000`. Upstream also uses `optim` without an
analytic gradient, with method/budget supplied through its control and a returned
Hessian. Our ODAL records solver status but does not require status zero or KKT
acceptance before returning coefficients. Its default negative-curvature action
is reporting, not rejection. The finite sentinel for nonfinite objectives is
another difference from upstream. Auditing raw objective values remains necessary.

## Tests and Limits of Parity

[`test-pda-parity.R`](../tests/testthat/test-pda-parity.R) checks the pinned
upstream functions, matched-standardization initialization, derivative signs
and weights, quadratic components, the actual upstream CV subtraction expressions
and bounded CD solutions. It does not assert equality of complete selected
ADAP estimates or ODAL initialization workflows.

[`test-surrogate-fidelity.R`](../tests/testthat/test-surrogate-fidelity.R)
characterizes actual dispatch, simultaneous versus cyclic updates, frozen
initialization across full/first/diagonal CV, current fold weighting and ODAL
initialization weights. Existing tests cover warm-start isolation, failed
candidates and search behavior. Characterization tests document current choices,
not an endorsement that every choice should become the final protocol.

## Decisions Before a Final Run

1. **Keep estimator names explicit.** Use `ADAP`/`ADAP2` for the full-quadratic
   path. Do not describe legacy `ADAP_PDA` as upstream-faithful. Renaming/removing
   legacy options or rejecting ignored flags needs a separate compatibility
   decision.
2. **Choose the CV boundary explicitly.** Retaining fixed-expansion CV is the
   closest starting point to pinned upstream PDA. Fully refitted initialization
   should be a named sensitivity, not a silent correction to that estimator.
3. **If fully refitting:** refit the lead initializer without each validation
   fold, recompute its sample-size-weighted averaged expansion, and obtain all
   training sites' derivatives at that new point. Remote local initializers can
   be reused. Both starting values and any data-dependent lambda bounds must be
   constructed without the validation outcomes to claim fold-clean tuning.
   This needs one additional expansion broadcast and derivative collection per
   inner split, not per lambda. For `K` folds and `M` sites, unbatched control
   entails roughly `K*M` additional derivative replies, carrying `O(p)` numbers
   per site for first/diagonal methods or `O(p^2)` for full Hessians. Batching can
   reduce exchanges, not the number of derivative evaluations or their payload.
   Count initialization updates and protocol broadcasts too. This is a design
   estimate, not a measured communication count.
4. **Implement agreed lead-score weighting separately:** use observation-weighted
   log loss. Also record initialization standardization and resolve the choice
   of ODAL sample-size versus upstream initialization before claiming workflow
   fidelity. These latter choices are not automatic default changes. Check
   proposed changes with matched synthetic inputs before a production sensitivity.
5. **Keep solver repairs separate:** full-objective step acceptance and raw
   nonfinite-objective handling require the matched-objective regression controls.
   Never infer a published algorithm's failure rate from our acceptance policy.

This audit leaves all estimators and defaults unchanged. The fixed-expansion
versus fully refitted choice still requires study approval.
