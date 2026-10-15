# Matched Surrogate Solver Experiments

This is an opt-in diagnostic harness, not a new fitting backend. It changes no
production algorithm, curvature rejection, tuning, or stopping defaults.

## Run the Controls

Install the working package and the official PDA reference pinned by CI:

```r
remotes::install_github("Penncil/pda", ref = "ee90de152f6886bcba1404cce9cde11cb31f3777")
source("extras/auditCapturedSurrogates.R")
source("extras/compareSurrogateSolvers.R")
results <- runSurrogateSolverControls()
results$summary

dir.create("results/solver-controls", recursive = TRUE)
saveRDS(results, "results/solver-controls/results.rds")
write.csv(results$summary, "results/solver-controls/summary.csv", row.names = FALSE)
write.csv(results$steps, "results/solver-controls/quadratic-steps.csv", row.names = FALSE)
```

The harness refuses an unpinned installed PDA. It extracts the original nested
`soft` and `coordi` definitions from `ADAP.estimate`, without editing their bodies.
The reference is [Penncil/pda at ee90de1](https://github.com/Penncil/pda/blob/ee90de152f6886bcba1404cce9cde11cb31f3777/R/ADAP.R).

## What Is Matched

Each comparison fixes the lead rows, labels, coefficient scale, expansion point,
actual optimizer start, linear and quadratic corrections, and penalty. The
intercept is unpenalized. The stable, independently implemented objective and
derivatives from `auditCapturedSurrogates.R` are used for every backend. No
upstream logistic-helper overflow is introduced into the CD comparison.

- BFGS compares finite-difference and analytic gradients only at `lambda = 0`.
  Both use the same `optim` controls and the existing ODAL nonfinite-value
  sentinel. Raw values before sentinel replacement are retained.
- Current compiled CD and pinned upstream CD receive the same quadratic models
  in a common full-step outer loop. There is no nonlinear line search or
  pre-fit rejection of the fixed correction. These bypasses are diagnostic only.
- Both CD loops stop at the same exact-zero KKT threshold or experiment budget.
  The inner algorithms retain their own rules. Current CD uses bounded
  coordinate updates, up to 1,000 sweeps and tolerance 1e-10 by default.
  Upstream uses up to 100 sweeps and its fixed 1e-4 smooth-quadratic-change rule.
  Its inner message is recorded, not treated as nonlinear convergence.
- BFGS retains its native termination rule. Its code and the independent KKT
  audit are separate. It is not forced to use the CD stopping rule.

The controls cover first-order, full and diagonal corrections, penalized and
unpenalized objectives, dense/sparse designs, and an unpenalized intercept.
Constructed bounded cases have known minimizers. The two-site rare-outcome
example has a known local stationary point and an unbounded negative tail.
Only the actual start changes between its near/far cases, not the expansion
point or derivative summaries. A poorly initialized bounded intercept-only
case separates full-step failure from negative correction curvature.

## Read the Outputs

- `summary`: raw objective, exact KKT, complete and correction curvature,
  native termination, diagnostic limits, reference errors, and timing.
- `steps`: both the frozen quadratic and full nonlinear objective changes after
  each completed CD step. A decrease in the former is not a decrease in the
  latter. Kernel errors leave the last completed iterate, which may be the start.
- `evaluations`: BFGS function calls, **not accepted-iteration trajectories**.
  Numerical-gradient evaluations are included in `functionCalls`. Function-call
  counts are not direct work or runtime comparisons with analytic gradients.
- `coefficients`: actual `optim` return or last completed CD outer iterate.
  If `optim` throws, no returned coefficient vector is invented from its trials.
- `settings`: controls, input hashes, upstream pin and extracted CD-body hash.

The beta limit is an experiment budget, not a coefficient constraint or a
convergence rule. A small KKT residual with indefinite fixed correction can
describe a local solution of an unbounded problem. Small control examples do
not establish production accuracy, runtime gains, or full PDA estimator parity.

## Single Comparison or Sensitivity

```r
f <- surrogateSolverControls()$bounded_full
comparison <- compareSurrogateSolvers(f$data, f$inputs,
  solvers = c("bfgs_numeric", "bfgs_analytic"),
  optimReltol = 1e-15, ndeps = 1e-5)
comparison$summary
```

`compareSurrogateSolvers()` also accepts data/inputs from a validated private
solver snapshot. This performs new diagnostic fitting, unlike the read-only
auditor. Keep private snapshots and returned coefficients inside production.
This script does not authorize disclosure or automatically read, export, or
upload captures. Production replays should follow review of the read-only audit.
