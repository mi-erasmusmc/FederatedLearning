# Reproducible comparison runner for federated methods across tasks and feature sets.
#
# Example:
# Rscript extras/runComparisonMatrix.R \
#   --data-root=data \
#   --tasks=taskA,taskB \
#   --feature-sets=ageSex,ageSexPhenotypes \
#   --methods=DualAvg,ODAL,ODAL1,ADAP,ADAP_PDA,ADAP1,ADAPDiag,PooledLasso \
#   --result-directory=results/comparisonMatrix \
#   --clients=5 \
#   --client-ids=databaseA,databaseB,databaseC,databaseD,databaseE \
#   --folds=1:5 \
#   --taskA-risk-window-end=365 \
#   --taskB-risk-window-end=30
#
# Model artifacts are saved by default under <result-directory>/models.
# To backfill missing artifacts, repeat the original command with
# --resume=true --rerun-missing-models=true (and restrict --methods as needed).
# For guarded DualAvg tuning and final fitting, use --dualavg-kkt-tolerance=1e-7
# and --dualavg-kkt-check-every=100. Nonconverged fits fail the method rather than
# being scored; --debug-diagnostics=true preserves their KKT history and CV trace.
# Matched filtering: --federated-preprocess=baseline (no extra normalization).
# Audit without fitting: --preprocessing-audit-only=true
# Verify saved pooled masks: --preprocessing-reference-directory=<previous results>
# Private surrogate diagnostics, on a local ODAL1/2 or exact ADAP1/2/Diag-only run:
# --surrogate-capture-directory=private/surrogate-captures
# This saves patient-level lead data. Keep it in production, outside results.
# It records a NEW fit, not the missing optimizer state of an old saved model.
#
# The runner is implemented in R/ComparisonRunner.R; this file is its command-line entry point.

if (sys.nframe() == 0L) {
  FederatedLearning:::runComparisonFromArgs(FederatedLearning:::parseArgs())
}
