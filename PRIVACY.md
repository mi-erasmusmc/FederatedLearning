# Privacy and Execution Model

FederatedLearning is currently a framework for simulated federated learning experiments run from one coordinating R session. In the usual workflow, each "client" is represented by a separate worker process, and all workers are started and controlled by the same machine.

The package is useful for testing federated optimization methods on data split into site-like folders, but it should not be interpreted as a deployed privacy-preserving federated-learning system.

## Current Model

In the standard local setup:

- all client workers run under the same user-controlled R environment;
- `clientPaths` point to local or mounted PLP data folders;
- communication happens between the coordinating R process and worker processes;
- there is no package-managed communication with independent external sites.

`clusterInit(..., mirai = FALSE)` uses PSOCK workers through `parallelly::makeClusterPSOCK()`. In principle, PSOCK can be configured for remote workers, but this package does not add the operational controls needed for a production multi-institution deployment.

`mirai = TRUE` currently creates a `mirai` cluster sized by the number of clients. Treat this as process-based execution for experiments unless you have separately configured and validated a remote `mirai` setup.

## What Is Shared Internally

Depending on the algorithm and helper functions used, the coordinating process may collect:

- covariate reference metadata;
- sample counts, outcome counts, and diagnostic summaries;
- local coefficients;
- gradients and Hessians or Hessian diagonals;
- dual-averaging deltas;
- lead-site surrogate fit results and tuning metrics.

These are exchanged inside the simulated federation. Raw patient rows are not intentionally pooled into one analysis table by the fitting workflow, but the local coordinating environment still has access to the configured client folders and worker outputs.

## Practical Guidance

- Use this package for method development, simulation, and controlled research experiments.
- Do not describe current runs as secure, private, or production federated learning.
- Do not commit real manifests, database credentials, ATLAS JSON from private instances, JDBC paths, or site-specific connection details.
- If deploying across independent organizations, add external controls for authentication, authorization, transport security, auditing, governance, and disclosure review.
