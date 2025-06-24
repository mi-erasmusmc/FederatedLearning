# ——————————————————————————————————————————————
# This script benchmarks
#    clientUpdateDualAveraging (pure R)
#    clientUpdateDualAveragingCpp (Rcpp/Eigen)
# from your FederatedLearning package.

# 1) load everything
library(FederatedLearning) # your package with both functions
library(Matrix) # for sparse matrix support
library(microbenchmark) # the benchmark harness
library(ggplot2) # for plotting

# 2) simulate a small dataset
set.seed(123)
n <- 100000 # # samples
p <- 200 # # features
density <- 0.01 # sparsity of X

# a random sparse design matrix
Xmat <- rsparsematrix(
  nrow = n,
  ncol = p,
  density = density
)

# binary labels {0,1}
yvec <- sample(0:1, size = n, replace = TRUE)

# initial dual variable z
z0 <- rep(0, p)

# a toy config
config <- list(
  k         = 20,
  etaClient = 0.1,
  etaServer = 0.05,
  lambda    = 1e-3
)

# pick one round index
r <- 3

# 3) warm up (so that any JIT, lazy loading, copy-on-write, etc. is done)
invisible(clientUpdateDualAveraging(Xmat, yvec, z0, r, config))
invisible(clientUpdateDualAveragingCpp(Xmat, yvec, z0, r, config))

# 4) run microbenchmark
bm <- microbenchmark(
  R = clientUpdateDualAveraging(Xmat, yvec, z0, r, config),
  Cpp = clientUpdateDualAveragingCpp(Xmat, yvec, z0, r, config),
  times = 20L, # you can increase/decrease
  unit = "ms" # report in milliseconds
)

# bench server functions
sm <- micromebenchmark(
  R = serverRoundDualAveraging(deltaR$z, deltaR$delta, r, config),
  Cpp = serverUpdateDualAveragingCpp(Xmat, yvec, z0, r, config),
  times = 20L,
  unit = "ms"
)



print(bm)
autoplot(bm) +
  ggtitle("Benchmark: R vs Rcpp dual-averaging clientUpdate") +
  theme_minimal()

# 5) extract a simple summary table if you like
agg <- summary(bm)[, c("expr", "mean", "median", "min", "max", "lq", "uq")]
print(agg)

# profile R function
# 1) start the profiler, capturing C calls too
fn <- tempfile()
Rprof(fn, line.profiling = TRUE, memory.profiling = FALSE)

# 2) run a handful of calls to your two functions
for(i in 1:10) {
  clientUpdateDualAveraging(Xmat, yvec, z0, r, config)
}

# 3) stop profiling
Rprof(NULL)

# 4) summarize
print(summaryRprof(fn))
