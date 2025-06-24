library(FederatedLearning)
library(Matrix)

# make exactly the same Xmat, yvec, z0, config, r
# as in your experiments:
n <- 1000
p <- 200
Xmat <- rsparsematrix(n, p, 0.05)
yvec <- sample(0:1, n, TRUE)
z0 <- rep(0, p)
cfg <- list(k = 20, etaClient = 0.1, etaServer = 0.05, lambda = 1e-3)
r <- 3

# call your C++ function enough times
for (i in 1:1000) {
  clientUpdateDualAveragingCpp(Xmat, yvec, z0, r, cfg)
}
