library(Matrix)
library(microbenchmark)
library(FederatedLearning)

n <- 100000
p <- 500
X <- rsparsematrix(n, p, 0.01)
w <- rnorm(p)
y <- rbinom(n, 1, 0.3)

mb <- microbenchmark(
  R = gradLogistic(w, X, y),
  CPP = gradLogisticEigen(w, X, y),
  CPPFUSED = gradLogisticEigenFused(w, X, y),
  times = 20
)
print(mb)
