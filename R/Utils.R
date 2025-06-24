#' Compute density = fraction of |w|>threshold
computeDensity <- function(w, threshold = 1e-4) {
  mean(abs(w) > threshold)
}

# Cross‐entropy loss
logLoss <- function(y, pHat) {
  eps <- 1e-15
  pHat <- pmin(pmax(pHat, eps), 1 - eps)
  -mean(y * log(pHat) + (1 - y) * log(1 - pHat))
}

# Accuracy
accuracy <- function(y, pHat, cutoff = 0.5) mean((pHat > cutoff) == y)

# AUC
auc <- function(y, pHat) {
  if (length(unique(y)) == 2) {
    pROC::roc(y, pHat)$auc
  } else {
    warning("AUC is not defined for non-binary outcomes")
    NA
  }
}
