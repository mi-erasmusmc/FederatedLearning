#ifndef FEDERATED_LEARNING_LOGISTIC_MATH_H
#define FEDERATED_LEARNING_LOGISTIC_MATH_H

#include <RcppEigen.h>
#include <cmath>

namespace fl {

inline double logisticProbability(double eta) {
  if (eta >= 0.0) {
    const double z = std::exp(-eta);
    return 1.0 / (1.0 + z);
  }
  const double z = std::exp(eta);
  return z / (1.0 + z);
}

inline Eigen::VectorXd logisticResiduals(const Eigen::VectorXd& eta,
                                       const Eigen::VectorXd& y) {
  Eigen::VectorXd residual(eta.size());
  for (int i = 0; i < eta.size(); ++i) {
    // Avoid subtracting one from a probability rounded to one.
    residual[i] = y[i] == 1.0 ? -logisticProbability(-eta[i]) :
      logisticProbability(eta[i]) - y[i];
  }
  return residual;
}

} // namespace fl

#endif
