// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include "LogisticMath.h"

using namespace Rcpp;

static Eigen::ArrayXd stable_sigmoid_array(const Eigen::VectorXd& eta) {
  Eigen::ArrayXd out(eta.size());
  for (int i = 0; i < eta.size(); ++i) {
    out[i] = fl::logisticProbability(eta[i]);
  }
  return out;
}

static Eigen::ArrayXd clipped_probabilities(const Eigen::VectorXd& eta,
                                            double eps) {
  Eigen::ArrayXd p = stable_sigmoid_array(eta);
  for (int i = 0; i < p.size(); ++i) {
    if (p[i] < eps) {
      p[i] = eps;
    } else if (p[i] > 1.0 - eps) {
      p[i] = 1.0 - eps;
    }
  }
  return p;
}

// Dual feasibility involves subtracting class totals much larger than lambda.
struct CompensatedSum {
  double sum = 0.0;
  double correction = 0.0;
  void add(double value) {
    const double next = sum + value;
    correction += std::abs(sum) >= std::abs(value) ?
      (sum - next) + value : (value - next) + sum;
    sum = next;
  }
  double value() const { return sum + correction; }
};

// [[Rcpp::export]]
List logisticObjectiveGradientCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                   const Eigen::VectorXd& beta,
                                   const Eigen::VectorXd& y,
                                   bool dualStats = false,
                                   bool computeGradient = true) {
  if (x.cols() != beta.size() || x.rows() != y.size() || y.size() == 0) {
    stop("logisticObjectiveGradientCpp requires conformable, nonempty inputs");
  }
  if (!beta.allFinite() || !y.allFinite() || (y.array() < 0).any() || (y.array() > 1).any()) {
    stop("logisticObjectiveGradientCpp requires finite coefficients and labels in [0, 1]");
  }
  const Eigen::VectorXd eta = x * beta;
  if (!eta.allFinite()) {
    stop("logisticObjectiveGradientCpp requires finite linear predictors");
  }
  CompensatedSum lossSum;
  for (int i = 0; i < eta.size(); ++i) {
    // Avoid both overflow and cancellation for correctly classified extreme logits.
    lossSum.add(std::log1p(std::exp(-std::abs(eta[i]))) +
      (eta[i] >= 0.0 ? (1.0 - y[i]) * eta[i] : -y[i] * eta[i]));
  }
  const double loss = lossSum.value();
  const double cyclopsObjective = eta.dot(y);
  if (!R_finite(loss) || !R_finite(cyclopsObjective)) {
    stop("logisticObjectiveGradientCpp produced non-finite statistics");
  }
  List result = List::create(_["loss"] = loss, _["cyclopsObjective"] = cyclopsObjective,
                             _["n"] = y.size());
  Eigen::VectorXd residual;
  if (computeGradient || dualStats) residual = fl::logisticResiduals(eta, y);
  if (computeGradient) {
    const Eigen::VectorXd gradient = x.transpose() * residual / static_cast<double>(y.size());
    if (!gradient.allFinite()) stop("logisticObjectiveGradientCpp produced non-finite gradient");
    result["gradient"] = gradient;
  }
  if (dualStats) {
    CompensatedSum mass[2];
    for (int i = 0; i < y.size(); ++i) {
      if (y[i] != 0.0 && y[i] != 1.0) stop("Dual diagnostics require binary labels");
      const double magnitude = std::abs(residual[i]);
      mass[static_cast<int>(y[i])].add(magnitude);
    }
    Eigen::MatrixXd classGradient(x.cols(), 2);
    double gradientScale = 0.0;
    for (int j = 0; j < x.cols(); ++j) {
      CompensatedSum totals[2], absolute;
      for (Eigen::Map<Eigen::SparseMatrix<double> >::InnerIterator it(x, j); it; ++it) {
        const double value = it.value() * std::abs(residual[it.row()]);
        totals[static_cast<int>(y[it.row()])].add(value);
        absolute.add(std::abs(value));
      }
      for (int label = 0; label < 2; ++label) {
        classGradient(j, label) = totals[label].value() / y.size();
      }
      gradientScale = std::max(gradientScale, absolute.value() / y.size());
    }
    if (!classGradient.allFinite() || !R_finite(gradientScale)) {
      stop("Dual diagnostics produced non-finite class gradients");
    }
    Eigen::Vector2d classMass;
    classMass << mass[0].value() / y.size(), mass[1].value() / y.size();
    result["dualMass"] = classMass;
    result["dualClassGradient"] = classGradient;
    result["dualGradientScale"] = gradientScale;
    result["dualResidual"] = residual;
  }
  return result;
}

// [[Rcpp::export]]
List logisticDualEntropyCpp(const Eigen::VectorXd& residual,
                             const Eigen::VectorXd& y,
                             const Eigen::VectorXd& scales) {
  if (residual.size() == 0 || residual.size() != y.size() || scales.size() != 2 ||
      !residual.allFinite() || !y.allFinite() || !scales.allFinite() ||
      (scales.array() < 0).any() || (scales.array() > 1).any()) {
    stop("Dual entropy requires conformable finite residuals and two scales in [0, 1]");
  }
  long double entropy = 0.0L;
  long double balance = 0.0L;
  for (int i = 0; i < residual.size(); ++i) {
    if ((y[i] != 0.0 && y[i] != 1.0) || residual[i] < -y[i] || residual[i] > 1.0 - y[i]) {
      stop("Dual entropy residual violates its label-dependent domain");
    }
    const double r = residual[i] * scales[static_cast<int>(y[i])];
    const double q = std::abs(r);
    // Binary entropy is symmetric, so use |r| without subtracting from one.
    if (q > 0.0 && q < 1.0) entropy -= q * std::log(q) + (1.0 - q) * std::log1p(-q);
    balance += r;
  }
  return List::create(_["entropy"] = static_cast<double>(entropy / y.size()),
                      _["balance"] = static_cast<double>(balance / y.size()));
}

// [[Rcpp::export]]
double cyclopsGradientObjectiveCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                   const Eigen::VectorXd& beta,
                                   const Eigen::VectorXd& y) {
  if (x.cols() != beta.size()) {
    stop("cyclopsGradientObjectiveCpp dimension mismatch: xMatrix has %d columns but weights has length %d",
         static_cast<int>(x.cols()), static_cast<int>(beta.size()));
  }
  if (x.rows() != y.size()) {
    stop("cyclopsGradientObjectiveCpp dimension mismatch: xMatrix has %d rows but y has length %d",
         static_cast<int>(x.rows()), static_cast<int>(y.size()));
  }
  const Eigen::VectorXd eta = x * beta;
  return eta.dot(y);
}

// [[Rcpp::export]]
Eigen::VectorXd logisticGradientCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                    const Eigen::VectorXd& beta,
                                    const Eigen::VectorXd& y,
                                    double eps = 1e-8) {
  if (x.cols() != beta.size()) {
    stop("logisticGradientCpp dimension mismatch: xMatrix has %d columns but weights has length %d",
         static_cast<int>(x.cols()), static_cast<int>(beta.size()));
  }
  if (x.rows() != y.size()) {
    stop("logisticGradientCpp dimension mismatch: xMatrix has %d rows but y has length %d",
         static_cast<int>(x.rows()), static_cast<int>(y.size()));
  }
  const int n = y.size();
  if (n == 0) stop("logisticGradientCpp requires nonempty inputs");
  Eigen::VectorXd eta = x * beta;
  Eigen::VectorXd residual;
  if (eps == 0.0) {
    residual = fl::logisticResiduals(eta, y);
  } else {
    // PDA-specific callers retain their explicit probability-floor convention.
    Eigen::ArrayXd p = clipped_probabilities(eta, eps);
    residual = (p - y.array()).matrix();
  }
  return (x.transpose() * residual) / static_cast<double>(n);
}

// [[Rcpp::export]]
List logisticGradientHessianDiagCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                    const Eigen::VectorXd& beta,
                                    const Eigen::VectorXd& y,
                                    double eps = 1e-8) {
  if (x.cols() != beta.size()) {
    stop("logisticGradientHessianDiagCpp dimension mismatch: xMatrix has %d columns but weights has length %d",
         static_cast<int>(x.cols()), static_cast<int>(beta.size()));
  }
  if (x.rows() != y.size()) {
    stop("logisticGradientHessianDiagCpp dimension mismatch: xMatrix has %d rows but y has length %d",
         static_cast<int>(x.rows()), static_cast<int>(y.size()));
  }
  const int n = y.size();
  const int pDim = x.cols();
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd prob = clipped_probabilities(eta, eps);
  Eigen::VectorXd residual = (prob - y.array()).matrix();
  Eigen::VectorXd gradient = (x.transpose() * residual) / static_cast<double>(n);
  Eigen::ArrayXd weights = prob * (1.0 - prob);
  Eigen::VectorXd diag = Eigen::VectorXd::Zero(pDim);

  for (int outer = 0; outer < x.outerSize(); ++outer) {
    for (Eigen::Map<Eigen::SparseMatrix<double> >::InnerIterator it(x, outer); it; ++it) {
      diag[it.col()] += it.value() * it.value() * weights[it.row()];
    }
  }

  return List::create(
    _["gradient"] = gradient,
    _["hessianDiag"] = diag / static_cast<double>(n)
  );
}

// [[Rcpp::export]]
Eigen::VectorXd logisticHessianDiagCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                       const Eigen::VectorXd& beta,
                                       double eps = 1e-8) {
  if (x.cols() != beta.size()) {
    stop("logisticHessianDiagCpp dimension mismatch: xMatrix has %d columns but weights has length %d",
         static_cast<int>(x.cols()), static_cast<int>(beta.size()));
  }
  const int n = x.rows();
  const int pDim = x.cols();
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd prob = clipped_probabilities(eta, eps);
  Eigen::ArrayXd weights = prob * (1.0 - prob);
  Eigen::VectorXd diag = Eigen::VectorXd::Zero(pDim);

  for (int outer = 0; outer < x.outerSize(); ++outer) {
    for (Eigen::Map<Eigen::SparseMatrix<double> >::InnerIterator it(x, outer); it; ++it) {
      diag[it.col()] += it.value() * it.value() * weights[it.row()];
    }
  }
  return diag / static_cast<double>(n);
}

// [[Rcpp::export]]
Eigen::MatrixXd logisticHessianCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                   const Eigen::VectorXd& beta,
                                   double eps = 1e-8) {
  if (x.cols() != beta.size()) {
    stop("logisticHessianCpp dimension mismatch: xMatrix has %d columns but weights has length %d",
         static_cast<int>(x.cols()), static_cast<int>(beta.size()));
  }
  const int n = x.rows();
  const int pDim = x.cols();
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd prob = clipped_probabilities(eta, eps);
  Eigen::VectorXd weights = (prob * (1.0 - prob)).matrix();
  Eigen::SparseMatrix<double> weightedX = x;

  for (int outer = 0; outer < weightedX.outerSize(); ++outer) {
    for (Eigen::SparseMatrix<double>::InnerIterator it(weightedX, outer); it; ++it) {
      it.valueRef() *= weights[it.row()];
    }
  }
  Eigen::SparseMatrix<double> hSparse = x.transpose() * weightedX;
  return Eigen::MatrixXd(hSparse) / static_cast<double>(n);
}

// [[Rcpp::export]]
List logisticGradientHessianCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                                const Eigen::VectorXd& beta,
                                const Eigen::VectorXd& y,
                                double eps = 1e-8) {
  if (x.cols() != beta.size()) {
    stop("logisticGradientHessianCpp dimension mismatch: xMatrix has %d columns but weights has length %d",
         static_cast<int>(x.cols()), static_cast<int>(beta.size()));
  }
  if (x.rows() != y.size()) {
    stop("logisticGradientHessianCpp dimension mismatch: xMatrix has %d rows but y has length %d",
         static_cast<int>(x.rows()), static_cast<int>(y.size()));
  }
  const int n = y.size();
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd prob = clipped_probabilities(eta, eps);
  Eigen::VectorXd residual = (prob - y.array()).matrix();
  Eigen::VectorXd gradient = (x.transpose() * residual) / static_cast<double>(n);
  Eigen::VectorXd weights = (prob * (1.0 - prob)).matrix();
  Eigen::SparseMatrix<double> weightedX = x;

  for (int outer = 0; outer < weightedX.outerSize(); ++outer) {
    for (Eigen::SparseMatrix<double>::InnerIterator it(weightedX, outer); it; ++it) {
      it.valueRef() *= weights[it.row()];
    }
  }
  Eigen::SparseMatrix<double> hSparse = x.transpose() * weightedX;

  return List::create(
    _["gradient"] = gradient,
    _["hessian"] = Eigen::MatrixXd(hSparse) / static_cast<double>(n)
  );
}

static inline double soft_threshold_scalar(double value, double threshold) {
  if (value > threshold) {
    return value - threshold;
  }
  if (value < -threshold) {
    return value + threshold;
  }
  return 0.0;
}

static void logistic_gradient_hessian_full(
    const Eigen::Map<Eigen::SparseMatrix<double> >& x,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& y,
    double eps,
    Eigen::VectorXd& gradient,
    Eigen::MatrixXd& hessian) {
  const int n = y.size();
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd prob = clipped_probabilities(eta, eps);
  Eigen::VectorXd residual = (prob - y.array()).matrix();
  gradient = (x.transpose() * residual) / static_cast<double>(n);
  Eigen::VectorXd weights = (prob * (1.0 - prob)).matrix();
  Eigen::SparseMatrix<double> weightedX = x;

  for (int outer = 0; outer < weightedX.outerSize(); ++outer) {
    for (Eigen::SparseMatrix<double>::InnerIterator it(weightedX, outer); it; ++it) {
      it.valueRef() *= weights[it.row()];
    }
  }
  Eigen::SparseMatrix<double> hSparse = x.transpose() * weightedX;
  hessian = Eigen::MatrixXd(hSparse) / static_cast<double>(n);
}

static void logistic_gradient_hessian_diag(
    const Eigen::Map<Eigen::SparseMatrix<double> >& x,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& y,
    double eps,
    Eigen::VectorXd& gradient,
    Eigen::VectorXd& hessianDiag) {
  const int n = y.size();
  const int pDim = x.cols();
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd prob = clipped_probabilities(eta, eps);
  Eigen::VectorXd residual = (prob - y.array()).matrix();
  gradient = (x.transpose() * residual) / static_cast<double>(n);
  Eigen::ArrayXd weights = prob * (1.0 - prob);
  hessianDiag = Eigen::VectorXd::Zero(pDim);

  for (int outer = 0; outer < x.outerSize(); ++outer) {
    for (Eigen::Map<Eigen::SparseMatrix<double> >::InnerIterator it(x, outer); it; ++it) {
      hessianDiag[it.col()] += it.value() * it.value() * weights[it.row()];
    }
  }
  hessianDiag /= static_cast<double>(n);
}

struct QuadraticCdResult {
  Eigen::VectorXd beta;
  int iterations;
  bool converged;
  std::string failureReason;
  double objective;
  double maxAbsStep;
  int backtracks;
  int failingCoordinate;
  double coordinateCurvature;
  double coordinateGradient;
  double diagMin;
  double diagMax;
  int diagNonPositive;
};

static double l1_penalty_value(const Eigen::VectorXd& beta,
                               double lambda,
                               const std::vector<int>& penalize) {
  double penalty = 0.0;
  for (int j = 0; j < beta.size(); ++j) {
    if (penalize[j]) {
      penalty += std::abs(beta[j]);
    }
  }
  return lambda * penalty;
}

static QuadraticCdResult make_cd_result(const Eigen::VectorXd& beta,
                                        const std::string& failureReason = "") {
  QuadraticCdResult result;
  result.beta = beta;
  result.iterations = 0;
  result.converged = false;
  result.failureReason = failureReason;
  result.objective = NA_REAL;
  result.maxAbsStep = NA_REAL;
  result.backtracks = 0;
  result.failingCoordinate = NA_INTEGER;
  result.coordinateCurvature = NA_REAL;
  result.coordinateGradient = NA_REAL;
  result.diagMin = NA_REAL;
  result.diagMax = NA_REAL;
  result.diagNonPositive = NA_INTEGER;
  return result;
}

static void set_cd_failure(QuadraticCdResult& result,
                           const std::string& reason,
                           int iter,
                           const Eigen::VectorXd& beta,
                           double objective,
                           double maxAbsStep,
                           int coordinate,
                           double curvature,
                           double gradient) {
  result.failureReason = reason;
  result.iterations = iter;
  result.beta = beta;
  result.objective = objective;
  result.maxAbsStep = maxAbsStep;
  result.failingCoordinate = coordinate >= 0 ? coordinate + 1 : NA_INTEGER;
  result.coordinateCurvature = curvature;
  result.coordinateGradient = gradient;
}

static QuadraticCdResult quadratic_lasso_cd_impl(
    const Eigen::VectorXd& aTilde,
    const Eigen::MatrixXd& bMatrix,
    const Eigen::VectorXd& betaInit,
    double lambda,
    int maxIter,
    double tol,
    const std::vector<int>& penalize,
    double initialStepBound,
    double minStep,
    int maxBacktracks) {
  const int p = betaInit.size();
  Eigen::VectorXd beta = betaInit;
  Eigen::VectorXd bBeta = bMatrix * beta;
  const Eigen::VectorXd diagB = bMatrix.diagonal();
  Eigen::VectorXd stepBounds = Eigen::VectorXd::Constant(p, initialStepBound);
  QuadraticCdResult result = make_cd_result(beta);
  const double curvatureTol = 1e-14;

  if (!aTilde.allFinite() || !bMatrix.allFinite() || !beta.allFinite() || !bBeta.allFinite()) {
    return make_cd_result(beta, "non_finite_surrogate_input");
  }
  if (p > 0) {
    result.diagMin = diagB.minCoeff();
    result.diagMax = diagB.maxCoeff();
    result.diagNonPositive = 0;
  }
  for (int j = 0; j < p; ++j) {
    if (!R_finite(diagB[j])) {
      set_cd_failure(result, "non_finite_coordinate_curvature", 0, beta, NA_REAL, NA_REAL,
                     j, diagB[j], NA_REAL);
      return result;
    }
    if (diagB[j] <= 0.0) {
      ++result.diagNonPositive;
    }
    if (diagB[j] < -curvatureTol) {
      set_cd_failure(result, "non_positive_coordinate_curvature", 0, beta, NA_REAL, NA_REAL,
                     j, diagB[j], NA_REAL);
      return result;
    }
  }

  auto objective = [&]() {
    return aTilde.dot(beta) + 0.5 * beta.dot(bBeta) +
      l1_penalty_value(beta, lambda, penalize);
  };
  double currentObjective = objective();
  if (!R_finite(currentObjective)) {
    return make_cd_result(beta, "non_finite_objective");
  }
  result.objective = currentObjective;

  for (int iter = 0; iter < maxIter; ++iter) {
    const double oldObjective = currentObjective;
    double maxAbsStepThisIter = 0.0;

    for (int j = 0; j < p; ++j) {
      const double hjj = diagB[j];
      const double oldBeta = beta[j];
      const double linearWithoutJ = aTilde[j] + bBeta[j] - diagB[j] * oldBeta;
      const double smoothGradient = aTilde[j] + bBeta[j];
      if (!R_finite(hjj) || hjj < -curvatureTol) {
        set_cd_failure(result, "non_positive_coordinate_curvature", iter + 1, beta,
                       currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
        return result;
      }
      if (std::abs(hjj) <= curvatureTol) {
        if (!R_finite(smoothGradient)) {
          set_cd_failure(result, "non_finite_coordinate_update", iter + 1, beta,
                         currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
          return result;
        }
        if (penalize[j]) {
          const double kktTol = lambda + 1e-10 * (std::abs(currentObjective) + 1.0);
          if (std::abs(smoothGradient) <= kktTol) {
            if (std::abs(oldBeta) <= minStep) {
              continue;
            }
            const double deltaToZero = -oldBeta;
            const double l1Delta = -lambda * std::abs(oldBeta);
            const double trialObjective = currentObjective +
              smoothGradient * deltaToZero + l1Delta;
            const double descentTol = 1e-12 * (std::abs(currentObjective) + 1.0);
            if (R_finite(trialObjective) && trialObjective <= currentObjective + descentTol) {
              beta[j] = 0.0;
              bBeta.noalias() += bMatrix.col(j) * deltaToZero;
              currentObjective = trialObjective;
              maxAbsStepThisIter = std::max(maxAbsStepThisIter, std::abs(deltaToZero));
              continue;
            }
          }
          set_cd_failure(result, "zero_coordinate_curvature_unbounded", iter + 1, beta,
                         currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
          return result;
        }
        if (std::abs(smoothGradient) <= 1e-10 * (std::abs(currentObjective) + 1.0)) {
          continue;
        }
        set_cd_failure(result, "zero_unpenalized_coordinate_curvature", iter + 1, beta,
                       currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
        return result;
      }
      double z = -linearWithoutJ / hjj;
      if (!R_finite(z)) {
        set_cd_failure(result, "non_finite_coordinate_update", iter + 1, beta,
                       currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
        return result;
      }
      const double unboundedNewBeta = penalize[j] ? soft_threshold_scalar(z, lambda / hjj) : z;
      double delta = unboundedNewBeta - oldBeta;
      if (!R_finite(delta)) {
        set_cd_failure(result, "non_finite_coordinate_step", iter + 1, beta,
                       currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
        return result;
      }
      if (delta > stepBounds[j]) {
        delta = stepBounds[j];
      } else if (delta < -stepBounds[j]) {
        delta = -stepBounds[j];
      }
      if (std::abs(delta) <= minStep) {
        continue;
      }

      double acceptedDelta = delta;
      double acceptedObjective = NA_REAL;
      bool accepted = false;
      int backtrack = 0;
      while (backtrack <= maxBacktracks) {
        const double trialBeta = oldBeta + acceptedDelta;
        const double l1Delta = penalize[j] ?
          lambda * (std::abs(trialBeta) - std::abs(oldBeta)) : 0.0;
        const double trialObjective = currentObjective +
          smoothGradient * acceptedDelta +
          0.5 * hjj * acceptedDelta * acceptedDelta +
          l1Delta;
        const double descentTol = 1e-12 * (std::abs(currentObjective) + 1.0);
        if (R_finite(trialObjective) && trialObjective <= currentObjective + descentTol) {
          accepted = true;
          acceptedObjective = trialObjective;
          break;
        }
        acceptedDelta *= 0.5;
        ++backtrack;
        ++result.backtracks;
        if (std::abs(acceptedDelta) <= minStep) {
          break;
        }
      }

      if (!accepted) {
        set_cd_failure(result, "non_descent_coordinate_step", iter + 1, beta,
                       currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
        return result;
      }

      beta[j] = oldBeta + acceptedDelta;
      bBeta.noalias() += bMatrix.col(j) * acceptedDelta;
      currentObjective = acceptedObjective;
      maxAbsStepThisIter = std::max(maxAbsStepThisIter, std::abs(acceptedDelta));
      stepBounds[j] = std::max(std::max(2.0 * std::abs(acceptedDelta), stepBounds[j] / 2.0), minStep);
      if (!beta.allFinite() || !bBeta.allFinite() || !R_finite(currentObjective)) {
        set_cd_failure(result, "non_finite_coordinate_state", iter + 1, beta,
                       currentObjective, maxAbsStepThisIter, j, hjj, smoothGradient);
        return result;
      }
    }

    const double diffObjective = currentObjective - oldObjective;
    result.iterations = iter + 1;
    result.objective = currentObjective;
    result.maxAbsStep = maxAbsStepThisIter;
    if (R_finite(diffObjective) &&
        (std::abs(diffObjective) < tol || maxAbsStepThisIter < tol)) {
      result.converged = true;
      break;
    }
  }

  result.beta = beta;
  return result;
}

// [[Rcpp::export]]
Eigen::VectorXd quadraticLassoCdCpp(const Eigen::VectorXd& aTilde,
                                    const Eigen::MatrixXd& bMatrix,
                                    const Eigen::VectorXd& betaInit,
                                    double lambda,
                                    int maxIter = 100,
                                    double tol = 1e-5,
                                    Rcpp::Nullable<Rcpp::LogicalVector> penalizeNullable = R_NilValue,
                                    double initialStepBound = 1.0,
                                    double minStep = 1e-8,
                                    int maxBacktracks = 25) {
  const int p = betaInit.size();
  if (aTilde.size() != p) {
    stop("quadraticLassoCdCpp dimension mismatch: aTilde has length %d but betaInit has length %d",
         static_cast<int>(aTilde.size()), p);
  }
  if (bMatrix.rows() != p || bMatrix.cols() != p) {
    stop("quadraticLassoCdCpp dimension mismatch: B is %d x %d but betaInit has length %d",
         static_cast<int>(bMatrix.rows()), static_cast<int>(bMatrix.cols()), p);
  }
  if (maxIter < 1) {
    stop("maxIter must be positive");
  }
  if (!R_finite(lambda) || lambda < 0.0) {
    stop("lambda must be a finite non-negative value");
  }
  if (!R_finite(tol) || tol < 0.0) {
    stop("tol must be a finite non-negative value");
  }
  if (!R_finite(initialStepBound) || initialStepBound <= 0.0) {
    stop("initialStepBound must be a positive finite value");
  }
  if (!R_finite(minStep) || minStep <= 0.0) {
    stop("minStep must be a positive finite value");
  }
  if (maxBacktracks < 0) {
    stop("maxBacktracks must be non-negative");
  }

  std::vector<int> penalize(p, 1);
  if (penalizeNullable.isNotNull()) {
    Rcpp::LogicalVector penalizeVector(penalizeNullable);
    if (penalizeVector.size() != p) {
      stop("penalize length mismatch: expected %d but got %d",
           p, static_cast<int>(penalizeVector.size()));
    }
    for (int j = 0; j < p; ++j) {
      penalize[j] = penalizeVector[j] == TRUE;
    }
  } else if (p > 0) {
    penalize[0] = 0;
  }

  QuadraticCdResult result = quadratic_lasso_cd_impl(
    aTilde,
    bMatrix,
    betaInit,
    lambda,
    maxIter,
    tol,
    penalize,
    initialStepBound,
    minStep,
    maxBacktracks
  );
  if (!result.failureReason.empty()) {
    stop("quadraticLassoCdCpp failed: %s", result.failureReason);
  }
  return result.beta;
}

// [[Rcpp::export]]
List adapFullSurrogateFitCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                             const Eigen::VectorXd& y,
                             const Eigen::VectorXd& betaStart,
                             const Eigen::VectorXd& betaBar,
                             const Eigen::VectorXd& globalGrad,
                             const Eigen::MatrixXd& globalHess,
                             const Eigen::VectorXd& gradBar,
                             const Eigen::MatrixXd& hBar,
                             double lambda,
                             double leadWeight = 1.0,
                             double proxRho = 0.0,
                             int maxOuter = 100,
                             int maxInner = 100,
                             double tol = 1e-5,
                             double eps = 1e-8,
                             double initialStepBound = 1.0,
                             double minStep = 1e-8,
                             int maxBacktracks = 25) {
  const int p = betaStart.size();
  if (x.cols() != p || y.size() != x.rows() || betaBar.size() != p ||
      globalGrad.size() != p || gradBar.size() != p ||
      globalHess.rows() != p || globalHess.cols() != p ||
      hBar.rows() != p || hBar.cols() != p) {
    stop("adapFullSurrogateFitCpp dimension mismatch");
  }
  if (!R_finite(leadWeight) || leadWeight <= 0.0) {
    stop("leadWeight must be a positive finite value");
  }
  if (!R_finite(proxRho) || proxRho < 0.0) {
    stop("proxRho must be a finite non-negative value");
  }
  std::vector<int> penalize(p, 1);
  if (p > 0) {
    penalize[0] = 0;
  }

  Eigen::VectorXd beta = betaStart;
  Eigen::MatrixXd hCorrection = globalHess - leadWeight * hBar;
  if (proxRho > 0.0) {
    hCorrection.diagonal().array() += proxRho;
  }
  const Eigen::VectorXd aCorrection = globalGrad - leadWeight * gradBar - hCorrection * betaBar;
  int iterations = 0;
  bool converged = false;
  std::string failureReason = "";
  QuadraticCdResult cdResult = make_cd_result(beta);

  for (int iter = 0; iter < maxOuter; ++iter) {
    iterations = iter + 1;
    Eigen::VectorXd old = beta;
    Eigen::VectorXd grad;
    Eigen::MatrixXd hEval;
    logistic_gradient_hessian_full(x, beta, y, eps, grad, hEval);
    Eigen::MatrixXd bMatrix = leadWeight * hEval + hCorrection;
    Eigen::VectorXd aTilde = leadWeight * grad - leadWeight * hEval * beta + aCorrection;
    cdResult = quadratic_lasso_cd_impl(
      aTilde, bMatrix, beta, lambda, maxInner, tol, penalize,
      initialStepBound, minStep, maxBacktracks);
    beta = cdResult.beta;
    if (!cdResult.failureReason.empty()) {
      failureReason = cdResult.failureReason;
      break;
    }
    double delta = (beta - old).cwiseAbs().maxCoeff();
    if (!R_finite(delta) || !beta.allFinite()) {
      failureReason = "non_finite_outer_state";
      break;
    }
    if (R_finite(delta) && delta < tol) {
      converged = true;
      break;
    }
  }

  return List::create(
    _["beta"] = beta,
    _["outerIterations"] = iterations,
    _["converged"] = converged,
    _["failureReason"] = failureReason,
    _["innerIterations"] = cdResult.iterations,
    _["innerConverged"] = cdResult.converged,
    _["innerObjective"] = cdResult.objective,
    _["innerMaxAbsStep"] = cdResult.maxAbsStep,
    _["innerBacktracks"] = cdResult.backtracks,
    _["failingCoordinate"] = cdResult.failingCoordinate,
    _["coordinateCurvature"] = cdResult.coordinateCurvature,
    _["coordinateGradient"] = cdResult.coordinateGradient,
    _["failureDiagMin"] = cdResult.diagMin,
    _["failureDiagMax"] = cdResult.diagMax,
    _["failureDiagNonPositive"] = cdResult.diagNonPositive
  );
}

// [[Rcpp::export]]
List adapFirstSurrogateFitCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                              const Eigen::VectorXd& y,
                              const Eigen::VectorXd& betaStart,
                              const Eigen::VectorXd& betaBar,
                              const Eigen::VectorXd& globalGrad,
                              const Eigen::VectorXd& gradBar,
                              double lambda,
                              int maxOuter = 100,
                              int maxInner = 100,
                              double tol = 1e-5,
                              double eps = 1e-8,
                              double initialStepBound = 1.0,
                              double minStep = 1e-8,
                              int maxBacktracks = 25) {
  const int p = betaStart.size();
  if (x.cols() != p || y.size() != x.rows() || betaBar.size() != p ||
      globalGrad.size() != p || gradBar.size() != p) {
    stop("adapFirstSurrogateFitCpp dimension mismatch");
  }
  std::vector<int> penalize(p, 1);
  if (p > 0) {
    penalize[0] = 0;
  }

  Eigen::VectorXd beta = betaStart;
  const Eigen::VectorXd aCorrection = globalGrad - gradBar;
  int iterations = 0;
  bool converged = false;
  std::string failureReason = "";
  QuadraticCdResult cdResult = make_cd_result(beta);

  for (int iter = 0; iter < maxOuter; ++iter) {
    iterations = iter + 1;
    Eigen::VectorXd old = beta;
    Eigen::VectorXd grad;
    Eigen::MatrixXd hEval;
    logistic_gradient_hessian_full(x, beta, y, eps, grad, hEval);
    Eigen::VectorXd aTilde = grad - hEval * beta + aCorrection;
    cdResult = quadratic_lasso_cd_impl(
      aTilde, hEval, beta, lambda, maxInner, tol, penalize,
      initialStepBound, minStep, maxBacktracks);
    beta = cdResult.beta;
    if (!cdResult.failureReason.empty()) {
      failureReason = cdResult.failureReason;
      break;
    }
    double delta = (beta - old).cwiseAbs().maxCoeff();
    if (!R_finite(delta) || !beta.allFinite()) {
      failureReason = "non_finite_outer_state";
      break;
    }
    if (R_finite(delta) && delta < tol) {
      converged = true;
      break;
    }
  }

  return List::create(
    _["beta"] = beta,
    _["outerIterations"] = iterations,
    _["converged"] = converged,
    _["failureReason"] = failureReason,
    _["innerIterations"] = cdResult.iterations,
    _["innerConverged"] = cdResult.converged,
    _["innerObjective"] = cdResult.objective,
    _["innerMaxAbsStep"] = cdResult.maxAbsStep,
    _["innerBacktracks"] = cdResult.backtracks,
    _["failingCoordinate"] = cdResult.failingCoordinate,
    _["coordinateCurvature"] = cdResult.coordinateCurvature,
    _["coordinateGradient"] = cdResult.coordinateGradient,
    _["failureDiagMin"] = cdResult.diagMin,
    _["failureDiagMax"] = cdResult.diagMax,
    _["failureDiagNonPositive"] = cdResult.diagNonPositive
  );
}

// [[Rcpp::export]]
List adapDiagSurrogateFitCpp(const Eigen::Map<Eigen::SparseMatrix<double> >& x,
                             const Eigen::VectorXd& y,
                             const Eigen::VectorXd& betaStart,
                             const Eigen::VectorXd& betaBar,
                             const Eigen::VectorXd& globalGrad,
                             const Eigen::VectorXd& globalHessDiag,
                             const Eigen::VectorXd& gradBar,
                             const Eigen::VectorXd& hBarDiag,
                             double lambda,
                             int maxOuter = 100,
                             int maxInner = 100,
                             double tol = 1e-5,
                             double eps = 1e-8,
                             double initialStepBound = 1.0,
                             double minStep = 1e-8,
                             int maxBacktracks = 25) {
  const int p = betaStart.size();
  if (x.cols() != p || y.size() != x.rows() || betaBar.size() != p ||
      globalGrad.size() != p || globalHessDiag.size() != p ||
      gradBar.size() != p || hBarDiag.size() != p) {
    stop("adapDiagSurrogateFitCpp dimension mismatch");
  }
  std::vector<int> penalize(p, 1);
  if (p > 0) {
    penalize[0] = 0;
  }

  Eigen::VectorXd beta = betaStart;
  const Eigen::VectorXd hCorrection = globalHessDiag - hBarDiag;
  const Eigen::VectorXd aCorrection = globalGrad - gradBar - (betaBar.array() * hCorrection.array()).matrix();
  int iterations = 0;
  bool converged = false;
  std::string failureReason = "";
  QuadraticCdResult cdResult = make_cd_result(beta);

  for (int iter = 0; iter < maxOuter; ++iter) {
    iterations = iter + 1;
    Eigen::VectorXd old = beta;
    Eigen::VectorXd grad;
    Eigen::MatrixXd hEval;
    logistic_gradient_hessian_full(x, beta, y, eps, grad, hEval);
    Eigen::MatrixXd bMatrix = hEval;
    bMatrix.diagonal() += hCorrection;
    Eigen::VectorXd aTilde = grad - hEval * beta + aCorrection;
    cdResult = quadratic_lasso_cd_impl(
      aTilde, bMatrix, beta, lambda, maxInner, tol, penalize,
      initialStepBound, minStep, maxBacktracks);
    beta = cdResult.beta;
    if (!cdResult.failureReason.empty()) {
      failureReason = cdResult.failureReason;
      break;
    }
    double delta = (beta - old).cwiseAbs().maxCoeff();
    if (!R_finite(delta) || !beta.allFinite()) {
      failureReason = "non_finite_outer_state";
      break;
    }
    if (R_finite(delta) && delta < tol) {
      converged = true;
      break;
    }
  }

  return List::create(
    _["beta"] = beta,
    _["outerIterations"] = iterations,
    _["converged"] = converged,
    _["failureReason"] = failureReason,
    _["innerIterations"] = cdResult.iterations,
    _["innerConverged"] = cdResult.converged,
    _["innerObjective"] = cdResult.objective,
    _["innerMaxAbsStep"] = cdResult.maxAbsStep,
    _["innerBacktracks"] = cdResult.backtracks,
    _["failingCoordinate"] = cdResult.failingCoordinate,
    _["coordinateCurvature"] = cdResult.coordinateCurvature,
    _["coordinateGradient"] = cdResult.coordinateGradient,
    _["failureDiagMin"] = cdResult.diagMin,
    _["failureDiagMax"] = cdResult.diagMax,
    _["failureDiagNonPositive"] = cdResult.diagNonPositive
  );
}
