// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;

static inline double stable_sigmoid_scalar(double eta) {
  if (eta >= 0.0) {
    const double z = std::exp(-eta);
    return 1.0 / (1.0 + z);
  }
  const double z = std::exp(eta);
  return z / (1.0 + z);
}

static Eigen::ArrayXd stable_sigmoid_array(const Eigen::VectorXd& eta) {
  Eigen::ArrayXd out(eta.size());
  for (int i = 0; i < eta.size(); ++i) {
    out[i] = stable_sigmoid_scalar(eta[i]);
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
  Eigen::VectorXd eta = x * beta;
  Eigen::ArrayXd p = clipped_probabilities(eta, eps);
  Eigen::VectorXd residual = (p - y.array()).matrix();
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

static Eigen::VectorXd quadratic_lasso_cd_impl(
    const Eigen::VectorXd& aTilde,
    const Eigen::MatrixXd& bMatrix,
    const Eigen::VectorXd& betaInit,
    double lambda,
    int maxIter,
    double tol,
    const std::vector<int>& penalize) {
  const int p = betaInit.size();
  Eigen::VectorXd beta = betaInit;
  Eigen::VectorXd bBeta = bMatrix * beta;
  const Eigen::VectorXd diagB = bMatrix.diagonal();

  auto objective = [&]() {
    return aTilde.dot(beta) + 0.5 * beta.dot(bBeta);
  };

  for (int iter = 0; iter < maxIter; ++iter) {
    const double oldObjective = objective();

    for (int j = 0; j < p; ++j) {
      double hjj = diagB[j];
      if (!R_finite(hjj) || hjj <= 0.0) {
        hjj = 1e-10;
      }
      const double oldBeta = beta[j];
      const double linearWithoutJ = aTilde[j] + bBeta[j] - diagB[j] * oldBeta;
      double z = -linearWithoutJ / hjj;
      if (!R_finite(z)) {
        z = oldBeta;
      }
      const double newBeta = penalize[j] ? soft_threshold_scalar(z, lambda / hjj) : z;
      const double delta = newBeta - oldBeta;
      if (delta != 0.0) {
        beta[j] = newBeta;
        bBeta.noalias() += bMatrix.col(j) * delta;
      }
    }

    const double newObjective = objective();
    const double diffObjective = newObjective - oldObjective;
    if (R_finite(diffObjective) && std::abs(diffObjective) < tol) {
      break;
    }
  }

  return beta;
}

// [[Rcpp::export]]
Eigen::VectorXd quadraticLassoCdCpp(const Eigen::VectorXd& aTilde,
                                    const Eigen::MatrixXd& bMatrix,
                                    const Eigen::VectorXd& betaInit,
                                    double lambda,
                                    int maxIter = 100,
                                    double tol = 1e-5,
                                    Rcpp::Nullable<Rcpp::LogicalVector> penalizeNullable = R_NilValue) {
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

  return quadratic_lasso_cd_impl(
    aTilde,
    bMatrix,
    betaInit,
    lambda,
    maxIter,
    tol,
    penalize
  );
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
                             int maxOuter = 100,
                             int maxInner = 100,
                             double tol = 1e-5,
                             double eps = 1e-8) {
  const int p = betaStart.size();
  if (x.cols() != p || y.size() != x.rows() || betaBar.size() != p ||
      globalGrad.size() != p || gradBar.size() != p ||
      globalHess.rows() != p || globalHess.cols() != p ||
      hBar.rows() != p || hBar.cols() != p) {
    stop("adapFullSurrogateFitCpp dimension mismatch");
  }
  std::vector<int> penalize(p, 1);
  if (p > 0) {
    penalize[0] = 0;
  }

  Eigen::VectorXd beta = betaStart;
  const Eigen::MatrixXd hCorrection = globalHess - hBar;
  const Eigen::VectorXd aCorrection = globalGrad - gradBar - hCorrection * betaBar;
  int iterations = 0;
  bool converged = false;

  for (int iter = 0; iter < maxOuter; ++iter) {
    iterations = iter + 1;
    Eigen::VectorXd old = beta;
    Eigen::VectorXd grad;
    Eigen::MatrixXd hEval;
    logistic_gradient_hessian_full(x, beta, y, eps, grad, hEval);
    Eigen::MatrixXd bMatrix = hEval + hCorrection;
    Eigen::VectorXd aTilde = grad - hEval * beta + aCorrection;
    beta = quadratic_lasso_cd_impl(aTilde, bMatrix, beta, lambda, maxInner, tol, penalize);
    double delta = (beta - old).cwiseAbs().maxCoeff();
    if (R_finite(delta) && delta < tol) {
      converged = true;
      break;
    }
  }

  return List::create(
    _["beta"] = beta,
    _["outerIterations"] = iterations,
    _["converged"] = converged
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
                              double eps = 1e-8) {
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

  for (int iter = 0; iter < maxOuter; ++iter) {
    iterations = iter + 1;
    Eigen::VectorXd old = beta;
    Eigen::VectorXd grad;
    Eigen::MatrixXd hEval;
    logistic_gradient_hessian_full(x, beta, y, eps, grad, hEval);
    Eigen::VectorXd aTilde = grad - hEval * beta + aCorrection;
    beta = quadratic_lasso_cd_impl(aTilde, hEval, beta, lambda, maxInner, tol, penalize);
    double delta = (beta - old).cwiseAbs().maxCoeff();
    if (R_finite(delta) && delta < tol) {
      converged = true;
      break;
    }
  }

  return List::create(
    _["beta"] = beta,
    _["outerIterations"] = iterations,
    _["converged"] = converged
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
                             double eps = 1e-8) {
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

  for (int iter = 0; iter < maxOuter; ++iter) {
    iterations = iter + 1;
    Eigen::VectorXd old = beta;
    Eigen::VectorXd grad;
    Eigen::MatrixXd hEval;
    logistic_gradient_hessian_full(x, beta, y, eps, grad, hEval);
    Eigen::MatrixXd bMatrix = hEval;
    bMatrix.diagonal() += hCorrection;
    Eigen::VectorXd aTilde = grad - hEval * beta + aCorrection;
    beta = quadratic_lasso_cd_impl(aTilde, bMatrix, beta, lambda, maxInner, tol, penalize);
    double delta = (beta - old).cwiseAbs().maxCoeff();
    if (R_finite(delta) && delta < tol) {
      converged = true;
      break;
    }
  }

  return List::create(
    _["beta"] = beta,
    _["outerIterations"] = iterations,
    _["converged"] = converged
  );
}
