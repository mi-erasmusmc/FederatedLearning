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
