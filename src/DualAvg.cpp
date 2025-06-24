// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
using namespace Rcpp;

// A tiny inline soft‐threshold helper
static inline double soft(double v, double a) {
  if (v > a)
    return v - a;
  else if (v < -a)
    return v + a;
  else
    return 0.0;
}

// Apply soft‐threshold to an Eigen vector
static Eigen::VectorXd proxL1_E(const Eigen::VectorXd &z, double alpha) {
  Eigen::VectorXd w(z.size());
  w[0] = z[0]; 
  for (int j = 1; j < z.size(); ++j) {
    w[j] = soft(z[j], alpha);
  }
  return w;
}

//' @export
// [[Rcpp::export]]
List serverInitDualAveragingCpp(List config) {
  bool interceptFlag = config["intercept"];
  int intercept = interceptFlag ? 1 : 0;
  int p = config["p"];
  Eigen::VectorXd z = Eigen::VectorXd::Zero(p + intercept);
  return List::create(_["z"] = z);

}


//' @export
// [[Rcpp::export]]
List clientUpdateDualAveragingCpp(List &clientData,
                                  List &serverBroadcast,
                                  List &config) {

  Eigen::SparseMatrix<double> xMatrix = clientData["xMatrix"];
  Eigen::VectorXd yLabels = clientData["yLabels"];

  Eigen::VectorXd z = serverBroadcast["z"];
  int r = serverBroadcast["r"];

  int k = config["k"];
  double etaC = config["etaClient"];
  double etaS = config["etaServer"];
  double lambda = config["lambda"];

  Eigen::VectorXd z0 = z;
  int n = yLabels.size();
  for (int i = 0; i < k; ++i) {
    // composite penalty weight ˜η_{r,k}
    double alpha = etaS * etaC * (double)r * (double)k + etaC * (double)i;
    // 1) mirror‐prox (primal retrieval via soft‐threshold)
    Eigen::VectorXd w = proxL1_E(z, alpha * lambda);

    // 2) compute logistic gradient: g = (Xᵀ(σ(Xw) – y))/n
    Eigen::VectorXd lin = xMatrix * w; // size‐n
    Eigen::ArrayXd sig = 1.0 / (1.0 + (-lin.array()).exp());
    Eigen::VectorXd g = xMatrix.transpose() * (sig - yLabels.array()).matrix();
    g /= double(n);

    // 3) dual‐averaging step
    z -= etaC * g;
  }
  Eigen::VectorXd delta = z - z0;
  return List::create(_["delta"] = delta);
}

//' @export
// [[Rcpp::export]]
List serverRoundDualAveragingCpp(List &serverState, 
                                 List &clientReports,
                                 List &config) {
  Eigen::VectorXd z = serverState["z"];
  int r = serverState["r"];
  double etaS = config["etaServer"];
  double etaC = config["etaClient"];
  int k = config["k"];
  double lambda = config["lambda"];

  int M = clientReports.size();
  Eigen::VectorXd avg = Eigen::VectorXd::Zero(z.size());
  // 1) average the list of Δz
  for (int i = 0; i < M; ++i) {
    const List &cliRep = clientReports[i];
    avg += as<Eigen::VectorXd>(cliRep["delta"]);
  }
  avg /= double(M);

  // 2) server dual update

  Eigen::VectorXd zNew = z + etaS * avg;

  // 3) optional primal retrieval
  double alpha = etaS * etaC * (double)(r + 1) * k;
  Eigen::VectorXd wNew = proxL1_E(zNew, alpha * lambda);

  List state = List::create(_["z"] = zNew);
  List report = List::create(_["w"] = wNew);
  
  return List::create(_["state"] = state, 
                      _["report"] = report);
}
