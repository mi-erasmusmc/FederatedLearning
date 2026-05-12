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

static inline double stable_sigmoid_scalar_da(double eta) {
  if (eta >= 0.0) {
    const double z = std::exp(-eta);
    return 1.0 / (1.0 + z);
  }
  const double z = std::exp(eta);
  return z / (1.0 + z);
}

static Eigen::ArrayXd stable_sigmoid_array_da(const Eigen::VectorXd& eta,
                                              double eps = 1e-8) {
  Eigen::ArrayXd out(eta.size());
  for (int i = 0; i < eta.size(); ++i) {
    out[i] = stable_sigmoid_scalar_da(eta[i]);
    if (out[i] < eps) {
      out[i] = eps;
    } else if (out[i] > 1.0 - eps) {
      out[i] = 1.0 - eps;
    }
  }
  return out;
}

//' @export
// [[Rcpp::export]]
List serverInitDualAveragingCpp(List config) {
  bool interceptFlag = config["intercept"];
  int intercept = interceptFlag ? 1 : 0;
  int p = config["p"];
  int dim = p + intercept;
  Eigen::VectorXd z = Eigen::VectorXd::Zero(dim);
  if (config.containsElementNamed("initialZ") && !Rf_isNull(config["initialZ"])) {
    z = as<Eigen::VectorXd>(config["initialZ"]);
    if (z.size() != dim) {
      stop("initialZ length mismatch: expected %s but got %s", dim, z.size());
    }
  }
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
  double reportN = double(n);
  if (clientData.containsElementNamed("n") && !Rf_isNull(clientData["n"])) {
    reportN = as<double>(clientData["n"]);
  }
  for (int i = 0; i < k; ++i) {
    // composite penalty weight ˜η_{r,k}
    double alpha = etaS * etaC * (double)r * (double)k + etaC * (double)i;
    // 1) mirror‐prox (primal retrieval via soft‐threshold)
    Eigen::VectorXd w = proxL1_E(z, alpha * lambda);

    // 2) compute logistic gradient: g = (Xᵀ(σ(Xw) – y))/n
    Eigen::VectorXd lin = xMatrix * w; // size‐n
    Eigen::ArrayXd sig = stable_sigmoid_array_da(lin);
    Eigen::VectorXd g = xMatrix.transpose() * (sig - yLabels.array()).matrix();
    g /= double(n);

    // 3) dual‐averaging step
    z -= etaC * g;
  }
  Eigen::VectorXd delta = z - z0;
  return List::create(_["delta"] = delta,
                      _["n"] = reportN);
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
  std::string aggregation = "sampleSize";
  if (config.containsElementNamed("aggregation") &&
      !Rf_isNull(config["aggregation"])) {
    aggregation = as<std::string>(config["aggregation"]);
  }

  int M = clientReports.size();
  Eigen::VectorXd avg = Eigen::VectorXd::Zero(z.size());
  if (aggregation == "equalClient") {
    // 1) average the list of Δz
    for (int i = 0; i < M; ++i) {
      const List &cliRep = clientReports[i];
      avg += as<Eigen::VectorXd>(cliRep["delta"]);
    }
    avg /= double(M);
  } else if (aggregation == "sampleSize") {
    double totalN = 0.0;
    std::vector<double> ns(M);
    for (int i = 0; i < M; ++i) {
      const List &cliRep = clientReports[i];
      if (!cliRep.containsElementNamed("n") || Rf_isNull(cliRep["n"])) {
        stop("sampleSize aggregation requires each client report to include positive finite n");
      }
      double ni = as<double>(cliRep["n"]);
      if (!R_finite(ni) || ni <= 0.0) {
        stop("sampleSize aggregation requires each client report to include positive finite n");
      }
      ns[i] = ni;
      totalN += ni;
    }
    for (int i = 0; i < M; ++i) {
      const List &cliRep = clientReports[i];
      avg += as<Eigen::VectorXd>(cliRep["delta"]) * (ns[i] / totalN);
    }
  } else {
    stop("aggregation must be 'sampleSize' or 'equalClient'");
  }

  // 2) server dual update

  Eigen::VectorXd zNew = z + etaS * avg;

  // 3) optional primal retrieval
  double alpha = etaS * etaC * (double)(r + 1) * k;
  Eigen::VectorXd wNew = proxL1_E(zNew, alpha * lambda);

  List state = List::create(_["z"] = zNew);
  List report = List::create(_["w"] = wNew,
                             _["z"] = zNew);
  
  return List::create(_["state"] = state, 
                      _["report"] = report);
}
