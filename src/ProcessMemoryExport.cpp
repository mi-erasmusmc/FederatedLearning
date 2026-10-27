#include <Rcpp.h>

double processPeakMemoryMb();

// Peak resident memory (working set on Windows) of this R process so far, in MB;
// NA when the operating system does not report it.
// [[Rcpp::export]]
double peakMemoryMbCpp() {
  double mb = processPeakMemoryMb();
  return mb < 0 ? NA_REAL : mb;
}
