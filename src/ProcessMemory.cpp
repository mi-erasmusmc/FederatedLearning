// Peak resident memory of the current process. Kept apart from the Rcpp
// sources because windows.h clashes with R's headers.
#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#else
#include <sys/resource.h>
#endif

double processPeakMemoryMb() {
#ifdef _WIN32
  PROCESS_MEMORY_COUNTERS counters;
  if (!GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters))) return -1.0;
  return counters.PeakWorkingSetSize / 1048576.0;
#else
  struct rusage usage;
  if (getrusage(RUSAGE_SELF, &usage) != 0) return -1.0;
#ifdef __APPLE__
  return usage.ru_maxrss / 1048576.0;
#else
  return usage.ru_maxrss / 1024.0;
#endif
#endif
}
