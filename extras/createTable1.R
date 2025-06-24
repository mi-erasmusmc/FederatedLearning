library(PatientLevelPrediction)
rootDataDir <- "data" # adjust if your script is called from another wd
getPlpSummary <- function(dataset, clientPath) {
  plpData <- PatientLevelPrediction::loadPlpData(clientPath)
  data.frame(
    dataset = dataset,
    client = basename(clientPath),
    nSubjects = nrow(plpData$cohorts),
    Features = I(list(plpData$covariateData$covariates |> 
      dplyr::pull(.data$covariateId) |>
      unique())),
    nOutcomes = nrow(plpData$outcomes),
    stringsAsFactors = FALSE
  )
}
# Scan every dataset folder (dementia, dementiaPhenotypes, lungCancer, lungCancerPhenotypes, readmission, readmissionPhenotypes)
datasetDirs <- list.dirs(rootDataDir, full.names = TRUE, recursive = FALSE)
table1 <- do.call(rbind, lapply(datasetDirs, function(ds) {
  clientDirs <- list.dirs(ds, full.names = TRUE, recursive = FALSE)
  # build one row per client
  do.call(rbind, lapply(clientDirs, function(cl) getPlpSummary(basename(ds), cl)))
}))
# take intersectoion of table1$features for each unique dataset
grouped_table1 <- table1 |> 
  dplyr::group_by(.data$dataset) |> 
  dplyr::mutate(nFeatures = length(unique(unlist(.data$Features))),
                nSubjects = sum(.data$nSubjects),
                nOutcomes = sum(.data$nOutcomes)) |> 
  dplyr::select(-"Features") |> 
  dplyr::distinct()

table1_by_dataset <- aggregate(. ~ dataset,
  data = table1[, c(
    "dataset", "nSubjects", "nFeatures",
    "nOutcomes"
  )],
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)

