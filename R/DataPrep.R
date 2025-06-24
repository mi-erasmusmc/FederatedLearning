#' Load one client's PLP data and build feature/label matrices
#' @param path   path to a client's folder
#' @param popSettings  populationSettings for PatientLevelPrediction
#' @return plpData object with population
#' @export
loadClientData <- function(path, popSettings) {
  plpData <- PatientLevelPrediction::loadPlpData(path)
  population <- PatientLevelPrediction::createStudyPopulation(
    populationSettings = popSettings,
    plpData = plpData
  )
  plpData$population <- population
  plpData$covariateData$covariates <- dplyr::mutate(
    plpData$covariateData$covariates,
    covariateValue = ifelse(
      .data$covariateId == 1002,
      .data$covariateValue / 100,
      .data$covariateValue
    )
  )
  plpData
}

#' Get the covariateRef data.frame from a plpData object
#' @param plpData a plpData object
#' @return covariateRef data.frame with covarateId, covariateName, etc.
#' @export
getClientFeatures <- function(plpData) {
  covariateRef <- plpData$covariateData$covariateRef |> dplyr::collect()
  covariateRef
}

#' Create a global covariateId → columnId mapping
#' @param covRefList a list of data.frames, each with a column `covariateId`
#' @param type       "union" or "intersection"
#' @return data.frame with covariateId and columnId (1..P_global)
#' @export
createGlobalMap <- function(covRefList,
                            type = c("union", "intersection")) {
  type <- match.arg(type)
  idLists <- lapply(covRefList, `[[`, "covariateId")
  if (type == "union") {
    allIds <- sort(unique(unlist(idLists)))
  } else {
    allIds <- sort(Reduce(intersect, idLists))
  }
  data.frame(
    covariateId = allIds,
    columnId = seq_along(allIds)
  )
}

#' @export
createClientMatrix <- function(plpData, config) {
  sp <- PatientLevelPrediction::toSparseM(plpData,
    cohort = plpData$population,
    map = config$mapping
  )
  xMatrix <- sp$dataMatrix
  if (config$intercept) {
    xMatrix <- cbind(
      Matrix::Matrix(1, nrow(xMatrix), sparse = TRUE),
      xMatrix
    )
  }
  yLabels <- as.integer(plpData$population$outcomeCount)
  n <- nrow(xMatrix)
  xMeans <- Matrix::colMeans(xMatrix)
  x2Means <- xMeans^2
  if (config$intercept) {
    xMeans[1] <- 0
    x2Means[1] <- 0
  }

  list(
    xMatrix = xMatrix,
    xMeans = xMeans,
    x2Means = x2Means,
    yLabels = yLabels,
    n = n
  )
}
