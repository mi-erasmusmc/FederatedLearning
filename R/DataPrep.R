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

#' Create a global covariateId to columnId mapping
#' @param covRefList a list of data.frames, each with a column `covariateId`
#' @param type       "union" or "intersection"
#' @param featureSet optional named feature set: "all", "ageSex", "phenotypes",
#'   or "ageSexPhenotypes"
#' @param covariateIds optional explicit covariate ids to retain
#' @param analysisIds optional explicit analysis ids to retain
#' @return data.frame with covariateId and columnId (1..P_global)
#' @export
createGlobalMap <- function(covRefList,
                            type = c("union", "intersection"),
                            featureSet = NULL,
                            covariateIds = NULL,
                            analysisIds = NULL) {
  type <- match.arg(type)
  covRefList <- lapply(covRefList, filterCovariateRef,
    featureSet = featureSet,
    covariateIds = covariateIds,
    analysisIds = analysisIds
  )
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

#' Filter covariate references for a named experiment feature set
#' @param covariateRef covariate reference data.frame
#' @param featureSet one of "all", "ageSex", "phenotypes", "ageSexPhenotypes"
#' @param covariateIds optional explicit covariate ids to retain
#' @param analysisIds optional explicit analysis ids to retain
#' @return filtered covariateRef data.frame
#' @export
filterCovariateRef <- function(covariateRef,
                               featureSet = NULL,
                               covariateIds = NULL,
                               analysisIds = NULL) {
  if (is.null(covariateRef) || nrow(covariateRef) == 0) {
    return(covariateRef)
  }
  featureSet <- featureSet %||% "all"
  keep <- rep(TRUE, nrow(covariateRef))
  ageSexIds <- c(1002, 8532001)

  if (!identical(featureSet, "all")) {
    if (identical(featureSet, "ageSex")) {
      keep <- covariateRef$covariateId %in% ageSexIds
    } else if (identical(featureSet, "phenotypes")) {
      keep <- covariateRef$analysisId %in% 49
    } else if (identical(featureSet, "ageSexPhenotypes")) {
      keep <- covariateRef$covariateId %in% ageSexIds |
        covariateRef$analysisId %in% 49
    } else {
      stop("Unknown featureSet: ", featureSet)
    }
  }
  if (!is.null(covariateIds)) {
    keep <- keep & covariateRef$covariateId %in% covariateIds
  }
  if (!is.null(analysisIds)) {
    keep <- keep & covariateRef$analysisId %in% analysisIds
  }
  covariateRef[keep, , drop = FALSE]
}

.clientMatrixMoments <- function(xMatrix, intercept = FALSE) {
  xMeans <- Matrix::colMeans(xMatrix)
  x2Means <- Matrix::colMeans(xMatrix^2)
  if (intercept) {
    xMeans[1] <- 0
    x2Means[1] <- 0
  }
  list(xMeans = xMeans, x2Means = x2Means)
}

.clientMatrixVariance <- function(xMeans, x2Means) {
  pmax(x2Means - xMeans^2, 0)
}

.diagnosticDownsampleRows <- function(yLabels, controlsPerCase = Inf, seed = 42L) {
  if (!is.finite(controlsPerCase)) {
    return(NULL)
  }
  controlsPerCase <- as.numeric(controlsPerCase)
  if (length(controlsPerCase) != 1L || is.na(controlsPerCase) || controlsPerCase < 0) {
    stop("config$diagnosticControlsPerCase must be a non-negative finite value")
  }
  cases <- which(yLabels == 1L)
  controls <- which(yLabels == 0L)
  if (length(cases) == 0L || length(controls) == 0L) {
    return(seq_along(yLabels))
  }

  nControls <- min(length(controls), ceiling(controlsPerCase * length(cases)))
  if (nControls == length(controls)) {
    return(seq_along(yLabels))
  }

  oldSeed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (is.null(oldSeed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", oldSeed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  sort(c(cases, sample(controls, nControls)))
}

#' Create a local sparse design matrix from PLP data and a global map
#' @param plpData a PLP data object with a populated `population`
#' @param config list containing `mapping` and `intercept`
#' @return list with `xMatrix`, summary vectors, labels, and row count
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
  rows <- .diagnosticDownsampleRows(
    yLabels,
    controlsPerCase = config$diagnosticControlsPerCase %||% Inf,
    seed = config$diagnosticDownsampleSeed %||% 42L
  )
  if (!is.null(rows)) {
    xMatrix <- xMatrix[rows, , drop = FALSE]
    yLabels <- yLabels[rows]
  }
  n <- nrow(xMatrix)
  moments <- .clientMatrixMoments(xMatrix, intercept = isTRUE(config$intercept))

  list(
    xMatrix = xMatrix,
    xMeans = moments$xMeans,
    x2Means = moments$x2Means,
    yLabels = yLabels,
    n = n
  )
}
