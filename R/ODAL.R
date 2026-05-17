.odalVariant <- function(config) {
  variant <- config$odalVariant %||% "second"
  if (identical(variant, "ODAL1")) {
    variant <- "first"
  }
  if (identical(variant, "ODAL2")) {
    variant <- "second"
  }
  if (!variant %in% c("first", "second")) {
    stop("config$odalVariant must be one of: first, second, ODAL1, ODAL2")
  }
  variant
}

.odalCorrectionDiagnostics <- function(globalHess, leadHess, tau = 1e-10) {
  correction <- globalHess - leadHess
  eig <- tryCatch(
    eigen((correction + t(correction)) / 2, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NA_real_
  )
  eigFinite <- is.finite(eig)
  diagVals <- diag(correction)
  epsilon <- .adapEpsilonEig(globalHess, tau = tau)
  negative <- if (any(eigFinite)) sum(eig[eigFinite] < -epsilon) else NA_integer_
  status <- if (is.na(negative)) {
    "unknown"
  } else if (negative > 0L) {
    "indefinite"
  } else {
    "psd"
  }
  list(
    correction = correction,
    curvatureStatus = status,
    correctionEigenMin = if (any(eigFinite)) min(eig[eigFinite]) else NA_real_,
    correctionEigenMax = if (any(eigFinite)) max(eig[eigFinite]) else NA_real_,
    correctionEigenNegative = negative,
    correctionDiagMin = suppressWarnings(min(diagVals, na.rm = TRUE)),
    correctionDiagMax = suppressWarnings(max(diagVals, na.rm = TRUE)),
    correctionDiagNegative = sum(is.finite(diagVals) & diagVals < -epsilon),
    correctionEpsilon = epsilon
  )
}

.serverInitODAL <- function(config) {
  p <- config[["p"]] + 1L
  list(
    phase = 0L,
    p = p,
    odalVariant = .odalVariant(config),
    betaBar = rep(0, p),
    betaBarMaxAbs = NA_real_,
    betaBarL2 = NA_real_,
    leadIndex = NA_integer_,
    odalInitDiagnostics = NULL,
    otherGrad = NULL,
    otherHess = NULL,
    curvatureStatus = NA_character_,
    correctionEigenMin = NA_real_,
    correctionEigenMax = NA_real_,
    correctionEigenNegative = NA_integer_,
    correctionDiagMin = NA_real_,
    correctionDiagMax = NA_real_,
    correctionDiagNegative = NA_integer_,
    correctionEpsilon = NA_real_,
    globalHessianEigenMin = NA_real_,
    leadHessianEigenMin = NA_real_,
    siteHessianEigenMin = NA_real_,
    siteHessianNegative = NA_integer_,
    w = rep(0, p)
  )
}

.serverInitODAL1 <- function(config) {
  config$odalVariant <- "first"
  .serverInitODAL(config)
}

.serverInitODAL2 <- function(config) {
  config$odalVariant <- "second"
  .serverInitODAL(config)
}

.fitLocalLogistic <- function(xRaw, y) {
  xDf <- as.data.frame(as.matrix(xRaw))
  names(xDf) <- paste0("x", seq_len(ncol(xDf)))
  dat <- cbind(status = y, xDf)
  fit <- stats::glm(status ~ 0 + ., data = dat, family = stats::binomial())
  beta <- as.numeric(stats::coef(fit))
  if (length(beta) != ncol(xRaw) || any(!is.finite(beta))) {
    stop("glm returned non-finite or incorrectly sized coefficients")
  }
  beta
}

.fitLocalLogisticRidge <- function(xRaw, xDesign, y, config, reason) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("ODAL local logistic fit failed and glmnet is unavailable: ", conditionMessage(reason))
  }
  fit <- glmnet::glmnet(
    x = xRaw,
    y = y,
    family = "binomial",
    alpha = 0,
    lambda = config$odalRidgeLambda %||% 1e-8,
    intercept = TRUE,
    standardize = config$standardize %||% TRUE
  )
  beta <- as.numeric(stats::coef(fit, s = config$odalRidgeLambda %||% 1e-8))
  if (length(beta) != ncol(xDesign) || any(!is.finite(beta))) {
    stop("ODAL ridge fallback produced non-finite or incorrectly sized coefficients")
  }
  beta
}

.captureWarnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
    }
  )
  list(value = value, warnings = unique(warnings))
}

.fitLocalLogisticDiagnosed <- function(xRaw, xDesign, y, config, allowRidgeFallback) {
  glmWarnings <- character()
  glmError <- NULL
  beta <- tryCatch(
    {
      out <- .captureWarnings(.fitLocalLogistic(xDesign, y))
      glmWarnings <- out$warnings
      out$value
    },
    error = function(e) {
      glmError <<- e
      NULL
    }
  )

  initMethod <- "glm"
  ridgeWarnings <- character()
  fallbackReason <- NA_character_
  if (is.null(beta)) {
    if (!isTRUE(allowRidgeFallback)) {
      stop(glmError)
    }
    initMethod <- "ridgeFallback"
    fallbackReason <- conditionMessage(glmError)
    out <- .captureWarnings(.fitLocalLogisticRidge(xRaw, xDesign, y, config, glmError))
    beta <- out$value
    ridgeWarnings <- out$warnings
  }

  list(
    beta = beta,
    diagnostics = list(
      localId = getOption("FederatedLearning.localId", NA_integer_),
      initMethod = initMethod,
      usedRidgeFallback = identical(initMethod, "ridgeFallback"),
      fallbackReason = fallbackReason,
      glmWarnings = paste(glmWarnings, collapse = " | "),
      ridgeWarnings = paste(ridgeWarnings, collapse = " | "),
      n = length(y),
      outcomes = sum(y),
      p = length(beta),
      betaMaxAbs = max(abs(beta), na.rm = TRUE),
      betaL2 = sqrt(sum(beta^2)),
      betaNonFinite = sum(!is.finite(beta)),
      betaNonZero = sum(abs(beta) > 1e-8, na.rm = TRUE)
    )
  )
}

.odalInitMode <- function(config) {
  if (isTRUE(config$odalRidgeFallback)) {
    return("ridgeFallback")
  }
  mode <- config$odalInit %||% "pda"
  if (!identical(mode, "pda") && !identical(mode, "ridgeFallback")) {
    stop("config$odalInit must be one of: pda, ridgeFallback")
  }
  mode
}

.clientUpdateODAL <- function(clientData, serverBroadcast, config) {
  phase <- serverBroadcast$phase %||% 0L
  xRaw <- .stripInterceptColumn(clientData$xMatrix, config)
  xDesign <- .addInterceptColumn(xRaw)
  y <- clientData$yLabels

  if (phase == 0L) {
    init <- .fitLocalLogisticDiagnosed(
      xRaw = xRaw,
      xDesign = xDesign,
      y = y,
      config = config,
      allowRidgeFallback = identical(.odalInitMode(config), "ridgeFallback")
    )
    return(list(bhat = init$beta, n = clientData$n, odalInit = init$diagnostics))
  }

  if (phase == 1L) {
    betaBar <- serverBroadcast$betaBar
    out <- list(
      grad = .logisticNegGradient(betaBar, xDesign, y),
      n = clientData$n
    )
    if (identical(serverBroadcast$odalVariant %||% .odalVariant(config), "second")) {
      out$Hess <- .logisticNegHessian(betaBar, xDesign)
    }
    return(out)
  }

  if (phase == 2L) {
    localId <- getOption("FederatedLearning.localId", NA_integer_)
    if (!isTRUE(localId == serverBroadcast$leadIndex)) {
      return(NULL)
    }
    betaBar <- serverBroadcast$betaBar
    otherGrad <- serverBroadcast$otherGrad
    otherHess <- serverBroadcast$otherHess
    odalVariant <- serverBroadcast$odalVariant %||% .odalVariant(config)
    localGradBar <- .logisticNegGradient(betaBar, xDesign, y)
    localHessBar <- if (identical(odalVariant, "second")) {
      .logisticNegHessian(betaBar, xDesign)
    } else {
      NULL
    }
    objective <- function(beta) {
      delta <- beta - betaBar
      val <- .negLogLikMean(beta, xDesign, y) +
        sum((otherGrad - localGradBar) * beta)
      if (identical(odalVariant, "second")) {
        val <- val + as.numeric(t(delta) %*% (otherHess - localHessBar) %*% delta / 2)
      }
      if (is.finite(val)) val else .Machine$double.xmax / 1e100
    }
    fit <- stats::optim(
      par = betaBar,
      fn = objective,
      method = config$optimMethod %||% "BFGS",
      control = list(maxit = config$optimMaxit %||% 1000L)
    )
    return(list(
      w = fit$par,
      convergence = fit$convergence,
      value = fit$value,
      optimMessage = fit$message %||% NA_character_
    ))
  }

  list()
}

.serverRoundODAL <- function(serverState, clientReports, config) {
  phase <- serverState$phase %||% 0L
  if (phase == 0L) {
    bhats <- lapply(clientReports, `[[`, "bhat")
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    bmat <- do.call(cbind, bhats)
    weights <- ns / sum(ns)
    betaBar <- as.numeric(bmat %*% weights)
    initDiagnostics <- do.call(rbind, lapply(seq_along(clientReports), function(i) {
      d <- clientReports[[i]]$odalInit %||% list()
      data.frame(
        client = i,
        localId = d$localId %||% i,
        n = d$n %||% ns[[i]],
        outcomes = d$outcomes %||% NA_real_,
        p = d$p %||% length(bhats[[i]]),
        initMethod = d$initMethod %||% NA_character_,
        usedRidgeFallback = d$usedRidgeFallback %||% NA,
        fallbackReason = d$fallbackReason %||% NA_character_,
        glmWarnings = d$glmWarnings %||% NA_character_,
        ridgeWarnings = d$ridgeWarnings %||% NA_character_,
        betaMaxAbs = d$betaMaxAbs %||% max(abs(bhats[[i]]), na.rm = TRUE),
        betaL2 = d$betaL2 %||% sqrt(sum(bhats[[i]]^2)),
        betaNonFinite = d$betaNonFinite %||% sum(!is.finite(bhats[[i]])),
        betaNonZero = d$betaNonZero %||% sum(abs(bhats[[i]]) > 1e-8, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    leadIndex <- config$leadIndex %||% which.max(ns)
    state <- serverState
    state$phase <- 1L
    state$betaBar <- betaBar
    state$betaBarMaxAbs <- max(abs(betaBar), na.rm = TRUE)
    state$betaBarL2 <- sqrt(sum(betaBar^2))
    state$odalInitDiagnostics <- initDiagnostics
    state$leadIndex <- leadIndex
    state$w <- betaBar
    state$totalN <- sum(ns)
    return(list(
      state = state,
      report = list(
        w = betaBar,
        leadIndex = leadIndex,
        odalVariant = state$odalVariant,
        betaBarMaxAbs = state$betaBarMaxAbs,
        betaBarL2 = state$betaBarL2,
        odalInitDiagnostics = initDiagnostics,
        skipConvergence = TRUE,
        communicationNumbers = length(betaBar) * length(clientReports)
      )
    ))
  }

  if (phase == 1L) {
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    weights <- ns / sum(ns)
    grads <- do.call(cbind, lapply(clientReports, `[[`, "grad"))
    globalGrad <- as.numeric(grads %*% weights)
    state <- serverState
    state$phase <- 2L
    state$otherGrad <- globalGrad
    communicationNumbers <- length(globalGrad) * length(clientReports)
    if (identical(state$odalVariant, "second")) {
      hessList <- lapply(clientReports, `[[`, "Hess")
      globalHess <- Reduce(`+`, Map(function(H, w) H * w, hessList, weights))
      leadHess <- hessList[[state$leadIndex]]
      siteEigenMins <- vapply(hessList, function(H) .adapEigenRange(H)$min, numeric(1))
      globalEig <- .adapEigenRange(globalHess)
      correctionDiagnostics <- .odalCorrectionDiagnostics(
        globalHess,
        leadHess,
        tau = config$odalCurvatureTau %||% 1e-10
      )
      if (identical(config$odalCurvatureAction %||% "report", "fail") &&
          identical(correctionDiagnostics$curvatureStatus, "indefinite")) {
        stop("ODAL2 second-order surrogate has indefinite Hessian correction", call. = FALSE)
      }
      state$otherHess <- globalHess
      state$curvatureStatus <- correctionDiagnostics$curvatureStatus
      state$correctionEigenMin <- correctionDiagnostics$correctionEigenMin
      state$correctionEigenMax <- correctionDiagnostics$correctionEigenMax
      state$correctionEigenNegative <- correctionDiagnostics$correctionEigenNegative
      state$correctionDiagMin <- correctionDiagnostics$correctionDiagMin
      state$correctionDiagMax <- correctionDiagnostics$correctionDiagMax
      state$correctionDiagNegative <- correctionDiagnostics$correctionDiagNegative
      state$correctionEpsilon <- correctionDiagnostics$correctionEpsilon
      state$globalHessianEigenMin <- globalEig$min
      state$leadHessianEigenMin <- siteEigenMins[[state$leadIndex]]
      state$siteHessianEigenMin <- min(siteEigenMins, na.rm = TRUE)
      state$siteHessianNegative <- sum(is.finite(siteEigenMins) & siteEigenMins < -correctionDiagnostics$correctionEpsilon)
      hDiag <- diag(globalHess)
      hCond <- tryCatch(kappa(globalHess), error = function(e) NA_real_)
      state$hessianDim <- paste(dim(globalHess), collapse = "x")
      state$hessianDiagMin <- min(hDiag, na.rm = TRUE)
      state$hessianDiagMax <- max(hDiag, na.rm = TRUE)
      state$hessianCondition <- hCond
      communicationNumbers <- communicationNumbers + length(globalHess) * length(clientReports)
    } else {
      state$otherHess <- NULL
      state$curvatureStatus <- "first_order"
      state$hessianDim <- NA_character_
      state$hessianDiagMin <- NA_real_
      state$hessianDiagMax <- NA_real_
      state$hessianCondition <- NA_real_
    }
    return(list(
      state = state,
      report = list(
        w = state$w,
        leadIndex = state$leadIndex,
        odalVariant = state$odalVariant,
        skipConvergence = TRUE,
        hessianDim = state$hessianDim,
        hessianDiagMin = state$hessianDiagMin,
        hessianDiagMax = state$hessianDiagMax,
        hessianCondition = state$hessianCondition,
        curvatureStatus = state$curvatureStatus,
        correctionEigenMin = state$correctionEigenMin,
        correctionEigenMax = state$correctionEigenMax,
        correctionEigenNegative = state$correctionEigenNegative,
        correctionDiagMin = state$correctionDiagMin,
        correctionDiagMax = state$correctionDiagMax,
        correctionDiagNegative = state$correctionDiagNegative,
        correctionEpsilon = state$correctionEpsilon,
        globalHessianEigenMin = state$globalHessianEigenMin,
        leadHessianEigenMin = state$leadHessianEigenMin,
        siteHessianEigenMin = state$siteHessianEigenMin,
        siteHessianNegative = state$siteHessianNegative,
        communicationNumbers = communicationNumbers
      )
    ))
  }

  if (phase == 2L) {
    leadReport <- clientReports[[serverState$leadIndex]]
    if (!is.null(leadReport$w)) {
      state <- serverState
      state$phase <- 3L
      state$w <- leadReport$w
      return(list(
        state = state,
        report = list(
          w = leadReport$w,
          done = TRUE,
          leadIndex = state$leadIndex,
          odalVariant = state$odalVariant,
          betaBarMaxAbs = state$betaBarMaxAbs %||% NA_real_,
          betaBarL2 = state$betaBarL2 %||% NA_real_,
          odalInitDiagnostics = state$odalInitDiagnostics,
          convergence = leadReport$convergence,
          optimConvergence = leadReport$convergence,
          optimMessage = leadReport$optimMessage %||% NA_character_,
          objective = leadReport$value,
          hessianDim = state$hessianDim %||% NA_character_,
          hessianDiagMin = state$hessianDiagMin %||% NA_real_,
          hessianDiagMax = state$hessianDiagMax %||% NA_real_,
          hessianCondition = state$hessianCondition %||% NA_real_,
          curvatureStatus = state$curvatureStatus %||% NA_character_,
          correctionEigenMin = state$correctionEigenMin %||% NA_real_,
          correctionEigenMax = state$correctionEigenMax %||% NA_real_,
          correctionEigenNegative = state$correctionEigenNegative %||% NA_integer_,
          correctionDiagMin = state$correctionDiagMin %||% NA_real_,
          correctionDiagMax = state$correctionDiagMax %||% NA_real_,
          correctionDiagNegative = state$correctionDiagNegative %||% NA_integer_,
          correctionEpsilon = state$correctionEpsilon %||% NA_real_,
          globalHessianEigenMin = state$globalHessianEigenMin %||% NA_real_,
          leadHessianEigenMin = state$leadHessianEigenMin %||% NA_real_,
          siteHessianEigenMin = state$siteHessianEigenMin %||% NA_real_,
          siteHessianNegative = state$siteHessianNegative %||% NA_integer_,
          communicationNumbers = length(leadReport$w)
        )
      ))
    }
  }

  list(state = serverState, report = list(w = serverState$w))
}

.lambdaStrategyODAL <- function() {
  list(
    seed = NULL,
    initial = function(lambda, totalPopSize, context) lambda,
    final = function(lambda, totalPopSize, context) lambda
  )
}

.registerAlgorithm(
  "ODAL",
  serverInit = .serverInitODAL2,
  clientInit = NULL,
  clientUpdate = .clientUpdateODAL,
  serverRound = .serverRoundODAL,
  lambdaStrategy = .lambdaStrategyODAL()
)

.registerAlgorithm(
  "ODAL1",
  serverInit = .serverInitODAL1,
  clientInit = NULL,
  clientUpdate = .clientUpdateODAL,
  serverRound = .serverRoundODAL,
  lambdaStrategy = .lambdaStrategyODAL()
)

.registerAlgorithm(
  "ODAL2",
  serverInit = .serverInitODAL2,
  clientInit = NULL,
  clientUpdate = .clientUpdateODAL,
  serverRound = .serverRoundODAL,
  lambdaStrategy = .lambdaStrategyODAL()
)
