.serverInitODAL <- function(config) {
  p <- config[["p"]] + 1L
  list(
    phase = 0L,
    p = p,
    betaBar = rep(0, p),
    leadIndex = NA_integer_,
    otherGrad = NULL,
    otherHess = NULL,
    w = rep(0, p)
  )
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
    beta <- if (identical(.odalInitMode(config), "ridgeFallback")) {
      tryCatch(
        .fitLocalLogistic(xDesign, y),
        error = function(e) .fitLocalLogisticRidge(xRaw, xDesign, y, config, e)
      )
    } else {
      .fitLocalLogistic(xDesign, y)
    }
    return(list(bhat = beta, n = clientData$n))
  }

  if (phase == 1L) {
    betaBar <- serverBroadcast$betaBar
    return(list(
      grad = .logisticNegGradient(betaBar, xDesign, y),
      Hess = .logisticNegHessian(betaBar, xDesign),
      n = clientData$n
    ))
  }

  if (phase == 2L) {
    localId <- getOption("FederatedLearning.localId", NA_integer_)
    if (!isTRUE(localId == serverBroadcast$leadIndex)) {
      return(NULL)
    }
    betaBar <- serverBroadcast$betaBar
    otherGrad <- serverBroadcast$otherGrad
    otherHess <- serverBroadcast$otherHess
    localGradBar <- .logisticNegGradient(betaBar, xDesign, y)
    localHessBar <- .logisticNegHessian(betaBar, xDesign)
    objective <- function(beta) {
      delta <- beta - betaBar
      val <- .negLogLikMean(beta, xDesign, y) +
        sum((otherGrad - localGradBar) * beta) +
        as.numeric(t(delta) %*% (otherHess - localHessBar) %*% delta / 2)
      if (is.finite(val)) val else .Machine$double.xmax / 1e100
    }
    fit <- stats::optim(
      par = betaBar,
      fn = objective,
      method = config$optimMethod %||% "BFGS",
      control = list(maxit = config$optimMaxit %||% 1000L)
    )
    return(list(w = fit$par, convergence = fit$convergence, value = fit$value))
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
    leadIndex <- config$leadIndex %||% which.max(ns)
    state <- serverState
    state$phase <- 1L
    state$betaBar <- betaBar
    state$leadIndex <- leadIndex
    state$w <- betaBar
    state$totalN <- sum(ns)
    return(list(
      state = state,
      report = list(
        w = betaBar,
        leadIndex = leadIndex,
        skipConvergence = TRUE,
        communicationNumbers = length(betaBar) * length(clientReports)
      )
    ))
  }

  if (phase == 1L) {
    ns <- vapply(clientReports, `[[`, numeric(1), "n")
    weights <- ns / sum(ns)
    grads <- do.call(cbind, lapply(clientReports, `[[`, "grad"))
    hessList <- lapply(clientReports, `[[`, "Hess")
    globalGrad <- as.numeric(grads %*% weights)
    globalHess <- Reduce(`+`, Map(function(H, w) H * w, hessList, weights))
    state <- serverState
    state$phase <- 2L
    state$otherGrad <- globalGrad
    state$otherHess <- globalHess
    hDiag <- diag(globalHess)
    hCond <- tryCatch(kappa(globalHess), error = function(e) NA_real_)
    state$hessianDim <- paste(dim(globalHess), collapse = "x")
    state$hessianDiagMin <- min(hDiag, na.rm = TRUE)
    state$hessianDiagMax <- max(hDiag, na.rm = TRUE)
    state$hessianCondition <- hCond
    return(list(
      state = state,
      report = list(
        w = state$w,
        leadIndex = state$leadIndex,
        skipConvergence = TRUE,
        hessianDim = state$hessianDim,
        hessianDiagMin = state$hessianDiagMin,
        hessianDiagMax = state$hessianDiagMax,
        hessianCondition = state$hessianCondition,
        communicationNumbers = length(globalGrad) * length(clientReports) +
          length(globalHess) * length(clientReports)
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
          convergence = leadReport$convergence,
          objective = leadReport$value,
          hessianDim = state$hessianDim %||% NA_character_,
          hessianDiagMin = state$hessianDiagMin %||% NA_real_,
          hessianDiagMax = state$hessianDiagMax %||% NA_real_,
          hessianCondition = state$hessianCondition %||% NA_real_,
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
  serverInit = .serverInitODAL,
  clientInit = NULL,
  clientUpdate = .clientUpdateODAL,
  serverRound = .serverRoundODAL,
  lambdaStrategy = .lambdaStrategyODAL()
)
