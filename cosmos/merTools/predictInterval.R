

safeDeparse = function (x, collapse = " ") 
  paste(deparse(x, 500L), collapse = collapse)

reOnly <- function (f, response = FALSE) 
{
  response <- if (response && length(f) == 3) 
    f[[2]]
  else NULL
  reformulate(paste0("(", vapply(findbars(f), safeDeparse, 
                                 ""), ")"), response = response)
}

RHSForm <- function (form, as.form = FALSE) 
{
  rhsf <- form[[length(form)]]
  if (as.form) 
    reformulate(deparse(rhsf))
  else rhsf
}


buildModelMatrix <- function (model, newdata, which = "full") 
{
    X <- getME(model, "X")
    X.col.dropped <- attr(X, "col.dropped")
    if (is.null(newdata)) {
        newdata <- model@frame
    }
    RHS <- formula(substitute(~R, list(R = RHSForm(formula(model, 
        fixed.only = TRUE)))))
    Terms <- terms(model, fixed.only = TRUE)
    mf <- model.frame(model, fixed.only = FALSE)
    isFac <- vapply(mf, is.factor, FUN.VALUE = TRUE)
    isFac[attr(Terms, "response")] <- FALSE
    orig_levs <- if (length(isFac) == 0) 
        NULL
    else lapply(mf[isFac], levels)
    mfnew <- suppressWarnings(model.frame(delete.response(Terms), 
        newdata, na.action = "na.pass", xlev = orig_levs))
    X <- model.matrix(RHS, data = mfnew, contrasts.arg = attr(X, 
        "contrasts"))
    offset <- 0
    tt <- terms(model)
    if (!is.null(off.num <- attr(tt, "offset"))) {
        for (i in off.num) offset <- offset + eval(attr(tt, "variables")[[i + 
            1]], newdata)
    }
    fit.na.action <- attr(mfnew, "na.action")
    if (is.numeric(X.col.dropped) && length(X.col.dropped) > 
        0) {
        X <- X[, -X.col.dropped, drop = FALSE]
    }
    re.form <- reOnly(formula(model))
    newRE <- mkNewReTrms(object = model, newdata = newdata, re.form, 
        na.action = "na.pass", allow.new.levels = TRUE)
    reMat <- Matrix::t(newRE$Zt)
    colnames(reMat) <- rownames(newRE$Zt)
    mm <- cbind(X, reMat)
    if (which == "full") {
        return(mm)
    }
    else if (which == "fixed") {
        return(X)
    }
    else if (which == "random") {
        return(reMat)
    }
}














predictInterval <- function (merMod, newdata, which = c("full", "fixed", "random", 
                                     "all"), level = 0.8, n.sims = 1000, stat = c("median", "mean"), 
          type = c("linear.prediction", "probability"), include.resid.var = TRUE, 
          returnSims = FALSE, seed = NULL, .parallel = FALSE, .paropts = NULL, 
          fix.intercept.variance = FALSE, ignore.fixed.terms = NULL) 
{
  if (missing(newdata)) {
    newdata <- merMod@frame
  }
  if (any(c("data.frame") != class(newdata))) {
    if (any(c("tbl_df", "tbl") %in% class(newdata))) {
      newdata <- as.data.frame(newdata)
      warning("newdata is tbl_df or tbl object from dplyr package and has been\n              coerced to a data.frame")
    }
    else {
      newdata <- as.data.frame(newdata)
    }
  }
  predict.type <- match.arg(type, c("linear.prediction", "probability"), 
                            several.ok = FALSE)
  stat.type <- match.arg(stat, c("median", "mean"), several.ok = FALSE)
  which.eff <- match.arg(which, c("full", "fixed", "random", 
                                  "all"), several.ok = FALSE)
  if (!is.null(seed)) 
    set.seed(seed)
  else if (!exists(".Random.seed", envir = .GlobalEnv)) 
    runif(1)
  merMod.devcomp <- getME(merMod, "devcomp")
  if (merMod.devcomp$dims[["GLMM"]] == 0 & merMod.devcomp$dims[["NLMM"]] == 
      0) {
    sigmahat <- sqrt(1/rgamma(n.sims, 0.5 * residDF.merMod(merMod), 
                              0.5 * merMod.devcomp$cmp[["pwrss"]]))
    if (predict.type == "probability") {
      predict.type = "linear.prediction"
      warning("    Asking for predictions on the probability scale makes no sense, resetting predict.type to linear.prediction", 
              call. = FALSE)
    }
  }
  else if (merMod.devcomp$dims[["GLMM"]] == TRUE & merMod@resp$family$family == 
           "binomial" & merMod@resp$family$link %in% c("logit", 
                                                       "probit")) {
    sigmahat <- rep(1, n.sims)
  }
  else {
    warning("   Prediction for NLMMs or GLMMs that are not mixed binomial regressions is not tested. Sigma set at 1.")
    sigmahat <- rep(1, n.sims)
  }
  newdata.modelMatrix <- buildModelMatrix(model = merMod, newdata = newdata)
  rr <- ranef(merMod, condVar = TRUE)
  re.xb <- vector(getME(merMod, "n_rfacs"), mode = "list")
  names(re.xb) <- names(ngrps(merMod))
  for (j in names(re.xb)) {
    reMeans <- as.matrix(rr[[j]])
    reMatrix <- attr(rr[[j]], which = "postVar")
    if (j %in% names(newdata)) {
      obslvl <- unique(as.character(newdata[, j]))
      alllvl <- rownames(reMeans)
      keep <- intersect(obslvl, alllvl)
    }
    else {
      obslvl <- colnames(newdata.modelMatrix)
      alllvl <- rownames(reMeans)
      keep <- intersect(obslvl, alllvl)
    }
    if (length(keep) > 0 & !identical(keep, alllvl)) {
      reMeans <- reMeans[keep, , drop = FALSE]
      dimnames(reMatrix)[[3]] <- alllvl
      reMatrix <- reMatrix[, , keep, drop = FALSE]
    }
    else if (length(keep) > 0 & identical(keep, alllvl)) {
      dimnames(reMatrix)[[3]] <- alllvl
      reMatrix <- reMatrix[, , keep, drop = FALSE]
    }
    else {
      reMeans <- reMeans[1, , drop = FALSE]
      reMatrix <- reMatrix[, , 1, drop = FALSE]
    }
    tmpList <- vector(length = nrow(reMeans), mode = "list")
    for (k in 1:nrow(reMeans)) {
      meanTmp <- reMeans[k, ]
      names(meanTmp) <- NULL
      matrixTmp <- as.matrix(reMatrix[, , k])
      tmpList[[k]] <- as.matrix(mvtnorm::rmvnorm(n = n.sims, 
                                                 mean = meanTmp, sigma = matrixTmp, method = "chol"))
    }
    REcoefs <- sapply(tmpList, identity, simplify = "array")
    dimnames(REcoefs) <- list(1:n.sims, attr(reMeans, "dimnames")[[2]], 
                              attr(reMeans, "dimnames")[[1]])
    if (j %in% names(newdata)) {
      newdata.modelMatrix <- as.matrix(newdata.modelMatrix)
      tmp <- cbind(as.data.frame(newdata.modelMatrix), 
                   var = newdata[, j])
      tmp <- tmp[, !duplicated(colnames(tmp))]
      keep <- names(tmp)[names(tmp) %in% colnames(REcoefs)]
      if (length(keep) == 0) {
        keep <- grep(dimnames(REcoefs)[[2]], names(tmp), 
                     value = TRUE)
      }
      if (length(keep) == 0) {
        tmp <- cbind(model.frame(subbars(formula(merMod)), 
                                 data = newdata), var = newdata[, j])
        keep <- grep(dimnames(REcoefs)[[2]], names(tmp), 
                     value = TRUE)
      }
      if (length(keep) == 0) {
        tmp <- cbind(as.data.frame(newdata.modelMatrix), 
                     var = newdata[, j])
        tmp <- tmp[, !duplicated(colnames(tmp))]
        tmp <- cbind(data.frame(1), tmp)
        names(tmp)[1] <- "(Intercept)"
        keep <- "(Intercept)"
      }
      tmp <- tmp[, c(keep, "var"), drop = FALSE]
      tmp[, "var"] <- as.character(tmp[, "var"])
      colnames(tmp)[which(names(tmp) == "var")] <- names(newdata[, 
                                                                 j, drop = FALSE])
      if (all(grepl(":", keep))) {
        keep <- unique(gsub("(.*):.*", "\\1", keep))
      }
    }
    else {
      if (is(newdata.modelMatrix, "dgCMatrix")) {
        newdata.modelMatrix <- as.matrix(newdata.modelMatrix)
        tmp <- as.data.frame(newdata.modelMatrix)
      }
      else {
        tmp <- as.data.frame(newdata.modelMatrix)
      }
      tmp <- tmp[, !duplicated(colnames(tmp))]
      tmp$var <- names(tmp[keep])[max.col(tmp[keep])]
      keep <- names(tmp)[names(tmp) %in% dimnames(REcoefs)[[2]]]
      tmp <- tmp[, c(keep, "var"), drop = FALSE]
      tmp[, "var"] <- as.character(tmp[, "var"])
      colnames(tmp)[which(names(tmp) == "var")] <- j
    }
    tmp.pred <- function(data, coefs, group) {
      new.levels <- unique(as.character(data[, group])[!as.character(data[, 
                                                                          group]) %in% dimnames(coefs)[[3]]])
      msg <- paste("     The following levels of ", group, 
                   " from newdata \n -- ", paste0(new.levels, collapse = ", "), 
                   " -- are not in the model data. \n     Currently, predictions for these values are based only on the \n fixed coefficients and the observation-level error.", 
                   sep = "")
      if (length(new.levels > 0)) {
        warning(msg, call. = FALSE)
      }
      yhatTmp <- array(data = NA, dim = c(nrow(data), dim(coefs)[1]))
      colIdx <- ncol(data) - 1
      colLL <- length(1:colIdx)
      if (colLL > dim(coefs)[2]) {
        coefs_new <- array(NA, dim = c(dim(coefs)[1], 
                                       colLL, dim(coefs)[3]))
        dimnames(coefs_new)[c(1, 3)] <- dimnames(coefs)[c(1, 
                                                          3)]
        dimnames(coefs_new)[[2]] <- rep(dimnames(coefs)[[2]], 
                                        dim(coefs_new)[2])
        for (k in 1:colLL) {
          coefs_new[, k, 1:dim(coefs)[3]] <- coefs[, 
                                                   1, 1:dim(coefs)[3]]
        }
        coefs <- coefs_new
      }
      for (i in 1:nrow(data)) {
        lvl <- as.character(data[, group][i])
        if (!lvl %in% new.levels) {
          yhatTmp[i, ] <- as.numeric(data[i, 1:colIdx]) %*% 
            t(coefs[, 1:colIdx, lvl])
        }
        else {
          yhatTmp[i, ] <- rep(0, colIdx) %*% t(coefs[, 
                                                     1:colIdx, 1])
        }
      }
      rownames(yhatTmp) <- rownames(data)
      rm(data)
      return(yhatTmp)
    }
    if (nrow(tmp) > 1000 | .parallel) {
      if (requireNamespace("foreach", quietly = TRUE)) {
        if (.parallel) {
          setup_parallel()
        }
        tmp2 <- split(tmp, (1:nrow(tmp)%/%500))
        tmp2 <- tmp2[lapply(tmp2, length) > 0]
        fe_call <- as.call(c(list(quote(foreach::foreach), 
                                  i = seq_along(tmp2), .combine = "rbind")))
        fe <- eval(fe_call)
        re.xb[[j]] <- foreach::`%dopar%`(fe, tmp.pred(data = tmp2[[i]], 
                                                      coefs = REcoefs[, keep, , drop = FALSE], group = j))
        rm(tmp2)
      }
      else {
        warning("foreach package is unavailable, parallel computing not available")
        re.xb[[j]] <- tmp.pred(data = tmp, coefs = REcoefs[, 
                                                           keep, , drop = FALSE], group = j)
      }
    }
    else {
      re.xb[[j]] <- tmp.pred(data = tmp, coefs = REcoefs[, 
                                                         keep, , drop = FALSE], group = j)
    }
    rm(tmp)
  }
  rm(REcoefs)
  if (include.resid.var == FALSE) {
    sigmahat <- rep(1, n.sims)
  }
  fe.tmp <- fixef(merMod)
  vcov.tmp <- as.matrix(vcov(merMod))
  if (is.na(names(attr(VarCorr(merMod)[[j]], "stddev")["(Intercept)"]))) {
    fix.intercept.variance <- FALSE
    message("No intercept detected, setting fix.intercept.variance to FALSE")
  }
  if (!"(Intercept)" %in% names(fixef(merMod)) && fix.intercept.variance) {
    warning("No fixed-effect intercept detected. Variance adjustment may be unreliable.")
  }
  if (fix.intercept.variance) {
    intercept.variance <- vcov.tmp[1, 1]
    groupsizes <- ngrps(merMod)
    for (j in names(groupsizes)) {
      groupExtraPrecision <- 0
      groupVar <- (attr(VarCorr(merMod)[[j]], "stddev")["(Intercept)"])^2
      reMatrix <- attr(rr[[j]], which = "postVar")
      for (eff in 1:dim(reMatrix)[3]) {
        term <- 1/(reMatrix[1, 1, eff] + groupVar)
        if (term > 0) {
          groupExtraPrecision <- groupExtraPrecision + 
            term
        }
        else {
          warning("fix.intercept.variance got negative precision; better turn it off.")
        }
      }
      intercept.variance <- intercept.variance - 1/groupExtraPrecision
    }
    if (intercept.variance < 0) {
      warning("fix.intercept.variance got negative variance; better turn it off.")
    }
    ratio <- intercept.variance/vcov.tmp[1, 1]
    prec.tmp <- solve(vcov.tmp)
    prec.tmp[1, 1] <- prec.tmp[1, 1]/ratio
    vcov.tmp[1, ] <- vcov.tmp[1, ] * ratio
    vcov.tmp <- solve(prec.tmp, tol = 1e-50)
  }
  if (!is.null(ignore.fixed.terms)) {
    prec.tmp <- solve(vcov.tmp)
    for (term in ignore.fixed.terms) {
      prec.tmp[term, term] <- prec.tmp[term, term] * 1e+15
    }
    vcov.tmp <- solve(prec.tmp, tol = 1e-50)
  }
  if (n.sims > 2000 | .parallel) {
    if (.parallel) {
      setup_parallel()
    }
    i <- 1:n.sims
    fe_call <- as.call(c(list(quote(foreach::foreach), i = i, 
                              .combine = "rbind")))
    fe <- eval(fe_call)
    betaSim <- foreach::`%dopar%`(fe, mvtnorm::rmvnorm(n = 1, 
                                                       mean = fe.tmp, sigma = vcov.tmp, method = "chol"))
  }
  else {
    betaSim <- abind::abind(lapply(1:n.sims, function(x) mvtnorm::rmvnorm(n = 1, 
                                                                          mean = fe.tmp, sigma = vcov.tmp, method = "chol")), 
                            along = 1)
  }
  colnames(betaSim) <- names(fe.tmp)
  rownames(betaSim) <- 1:n.sims
  newdata.modelMatrix <- buildModelMatrix(merMod, newdata = newdata, 
                                          which = "fixed")
  if (ncol(newdata.modelMatrix) > ncol(betaSim)) {
    pad <- matrix(rep(0), nrow = nrow(betaSim), ncol = ncol(newdata.modelMatrix) - 
                    ncol(betaSim))
    if (ncol(pad) > 0) {
      message("Fixed effect matrix has been padded with 0 coefficients\n            for random slopes not included in the fixed effects and interaction terms.")
    }
    colnames(pad) <- setdiff(colnames(newdata.modelMatrix), 
                             colnames(betaSim))
    betaSim <- cbind(betaSim, pad)
    keep <- intersect(colnames(newdata.modelMatrix), colnames(betaSim))
    newdata.modelMatrix <- newdata.modelMatrix[, keep]
    betaSim <- betaSim[, keep]
  }
  re.xb$fixed <- newdata.modelMatrix %*% t(betaSim)
  if (which.eff == "full") {
    yhat <- Reduce("+", re.xb)
  }
  else if (which.eff == "fixed") {
    yhat <- Reduce("+", re.xb["fixed"])
  }
  else if (which.eff == "random") {
    re.xb["fixed"] <- NULL
    yhat <- Reduce("+", re.xb)
  }
  else if (which.eff == "all") {
    yhat <- Reduce("+", re.xb)
    N <- nrow(newdata)
    if (include.resid.var == TRUE) {
      for (i in 1:length(re.xb)) {
        re.xb[[i]] <- abind::abind(lapply(1:n.sims, function(x) rnorm(N, 
                                                                      re.xb[[i]][, x], sigmahat[x])), along = 2)
      }
    }
    pi.comps <- re.xb
  }
  rm(re.xb)
  N <- nrow(newdata)
  outs <- data.frame(fit = rep(NA, N), upr = rep(NA, N), lwr = rep(NA, 
                                                                   N))
  upCI <- 1 - ((1 - level)/2)
  loCI <- ((1 - level)/2)
  if (include.resid.var == TRUE) {
    yhat <- abind::abind(lapply(1:n.sims, function(x) rnorm(N, 
                                                            yhat[, x], sigmahat[x])), along = 2)
  }
  if (stat.type == "median") {
    outs[, 1:3] <- t(apply(yhat, 1, quantile, prob = c(0.5, 
                                                       upCI, loCI), na.rm = TRUE))
  }
  if (stat.type == "mean") {
    outs$fit <- apply(yhat, 1, mean, na.rm = TRUE)
    outs[, 2:3] <- t(apply(yhat, 1, quantile, prob = c(upCI, 
                                                       loCI), na.rm = TRUE))
  }
  if (predict.type == "probability") {
    if (nrow(outs) == 1) {
      outs <- t(apply(outs, 2, merMod@resp$family$linkinv))
    }
    else {
      outs <- apply(outs, 2, merMod@resp$family$linkinv)
    }
  }
  if (which.eff == "all") {
    if (returnSims == TRUE) {
      allSims <- pi.comps
    }
    for (i in 1:length(pi.comps)) {
      if (stat.type == "median") {
        pi.comps[[i]] <- t(apply(pi.comps[[i]], 1, quantile, 
                                 prob = c(0.5, upCI, loCI), na.rm = TRUE))
        pi.comps[[i]] <- as.data.frame(pi.comps[[i]])
        names(pi.comps[[i]]) <- c("fit", "upr", "lwr")
      }
      if (stat.type == "mean") {
        tmp <- pi.comps[[i]]
        pi.comps[[i]] <- data.frame(fit = rep(NA, N), 
                                    upr = NA, lwr = NA)
        pi.comps[[i]]$fit <- apply(tmp, 1, mean, na.rm = TRUE)
        pi.comps[[i]][, 2:3] <- t(apply(tmp, 1, quantile, 
                                        prob = c(upCI, loCI), na.rm = TRUE))
      }
      if (predict.type == "probability") {
        pi.comps[[i]] <- apply(pi.comps[[i]], 2, merMod@resp$family$linkinv)
        pi.comps[[i]] <- as.data.frame(pi.comps[[i]])
        names(pi.comps[[i]]) <- c("fit", "upr", "lwr")
      }
    }
    componentOut <- dplyr::bind_rows(pi.comps, .id = "effect")
    outs <- cbind(data.frame(effect = "combined"), outs)
    outs <- suppressWarnings(bind_rows(outs, componentOut))
    outs$obs <- rep(1:N, nrow(outs)%/%N)
    rm(pi.comps)
  }
  if (returnSims == FALSE) {
    return(as.data.frame(outs))
  }
  else if (returnSims == TRUE) {
    outs <- as.data.frame(outs)
    if (which.eff == "all") {
      attr(outs, "sim.results") <- allSims
    }
    else {
      attr(outs, "sim.results") <- yhat
    }
    return(outs)
  }
}

