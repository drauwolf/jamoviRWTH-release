# This file is a generated template, your changes will not be overwritten

d4confidenceintervalsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "d4confidenceintervalsClass",
  inherit = d4confidenceintervalsBase,
  private = list(
    # store the names of variables that have been transformed to numeric with a notice
    .transformedVars = c(),
    # store the names of variables that have missing values
    .missingvaluesVars = c(),
    .noticeInsertPosition = 1L,
    .run = function() {
      private$.noticeInsertPosition <- 1L
      private$.transformedVars <- c()
      private$.missingvaluesVars <- c()
      
      numberofvariables <- private$.numberOfVariables()
      # return nothing if input is empty
      if (numberofvariables==0) {
        return()
      }
      
      preparedInput <- private$.prepareInputData(varexp = self$options$varexp,
                                                 varbin = self$options$varbinom,
                                                 varnorm = self$options$varnorm,
                                                 varnormtwosample = self$options$varnormtwosample,
                                                 splitVar = self$options$split)
      
      xexp <- preparedInput$xexp
      xbin <- preparedInput$xbin
      xnorm <- preparedInput$xnorm
      normalTwoSampleData <- preparedInput$normalTwoSampleData
      
      private$.runExponentialAnalysis(xexp = xexp,varexp = self$options$varexp)
      private$.runBinomialAnalysis(xbin = xbin,varbin = self$options$varbinom)
      private$.runNormalOneSampleAnalysis(xnorm = xnorm,varnorm = self$options$varnorm)
      private$.runNormalTwoSampleAnalysis(normalTwoSampleData = normalTwoSampleData,varnormtwosample = self$options$varnormtwosample,splitVar = self$options$split)
      
      private$.showMissingValuesNotice()
      private$.showTransformedVarsNotice()
    },
    
    .prepareInputData = function(varexp,varbin,varnorm,varnormtwosample,splitVar) {
      xexp <- NULL
      xbin <- NULL
      xnorm <- NULL
      normalTwoSampleData <- NULL
      
      if (!is.null(varexp)) {
        xexp <- private$.prepareNumericVector(
          var = varexp,
          issueName = paste0("analysesNotApplicable_ExpCI_", varexp),
          issueMessage = paste0("Confidence intervals for an Exponential distribution can only be computed for numeric data, but '",varexp,"' is not numeric."),
          tableNames = "CIexp")
      }
      
      if (!is.null(varbin)) {
        xbin <- private$.prepareNumericVector(
          var = varbin,
          issueName = paste0("analysesNotApplicable_BinCI_", varbin),
          issueMessage = paste0("Confidence intervals for a Binomial distribution can only be computed for numeric data, but '",varbin,"' is not numeric."),
          tableNames = "CIbin")
      }
      
      if (!is.null(varnorm)) {
        xnorm <- private$.prepareNumericVector(
          var = varnorm,
          issueName = paste0("analysesNotApplicable_NormCI_", varnorm),
          issueMessage = paste0("Confidence intervals for a Normal distribution can only be computed for numeric data, but '",varnorm,"' is not numeric."),
          tableNames = "CInorm")
      }
      
      if (!is.null(varnormtwosample) && !is.null(splitVar)) {
        normalTwoSampleData <- private$.prepareNormalTwoSampleData(
          var = varnormtwosample,
          splitVar = splitVar)
      }
      
      return(list(xexp = xexp,xbin = xbin,xnorm = xnorm,normalTwoSampleData = normalTwoSampleData))
    },
   
    .runExponentialAnalysis = function(xexp, varexp) {
      
      if (is.null(varexp) || is.null(xexp))
        return(invisible(NULL))
      
      alphaexp <- 1 - self$options$expalpha
      CIexp <- self$results$CIexp
      
      if (!private$.isValidAlpha(alphaexp)) {
        private$.handleAnalysisIssue(
          name = paste0("confLevelEXP_", alphaexp),
          message = "Exponential distribution: Confidence level must be between 0 and 1!",
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CIexp")
        return(invisible(NULL))
      }
      
      if (length(xexp) == 0) {
        private$.handleAnalysisIssue(
          name = paste0("noValidObservationsEXP_", varexp),
          message = paste0("Confidence intervals for an Exponential distribution cannot be computed because '",varexp,"' contains no non-missing observations."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CIexp")
        return(invisible(NULL))
      }
      
      if (!all(xexp > 0)) {
        private$.handleAnalysisIssue(
          name = paste0("dataNotPositiveEXP_", varexp),
          message = paste0("Confidence intervals for an Exponential distribution can only be computed for positive data, but '",varexp,"' is not positive."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CIexp")
        return(invisible(NULL))
      }
      
      if (self$options$lambda) {
        ci <- private$.ciExpLambda(x = xexp,alpha = alphaexp,side = self$options$sideexp)
        
        CIexp$addRow(rowKey = 1, values = list(
          Parameter = "\U03BB",
          Level = alphaexp,
          Lower = ci$Lower,
          Upper = ci$Upper
        ))
      }
      
      if (self$options$beta) {
        ci <- private$.ciExpBeta(x = xexp,alpha = alphaexp,side = self$options$sidebetaexp)
        
        CIexp$addRow(rowKey = 2, values = list(
          Parameter = "\U03B2",
          Level = alphaexp,
          Lower = ci$Lower,
          Upper = ci$Upper
        ))
      }
      
      return(invisible(NULL))
    },
    
    .runBinomialAnalysis = function(xbin, varbin) {
      
      if (is.null(varbin) || is.null(xbin))
        return(invisible(NULL))
      
      alphabin <- 1 - self$options$binalpha
      CIbin <- self$results$CIbin
      
      if (!private$.isValidAlpha(alphabin)) {
        private$.handleAnalysisIssue(
          name = paste0("confLevelBIN_", alphabin),
          message = "Binomial distribution: Confidence level must be between 0 and 1!",
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CIbin")
        return(invisible(NULL))
      }
      
      if (length(xbin) == 0) {
        private$.handleAnalysisIssue(
          name = paste0("noValidObservationsBIN_", varbin),
          message = paste0("Confidence intervals for a Binomial distribution cannot be computed because '",varbin,"' contains no non-missing observations."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CIbin")
        return(invisible(NULL))
      }
      
      if (!all(xbin == 0 | xbin == 1)) {
        private$.handleAnalysisIssue(
          name = paste0("dataNotZeroOneBIN_", varbin),
          message = paste0("Confidence intervals for a Binomial distribution can only be computed for 0-1 data, but '",varbin,"' contains values other than 0 and 1."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CIbin")
        return(invisible(NULL))
      }
      
      if (self$options$approx) {
        ci <- private$.ciBinApprox(x = xbin,alpha = alphabin,side = self$options$sidebinapprox)
        
        CIbin$addRow(rowKey = 1, values = list(
          Parameter = "p",
          Level = alphabin,
          Lower = ci$Lower,
          Upper = ci$Upper,
          Type = "approximate"
        ))
      }
      
      if (self$options$exact) {
        ci <- private$.ciBinExact(x = xbin,alpha = alphabin,side = self$options$sidebinexact)
        
        CIbin$addRow(rowKey = 2, values = list(
          Parameter = "p",
          Level = alphabin,
          Lower = ci$Lower,
          Upper = ci$Upper,
          Type = "exact"
        ))
      }
      
      return(invisible(NULL))
    },
    
    .runNormalOneSampleAnalysis = function(xnorm, varnorm) {
      
      if (is.null(varnorm) || is.null(xnorm))
        return(invisible(NULL))
      
      alphanorm <- 1 - self$options$normalpha
      CInorm <- self$results$CInorm
      
      if (!private$.isValidAlpha(alphanorm)) {
        private$.handleAnalysisIssue(
          name = paste0("confLevelNORM_", alphanorm),
          message = "Normal distribution (one sample): Confidence level must be between 0 and 1!",
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CInorm")
        return(invisible(NULL))
      }
      
      if (length(xnorm) == 0) {
        private$.handleAnalysisIssue(
          name = paste0("noValidObservationsNORM_", varnorm),
          message = paste0("Confidence intervals for a Normal distribution cannot be computed because '",varnorm,"' contains no non-missing observations."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CInorm")
        return(invisible(NULL))
      }
      
      n <- length(xnorm)
      
      # confidence interval for mu with known variance, one sample
      if (self$options$muknownvar) {
        if (self$options$sigmasq <= 0) {
          otherCInormAnalysesSelected <- isTRUE(self$options$muunknownvar) || isTRUE(self$options$sigmaunknownmu) || 
                                        isTRUE(self$options$sigmaknownmu)
          
          if (!otherCInormAnalysesSelected) {
            private$.hideTables("CInorm")
          }
          private$.createNotice(name = paste0("negativeSigmaNORM_", self$options$sigmasq),
            message = "Normal distribution (one sample): \U03C3\U00B2 must be greater than 0!",
            type = jmvcore::NoticeType$STRONG_WARNING)
        } else {
          ci <- private$.ciNormalMuKnownVar(x = xnorm,alpha = alphanorm,sigmasq = self$options$sigmasq,
                                            side = self$options$sidemuknownvar)
          
          sigma <- sqrt(self$options$sigmasq)
          nobs <- ''
          
          if (self$options$n && self$options$L0 > 0) {
            nobs <- private$.nNormalKnownVar(alpha = alphanorm,sigmasq = self$options$sigmasq,L0 = self$options$L0)
          } else if (self$options$n && self$options$L0 <= 0) {
              private$.createNotice(
                name = paste0("lengthNORM_", self$options$L0),
                message = paste0("Normal distribution: Interval length L\U2092 must be a value greater than 0!"),
                type = jmvcore::NoticeType$WARNING)
          }
          CInorm$addRow(rowKey = 1, values = list(
            Parameter = "\U03BC",
            Level = alphanorm,
            n = nobs,
            Lower = ci$Lower,
            Upper = ci$Upper,
            Type = paste0("known variance ", "\U03C3\U00B2", "=", sigma^2)
          ))
        }
      }
      
      # confidence interval for mu with unknown variance, one sample
      if (self$options$muunknownvar) {
        if (n < 2) {
          private$.createNotice(
            name = paste0("insufficientObservationsNORM_muunknownvar_", varnorm),
            message = paste0("Normal distribution (one sample): At least two non-missing observations are required to compute a confidence interval for \U03BC with unknown variance."),
            type = jmvcore::NoticeType$STRONG_WARNING)
        } else {
          ci <- private$.ciNormalMuUnknownVar(x = xnorm,alpha = alphanorm,side = self$options$sidemuunknownvar)
          
          CInorm$addRow(rowKey = 2, values = list(
            Parameter = "\U03BC",
            Level = alphanorm,
            Lower = ci$Lower,
            Upper = ci$Upper,
            Type = "unknown variance"
          ))
        }
      }
      
      # confidence interval for sigma with unknown mu, one sample
      if (self$options$sigmaunknownmu) {
        if (n < 2) {
          private$.createNotice(
            name = paste0("insufficientObservationsNORM_sigmaunknownmu_", varnorm),
            message = paste0("Normal distribution (one sample): At least two non-missing observations are required to compute a confidence interval for \U03C3\U00B2 with unknown expectation."),
            type = jmvcore::NoticeType$STRONG_WARNING)
        } else {
          ci <- private$.ciNormalSigmaUnknownMu(x = xnorm,alpha = alphanorm,side = self$options$sidesigmaunknownmu)
          
          CInorm$addRow(rowKey = 3, values = list(
            Parameter = "\U03C3\U00B2",
            Level = alphanorm,
            Lower = ci$Lower,
            Upper = ci$Upper,
            Type = "unknown expectation"
          ))
        }
      }
      
      # confidence interval for sigma with known mu, one sample
      if (self$options$sigmaknownmu) {
        ci <- private$.ciNormalSigmaKnownMu(x = xnorm,mu = self$options$mu,alpha = alphanorm,side = self$options$sidesigmaknownmu)
        
        CInorm$addRow(rowKey = 4, values = list(
          Parameter = "\U03C3\U00B2",
          Level = alphanorm,
          Lower = ci$Lower,
          Upper = ci$Upper,
          Type = paste0("known expectation ", "\U03BC", "=", self$options$mu)
        ))
      }
      return(invisible(NULL))
    },
    
    .runNormalTwoSampleAnalysis = function(normalTwoSampleData,varnormtwosample,splitVar) {
      if (is.null(varnormtwosample) || is.null(splitVar) || is.null(normalTwoSampleData)) {
        return(invisible(NULL))
      }
      
      alphanormtwosample <- 1 - self$options$normalphatwosample
      CInormtwosample <- self$results$CInormtwosample
      
      if (!private$.isValidAlpha(alphanormtwosample)) {
        private$.handleAnalysisIssue(
          name = paste0("confLevelNORMtwo_", alphanormtwosample),
          message = "Normal distribution (two independent samples): Confidence level must be between 0 and 1!",
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CInormtwosample")
        return(invisible(NULL))
      }
      
      if (self$options$diff) {
        xnormtwosample <- normalTwoSampleData$x
        group <- normalTwoSampleData$group
        
        groupForSplit <- private$.asObservedGroupFactor(group)
        x <- split(xnormtwosample, groupForSplit)
        
        if (length(x) != 2) {
          private$.handleAnalysisIssue(
            name = paste0("splitVariableLevels_", splitVar),
            message = paste0("Normal distribution (two independent samples): 'Split by' variable '",splitVar,"' must have exactly two levels after removing rows with missing values."),
            type = jmvcore::NoticeType$STRONG_WARNING,
            tableNames = "CInormtwosample")
          return(invisible(NULL))
        }
        
        x1 <- x[[1]]
        x2 <- x[[2]]
        
        n1 <- length(x1)
        n2 <- length(x2)
        
        if ((n1 + n2 - 2) <= 0) {
          private$.handleAnalysisIssue(
            name = paste0("insufficientObservationsNORMtwo_", varnormtwosample),
            message = paste0("Normal distribution (two independent samples): At least three complete observations in total are required to compute a confidence interval for the difference in means with unknown equal variances."),
            type = jmvcore::NoticeType$STRONG_WARNING,
            tableNames = "CInormtwosample")
          return(invisible(NULL))
        }
        
        ci <- private$.ciNormalDiffEqualVar(x1 = x1,x2 = x2,alpha = alphanormtwosample,side = self$options$sidediff)
        
        CInormtwosample$addRow(rowKey = 1, values = list(
          Parameter = "\U03B4",
          Level = alphanormtwosample,
          Lower = ci$Lower,
          Upper = ci$Upper,
          Type = "unknown equal variances"
        ))
      }
      return(invisible(NULL))
    },
 
    .prepareNumericVector = function(var,issueName,issueMessage,tableNames) {
      if (is.null(var))
        return(NULL)
      
      if (!jmvcore::canBeNumeric(self$data[[var]])) {
        private$.handleAnalysisIssue(
          name = issueName,
          message = issueMessage,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = tableNames)
        return(NULL)
      }
      
      x <- private$.toNumericWithoutExplicitNotice(var)
      private$.noteMissingValues(var, x)
      
      return(x[!is.na(x)])
    },
    
    .prepareNormalTwoSampleData = function(var, splitVar) {
      if (is.null(var) || is.null(splitVar))
        return(NULL)
      
      if (!jmvcore::canBeNumeric(self$data[[var]])) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable_NormCItwo_", var),
          message = paste0("Confidence intervals for a Normal distribution can only be computed for numeric data, but '",var,"' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CInormtwosample")
        return(NULL)
      }
      
      xRaw <- private$.toNumericWithoutExplicitNotice(var)
      groupRaw <- self$data[[splitVar]]
      
      private$.noteMissingValues(var, xRaw)
      private$.noteMissingValues(splitVar, groupRaw)
      
      df <- data.frame(.x = xRaw,.group = groupRaw,stringsAsFactors = FALSE)
      df <- df[complete.cases(df), , drop = FALSE]
      
      x <- df$.x
      group <- df$.group
      
      observedGroups <- private$.observedGroups(group)
      
      if (length(observedGroups) != 2) {
        private$.handleAnalysisIssue(
          name = paste0("splitVariableLevels_", splitVar),
          message = paste0("Normal distribution (two independent samples): 'Split by' variable '",splitVar,"' must have exactly two non-missing groups after removing rows with missing values."),
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "CInormtwosample")
        return(NULL)
      }
      return(list(x = x,group = group))
    },
    
    .observedGroups = function(group) {
      group <- group[!is.na(group)]
      if (is.factor(group))
        group <- droplevels(group)
      
      return(unique(group))
    },
    
    .asObservedGroupFactor = function(group) {
      if (is.factor(group))
        return(droplevels(group))
      
      return(factor(group))
    },
    
    .ciExpLambda = function(x, alpha, side) {
      n <- length(x)
      lambdaHat <- 1 / mean(x)
      switch(side,
        twoexp = list(Lower = qchisq(alpha/2, 2*n)/(2*n)*lambdaHat,Upper = qchisq(1-alpha/2, 2*n)/(2*n)*lambdaHat),
        lowerexp = list(Lower = 0,Upper = qchisq(1-alpha, 2*n)/(2*n)*lambdaHat),
        upperexp = list(Lower = qchisq(alpha, 2*n)/(2*n)*lambdaHat,Upper = Inf))
    },
    
    .ciExpBeta = function(x, alpha, side) {
      n <- length(x)
      betaHat <- mean(x)
      
      switch(side,
        twobetaexp = list(Lower = n*betaHat/qgamma(1-alpha/2, n, 1),Upper = n*betaHat/qgamma(alpha/2, n, 1)),
        lowerbetaexp = list(Lower = 0,Upper = n*betaHat/qgamma(1-alpha, n, 1)),
        upperbetaexp = list(Lower = n*betaHat/qgamma(alpha, n, 1),Upper = Inf))
    },
    
    .ciBinApprox = function(x, alpha, side) {
      n <- length(x)
      pHat <- mean(x)
      se <- sqrt(pHat*(1 - pHat))/sqrt(n)
      
      switch(side,
        twobinapprox = list(Lower = pHat-qnorm(1-alpha/2)*se,Upper = pHat+qnorm(1-alpha/2)*se),
        lowerbinapprox = list(Lower = 0,Upper = pHat+qnorm(1-alpha)*se),
        upperbinapprox = list(Lower = pHat-qnorm(1-alpha)*se,Upper = 1))
    },
    
    .ciBinExact = function(x, alpha, side) {
      n <- length(x)
      eHat <- sum(x)
      
      lowerTwoSided <- function() {
        if (eHat == 0)
          return(0)
        
        eHat*qf(alpha/2, 2*eHat, 2*(n-eHat+1))/(n-eHat+1+eHat*qf(alpha/2, 2*eHat, 2*(n-eHat+1)))
      }
      
      upperTwoSided <- function() {
        if (eHat == n)
          return(1)
        
        (eHat+1)*qf(1-alpha/2, 2*(eHat+1), 2*(n-eHat))/(n-eHat+(eHat+1)*qf(1-alpha/2, 2*(eHat+1), 2*(n-eHat)))
      }
      
      lowerOneSidedUpper <- function() {
        if (eHat == n)
          return(1)
        
        (eHat+1)*qf(1-alpha, 2*(eHat+1), 2*(n-eHat))/(n-eHat+(eHat+1)*qf(1-alpha, 2*(eHat+1), 2*(n-eHat)))
      }
      
      upperOneSidedLower <- function() {
        if (eHat == 0)
          return(0)
        
        eHat*qf(alpha, 2*eHat, 2*(n-eHat+1))/(n-eHat+1+eHat*qf(alpha, 2*eHat, 2*(n-eHat+1)))
      }
      
      switch(side,
        twobinexact = list(Lower = lowerTwoSided(),Upper = upperTwoSided()),
        lowerbinexact = list(Lower = 0,Upper = lowerOneSidedUpper()),
        upperbinexact = list(Lower = upperOneSidedLower(),Upper = 1))
    },
    
    .ciNormalMuKnownVar = function(x, alpha, sigmasq, side) {
      n <- length(x)
      meanX <- mean(x)
      sigma <- sqrt(sigmasq)
      
      switch(side,
        twomuknownvar = list(Lower = meanX-qnorm(1-alpha/2)*sigma/sqrt(n),Upper = meanX+qnorm(1-alpha/2)*sigma/sqrt(n)),
        lowermuknownvar = list(Lower = -Inf,Upper = meanX+qnorm(1-alpha)*sigma/sqrt(n)),
        uppermuknownvar = list(Lower = meanX-qnorm(1-alpha)*sigma/sqrt(n),Upper = Inf))
    },
    
    .nNormalKnownVar = function(alpha, sigmasq, L0) {
      4*qnorm(1-alpha/2)^2*sigmasq/L0^2
    },
    
    .ciNormalMuUnknownVar = function(x, alpha, side) {
      n <- length(x)
      meanX <- mean(x)
      sigmaHat <- sqrt(1/(n-1)*sum((x-meanX)^2))
      
      switch(side,
        twomuunknownvar = list(Lower = meanX-qt(1-alpha/2, n-1)*sigmaHat/sqrt(n),Upper = meanX+qt(1-alpha/2, n-1)*sigmaHat/sqrt(n)),
        lowermuunknownvar = list(Lower = -Inf,Upper = meanX+qt(1-alpha, n-1)*sigmaHat/sqrt(n)),
        uppermuunknownvar = list(Lower = meanX-qt(1-alpha, n-1)*sigmaHat/sqrt(n),Upper = Inf))
    },
    
    .ciNormalSigmaUnknownMu = function(x, alpha, side) {
      n <- length(x)
      meanX <- mean(x)
      sigmaHat <- sqrt(1 / (n - 1) * sum((x - meanX)^2))
      
      switch(side,
        twosigmaunknownmu = list(Lower = (n-1)/qchisq(1-alpha/2, n-1)*sigmaHat^2,Upper = (n-1)/qchisq(alpha/2, n-1)*sigmaHat^2),
        lowersigmaunknownmu = list(Lower = 0,Upper = (n-1)/qchisq(alpha, n-1)*sigmaHat^2),
        uppersigmaunknownmu = list(Lower = (n-1)/qchisq(1-alpha, n-1)*sigmaHat^2,Upper = Inf))
    },
    
    .ciNormalSigmaKnownMu = function(x, mu, alpha, side) {
      n <- length(x)
      sigmaHat <- sqrt(mean((x-mu)^2))
      
      switch(side,
        twosigmaknownmu = list(Lower = n/qchisq(1-alpha/2, n)*sigmaHat^2,Upper = n/qchisq(alpha/2, n)*sigmaHat^2),
        lowersigmaknownmu = list(Lower = 0,Upper = n/qchisq(alpha, n)*sigmaHat^2),
        uppersigmaknownmu = list(Lower = n/qchisq(1-alpha, n)*sigmaHat^2,Upper = Inf))
    },
    
    .ciNormalDiffEqualVar = function(x1, x2, alpha, side) {
      n1 <- length(x1)
      n2 <- length(x2)
      deltaHat <- mean(x1) - mean(x2)
      sigmaPool <- sqrt(1/(n1+n2-2)*(sum((x1-mean(x1))^2)+sum((x2-mean(x2))^2)))
      se <- sigmaPool * sqrt(1 / n1 + 1 / n2)
      df <- n1 + n2 - 2
      
      switch(side,
        twodiff = list(Lower = deltaHat-qt(1-alpha/2, df) * se,Upper = deltaHat+qt(1-alpha/2, df)*se),
        lowerdiff = list(Lower = -Inf,Upper = deltaHat+qt(1-alpha, df)*se),
        upperdiff = list(Lower = deltaHat-qt(1-alpha, df)*se,Upper = Inf))
    },
    
    .isValidAlpha = function(alpha) {
      isTRUE(length(alpha) == 1 && !is.na(alpha) && alpha > 0 && alpha < 1)
    },
    
    .handleAnalysisIssue = function(name,message,type = jmvcore::NoticeType$WARNING,tableNames = NULL) {
      private$.hideTables(tableNames)
      private$.createNotice(name, message, type = type)
      return(invisible(NULL))
    },
    
    .hideTables = function(tableNames = NULL) {
      if (is.null(tableNames))
        return(invisible(NULL))
      
      for (tableName in tableNames) {
        tryCatch({table <- self$results[[tableName]]
          table$setVisible(FALSE)}, error = function(e) {NULL})
      }
      return(invisible(NULL))
    },
    
    .numberOfVariables = function() {
      n <- as.numeric(!is.null(self$options$varexp)) +
        as.numeric(!is.null(self$options$varbinom)) +
        as.numeric(!is.null(self$options$varnorm)) +
        as.numeric(!is.null(self$options$varnormtwosample))
      return(n)
    },
    
    # create a notice for the user
    .createNotice=function(name, message, type=jmvcore::NoticeType$WARNING){
      notice <- jmvcore::Notice$new(options=self$options, name=name, type=type)
      notice$setContent(message)
      self$results$insert(private$.noticeInsertPosition, notice)
      private$.noticeInsertPosition <- private$.noticeInsertPosition + 1L
    },
    
    # Missing values and numeric transformation
    .toNumericWithoutExplicitNotice = function(var) {
      if (!is.numeric(self$data[[var]]) && !(var %in% private$.transformedVars)) {
        private$.transformedVars <- c(private$.transformedVars, var)
      }
      return(jmvcore::toNumeric(self$data[[var]]))
    },
    
    .noteMissingValues = function(var, x) {
      if (!(var %in% private$.missingvaluesVars)) {
        nMissing <- sum(is.na(x))
        if (nMissing > 0) {
          private$.missingvaluesVars <- c(private$.missingvaluesVars, var)
        }
      }
      return(invisible(NULL))
    },
    
    .showMissingValuesNotice = function() {
      if (length(private$.missingvaluesVars) > 0) {
        private$.createNotice(
          name = paste0("missingValues_",paste(private$.missingvaluesVars, collapse = ", ")),
          message = paste0("Variable(s) '",paste(private$.missingvaluesVars, collapse = ", "),
            "' contain(s) missing value(s). Analyses will ignore missing values."),
          type = jmvcore::NoticeType$INFO)
      }
      return(invisible(NULL))
    },
    
    .showTransformedVarsNotice = function() {
      if (length(private$.transformedVars) > 0) {
        private$.createNotice(
          name = paste0("variablesNotNumeric_",paste(private$.transformedVars, collapse = ", ")),
          message = paste0("Treating variable(s) '",paste(private$.transformedVars, collapse = ", "),"' as numeric."),
          type = jmvcore::NoticeType$INFO)
      }
      return(invisible(NULL))
    }
  )
)