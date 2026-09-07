# This file is a generated template, your changes will not be overwritten

d7linearregressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "d7linearregressionClass",
  inherit = d7linearregressionBase,
  private = list(
    .transformedVars = c(),
    .missingvaluesVars = c(),
    .noticeInsertPosition = 1L,
    
    .init = function() {
      self$results$confbandplot$setSize(500, 250)
    },
    
    .run = function() {
      private$.noticeInsertPosition <- 1L
      private$.clearConfidenceBandPlot()
      
      xname <- self$options$xvariable
      yname <- self$options$yvariable
      
      selectedTableNames <- private$.selectedTableNames()
      
      # return empty output if there is no input
      if (any(is.null(xname), is.null(yname))) {
        private$.hideTables(selectedTableNames)
        return()
      }
      
      preparedData <- private$.checkandPrepareInputVariables(xname,yname)
      
      if (is.null(preparedData)) {
        private$.hideTables(selectedTableNames)
        return()
      }
      
      regressionContext <- private$.createRegressionContext(
        xname = xname,
        yname = yname,
        x = preparedData$x,
        y = preparedData$y,
        n = preparedData$n
      )
      
      private$.runPointEstimation(
        xname = xname,
        yname = yname,
        x = preparedData$x,
        y = preparedData$y,
        n = preparedData$n,
        context = regressionContext
      )
      
      confidenceLevelValid <- private$.checkCommonConfidenceLevel()
      
      private$.runConfidenceIntervals(
        xname = xname,
        yname = yname,
        x = preparedData$x,
        y = preparedData$y,
        n = preparedData$n,
        context = regressionContext,
        confidenceLevelValid = confidenceLevelValid
      )
      
      private$.runConfidenceBandPlot(
        xname = xname,
        yname = yname,
        x = preparedData$x,
        y = preparedData$y,
        n = preparedData$n,
        context = regressionContext,
        confidenceLevelValid = confidenceLevelValid
      )
      
      private$.runHypothesisTests(
        xname = xname,
        yname = yname,
        x = preparedData$x,
        y = preparedData$y,
        n = preparedData$n,
        context = regressionContext
      )
      
      private$.showFinalNotices()
    },
    
    .checkandPrepareInputVariables = function(xname,yname) {

      if (any(is.null(xname), is.null(yname))) {
        return(NULL)
      }
      
      # check if both variables can be numeric
      if (!jmvcore::canBeNumeric(self$data[[xname]])) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable", testnameshort, "_", xname),
          message = paste0(testname, ": Input variable '", xname, "' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(NULL)
      }
      
      if (!jmvcore::canBeNumeric(self$data[[yname]])) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable", testnameshort, "_", yname),
          message = paste0(testname, ": Input variable '", yname, "' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(NULL)
      }
      
      x1Raw <- private$.toNumeric(xname)
      x2Raw <- private$.toNumeric(yname)
      
      private$.noteMissingValues(xname, x1Raw)
      private$.noteMissingValues(yname, x2Raw)
      
      # exclude any rows with missing data
      pairedData <- data.frame(.x1 = x1Raw,.x2 = x2Raw)
      
      completeRows <- complete.cases(pairedData)
      
      # catch completely empty variables / not enough complete cases early
      if (sum(completeRows) < 2L) {
        private$.createNotice(
          "notEnoughData",
          "Not enough data to perform regression. At least two complete cases are required.",
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(NULL)
      }
      
      pairedData <- pairedData[completeRows, , drop = FALSE]
      x1 <- pairedData$.x1
      x2 <- pairedData$.x2
      
      # since all missing values are excluded, x1 and x2 are of the same length
      n <- length(x1)
      
      distinctValuesValid <- private$.checkAtLeastTwoDistinctValues(xname,yname,x1,x2)
      
      if (!isTRUE(distinctValuesValid)) {
        return(NULL)
      }
      
      return(list(x = x1,y = x2,n = n))
    },
    
    .createRegressionContext = function(xname,yname,x,y,n) {
      context <- new.env(parent = emptyenv())
      
      context$xname <- xname
      context$yname <- yname
      context$x <- x
      context$y <- y
      context$n <- n
      
      context$.empiricalComputed <- FALSE
      context$.empiricalQuantities <- NULL
      context$.empiricalStatus <- NULL
      
      context$.regressionEstimatorsComputed <- FALSE
      context$.regressionEstimators <- NULL
      context$.regressionEstimatorsStatus <- NULL
      
      context$.sigmahat2Computed <- FALSE
      context$.sigmahat2 <- NULL
      context$.sigmahat2Status <- NULL
      
      context$.estimatorVariancesComputed <- FALSE
      context$.estimatorVariances <- NULL
      context$.estimatorVariancesStatus <- NULL
      
      context$.scaleAComputed <- FALSE
      context$.scaleA <- NULL
      context$.scaleAStatus <- NULL
      
      context$.scaleBComputed <- FALSE
      context$.scaleB <- NULL
      context$.scaleBStatus <- NULL
      
      context$getEmpiricalQuantities <- function() {
        if (!isTRUE(context$.empiricalComputed)) {
          value <- private$.calculateEmpiricalQuantities(
            xname = context$xname,
            yname = context$yname,
            x = context$x,
            y = context$y,
            analysisname = "Regression context"
          )
          
          requiredNames <- c("x_mean","x2_mean","y_mean","sx2","sy2","sxy")
          hasRequiredNames <- !is.null(value) && all(requiredNames %in% names(value))
          
          requiredValues <- numeric(0)
          if (isTRUE(hasRequiredNames)) {
            requiredValues <- unlist(value[requiredNames], use.names = FALSE)
          }
          
          baseNoNA <- isTRUE(hasRequiredNames) && !any(is.na(requiredValues))
          baseFinite <- isTRUE(baseNoNA) && all(is.finite(requiredValues))
          
          sx2 <- if (isTRUE(hasRequiredNames)) value$sx2 else NA
          sy2 <- if (isTRUE(hasRequiredNames)) value$sy2 else NA
          
          sx2Positive <- isTRUE(hasRequiredNames) && !is.na(sx2) && isTRUE(sx2 > 0)
          sy2Positive <- isTRUE(hasRequiredNames) && !is.na(sy2) && isTRUE(sy2 > 0)
          
          sx2PositiveFinite <- isTRUE(sx2Positive) && is.finite(sx2)
          sy2PositiveFinite <- isTRUE(sy2Positive) && is.finite(sy2)
          
          context$.empiricalQuantities <- value
          context$.empiricalStatus <- list(
            available = hasRequiredNames,
            valid = baseNoNA,
            finite = baseFinite,
            sx2Positive = sx2Positive,
            sy2Positive = sy2Positive,
            sx2PositiveFinite = sx2PositiveFinite,
            sy2PositiveFinite = sy2PositiveFinite
          )
          context$.empiricalComputed <- TRUE
        }
        
        return(context$.empiricalQuantities)
      }
      
      context$getEmpiricalStatus <- function() {
        context$getEmpiricalQuantities()
        return(context$.empiricalStatus)
      }
      
      context$getRegressionEstimators <- function() {
        if (!isTRUE(context$.regressionEstimatorsComputed)) {
          empiricalQuantities <- context$getEmpiricalQuantities()
          empiricalStatus <- context$getEmpiricalStatus()
          
          if (!isTRUE(empiricalStatus$valid)) {
            context$.regressionEstimators <- NULL
            context$.regressionEstimatorsStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              reason = "empiricalQuantities"
            )
          } else if (!isTRUE(empiricalStatus$sx2Positive)) {
            context$.regressionEstimators <- NULL
            context$.regressionEstimatorsStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              reason = "sx2"
            )
          } else {
            b_hat <- empiricalQuantities$sxy / empiricalQuantities$sx2
            a_hat <- empiricalQuantities$y_mean - b_hat * empiricalQuantities$x_mean
            
            value <- c(a_hat = a_hat,b_hat = b_hat)
            
            valid <- !any(is.na(value))
            finite <- isTRUE(valid) && all(is.finite(value))
            
            context$.regressionEstimators <- value
            context$.regressionEstimatorsStatus <- list(
              available = TRUE,
              valid = valid,
              finite = finite,
              reason = if (isTRUE(valid)) NULL else "invalidValue"
            )
          }
          
          context$.regressionEstimatorsComputed <- TRUE
        }
        
        return(context$.regressionEstimators)
      }
      
      context$getRegressionEstimatorsStatus <- function() {
        context$getRegressionEstimators()
        return(context$.regressionEstimatorsStatus)
      }
      
      context$getSigmahat2 <- function() {
        if (!isTRUE(context$.sigmahat2Computed)) {
          empiricalQuantities <- context$getEmpiricalQuantities()
          empiricalStatus <- context$getEmpiricalStatus()
          
          dfValid <- isTRUE(is.numeric(context$n) && length(context$n) == 1L &&
                              !is.na(context$n) && is.finite(context$n) && context$n > 2)
          
          if (!isTRUE(empiricalStatus$valid)) {
            context$.sigmahat2 <- NULL
            context$.sigmahat2Status <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              nonNegative = FALSE,
              dfValid = dfValid,
              reason = "empiricalQuantities"
            )
          } else if (!isTRUE(dfValid)) {
            context$.sigmahat2 <- NULL
            context$.sigmahat2Status <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              nonNegative = FALSE,
              dfValid = dfValid,
              reason = "df"
            )
          } else if (!isTRUE(empiricalStatus$sx2Positive) || !isTRUE(empiricalStatus$sy2Positive)) {
            context$.sigmahat2 <- NULL
            context$.sigmahat2Status <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              nonNegative = FALSE,
              dfValid = dfValid,
              reason = "empiricalVariance"
            )
          } else {
            rxy <- empiricalQuantities$sxy / sqrt(empiricalQuantities$sx2 * empiricalQuantities$sy2)
            sigmahat2 <- context$n / (context$n - 2) * empiricalQuantities$sy2 * (1 - rxy^2)
            
            valid <- !is.na(sigmahat2)
            finite <- isTRUE(valid) && is.finite(sigmahat2)
            nonNegative <- isTRUE(valid) && isTRUE(sigmahat2 >= 0)
            
            context$.sigmahat2 <- sigmahat2
            context$.sigmahat2Status <- list(
              available = TRUE,
              valid = valid,
              finite = finite,
              nonNegative = nonNegative,
              dfValid = dfValid,
              reason = if (isTRUE(valid)) NULL else "invalidValue"
            )
          }
          
          context$.sigmahat2Computed <- TRUE
        }
        
        return(context$.sigmahat2)
      }
      
      context$getSigmahat2Status <- function() {
        context$getSigmahat2()
        return(context$.sigmahat2Status)
      }
      
      context$getEstimatorVariances <- function() {
        if (!isTRUE(context$.estimatorVariancesComputed)) {
          empiricalQuantities <- context$getEmpiricalQuantities()
          empiricalStatus <- context$getEmpiricalStatus()
          sigmahat2 <- context$getSigmahat2()
          sigmahat2Status <- context$getSigmahat2Status()
          
          dfValid <- isTRUE(is.numeric(context$n) && length(context$n) == 1L &&
                              !is.na(context$n) && is.finite(context$n) && context$n > 2)
          
          if (!isTRUE(empiricalStatus$valid)) {
            context$.estimatorVariances <- NULL
            context$.estimatorVariancesStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              dfValid = dfValid,
              reason = "empiricalQuantities"
            )
          } else if (!isTRUE(empiricalStatus$sx2Positive)) {
            context$.estimatorVariances <- NULL
            context$.estimatorVariancesStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              dfValid = dfValid,
              reason = "sx2"
            )
          } else if (!isTRUE(dfValid)) {
            context$.estimatorVariances <- NULL
            context$.estimatorVariancesStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              dfValid = dfValid,
              reason = "df"
            )
          } else if (is.null(sigmahat2) || !isTRUE(sigmahat2Status$available)) {
            context$.estimatorVariances <- NULL
            context$.estimatorVariancesStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              dfValid = dfValid,
              reason = "sigmahat2"
            )
          } else {
            sigmahat2a <- empiricalQuantities$x2_mean / (context$n * empiricalQuantities$sx2) * sigmahat2
            sigmahat2b <- sigmahat2 / (context$n * empiricalQuantities$sx2)
            
            value <- c(sigmahat2a = sigmahat2a, sigmahat2b = sigmahat2b)
            
            valid <- !any(is.na(value))
            finite <- isTRUE(valid) && all(is.finite(value))
            
            context$.estimatorVariances <- value
            context$.estimatorVariancesStatus <- list(
              available = TRUE,
              valid = valid,
              finite = finite,
              dfValid = dfValid,
              reason = if (isTRUE(valid)) NULL else "invalidValue"
            )
          }
          
          context$.estimatorVariancesComputed <- TRUE
        }
        
        return(context$.estimatorVariances)
      }
      
      context$getEstimatorVariancesStatus <- function() {
        context$getEstimatorVariances()
        return(context$.estimatorVariancesStatus)
      }
      
      context$getScaleA <- function() {
        if (!isTRUE(context$.scaleAComputed)) {
          empiricalQuantities <- context$getEmpiricalQuantities()
          empiricalStatus <- context$getEmpiricalStatus()
          
          if (!isTRUE(empiricalStatus$valid)) {
            context$.scaleA <- NULL
            context$.scaleAStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              reason = "empiricalQuantities"
            )
          } else if (!isTRUE(empiricalStatus$sx2Positive)) {
            context$.scaleA <- NULL
            context$.scaleAStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              reason = "sx2"
            )
          } else {
            value <- sqrt(empiricalQuantities$x2_mean / (context$n * empiricalQuantities$sx2))
            
            valid <- !is.na(value)
            finite <- isTRUE(valid) && is.finite(value)
            
            context$.scaleA <- value
            context$.scaleAStatus <- list(
              available = TRUE,
              valid = valid,
              finite = finite,
              reason = if (isTRUE(valid)) NULL else "invalidValue"
            )
          }
          
          context$.scaleAComputed <- TRUE
        }
        
        return(context$.scaleA)
      }
      
      context$getScaleAStatus <- function() {
        context$getScaleA()
        return(context$.scaleAStatus)
      }
      
      context$getScaleB <- function() {
        if (!isTRUE(context$.scaleBComputed)) {
          empiricalQuantities <- context$getEmpiricalQuantities()
          empiricalStatus <- context$getEmpiricalStatus()
          
          if (!isTRUE(empiricalStatus$valid)) {
            context$.scaleB <- NULL
            context$.scaleBStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              reason = "empiricalQuantities"
            )
          } else if (!isTRUE(empiricalStatus$sx2Positive)) {
            context$.scaleB <- NULL
            context$.scaleBStatus <- list(
              available = FALSE,
              valid = FALSE,
              finite = FALSE,
              reason = "sx2"
            )
          } else {
            value <- 1 / sqrt(context$n * empiricalQuantities$sx2)
            
            valid <- !is.na(value)
            finite <- isTRUE(valid) && is.finite(value)
            
            context$.scaleB <- value
            context$.scaleBStatus <- list(
              available = TRUE,
              valid = valid,
              finite = finite,
              reason = if (isTRUE(valid)) NULL else "invalidValue"
            )
          }
          
          context$.scaleBComputed <- TRUE
        }
        
        return(context$.scaleB)
      }
      
      context$getScaleBStatus <- function() {
        context$getScaleB()
        return(context$.scaleBStatus)
      }
      
      return(context)
    },
    
    .runPointEstimation = function(xname, yname, x, y, n, context) {
      analysisname <- "Point estimation"
      analysisnameshort <- "PointEstimation"
      
      regressionEstimators <- NULL
      sigmahat2 <- NULL
      variancesOfEstimators <- NULL
      
      if (isTRUE(self$options$linReg)) {
        regressionEstimators <- context$getRegressionEstimators()
        
        regressionEstimatorsStatus <- context$getRegressionEstimatorsStatus()
        if (!isTRUE(regressionEstimatorsStatus$valid) && identical(regressionEstimatorsStatus$reason, "sx2")) {
          private$.reportLeastSquaresEstimatorsIssue(xname = xname, analysisname = analysisname)
        }
      }
      
      if (isTRUE(self$options$ser)) {
        sigmahat2 <- context$getSigmahat2()
        
        sigmahat2Status <- context$getSigmahat2Status()
        
        if (!isTRUE(sigmahat2Status$available) && identical(sigmahat2Status$reason, "df")) {
          private$.checkDegreesOfFreedom(
            n = n,
            parameter = "\U03C3\U00B2 estimator",
            parameterKey = "sigmahat2",
            analysisname = analysisname,
            analysisnameshort = analysisnameshort,
            option = TRUE
          )
        } else if (!isTRUE(sigmahat2Status$available) && identical(sigmahat2Status$reason, "empiricalVariance")) {
          private$.reportBravaisPearsonIssue(analysisname = analysisname)
        }
      }
      
      if (isTRUE(self$options$varest)) {
        variancesOfEstimators <- context$getEstimatorVariances()
        
        estimatorVarianceStatus <- context$getEstimatorVariancesStatus()
        
        if (!isTRUE(estimatorVarianceStatus$available) && identical(estimatorVarianceStatus$reason, "df")) {
          private$.checkDegreesOfFreedom(
            n = n,
            parameter = "variances of least-squares estimators",
            parameterKey = "varest",
            analysisname = analysisname,
            analysisnameshort = analysisnameshort,
            option = TRUE
          )
        } else if (!isTRUE(estimatorVarianceStatus$available) && identical(estimatorVarianceStatus$reason, "sx2")) {
          private$.reportLeastSquaresEstimatorsIssue(xname = xname, analysisname = analysisname)
        } else if (!isTRUE(estimatorVarianceStatus$available) && identical(estimatorVarianceStatus$reason, "sigmahat2")) {
          sigmahat2Status <- context$getSigmahat2Status()
          if (!isTRUE(sigmahat2Status$available) && identical(sigmahat2Status$reason, "empiricalVariance")) {
            private$.reportBravaisPearsonIssue(analysisname = analysisname)
          }
        }
      }
      
      # populate results table
      self$results$linreg$addRow(rowKey = 1, values = list(
        a_hat = private$.getNamedValue(regressionEstimators, "a_hat"),
        b_hat = private$.getNamedValue(regressionEstimators, "b_hat"),
        sigmahat = sigmahat2,
        sigmahat_a = private$.getNamedValue(variancesOfEstimators, "sigmahat2a"),
        sigmahat_b = private$.getNamedValue(variancesOfEstimators, "sigmahat2b")
      ))
    },
    
    .checkCommonConfidenceLevel = function() {
      confalpha <- self$options$confalpha
      
      anyConfidenceSelected <- isTRUE(self$options$confibandf) ||
        private$.anySelected(c(self$options$confia,self$options$confib,self$options$confisigma,self$options$confif))
      
      return(private$.checkLevel(confalpha,"Confidence",anyConfidenceSelected))
    },
    
    .runConfidenceIntervals = function(xname,yname,x,y,n,context,confidenceLevelValid) {
      analysisname <- "Confidence intervals"
      analysisnameshort <- "Confidence"
      
      confalpha <- self$options$confalpha
      anySelectedAnalyses <- private$.anySelected(c(self$options$confia,self$options$confib,self$options$confisigma,self$options$confif))
      
      if (!isTRUE(anySelectedAnalyses)) {
        return(invisible(NULL))
      }
      
      if (!isTRUE(confidenceLevelValid)) {
        private$.hideTables("confi")
        return(invisible(NULL))
      }
      
      empiricalQuantities <- context$getEmpiricalQuantities()
      empiricalStatus <- context$getEmpiricalStatus()
      
      if (!isTRUE(empiricalStatus$valid)) {
        private$.reportRegressionBaseQuantitiesIssue(analysisname=analysisname,analysisnameshort=analysisnameshort)
        private$.hideTables("confi")
        return(invisible(NULL))
      }
      
      if (!isTRUE(empiricalStatus$sx2Positive)) {
        private$.reportPositiveEmpiricalVarianceIssue(varname=xname,analysisname=analysisname,analysisnameshort=analysisnameshort)
        private$.hideTables("confi")
        return(invisible(NULL))
      }
      
      x_mean <- empiricalQuantities$x_mean
      x2_mean <- empiricalQuantities$x2_mean
      sx2 <- empiricalQuantities$sx2
      
      needsRegressionEstimators <- private$.anySelected(c(self$options$confia,self$options$confib,self$options$confif))
      
      linreg_results <- NULL
      
      if (isTRUE(needsRegressionEstimators)) {
        linreg_results <- context$getRegressionEstimators()
        linregStatus <- context$getRegressionEstimatorsStatus()
        
        if (!isTRUE(linregStatus$valid)) {
          private$.reportCalculatedQuantityNotAvailable(
            quantityname="least-squares estimators",
            quantitykey="linreg",
            analysisname=analysisname,
            analysisnameshort=analysisnameshort
          )
          private$.hideTables("confi")
          return(invisible(NULL))
        }
      }
      
      needsSigmahat2 <- isTRUE(self$options$confisigma) || isTRUE(self$options$confif) ||
        (isTRUE(self$options$confia) && !isTRUE(self$options$confiaknownsigma)) ||
        (isTRUE(self$options$confib) && !isTRUE(self$options$confibknownsigma))
      
      sigmahat2 <- NULL
      
      if (isTRUE(needsSigmahat2) && isTRUE(n > 2)) {
        sigmahat2 <- context$getSigmahat2()
        sigmahat2Status <- context$getSigmahat2Status()
        
        if (!isTRUE(sigmahat2Status$valid)) {
          private$.reportCalculatedQuantityNotAvailable(
            quantityname="\U03C3\U00B2 estimator",
            quantitykey="sigmahat2",
            analysisname=analysisname,
            analysisnameshort=analysisnameshort
          )
          private$.hideTables("confi")
          return(invisible(NULL))
        }
      }
      
      if (isTRUE(self$options$confia)) {
        result <- private$.runCoefficientConfidenceInterval(
          parameter="a",
          parameterKey="a",
          estimate=private$.getNamedValue(linreg_results, "a_hat"),
          scale=context$getScaleA(),
          sideOption=self$options$confsidea,
          knownSigma=self$options$confiaknownsigma,
          sigma2=self$options$confiaknownsigmavalue,
          sigmahat2=sigmahat2,
          confalpha=confalpha,
          n=n,
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        
        private$.addConfidenceIntervalRow(rowKey=1,parameter="a",conflevel=confalpha,result=result)
      }
      
      if (isTRUE(self$options$confib)) {
        result <- private$.runCoefficientConfidenceInterval(
          parameter="b",
          parameterKey="b",
          estimate=private$.getNamedValue(linreg_results, "b_hat"),
          scale=context$getScaleB(),
          sideOption=self$options$confsideb,
          knownSigma=self$options$confibknownsigma,
          sigma2=self$options$confibknownsigmavalue,
          sigmahat2=sigmahat2,
          confalpha=confalpha,
          n=n,
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        
        private$.addConfidenceIntervalRow(rowKey=2,parameter="b",conflevel=confalpha,result=result)
      }
      
      if (isTRUE(self$options$confisigma)) {
        side <- private$.normalizeSide(self$options$confsidesigma)
        
        if (!private$.checkDegreesOfFreedom(n=n,parameter="\U03C3\U00B2",parameterKey="sigma",analysisname=analysisname,
                                            analysisnameshort=analysisnameshort,option=TRUE)) {
          result <- private$.emptyCIResult()
        } else {
          result <- private$.calculateSigma2ConfidenceInterval(sigmahat2=sigmahat2,n=n,side=side,confalpha=confalpha)
        }
        
        private$.addConfidenceIntervalRow(rowKey=3,parameter="\U03C3\U00B2",conflevel=confalpha,result=result)
      }
      
      if (isTRUE(self$options$confif)) {
        if (!private$.checkDegreesOfFreedom(n=n,parameter="f(x)",parameterKey="fx",analysisname=analysisname,analysisnameshort=analysisnameshort,option=TRUE)) {
          result <- private$.emptyCIResult()
        } else {
          result <- private$.calculateFunctionConfidenceInterval(
            a_hat=private$.getNamedValue(linreg_results, "a_hat"),
            b_hat=private$.getNamedValue(linreg_results, "b_hat"),
            xvalue=self$options$confifvalue,
            x_mean=x_mean,
            sx2=sx2,
            n=n,
            sigmahat2=sigmahat2,
            confalpha=confalpha
          )
        }
        
        private$.addConfidenceIntervalRow(rowKey=4,parameter="f(x)",conflevel=confalpha,result=result)
      }
      
      return(invisible(NULL))
    },
    
    .runConfidenceBandPlot = function(xname,yname,x,y,n,context,confidenceLevelValid) {
      if (!isTRUE(self$options$confibandf)) {
        return(invisible(NULL))
      }
      
      private$.clearConfidenceBandPlot()
      
      if (!isTRUE(confidenceLevelValid)) {
        return(invisible(NULL))
      }
      
      analysisname <- "Confidence band plot"
      analysisnameshort <- "ConfidenceBandPlot"
      conflevel <- self$options$confalpha
      
      if (!private$.checkConfidenceBandPlotDegreesOfFreedom(n=n,analysisname=analysisname,analysisnameshort=analysisnameshort)) {
        return(invisible(NULL))
      }
      
      empiricalQuantities <- context$getEmpiricalQuantities()
      empiricalStatus <- context$getEmpiricalStatus()
      
      if (!isTRUE(empiricalStatus$finite)) {
        private$.reportCalculatedQuantityNotAvailable(
          quantityname="empirical regression quantities",
          quantitykey="empiricalQuantities",
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        return(invisible(NULL))
      }
      
      x_mean <- empiricalQuantities$x_mean
      sx2 <- empiricalQuantities$sx2
      sy2 <- empiricalQuantities$sy2
      
      if (!isTRUE(empiricalStatus$sx2PositiveFinite)) {
        private$.reportConfidenceBandPlotPositiveEmpiricalVarianceIssue(
          varname=xname,
          variableKey="x",
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        return(invisible(NULL))
      }
      
      if (!isTRUE(empiricalStatus$sy2PositiveFinite)) {
        private$.reportConfidenceBandPlotPositiveEmpiricalVarianceIssue(
          varname=yname,
          variableKey="y",
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        return(invisible(NULL))
      }
      
      regressionEstimators <- context$getRegressionEstimators()
      regressionEstimatorsStatus <- context$getRegressionEstimatorsStatus()
      
      if (!isTRUE(regressionEstimatorsStatus$finite)) {
        private$.reportCalculatedQuantityNotAvailable(
          quantityname="least-squares estimators",
          quantitykey="linreg",
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        return(invisible(NULL))
      }
      
      sigmahat2 <- context$getSigmahat2()
      sigmahat2Status <- context$getSigmahat2Status()
      
      if (!isTRUE(sigmahat2Status$finite)) {
        private$.reportCalculatedQuantityNotAvailable(
          quantityname="\U03C3\U00B2 estimator",
          quantitykey="sigmahat2",
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        return(invisible(NULL))
      }
      
      if (!isTRUE(sigmahat2Status$nonNegative)) {
        private$.reportConfidenceBandPlotNonNegativeQuantityIssue(
          quantityname="\U03C3\U00B2 estimator",
          quantitykey="sigmahat2",
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        return(invisible(NULL))
      }
      
      xRange <- private$.calculateConfidenceBandPlotRange(x)
      
      if (!private$.checkConfidenceBandPlotRange(xRange=xRange,analysisname=analysisname,analysisnameshort=analysisnameshort)) {
        return(invisible(NULL))
      }
      
      xgrid <- private$.calculateConfidenceBandPlotGrid(xRange=xRange,gridLength=200L)
      
      if (!private$.checkCalculatedQuantityAvailable(value=xgrid,quantityname="plot grid",quantitykey="plotGrid",
                                                     minLength=2,analysisname=analysisname,analysisnameshort=analysisnameshort,
                                                     requireFinite=TRUE)) {
        return(invisible(NULL))
      }
      
      qValue <- private$.calculateConfidenceBandPlotCriticalValue(conflevel=conflevel,n=n)
      
      if (!private$.checkCalculatedQuantityAvailable(value=qValue,quantityname="F critical value",quantitykey="criticalValue",
                                                     minLength=1,analysisname=analysisname,analysisnameshort=analysisnameshort,
                                                     requireFinite=TRUE)) {
        return(invisible(NULL))
      }
      
      plotState <- private$.calculateConfidenceBandPlotData(
        x=x,
        y=y,
        xgrid=xgrid,
        xname=xname,
        yname=yname,
        a_hat=private$.getNamedValue(regressionEstimators, "a_hat"),
        b_hat=private$.getNamedValue(regressionEstimators, "b_hat"),
        x_mean=x_mean,
        sx2=sx2,
        sigmahat2=sigmahat2,
        n=n,
        qValue=qValue,
        conflevel=conflevel
      )
      
      if (!private$.checkConfidenceBandPlotState(plotState=plotState,analysisname=analysisname,analysisnameshort=analysisnameshort)) {
        return(invisible(NULL))
      }
      
      image <- self$results$confbandplot
      image$setState(plotState)
      image$setVisible(TRUE)
      
      return(invisible(NULL))
    },
    
    .runHypothesisTests = function(xname,yname,x,y,n,context) {
      analysisname <- "Hypothesis tests"
      analysisnameshort <- "Hypothesis"
      
      anySelectedAnalyses <- private$.anySelected(c(self$options$hyptesta,self$options$hyptestb,self$options$hyptestsigma))
      
      if (!isTRUE(anySelectedAnalyses)) {
        return(invisible(NULL))
      }
      
      hypalpha <- self$options$hypalpha
      isValidAlpha <- private$.checkLevel(hypalpha,"Significance",anySelectedAnalyses)
      
      if (!isTRUE(isValidAlpha)) {
        private$.hideTables("hyptest")
        return(invisible(NULL))
      }
      
      empiricalQuantities <- context$getEmpiricalQuantities()
      empiricalStatus <- context$getEmpiricalStatus()
      
      if (!isTRUE(empiricalStatus$valid)) {
        private$.reportRegressionBaseQuantitiesIssue(analysisname=analysisname,analysisnameshort=analysisnameshort)
        private$.hideTables("hyptest")
        return(invisible(NULL))
      }
      
      if (!isTRUE(empiricalStatus$sx2Positive)) {
        private$.reportPositiveEmpiricalVarianceIssue(varname=xname,analysisname=analysisname,analysisnameshort=analysisnameshort)
        private$.hideTables("hyptest")
        return(invisible(NULL))
      }
      
      x2_mean <- empiricalQuantities$x2_mean
      sx2 <- empiricalQuantities$sx2
      
      needsRegressionEstimators <- private$.anySelected(c(self$options$hyptesta,self$options$hyptestb))
      
      linreg_results <- NULL
      
      if (isTRUE(needsRegressionEstimators)) {
        linreg_results <- context$getRegressionEstimators()
        linregStatus <- context$getRegressionEstimatorsStatus()
        
        if (!isTRUE(linregStatus$valid)) {
          private$.reportCalculatedQuantityNotAvailable(
            quantityname="least-squares estimators",
            quantitykey="linreg",
            analysisname=analysisname,
            analysisnameshort=analysisnameshort
          )
          private$.hideTables("hyptest")
          return(invisible(NULL))
        }
      }
      
      needsEstimatorVariances <- (isTRUE(self$options$hyptesta) && !isTRUE(self$options$hyptestaknownsigma)) ||
        (isTRUE(self$options$hyptestb) && !isTRUE(self$options$hyptestbknownsigma))
      
      variancesOfEstimators <- NULL
      
      if (isTRUE(needsEstimatorVariances) && isTRUE(n > 2)) {
        variancesOfEstimators <- context$getEstimatorVariances()
      }
      
      needsSigmahat2 <- isTRUE(self$options$hyptestsigma)
      
      sigmahat2 <- NULL
      
      if (isTRUE(needsSigmahat2) && isTRUE(n > 2)) {
        sigmahat2 <- context$getSigmahat2()
      }
      
      if (isTRUE(self$options$hyptesta)) {
        estimatorVariance <- NULL
        
        if (!is.null(variancesOfEstimators)) {
          estimatorVariance <- private$.getNamedValue(variancesOfEstimators, "sigmahat2a")
        }
        
        result <- private$.runCoefficientHypothesisTest(
          parameter="a",
          parameterKey="a",
          estimate=private$.getNamedValue(linreg_results, "a_hat"),
          nullValue=self$options$testvaluea,
          scale=sqrt(x2_mean/(n*sx2)),
          sideOption=self$options$hyttestsidea,
          knownSigma=self$options$hyptestaknownsigma,
          sigma2=self$options$hyptestaknownsigmavalue,
          estimatorVariance=estimatorVariance,
          hypalpha=hypalpha,
          n=n,
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        
        private$.addHypothesisTestRow(rowKey=1,parameter="a",signlevel=hypalpha,result=result)
      }
      
      if (isTRUE(self$options$hyptestb)) {
        estimatorVariance <- NULL
        
        if (!is.null(variancesOfEstimators)) {
          estimatorVariance <- private$.getNamedValue(variancesOfEstimators, "sigmahat2b")
        }
        
        result <- private$.runCoefficientHypothesisTest(
          parameter="b",
          parameterKey="b",
          estimate=private$.getNamedValue(linreg_results, "b_hat"),
          nullValue=self$options$testvalueb,
          scale=1/sqrt(n*sx2),
          sideOption=self$options$hyttestsideb,
          knownSigma=self$options$hyptestbknownsigma,
          sigma2=self$options$hyptestbknownsigmavalue,
          estimatorVariance=estimatorVariance,
          hypalpha=hypalpha,
          n=n,
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        
        private$.addHypothesisTestRow(rowKey=2,parameter="b",signlevel=hypalpha,result=result)
      }
      
      if (isTRUE(self$options$hyptestsigma)) {
        result <- private$.runSigma2HypothesisTest(
          sigmahat2=sigmahat2,
          sigma2_0=self$options$testvaluesigma,
          sideOption=self$options$hyttestsidesigma,
          hypalpha=hypalpha,
          n=n,
          analysisname=analysisname,
          analysisnameshort=analysisnameshort
        )
        
        private$.addHypothesisTestRow(rowKey=3,parameter="\U03C3\U00B2",signlevel=hypalpha,result=result)
      }
      
      return(invisible(NULL))
    },
    
    # specific helper functions for checking inputs
    .checkLevel = function(alpha,analysisnameshort,option) {
      if(!(alpha <= 0 || alpha >= 1) && option) {
        return(TRUE)
      } else if (option) {
        private$.handleAnalysisIssue(
          name = paste0("SignLevel", analysisnameshort, "_", alpha),
          message = paste0(analysisnameshort, " level must be between 0 and 1!"),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkIfInputValuePositive = function(valuename,value,analysisname,analysisnameshort,option=TRUE) {
      if(isTRUE(option) && !isTRUE(value > 0)) {
        private$.handleAnalysisIssue(
          name = paste0("valueNotPositive", analysisnameshort, "_", valuename),
          message = paste0(analysisname, ": Analysis only applicable for positive input value ",valuename,", but ",valuename,"  = ", value, "."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkAtLeastTwoDistinctValues = function(x1name,x2name,x1,x2) {
      x1Valid <- private$.countDistinctNonMissingValues(x1) >= 2
      x2Valid <- private$.countDistinctNonMissingValues(x2) >= 2
      
      if (!isTRUE(x1Valid) || !isTRUE(x2Valid)) {
        private$.handleAnalysisIssue(
          name = paste0("notEnoughDistinctValues","_",x1name,"_",x2name),
          message = paste0("Each input variable must contain at least two distinct values after removing rows with missing values, but this is not the case for these inputs."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkRegressionBaseQuantities = function(empiricalQuantities,analysisname,analysisnameshort) {
      requiredNames <- c("x_mean","x2_mean","y_mean","sx2","sy2","sxy")
      
      if (is.null(empiricalQuantities) ||
          !all(requiredNames %in% names(empiricalQuantities)) ||
          any(is.na(unlist(empiricalQuantities[requiredNames], use.names = FALSE)))) {
        private$.handleAnalysisIssue(
          name = paste0("regressionQuantitiesNotAvailable_", analysisnameshort),
          message = paste0(analysisname, ": Required empirical quantities for the regression could not be calculated."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkPositiveEmpiricalVariance = function(varname,variance,analysisname,analysisnameshort) {
      if (!isTRUE(variance > 0)) {
        private$.handleAnalysisIssue(
          name = paste0("noPositiveEmpiricalVariance_", analysisnameshort, "_", varname),
          message = paste0(analysisname, ": Analysis cannot be calculated because the empirical variance of ", varname, " is not positive."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkDegreesOfFreedom = function(n,parameter,parameterKey,analysisname,analysisnameshort,option=TRUE) {
      if (isTRUE(option) && !isTRUE(n > 2)) {
        private$.handleAnalysisIssue(
          name = paste0("notEnoughDegreesOfFreedom_", analysisnameshort, "_", parameterKey),
          message = paste0(analysisname, " for ", parameter, ": Analysis only applicable for n > 2, but n = ", n, "."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkCalculatedQuantityAvailable = function(value,quantityname,quantitykey,minLength,analysisname,analysisnameshort,requireFinite=FALSE) {
      if (is.null(value) || length(value) < minLength) {
        private$.handleAnalysisIssue(
          name = paste0("quantityNotAvailable_", analysisnameshort, "_", quantitykey),
          message = paste0(analysisname, ": Required quantity '", quantityname, "' could not be calculated."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      
      requiredValues <- value[seq_len(minLength)]
      if (is.list(requiredValues)) {
        requiredValues <- unlist(requiredValues, use.names = FALSE)
      }
      
      invalid <- any(is.na(requiredValues))
      
      if (isTRUE(requireFinite)) {
        numericValues <- suppressWarnings(as.numeric(requiredValues))
        invalid <- invalid || any(is.na(numericValues)) || any(!is.finite(numericValues))
      }
      
      if (isTRUE(invalid)) {
        private$.handleAnalysisIssue(
          name = paste0("quantityNotAvailable_", analysisnameshort, "_", quantitykey),
          message = paste0(analysisname, ": Required quantity '", quantityname, "' could not be calculated."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkConfidenceBandPlotDegreesOfFreedom = function(n,analysisname,analysisnameshort) {
      valid <- isTRUE( is.numeric(n) && length(n) == 1L && !is.na(n) && is.finite(n) && n > 2)
      
      if (!isTRUE(valid)) {
        private$.handleAnalysisIssue(
          name = paste0("notEnoughDegreesOfFreedom_", analysisnameshort),
          message = paste0(analysisname, ": Analysis only applicable for n > 2, but n = ", n, "."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkConfidenceBandPlotPositiveEmpiricalVariance = function(varname,variance,variableKey,analysisname,analysisnameshort) {
      valid <- isTRUE(is.numeric(variance) && length(variance) == 1L && !is.na(variance) && is.finite(variance) && variance > 0)
      
      if (!isTRUE(valid)) {
        private$.handleAnalysisIssue(
          name = paste0("noPositiveEmpiricalVariance_", analysisnameshort, "_", variableKey),
          message = paste0(analysisname, ": Confidence band cannot be calculated because the empirical variance of ", varname, " is not positive."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkConfidenceBandPlotNonNegativeQuantity = function(value,quantityname,quantitykey,analysisname,analysisnameshort) {
      valid <- isTRUE( is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value) && value >= 0)
      
      if (!isTRUE(valid)) {
        private$.handleAnalysisIssue(
          name = paste0("quantityNegative_", analysisnameshort, "_", quantitykey),
          message = paste0(analysisname, ": Required quantity '", quantityname, "' must be non-negative."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .calculateConfidenceBandPlotRange = function(x) {
      return(c(min = min(x),max = max(x)))
    },
    
    .checkConfidenceBandPlotRange = function(xRange,analysisname,analysisnameshort) {
      valid <- !is.null(xRange) && length(xRange) >= 2 && !any(is.na(xRange[1:2])) && all(is.finite(xRange[1:2])) && isTRUE(xRange[1] < xRange[2])
      if (!isTRUE(valid)) {
        private$.handleAnalysisIssue(
          name = paste0("xRangeNotAvailable_", analysisnameshort),
          message = paste0(analysisname, ": Required x-range for the confidence band plot is not available."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkConfidenceBandPlotState = function(plotState,analysisname,analysisnameshort) {
      hasRequiredStructure <- !is.null(plotState) && !is.null(plotState$points) && !is.null(plotState$band) && 
        all(c("x", "y") %in% names(plotState$points)) && all(c("x", "fit", "lower", "upper") %in% names(plotState$band))
      
      if (!isTRUE(hasRequiredStructure)) {
        private$.handleAnalysisIssue(
          name = paste0("invalidPlotData_", analysisnameshort),
          message = paste0(analysisname, ": Final plot data for the confidence band could not be created."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      
      hasRows <- isTRUE(nrow(plotState$points) > 0) && isTRUE(nrow(plotState$band) >= 2)
      if (!isTRUE(hasRows)) {
        private$.handleAnalysisIssue(
          name = paste0("invalidPlotData_", analysisnameshort),
          message = paste0(analysisname, ": Final plot data for the confidence band could not be created."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      
      plotValues <- c(plotState$points$x,plotState$points$y,plotState$band$x,plotState$band$fit,plotState$band$lower,plotState$band$upper)
      
      validValues <- length(plotValues) > 0 && !any(is.na(plotValues)) && all(is.finite(plotValues))
      if (!isTRUE(validValues)) {
        private$.handleAnalysisIssue(
          name = paste0("invalidPlotData_", analysisnameshort),
          message = paste0(analysisname, ": Final plot data for the confidence band contain invalid values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      
      return(TRUE)
    },
    
    # notice/report helper functions
    .reportRegressionBaseQuantitiesIssue = function(analysisname,analysisnameshort) {
      private$.handleAnalysisIssue(
        name = paste0("regressionQuantitiesNotAvailable_", analysisnameshort),
        message = paste0(analysisname, ": Required empirical quantities for the regression could not be calculated."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    .reportPositiveEmpiricalVarianceIssue = function(varname,analysisname,analysisnameshort) {
      private$.handleAnalysisIssue(
        name = paste0("noPositiveEmpiricalVariance_", analysisnameshort, "_", varname),
        message = paste0(analysisname, ": Analysis cannot be calculated because the empirical variance of ", varname, " is not positive."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    .reportCalculatedQuantityNotAvailable = function(quantityname,quantitykey,analysisname,analysisnameshort) {
      private$.handleAnalysisIssue(
        name = paste0("quantityNotAvailable_", analysisnameshort, "_", quantitykey),
        message = paste0(analysisname, ": Required quantity '", quantityname, "' could not be calculated."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    .reportConfidenceBandPlotPositiveEmpiricalVarianceIssue = function(varname,variableKey,analysisname,analysisnameshort) {
      private$.handleAnalysisIssue(
        name = paste0("noPositiveEmpiricalVariance_", analysisnameshort, "_", variableKey),
        message = paste0(analysisname, ": Confidence band cannot be calculated because the empirical variance of ", varname, " is not positive."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    .reportConfidenceBandPlotNonNegativeQuantityIssue = function(quantityname,quantitykey,analysisname,analysisnameshort) {
      private$.handleAnalysisIssue(
        name = paste0("quantityNegative_", analysisnameshort, "_", quantitykey),
        message = paste0(analysisname, ": Required quantity '", quantityname, "' must be non-negative."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    .reportLeastSquaresEstimatorsIssue = function(xname,analysisname) {
      private$.handleAnalysisIssue(
        name = "noPositiveEmpiricalVariance_x",
        message = paste0(analysisname,": Least squared estimators for these inputs cannot be calculated because the empirical variance of ", xname," is not positive."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    .reportBravaisPearsonIssue = function(analysisname) {
      private$.handleAnalysisIssue(
        name = "noPositiveEmpiricalVariance_y",
        message = paste0(analysisname,": Bravais-Pearson Correlation Coefficient and other quantities cannot be calculated because a empirical variance  is not positive."),
        type = jmvcore::NoticeType$STRONG_WARNING)
      return(invisible(NULL))
    },
    
    # general helper functions 
    .clearConfidenceBandPlot = function() {
      image <- self$results$confbandplot
      if (!is.null(image)) {
        image$setState(NULL)
        image$setVisible(FALSE)
      }
      return(invisible(NULL))
    },
    
    .handleAnalysisIssue = function(name, message, type=jmvcore::NoticeType$WARNING) {
      private$.createNotice(name, message, type=type)
      return(invisible(NULL))
    },
    
    .createNotice=function(name, message, type=jmvcore::NoticeType$WARNING){
      notice <- jmvcore::Notice$new(options=self$options, name=name, type=type)
      notice$setContent(message)
      self$results$insert(private$.noticeInsertPosition, notice)
      private$.noticeInsertPosition <- private$.noticeInsertPosition + 1L
    },
    
    .toNumeric=function(var){
      if(!is.numeric(self$data[[var]]) && !(var %in% private$.transformedVars)){
        private$.transformedVars = c(private$.transformedVars, var)
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
    
    .hideTables = function(tableNames = NULL) {
      if (is.null(tableNames)) {
        return(invisible(NULL))
      }
      for (tableName in tableNames) {
        table <- self$results[[tableName]]
        table$setVisible(FALSE)
      }
      return(invisible(NULL))
    },
    
    .showFinalNotices = function() {
      if(length(private$.missingvaluesVars)>0) {
        private$.createNotice(paste0("missingValues_", paste(private$.missingvaluesVars, collapse = ", ")),
                              paste0("Variable(s) '", paste(private$.missingvaluesVars, collapse = ", "), "' contain(s) missing value(s). Analyses will ignore missing values."),
                              type = jmvcore::NoticeType$INFO)
      }
      
      if(length(private$.transformedVars)>0) {
        private$.createNotice(paste0("variablesNotNumeric_", paste(private$.transformedVars, collapse = ", ")), 
                              paste0("Treating variable(s) '", paste(private$.transformedVars, collapse = ", "), "' as numeric."),
                              type=jmvcore::NoticeType$INFO)
      }
    },
    
    .selectedTableNames = function() {
      tableNames <- c()
      if (isTRUE(self$options$linReg || self$options$ser || self$options$varest)) {
        tableNames <- c(tableNames, "linreg")
      }
      if (isTRUE(self$options$confia || self$options$confib || self$options$confisigma || self$options$confif)) {
        tableNames <- c(tableNames, "confi")
      }
      if (isTRUE(self$options$hyptesta || self$options$hyptestb || self$options$hyptestsigma)) {
        tableNames <- c(tableNames, "hyptest")
      }
      tableNames
    },
    
    .anySelected = function(options) {
      any(as.logical(options))
    },
    
    .countDistinctNonMissingValues = function(x) {
      x <- x[!is.na(x)]
      return(length(unique(x)))
    },
    
    .normalizeSide = function(side) {
      if (startsWith(side, "two")) {
        return("two")
      }
      if (startsWith(side, "lower")) {
        return("lower")
      }
      if (startsWith(side, "upper")) {
        return("upper")
      }
      stop(paste0("Unknown test side option: ", side))
    },
    
    .getNamedValue = function(value, name) {
      if (is.null(value) || is.null(names(value)) || !(name %in% names(value))) {
        return(NULL)
      }
      return(value[[name]])
    },
    
    .emptyCIResult = function() {
      return(list(lower = "",upper = "",success = FALSE))
    },
    
    .ciResult = function(lower,upper,success=TRUE) {
      return(list(lower = lower,upper = upper,success = success))
    },
    
    .runCoefficientConfidenceInterval = function(parameter,parameterKey,estimate,scale,sideOption,knownSigma,sigma2,sigmahat2,confalpha,n,analysisname,analysisnameshort) {
      side <- private$.normalizeSide(sideOption)
      
      if (isTRUE(knownSigma)) {
        sigma2Valid <- private$.checkIfInputValuePositive(valuename="\U03C3\U00B2",value=sigma2,analysisname=paste0(analysisname, " for ", parameter),
                                                          analysisnameshort=paste0(analysisnameshort, "_", parameterKey),option=TRUE)
        if (!isTRUE(sigma2Valid)) {
          return(private$.emptyCIResult())
        }
        return(private$.calculateCoefficientConfidenceInterval(estimate=estimate,scale=scale,side=side,confalpha=confalpha,
                                                               variance=sigma2,distribution="normal",df=NULL))
      }
      
      if (!private$.checkDegreesOfFreedom(n=n,parameter=parameter,parameterKey=parameterKey,
                                          analysisname=analysisname,analysisnameshort=analysisnameshort,option=TRUE)) {
        return(private$.emptyCIResult())
      }
      
      return(private$.calculateCoefficientConfidenceInterval(estimate=estimate,scale=scale,side=side,confalpha=confalpha,
                                                             variance=sigmahat2,distribution="t",df=n-2))
    },
    
    .addConfidenceIntervalRow = function(rowKey,parameter,conflevel,result) {
      lower <- ""
      upper <- ""
      
      if (!is.null(result) && isTRUE(result$success)) {
        lower <- result$lower
        upper <- result$upper
      }
      self$results$confi$addRow(rowKey=rowKey, values=list(parameter=parameter,conflevel=conflevel,lower=lower,upper=upper))
      return(invisible(NULL))
    },
    
    .addHypothesisTestRow = function(rowKey,parameter,signlevel,result) {
      statistic <- ""
      c_value <- ""
      d_value <- ""
      decision <- ""
      
      if (!is.null(result) && isTRUE(result$success)) {
        statistic <- result$statistic
        c_value <- result$c
        d_value <- result$d
        decision <- result$decision
      }
      
      self$results$hyptest$addRow(rowKey=rowKey, values=list(parameter=parameter,signlevel=signlevel,statistic=statistic,
                                                             c=c_value,d=d_value,decision=decision))
      
      return(invisible(NULL))
    },
    
    # helper functions for calculations
    .calculateEmpiricalQuantities = function(xname,yname,x,y,analysisname) {
      x_mean <- mean(x)
      y_mean <- mean(y)
      x2_mean <- mean(x*x)
      sx2 <- x2_mean - x_mean^2
      sy2 <- mean(y*y) - y_mean^2
      sxy <- mean(x*y) - x_mean * y_mean
      
      return(list(
        x_mean = x_mean,
        x2_mean = x2_mean,
        y_mean = y_mean,
        sx2 = sx2,
        sy2 = sy2,
        sxy = sxy
      ))
    },
    
    .calculateLinearRegressionPointEstimators = function(xname,yname,x,y,analysisname,option) {
      if (!isTRUE(option)) {
        return(invisible(NULL))
      }
      
      empiricalVariances <- private$.calculateEmpiricalQuantities(xname,yname,x,y,analysisname)
      x_mean <- empiricalVariances$x_mean
      y_mean <- empiricalVariances$y_mean
      sx2 <- empiricalVariances$sx2
      sxy <- empiricalVariances$sxy
      
      if(!isTRUE(sx2 > 0)) {
        return(invisible(NULL))
      }
      
      b_hat <- sxy/sx2
      a_hat <- y_mean - b_hat * x_mean
      
      return(c(a_hat = a_hat, b_hat = b_hat))
    },
    
    .calculateBravaisPearson = function(sx2,sy2,sxy,analysisname,option) {
      if (!isTRUE(option)) {
        return(NULL)
      }
      
      if(!isTRUE(sx2 > 0) || !isTRUE(sy2 > 0)) {
        return(NULL)
      }
      
      rxy <- sxy/sqrt(sx2*sy2) 
      
      return(rxy)
    },
    
    .calculateStandardError = function(xname,yname,x,y,n,analysisname,option) {
      if (!isTRUE(option)) {
        return(NULL)
      }
      
      empiricalVariances <- private$.calculateEmpiricalQuantities(xname,yname,x,y,analysisname)
      sx2 <- empiricalVariances$sx2
      sy2 <- empiricalVariances$sy2
      sxy <- empiricalVariances$sxy
      
      rxy <- private$.calculateBravaisPearson(sx2,sy2,sxy,analysisname,option)
      
      if(is.null(rxy)) {
        return(NULL)
      }
      
      sigmahat2 <- n/(n-2)*sy2*(1-rxy^2)
      
      return(sigmahat2)
    },
    
    .calculateVarianceOfLeastSquaresEstimators = function(xname,yname,x,y,n,analysisname,option) {
      if (!isTRUE(option)) {
        return(NULL)
      }
      
      empiricalVariances <- private$.calculateEmpiricalQuantities(xname,yname,x,y,analysisname)
      x2_mean <- empiricalVariances$x2_mean
      sx2 <- empiricalVariances$sx2
      
      sigmahat2 <- private$.calculateStandardError(xname,yname,x,y,n,analysisname,option)
      
      if(is.null(sigmahat2)) {
        return(c(sigmahat2a = "",sigmahat2b = ""))
      }
      
      sigmahat2a <- x2_mean/(n*sx2)*sigmahat2  
      sigmahat2b <- sigmahat2/(n*sx2)
      
      return(c(sigmahat2a = sigmahat2a,sigmahat2b = sigmahat2b))
    },
    
    .calculateCoefficientConfidenceInterval = function(estimate,scale,side,confalpha,variance,distribution,df=NULL) {
      if (distribution == "normal") {
        criticalTwoSided <- qnorm(1-(1-confalpha)/2)
        criticalOneSided <- qnorm(confalpha)
      } else {
        criticalTwoSided <- qt(1-(1-confalpha)/2, df)
        criticalOneSided <- qt(confalpha, df)
      }
      
      standardError <- sqrt(variance) * scale
      
      if (side == "two") {
        lower <- estimate - criticalTwoSided * standardError
        upper <- estimate + criticalTwoSided * standardError
      } else if (side == "lower") {
        lower <- -Inf
        upper <- estimate + criticalOneSided * standardError
      } else if (side == "upper") {
        lower <- estimate - criticalOneSided * standardError
        upper <- Inf
      }
      return(private$.ciResult(lower=lower,upper=upper,success=TRUE))
    },
    
    .calculateSigma2ConfidenceInterval = function(sigmahat2,n,side,confalpha) {
      if (side == "two") {
        lower <- (n-2)*sigmahat2/qchisq(1-(1-confalpha)/2, n-2)
        upper <- (n-2)*sigmahat2/qchisq((1-confalpha)/2, n-2)
      } else if (side == "lower") {
        lower <- 0
        upper <- (n-2)*sigmahat2/qchisq(confalpha, n-2)
      } else if (side == "upper") {
        lower <- (n-2)*sigmahat2/qchisq(confalpha, n-2)
        upper <- Inf
      }
      return(private$.ciResult(lower=lower,upper=upper,success=TRUE))
    },
    
    .calculateFunctionConfidenceInterval = function(a_hat,b_hat,xvalue,x_mean,sx2,n,sigmahat2,confalpha) {
      fdach <- a_hat + b_hat*xvalue
      lower <- fdach - qt(1-(1-confalpha)/2,n-2)*(1/n+(xvalue-x_mean)^2/(n*sx2))*sqrt(sigmahat2)
      upper <- fdach + qt(1-(1-confalpha)/2,n-2)*(1/n+(xvalue-x_mean)^2/(n*sx2))*sqrt(sigmahat2)
      return(private$.ciResult(lower=lower,upper=upper,success=TRUE))
    },
    
    .calculateConfidenceBandPlotGrid = function(xRange,gridLength=200L) {
      return(seq(from = xRange[1],to = xRange[2],length.out = gridLength))
    },
    
    .calculateConfidenceBandPlotCriticalValue = function(conflevel,n) {
      return(qf(conflevel, 2, n - 2))
    },
    
    .calculateConfidenceBandPlotData = function(x,y,xgrid,xname,yname,a_hat,b_hat,x_mean,sx2,sigmahat2,n,qValue,conflevel) {
      fit <- a_hat + xgrid * b_hat
      
      bandWidth <- sqrt(sigmahat2) * sqrt(2 * qValue * (1 / n + (x_mean - xgrid)^2 / (n * sx2)))
      
      lower <- fit - bandWidth
      upper <- fit + bandWidth
      
      return(list(
        points = data.frame(x = x,y = y),
        band = data.frame(x = xgrid,fit = fit,lower = lower,upper = upper),
        xname = xname,yname = yname,conflevel = conflevel))
    },
    
    .runCoefficientHypothesisTest = function(parameter,parameterKey,estimate,nullValue,scale,sideOption,knownSigma,sigma2,
                                             estimatorVariance,hypalpha,n,analysisname,analysisnameshort) {
      side <- private$.normalizeSide(sideOption)
      
      if (isTRUE(knownSigma)) {
        sigma2Valid <- private$.checkIfInputValuePositive(valuename="\U03C3\U00B2",value=sigma2,
                                                          analysisname=paste0(analysisname, " for ", parameter),
                                                          analysisnameshort=paste0(analysisnameshort, "_", parameterKey),
                                                          option=TRUE)
        
        if (!isTRUE(sigma2Valid)) {
          return(list(statistic="",c="",d="",decision="",success=FALSE))
        }
        
        return(private$.calculateCoefficientHypothesisTest(estimate=estimate,nullValue=nullValue,scale=scale,
                                                           variance=sigma2,side=side,hypalpha=hypalpha,
                                                           distribution="normal",df=NULL))
      }
      
      if (!private$.checkDegreesOfFreedom(n=n,parameter=parameter,parameterKey=parameterKey,
                                          analysisname=analysisname,analysisnameshort=analysisnameshort,option=TRUE)) {
        return(list(statistic="",c="",d="",decision="",success=FALSE))
      }
      
      if (!private$.checkCalculatedQuantityAvailable(value=estimatorVariance,
                                                     quantityname=paste0("variance of estimator for ", parameter),
                                                     quantitykey=paste0("estimatorVariance_", parameterKey),
                                                     minLength=1,
                                                     analysisname=paste0(analysisname, " for ", parameter),
                                                     analysisnameshort=analysisnameshort)) {
        return(list(statistic="",c="",d="",decision="",success=FALSE))
      }
      
      return(private$.calculateCoefficientHypothesisTest(estimate=estimate,nullValue=nullValue,scale=1,
                                                         variance=estimatorVariance,side=side,hypalpha=hypalpha,
                                                         distribution="t",df=n-2))
    },
    
    .runSigma2HypothesisTest = function(sigmahat2,sigma2_0,sideOption,hypalpha,n,analysisname,analysisnameshort) {
      side <- private$.normalizeSide(sideOption)
      
      sigma2Valid <- private$.checkIfInputValuePositive(valuename="\U03C3\U2080\U00B2",value=sigma2_0,
                                                        analysisname=paste0(analysisname, " for \U03C3\U00B2"),
                                                        analysisnameshort=paste0(analysisnameshort, "_sigma"),
                                                        option=TRUE)
      
      if (!isTRUE(sigma2Valid)) {
        return(list(statistic="",c="",d="",decision="",success=FALSE))
      }
      
      if (!private$.checkDegreesOfFreedom(n=n,parameter="\U03C3\U00B2",parameterKey="sigma",
                                          analysisname=analysisname,analysisnameshort=analysisnameshort,option=TRUE)) {
        return(list(statistic="",c="",d="",decision="",success=FALSE))
      }
      
      if (!private$.checkCalculatedQuantityAvailable(value=sigmahat2,
                                                     quantityname="\U03C3\U00B2 estimator",
                                                     quantitykey="sigmahat2_sigma",
                                                     minLength=1,
                                                     analysisname=paste0(analysisname, " for \U03C3\U00B2"),
                                                     analysisnameshort=analysisnameshort)) {
        return(list(statistic="",c="",d="",decision="",success=FALSE))
      }
      
      return(private$.calculateSigma2HypothesisTest(sigmahat2=sigmahat2,sigma2_0=sigma2_0,
                                                    n=n,side=side,hypalpha=hypalpha))
    },
    
    .calculateCoefficientHypothesisTest = function(estimate,nullValue,scale,variance,side,hypalpha,distribution,df=NULL) {
      statistic_value <- (estimate-nullValue)/(sqrt(variance)*scale)
      
      criticalValues <- private$.calculateCoefficientHypothesisCriticalValues(side=side,hypalpha=hypalpha,
                                                                              distribution=distribution,df=df)
      
      return(list(
        statistic=statistic_value,
        c=criticalValues$c,
        d=criticalValues$d,
        decision=criticalValues$decision,
        success=TRUE
      ))
    },
    
    .calculateCoefficientHypothesisCriticalValues = function(side,hypalpha,distribution,df=NULL) {
      d_value <- ""
      
      if (distribution == "normal") {
        if (side == "two") {
          c_value <- qnorm(1-hypalpha/2)
          decision <- "Reject H\U2080 if |Statistic| > c"
        } else if (side == "lower") {
          c_value <- -qnorm(1-hypalpha)
          decision <- "Reject H\U2080 if Statistic < c"
        } else if (side == "upper") {
          c_value <- qnorm(1-hypalpha)
          decision <- "Reject H\U2080 if Statistic > c"
        }
      } else {
        if (side == "two") {
          c_value <- qt(1-hypalpha/2, df)
          decision <- "Reject H\U2080 if |Statistic| > c"
        } else if (side == "lower") {
          c_value <- -qt(1-hypalpha, df)
          decision <- "Reject H\2080 if Statistic < c"
        } else if (side == "upper") {
          c_value <- qt(1-hypalpha, df)
          decision <- "Reject H\U2080 if Statistic > c"
        }
      }
      
      return(list(c=c_value,d=d_value,decision=decision))
    },
    
    .calculateSigma2HypothesisTest = function(sigmahat2,sigma2_0,n,side,hypalpha) {
      statistic_value <- (n-2)*sigmahat2/sigma2_0
      
      d_value <- ""
      
      if (side == "two") {
        c_value <- qchisq(hypalpha/2,n-2)
        d_value <- qchisq(1-hypalpha/2,n-2)
        decision <- "Reject H\U2080 if Statistic < c or Statistic > d"
      } else if (side == "lower") {
        c_value <- qchisq(hypalpha,n-2)
        decision <- "Reject H\U2080 if Statistic < c"
      } else if (side == "upper") {
        c_value <- qchisq(1-hypalpha,n-2)
        decision <- "Reject H\U2080 if Statistic > c"
      }
      
      return(list(statistic=statistic_value,c=c_value,d=d_value,decision=decision,success=TRUE))
    },
    
    # plots
    .confbandplot=function(image, ggtheme, theme, ...){
      state <- image$state
      
      if (is.null(state) || is.null(state$points) || is.null(state$band) ||
          !all(c("x", "y") %in% names(state$points)) || !all(c("x", "fit", "lower", "upper") %in% names(state$band))) {
        return(FALSE)
      }
      
      CONFBANDPLOT <- ggplot() +
        geom_point(data = state$points,aes(x = x, y = y, color = "Observed data"),alpha = 0.8) +
        geom_ribbon(data = state$band,aes(x = x, ymin = lower, ymax = upper, fill = "Confidence band"),alpha = 0.15, colour = NA) +
        geom_line(data = state$band,aes(x = x, y = lower, color = "Confidence band boundaries"),size = 0.8) +
        geom_line(data = state$band,aes(x = x, y = upper, color = "Confidence band boundaries"),size = 0.8) +
        geom_line(data = state$band,aes(x = x, y = fit, color = "Regression function"),size = 0.9) +
        labs(x = state$xname,y = state$yname,title = "Confidence band for the regression function") +
        ggtheme +
        scale_color_manual(name = "Legend",
                           breaks = c("Observed data","Regression function","Confidence band boundaries"),
                           values = c("Observed data" = "grey30","Regression function" = "blue","Confidence band boundaries" = "red")) +
        scale_fill_manual(name = " ",breaks = c("Confidence band"),values = c("Confidence band" = "red3")) +
        guides(color = guide_legend(order = 1),fill = guide_legend(order = 2,override.aes = list(alpha = 0.15))) 
      
      print(CONFBANDPLOT)
      TRUE
    })
)