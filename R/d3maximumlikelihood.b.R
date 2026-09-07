# This file is a generated template, your changes will not be overwritten

d3maximumlikelihoodClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "d3maximumlikelihoodClass",
  inherit = d3maximumlikelihoodBase,
  private = list(
    
    .transformedVars = c(),
    .missingvaluesVars = c(),
    .noticeInsertPosition = 1L,
    
    .run = function() {
      private$.noticeInsertPosition <- 1L
      data <- self$data
      variable <- self$options$var
      
      if (is.null(variable)) {
        return()
      }
      
      if (!jmvcore::canBeNumeric(data[[variable]])) {
        message <- paste0("Analyses can only be computed for numeric data '",variable,"'.")
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("DATA", variable, "NOT_NUMERIC"),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = private$.selectedTableNames()
        ))
      }
      
      # input variable is converted to numeric
      x <- private$.toNumericWithNotice(variable, createNotice = FALSE)
      x <- private$.checkMissingValues(variable, x, createNotice = FALSE)
      
      if (length(unique(x)) < 2) {
        message <- paste0("Variable '",variable,"' does not contain enough valid non-missing values. Analyses require at least two distinct non-missing numeric values.")
        private$.hideSelectedTables()
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("DATA", variable, "NOT_ENOUGH"),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = private$.selectedTableNames()
        ))
      }
      
      # run analyses
      private$.runBinomial(x, variable)
      private$.runGeometric(x, variable)
      private$.runPoisson(x, variable)
      private$.runNormal(x, variable)
      private$.runExponential(x, variable)
      private$.runUniform(x, variable)
      private$.runPareto(x, variable)
      private$.runPower(x, variable)
      
      # notices of conversion and/or missing values are only give if there is enough data left for the analyses
      x <- private$.toNumericWithNotice(variable, createNotice = TRUE)
      x <- private$.checkMissingValues(variable, x, createNotice = TRUE)
    },
    
    .runBinomial = function(x, variable) {
      if (!isTRUE(self$options$bindistr)) {
        return(invisible(NULL))
      }
      if (!all(x == 0 | x == 1)) {
        message <- paste0("You chose Binomial distribution, but variable '",variable,"' has values other than 0 and 1.")
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("BIN", variable),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEbin"
        ))
      }
      self$results$MLEbin$setRow(rowNo = 1, values = list(
        par = "p",
        MLE = mean(x)
      ))
      
      invisible(NULL)
    },
    
    .runGeometric = function(x, variable) {
      if (!isTRUE(self$options$geodistr)) {
        return(invisible(NULL))
      }
      if (!(all(ceiling(x) == floor(x)) && all(x >= 0))) {
        message <- paste0("You chose Geometric distribution, but variable '",variable,"' has values that are not discrete and/or not non-negative.")
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("GEO", variable),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEgeo"
        ))
      }
      self$results$MLEgeo$setRow(rowNo = 1, values = list(
        par = "p",
        MLE = 1 / (1 + mean(x))
      ))
      
      invisible(NULL)
    },
    
    .runPoisson = function(x, variable) {
      if (!isTRUE(self$options$podistr)) {
        return(invisible(NULL))
      }
      if (!(all(ceiling(x) == floor(x)) && all(x >= 0))) {
        message <- paste0("You chose Poisson distribution, but variable '",variable,"' has values that are not discrete and/or not non-negative.")
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("POI", variable),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEpo"
        ))
      }
      self$results$MLEpo$setRow(rowNo = 1, values = list(
        par = "\u03BB",
        MLE = mean(x)
      ))
      
      invisible(NULL)
    },
    
    .runNormal = function(x, variable) {
      if (!isTRUE(self$options$Ndistr)) {
        return(invisible(NULL))
      }
      
      tabnorm <- self$results$MLEnorm
      
      setVarianceNote <- function(samplevar) {
        tabnorm$setNote("key",
          paste("The MLE for \u03C3\u00B2 is equal to (n-1)/n times its sample variance. In particular,",
              "the MLE of \u03C3\u00B2 is not an unbiased estimator while the sample variance is an unbiased",
              "estimator for \u03C3\u00B2 and has value",
              round(samplevar, digits = 4))
        )
      }
      
      meanKnown <- isTRUE(self$options$mean)
      varianceKnown <- isTRUE(self$options$variance)
      
      if (meanKnown && varianceKnown) {
        message <- "All parameters of the Normal distribution are known, there is no parameter left to estimate."
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("NORM", variable, "ALL_KNOWN"),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEnorm"
        ))
      }
      
      if (meanKnown) {
        theta <- "\u03C3\u00B2"
        mle <- mean((x - self$options$mu)^2)
        samplevar <- length(x) / (length(x) - 1) * mle
        
        tabnorm$setRow(rowNo = 1, values = list(
          par = theta,
          MLE = mle
        ))
        setVarianceNote(samplevar)
        return(invisible(NULL))
      }
      
      if (varianceKnown) {
        theta <- "\u03BC"
        mle <- mean(x)
        
        tabnorm$setRow(rowNo = 1, values = list(
          par = theta,
          MLE = mle
        ))
        return(invisible(NULL))
      }
      
      theta1 <- "\u03BC"
      theta2 <- "\u03C3\u00B2"
      mle1 <- mean(x)
      mle2 <- mean((x - mle1)^2)
      samplevar <- length(x) / (length(x) - 1) * mle2
      
      tabnorm$setRow(rowNo = 1, values = list(
        par = theta1,
        MLE = mle1
      ))
      
      tabnorm$addRow(rowKey = 2, values = list(
        par = theta2,
        MLE = mle2
      ))
      
      setVarianceNote(samplevar)
      invisible(NULL)
    },
    
    .runExponential = function(x, variable) {
      if (!isTRUE(self$options$expdistr)) {
        return(invisible(NULL))
      }
      if (!(all(x >= 0) && mean(x) > 0)) {
        message <- paste0("You chose Exponential distribution, but variable '",variable,"' has values that are not non-negative.")
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("EXP", variable),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEexp"
        ))
      }
      
      if (isTRUE(self$options$expparameter == "lambda")) {
        theta <- "\u03BB"
        mle <- 1 / mean(x) 
      } else if (isTRUE(self$options$expparameter == "oneoverlambda")) {
        theta <- "\u03B2"
        mle <- mean(x)
       } else {
        private$.hideTable("MLEexp")
        return(invisible(NULL))
      }
      
      self$results$MLEexp$setRow(rowNo = 1, values = list(
        par = theta,
        MLE = mle
      ))
      
      invisible(NULL)
    },
    
    .runUniform = function(x, variable) {
      if (!isTRUE(self$options$Udistr)) {
        return(invisible(NULL))
      }
      tabunif <- self$results$MLEunif
      aKnown <- isTRUE(self$options$unifa)
      bKnown <- isTRUE(self$options$unifb)
      
      if (aKnown && bKnown) {
        message <- "All parameters of the Uniform distribution are known, there is no parameter left to estimate."
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("UNIF", variable, "ALL_KNOWN"),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEunif"
        ))
      }
      
      if (aKnown) {
        a <- self$options$a
        if (!all(x >= a)) {
          message <- paste0("You chose Uniform distribution with known parameter a = ",a,", but variable '",variable,"' has values that are smaller than a.")
          
          return(private$.handleAnalysisIssue(
            name = private$.analysisNoticeName("UNIF", variable, "A_INVALID"),
            message = message,
            type = jmvcore::NoticeType$STRONG_WARNING,
            tableNames = "MLEunif"
          ))
        }
        tabunif$setRow(rowNo = 1, values = list(
          par = "b",
          MLE = max(x)
        ))
        return(invisible(NULL))
      }
      
      if (bKnown) {
        b <- self$options$b
        if (!all(x <= b)) {
          message <- paste0("You chose Uniform distribution with known parameter b = ",b,", but variable '",variable,"' has values that are larger than b.")
          
          return(private$.handleAnalysisIssue(
            name = private$.analysisNoticeName("UNIF", variable, "B_INVALID"),
            message = message,
            type = jmvcore::NoticeType$STRONG_WARNING,
            tableNames = "MLEunif"
          ))
        }
        tabunif$setRow(rowNo = 1, values = list(
          par = "a",
          MLE = min(x)
        ))
        return(invisible(NULL))
      }
      tabunif$setRow(rowNo = 1, values = list(
        par = "a",
        MLE = min(x)
      ))
      tabunif$addRow(rowKey = 2, values = list(
        par = "b",
        MLE = max(x)
      ))
      invisible(NULL)
    },
    
    .runPareto = function(x, variable) {
      if (!isTRUE(self$options$pardistr)) {
        return(invisible(NULL))
      }
      if (!all(x > 1)) {
        message <- paste0("You chose Pareto distribution, but variable '",variable,"' has values that are smaller than one.")
        
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("PAR", variable),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEpar"
        ))
      }
      self$results$MLEpar$setRow(rowNo = 1, values = list(
        par = "\u03B1",
        MLE = 1 / mean(log(x))
      ))
      invisible(NULL)
    },
    
    .runPower = function(x, variable) {
      if (!isTRUE(self$options$powdistr)) {
        return(invisible(NULL))
      }
      if (!all(x > 0 & x < 1)) {
        message <- paste0("You chose Power distribution, but variable '",variable,"' has values that are not strictly between 0 and 1.")
        return(private$.handleAnalysisIssue(
          name = private$.analysisNoticeName("POW", variable),
          message = message,
          type = jmvcore::NoticeType$STRONG_WARNING,
          tableNames = "MLEpow"
        ))
      }
      self$results$MLEpow$setRow(rowNo = 1, values = list(
        par = "\u03B1",
        MLE = -1 / mean(log(x))
      ))
      invisible(NULL)
    },
    
    .handleAnalysisIssue = function(name,message,type = jmvcore::NoticeType$WARNING,tableNames = NULL) {
      private$.hideTables(tableNames)
      private$.createNotice(name, message, type = type)
      return(invisible(NULL))
    },
    
    .analysisNoticeName = function(analysis, variable, issue = NULL) {
      parts <- c(analysis, issue)
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste0(paste(parts, collapse = "_"),"_analysesNotApplicable_",variable)
    },
    
    .selectedTableNames = function() {
      tableNames <- c()
      if (isTRUE(self$options$bindistr)) {
        tableNames <- c(tableNames, "MLEbin")
      }
      if (isTRUE(self$options$geodistr)) {
        tableNames <- c(tableNames, "MLEgeo")
      }
      if (isTRUE(self$options$podistr)) {
        tableNames <- c(tableNames, "MLEpo")
      }
      if (isTRUE(self$options$Ndistr)) {
        tableNames <- c(tableNames, "MLEnorm")
      }
      if (isTRUE(self$options$expdistr)) {
        tableNames <- c(tableNames, "MLEexp")
      }
      if (isTRUE(self$options$Udistr)) {
        tableNames <- c(tableNames, "MLEunif")
      }
      if (isTRUE(self$options$pardistr)) {
        tableNames <- c(tableNames, "MLEpar")
      }
      if (isTRUE(self$options$powdistr)) {
        tableNames <- c(tableNames, "MLEpow")
      }
      tableNames
    },
    
    .hideSelectedTables = function() {
      private$.hideTables(private$.selectedTableNames())
    },
    
    .hideTables = function(tableNames) {
      if (is.null(tableNames) || length(tableNames) == 0) {
        return(invisible(NULL))
      }
      for (tableName in tableNames) {
        self$results[[tableName]]$setVisible(FALSE)
      }
      invisible(NULL)
    },
    
    .createNotice=function(name, message, type=jmvcore::NoticeType$WARNING){
      notice <- jmvcore::Notice$new(options=self$options, name=name, type=type)
      notice$setContent(message)
      self$results$insert(private$.noticeInsertPosition, notice)
      private$.noticeInsertPosition <- private$.noticeInsertPosition + 1L
    },
    
    .checkMissingValues = function(var, x = self$data[[var]], createNotice = TRUE) {
      nMissing <- sum(is.na(x))
      if (isTRUE(createNotice) && nMissing > 0) {
        private$.missingvaluesVars <-  var
        private$.createNotice(paste0("missingValues_", var),
                              paste0("Variable '",var,"' contains ",nMissing," missing value(s). Analyses will ignore missing values."),
                              type = jmvcore::NoticeType$INFO)
      }
      x[!is.na(x)]
    },
    
    .toNumericWithNotice = function(var, createNotice = TRUE) {
      if (isTRUE(createNotice) && !is.numeric(self$data[[var]])) {
        private$.createNotice(paste0("variableNotNumeric_", var),
                              paste0("Treating variable '", var, "' as numeric."),
                              type = jmvcore::NoticeType$INFO)
        private$.transformedVars <- var
      }
      jmvcore::toNumeric(self$data[[var]])
    }
  )
)