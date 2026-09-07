
# This file is a generated template, your changes will not be overwritten

a8linearregressionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a8linearregressionClass",
    inherit = a8linearregressionBase,
    private = list(
      .transformedVars = c(),
      .missingvaluesVars = c(),
      .noticeInsertPosition = 1L,
      
      .run = function() {
        if(is.null(self$options$var) || is.null(self$options$covariate)){
          # hide all results if no variable or covariate is selected
          private$.hideAll()
          return()
        }
        
        # make sure that the data is numeric and check if data has missing values
        data <- private$.prepData()
        if(is.null(data)){
          private$.hideAll()
          return()
        }
        if(nrow(data) < 2){
          private$.hideAll()
          private$.createNotice("notEnoughData", "Not enough data to perform regression. At least two complete cases are required.")
          return()
        }
        # calculate regression, residuals and coefficients
        if(self$options$linRegIntercept){
          reg <- private$.calcReg(data, intercept=self$options$interceptvalue)
        } else {
          reg <- private$.calcReg(data)
        }
        if(is.null(reg)){
          private$.hideAll()
          return()
        }
        res <- private$.calcRes(data, reg)
        coeff <- private$.calcCoeff(reg) 
        
        # fill results to tables and plots
        private$.fillTable(reg, coeff)
        private$.addCols(res)
        private$.prepareScatPlot(data, reg)
        private$.prepareResPlot(res)
        private$.predTable(reg)
        
        private$.showFinalNotices()
      },

      .hideAll=function(){
        self$results$linreg$setVisible(FALSE)
        self$results$predtable$setVisible(FALSE)
        self$results$scatterplot$setVisible(FALSE)
        self$results$resplot$setVisible(FALSE)
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
          if(any(is.na(x))) {
            private$.missingvaluesVars <- c(private$.missingvaluesVars, var)
          }
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
      
      # prepare data: check if it is numeric and has no missing values
      .prepData=function() {
        dep <- self$options$var
        cov <- self$options$covariate
        data <- self$data
        
        if(!jmvcore::canBeNumeric(data[[dep]])){
          private$.createNotice("dependentVariableNotNumeric", "Dependent variable must be numeric.")
          return()
        }
        
        if(!jmvcore::canBeNumeric(data[[cov]])){
          private$.createNotice("covariateNotNumeric", "Covariate must be numeric.")
          return()
        }
        
        private$.noteMissingValues(dep, data[[dep]])
        private$.noteMissingValues(cov, data[[cov]])
        
        
        transformed <- na.omit(data.frame(
          dep=private$.toNumeric(dep), 
          cov=private$.toNumeric(cov)
        ))
        
        return(transformed)
      },
      
      # calculate regression coefficients and other statistics
      .calcReg=function(data, intercept=NULL){
        x <- data$cov
        y <- data$dep
        
        x_mean <- mean(x)
        y_mean <- mean(y)
        sxy <- mean(x*y) - x_mean * y_mean
        sx2 <- mean(x*x) - x_mean^2
        sy2 <- mean(y*y) - y_mean^2
        if(sx2 <= 0){
          private$.createNotice("noPositiveCovariance", "Data do not have positive empirical covariance")
          return()
        }
        
        # calculate regression coefficients, depending on whether an intercept is specified
        if(is.null(intercept)){
          b_hat <- sxy/sx2
          a_hat <- y_mean - b_hat * x_mean
        } else {
          a_hat <- intercept
          b_hat <- sum(x*(y-intercept))/sum(x*x)
        }
        
        return(list(a_hat=a_hat, b_hat=b_hat, x_mean=x_mean, y_mean=y_mean, sxy=sxy, sx2=sx2, sy2=sy2))
      },
      
      # calculate residuals and normalized residuals
      .calcRes=function(data, reg){
        x <- data$cov
        y <- data$dep
        a_hat <- reg$a_hat
        b_hat <- reg$b_hat
        
        y_hat <- a_hat + b_hat * x
        res <- y - y_hat
        normedres <- res / sqrt(sum(res^2))
        
        return(data.frame(res=res, normedres=normedres, y_hat=y_hat))
      },
      
      # calculate Bravais-Pearson correlation coefficient and coefficient of determination
      .calcCoeff=function(reg){
        r <- reg$sxy / sqrt(reg$sx2 * reg$sy2)
        B <- r^2
        return(list(r=r, B=B))
      },
      
      # fill results to table
      .fillTable=function(reg, coeff){
        table <- self$results$linreg
        table$addRow(rowKey=1, values=list(
          a_hat=reg$a_hat,
          b_hat=reg$b_hat,
          r = coeff$r,
          B = coeff$B
        ))
      },
      
      # fill results to data columns
      .addCols=function(res){
        if (self$options$residsOV && self$results$residsOV$isNotFilled()) {
          self$results$residsOV$setRowNums(rownames(self$data))
          self$results$residsOV$setValues(res$res)
        }
        if (self$options$normedresidsOV && self$results$normedresidsOV$isNotFilled()) {
          self$results$normedresidsOV$setRowNums(rownames(self$data))
          self$results$normedresidsOV$setValues(res$normedres)
        }
        if (self$options$predictionsOV && self$results$predictionsOV$isNotFilled()) {
          self$results$predictionsOV$setRowNums(rownames(self$data))
          self$results$predictionsOV$setValues(res$y_hat)
        }
      },
      
      # write prediction to table
      .predTable=function(reg){
        if (!self$options$pred) {
          return()
        }
        table <- self$results$predtable
        xvalue <- self$options$predvalue
        prediction <- reg$a_hat + reg$b_hat * xvalue
        
        table$addRow(rowKey=1, values=list(
          xvalue = xvalue,
          prediction = prediction
        ))
      },
      
      # prepare scatter plot data and set state for plot
      .prepareScatPlot=function(data, reg){
        if (!self$options$scatter) {
          return()
        }
        
        plotData <- data.frame(x=data$cov, y=data$dep)
        
        image = self$results$scatterplot
        image$setState(list(plotData=plotData, a_hat=reg$a_hat, b_hat=reg$b_hat))
      },
      
      # prepare residual plot data and set state for plot
      .prepareResPlot=function(res){
        if (!self$options$resPlot) {
          return()
        }
        
        plotData <- data.frame(y_hat = res$y_hat)
        if(self$options$normedresPlot){
          plotData$res <- res$normedres
        } else {
          plotData$res <- res$res
        }
        
        image_res = self$results$resplot
        image_res$setState(plotData)
      },
      
      .scatplot=function(image, ggtheme, theme, ...){
        if (is.null(image$state)) {
          return(FALSE)
        }
        plotData <- image$state$plotData
        a_hat <- image$state$a_hat
        b_hat <- image$state$b_hat
        
        # color palette for points and regression line
        colors=jmvcore::colorPalette(n=4, theme$palette, type="color")
        
        # simple scatter plot of data points
        SCATTERPLOT = ggplot(plotData, aes(x=x, y=y)) + 
          geom_point() +
          labs(x=self$options$covariate, y=self$options$var) +
          ggtheme
        
        # linear regression line
        if(self$options$regplot){
          flinear=function(x){
            a_hat + x*b_hat
          }
          SCATTERPLOT = SCATTERPLOT +
            geom_function(aes(color="linear"), fun=flinear)
        }
        
        # quadratic regression line
        if(self$options$regplotquad){
          par.quad=lm(y~x + I(x^2), data = plotData)$coefficients
          fquad=function(x){
            par.quad[1] + par.quad[2]*x + par.quad[3]*x^2
          }
          SCATTERPLOT = SCATTERPLOT +
            geom_function(aes(color="quadratic"), fun=fquad)
        }
        
        # polynomial regression line
        if(self$options$regplotpoly){
          deg=self$options$degree
          par.poly=lm(plotData$y~poly(plotData$x, deg, raw=TRUE))$coefficients
          fpoly=function(x){
            expo=0:deg
            res=sum(par.poly*(x^expo))
            return(res)
          }
          fpoly.vec=function(x){
            sapply(x, fpoly)
          }
          SCATTERPLOT = SCATTERPLOT +
            geom_function(aes(color="polynomial"), fun=fpoly.vec)
        }
        
        # logistic regression line
        if(self$options$regplotlog){
          f=function(par){
            a=par[1]
            b=par[2]
            c=par[3]
            sum((a/(1 + b*exp(-c*plotData$x)) - plotData$y)^2)
          }
          
          KQ = optim(f, par=c(1,1,1), control=list(maxit=1000))
          par.log=KQ$par
          
          if(KQ$convergence==0){
            flog=function(x){
              par.log[1]/(1 + par.log[2]*exp(-par.log[3]*x))
            }
            SCATTERPLOT = SCATTERPLOT + 
              geom_function(aes(color="logistic"), fun=flog)
          } else {
            jmvcore::reject(jmvcore::format("Least Squares method does not converge."))
          }
        }
        
        SCATTERPLOT = SCATTERPLOT  + guides(color=guide_legend(title="Class of functions"))
        print(SCATTERPLOT)
        TRUE
      },
      
      .resplot=function(image_res, ggtheme, theme, ...){
        if (is.null(image_res$state)) {
          return(FALSE)
        }
        plotData <- image_res$state
        y_label <- ifelse(self$options$normedresPlot, "Normalized residuals", "Residuals")
        
        # Residualplot
        RESPLOT = ggplot(plotData, aes(x = y_hat, y = res)) +
          geom_point() +
          geom_hline(yintercept = 0) +
          labs(x="", y=y_label) +
          ggtheme
        
        print(RESPLOT)
      }
      
      )
)
