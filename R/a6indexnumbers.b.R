
# This file is a generated template, your changes will not be overwritten

a6indexnumbersClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a6indexnumbersClass",
    inherit = a6indexnumbersBase,
    private = list(
      .transformedVars = c(),
      .noticeInsertPosition = 1L,
      
      # functions for the price index calculations
      .funcs = list(
        plas=function(q0,p0,qt,pt) sum(pt*q0)/sum(p0*q0),
        ppaa=function(q0,p0,qt,pt) sum(pt*qt)/sum(p0*qt),
        pfi=function(q0,p0,qt,pt) sqrt((sum(pt*q0)/sum(p0*q0))*(sum(pt*qt)/sum(p0*qt))),
        qlas=function(q0,p0,qt,pt) sum(p0*qt)/sum(p0*q0),
        qpaa=function(q0,p0,qt,pt) sum(pt*qt)/sum(pt*q0),
        qfi=function(q0,p0,qt,pt) sqrt((sum(p0*qt)/sum(p0*q0))*(sum(pt*qt)/sum(pt*q0))),
        val=function(q0,p0,qt,pt) sum(pt*qt)/sum(p0*q0)
      ),
      .run = function() {
        # only run if data is available
        if(any(is.null(self$options$basket0), is.null(self$options$baskett), is.null(self$options$price0), is.null(self$options$pricet)) && is.null(self$options$var)){
          return()
        }
        
        private$.fillIndices()
        private$.fillPriceIndexTable()
        
        private$.showFinalNotices()
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
      
      .showFinalNotices = function() {
        if(length(private$.transformedVars)>0) {
          private$.createNotice(paste0("variablesNotNumeric_", paste(private$.transformedVars, collapse = ", ")), 
                                paste0("Treating variable(s) '", paste(private$.transformedVars, collapse = ", "), "' as numeric."),
                                type=jmvcore::NoticeType$INFO)
        }
        if(length(private$.transformedVars)>0) {
          private$.createNotice(paste0("variablesNotNumeric_", paste(private$.transformedVars, collapse = ", ")), 
                                paste0("Treating variable(s) '", paste(private$.transformedVars, collapse = ", "), "' as numeric."),
                                type=jmvcore::NoticeType$INFO)
        }
      },
      
      # fill the index number, growth factor and growth rate columns in the data
      .fillIndices=function() {
        if(is.null(self$options$var)){
          return()
        }
        var <- self$options$var
        if(!jmvcore::canBeNumeric(self$data[[var]])){
          private$.createNotice("analysesNotApplicableIndex", 
                                paste0("Variable '", var, "' is not numeric. Analyses cannot be performed."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
          self$results$indexnum$setVisible(FALSE)
          return()
        }
        data <- private$.toNumeric(var)
        if(any(is.na(data))){
          private$.createNotice("analysesNotApplicableIndex", 
                                paste0("Variable '", var, "' contains NA values. Analyses cannot be performed."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
          self$results$indexnum$setVisible(FALSE)
          return()
        }
        if(!all(data>0)){
          private$.createNotice("analysesNotApplicableIndex", 
                                paste0("Variable '", var, "' contains non-positive values. Analyses cannot be performed."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
          self$results$indexnum$setVisible(FALSE)
          return()
        }
        
        if(self$options$indexnum && self$results$indexnum$isNotFilled()) {
          # check if valid input
          k <- as.numeric(self$options$k)
          if(k<0 || k>=length(data)){
            private$.createNotice("analysesNotApplicableIndex", 
                                  paste0("Value of k is not in the range of the data. Analyses cannot be performed."),
                                  type=jmvcore::NoticeType$STRONG_WARNING)
            self$results$indexnum$setVisible(FALSE)
            return()
          }
          Ikt=data/data[k+1]
          title=paste0("Index numbers of ", var)
          
          self$results$indexnum$setRowNums(rownames(self$data))
          self$results$indexnum$setTitle(title)
          self$results$indexnum$setValues(Ikt)
        }
        
        # growth factor
        if(self$options$growthfac && self$results$growthfac$isNotFilled()) {
          n=length(data)
          wt=c(NA, data[2:n]/data[1:(n-1)])
          title=paste0("Growth factors of ", var)
          
          self$results$growthfac$setRowNums(rownames(self$data))
          self$results$growthfac$setTitle(title)
          self$results$growthfac$setValues(wt)
        }
        
        # growth rate
        if(self$options$growthrate && self$results$growthrate$isNotFilled()) {
          n=length(data)
          rt=c(NA, data[2:n]/data[1:(n-1)] - 1)
          title=paste0("Growth rates of ", var)
          
          self$results$growthrate$setRowNums(rownames(self$data))
          self$results$growthrate$setTitle(title)
          self$results$growthrate$setValues(rt)
        }
      },
      
      # fill the price index table
      .fillPriceIndexTable=function() {
        # check which values need to be calculated
        selected <- vapply(names(private$.funcs), function(f) self$options[[f]], FUN.VALUE = logical(1)) # works only if names of functions and options are the same
        if(!any(selected)){
          return()
        }
        if(any(is.null(self$options$basket0), is.null(self$options$baskett), is.null(self$options$price0), is.null(self$options$pricet))){
          return()
        }
        
        missingValuesVars <- c()
        for(var in c(self$options$basket0, self$options$baskett, self$options$price0, self$options$pricet)){
          if(any(is.na(self$data[[var]]))){
            missingValuesVars <- c(missingValuesVars, var)
          }
        }
        if(length(missingValuesVars)>0){
          private$.createNotice("analysesNotApplicablePriceIndex", 
                                paste0("Variable(s) '", paste(missingValuesVars, collapse = ", "), "' contain(s) NA values. Analyses cannot be performed."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
          self$results$Indices$setVisible(FALSE)
          self$results$WW$setVisible(FALSE)
          return()
        }
        
        nonNumericVars <- c()
        for(var in c(self$options$basket0, self$options$baskett, self$options$price0, self$options$pricet)){
          if(!jmvcore::canBeNumeric(self$data[[var]])){
            nonNumericVars <- c(nonNumericVars, var)
          }
        }
        
        if(length(nonNumericVars)>0){
          private$.createNotice("analysesNotApplicablePriceIndex", 
                                paste0("Variable(s) '", paste(nonNumericVars, collapse = ", "), "' is/are not numeric. Analyses cannot be performed."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
          self$results$Indices$setVisible(FALSE)
          self$results$WW$setVisible(FALSE)
          return()
        }
        
        datab0 <- private$.toNumeric(self$options$basket0)
        datap0 <- private$.toNumeric(self$options$price0)
        databt <- private$.toNumeric(self$options$baskett)
        datapt <- private$.toNumeric(self$options$pricet)
        
        # calculate the values of the selected indices and add them to the table
        values <- lapply(private$.funcs[selected], function(f) f(datab0,datap0,databt,datapt))
        names(values) <- names(private$.funcs)[selected] # works only if names of functions and table columns are the same
        self$results$Indices$addRow(rowKey= 1, values=values)
        
        # calculate the value of the baskets and add it to the results
        if(self$options$valBask){
          bask0 <- sum(datab0*datap0)
          baskt <- sum(databt*datapt)
          str <- paste0("The value of the basket at time 0 is ", bask0, 
                        "; the value of the basket at time t is ", baskt, ".", sep="")
          self$results$WW$setContent(str)
        }
      }
      
      )
)
