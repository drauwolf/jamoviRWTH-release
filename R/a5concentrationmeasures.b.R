
# This file is a generated template, your changes will not be overwritten

a5concentrationmeasuresClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a5concentrationmeasuresClass",
    inherit = a5concentrationmeasuresBase,
    private = list(
      .missingvaluesVars = c(),
      .noticeInsertPosition = 1L,
      
      .init=function(){
        if (is.null(self$options$var)) {
          return()
        }
        if (!any(self$options$Gini, self$options$norm, self$options$Herf, self$options$Lorenz, self$options$Tab)) {
          return()
        }
        private$.initSumTable()
        private$.initCoeffTable()
      },
      .run = function() {
        if (is.null(self$options$var)) {
          return()
        }
        if (!any(self$options$Gini, self$options$norm, self$options$Herf, self$options$Lorenz, self$options$Tab)) {
          return()
        }
        data <- private$.calculateData()
        private$.fillSumTable(data)
        private$.fillCoeffTable(data)
        private$.preparePlotData(data)
        
        private$.showFinalNotices()
      },
      
      # helper function to create a notice
      .createNotice=function(name, message, type=jmvcore::NoticeType$WARNING){
        notice <- jmvcore::Notice$new(options=self$options, name=name, type=type)
        notice$setContent(message)
        self$results$insert(private$.noticeInsertPosition, notice)
        private$.noticeInsertPosition <- private$.noticeInsertPosition + 1L
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
      },
      
      # calculate sum matrix for the given variable, group and plotby variable
      .calculateData=function(){
        var <- self$options$var
        groupvar <- self$options$group
        plotbyvar <- self$options$plotby
        # check if the variable is numeric
        if(!jmvcore::canBeNumeric(self$data[[var]])){
          jmvcore::reject(jmvcore::format("Variable '{var}' must be numeric", var=var))
        }
        if (!is.numeric(self$data[[var]])) {
          private$.createNotice(
            name='varNotNumeric',
            message=jmvcore::format("Variable '{var}' is not numeric, and will be coerced to numeric.", var=var),
          )
        }
        x <- jmvcore::toNumeric(self$data[[var]])
        
        private$.noteMissingValues(var, x)
        
        # check if the group variable is given and a factor
        if (!is.null(groupvar)) {
          if (!is.factor(self$data[[groupvar]])) {
            jmvcore::reject(jmvcore::format("Grouping variable '{groupvar}' must be a factor", groupvar=groupvar))
          }
          group <- self$data[[groupvar]]
          private$.noteMissingValues(groupvar, group)
        } else {
          group <- factor(seq_along(x))
        }
        
        # check if the plot by variable is given and a factor
        if (!is.null(plotbyvar)) {
          if (!is.factor(self$data[[plotbyvar]])) {
            jmvcore::reject(jmvcore::format("Plot by variable '{plotbyvar}' must be a factor", plotbyvar=plotbyvar))
          }
          plotby <- self$data[[plotbyvar]]
          private$.noteMissingValues(plotbyvar, plotby)
        } else {
          plotby <- factor(rep("Sum", length(x)))
        }
        
        # calculate the sum of the variable for each group and plot by combination
        result <- tapply(x, list(group=group, plotby=plotby), sum, na.rm=TRUE, default=0)
        # add a row with the total sum for each plot by level
        result <- rbind(result, Total=colSums(result, na.rm=TRUE))
        return(result)
      },
      
      # calculate the Gini coefficient for a given vector x, with optional normalization
      .GINI=function(x, norm=FALSE){
        s <- sum(x)
        x <- sort(x)
        n <- length(x)
        denom <- if (norm) (n-1) * s else n * s
        return((2 * sum(x * seq_len(n)) - (n+1) * s) / denom)
      },
      
      # calculate the Herfindahl index for a given vector x
      .HERFINDAHL=function(x){
        return(sum(x^2)/sum(x)^2)
      },
      
      .initSumTable=function(){
        if (!self$options$Tab) {
          return()
        }
        table <- self$results$Tabdata
        var <- self$options$var
        groupvar <- self$options$group
        plotbyvar <- self$options$plotby
        
        table$setTitle(var)
        title <- if (!is.null(groupvar)) groupvar else 'Group'
        table$addColumn(name=title, title=title, type='text')
      },
      
      .initCoeffTable=function(){
        if (!self$options$Gini && !self$options$norm && !self$options$Herf) {
          return()
        }
        table <- self$results$coeff
        var <- self$options$var
        groupvar <- self$options$group
        plotbyvar <- self$options$plotby
        
        table$setTitle(paste0('Measures of concentration of ', var))
        if(!is.null(plotbyvar)){
          table$addColumn(name='colplotby', title=plotbyvar, index=2, type='text')
        }
      },
      
      .fillSumTable=function(data){
        if (!self$options$Tab) {
          return()
        }
        table <- self$results$Tabdata
        plotbyvar <- self$options$plotby
        groupvar <- self$options$group
        
        # add columns to the table depending on whether a plot by variable is given
        if (is.null(plotbyvar)) {
          table$addColumn(name="Sum", title="Sum per group", type='number')
        } else { # add columns for each level of the plot by variable
          for (level in colnames(data)) {
            table$addColumn(name=level, title=level, superTitle=plotbyvar, type='number')
          }
        }
        
        # add rows to the table for each group and the total sum
        title <- if (!is.null(groupvar)) groupvar else 'Group'
        
        rows <- rownames(data)
        
        if (length(rows) > 50) {
          rows_to_show <- c(rows[1:49], NA, rows[length(rows)])
        } else {
          rows_to_show <- rows
        }
        
        for (row in rows_to_show) {
          if (is.na(row)) {
            values <- as.list(rep("...", length(colnames(data)) + 1))
            names(values) <- c(colnames(data), title)
            table$addRow(rowKey = "ellipsis", values = values)
            next
          }
          
          values <- as.list(data[row, ])
          names(values) <- colnames(data)
          values[[title]] <- row
          table$addRow(rowKey = row, values = values)
        }
      },
      
      .fillCoeffTable=function(data){
        if (!self$options$Gini && !self$options$norm && !self$options$Herf) {
          return()
        }
        table <- self$results$coeff
        
        for (col in colnames(data)) {
          values <- list(Group=self$options$group)
          if (!is.null(self$options$plotby)) {
            values$colplotby <- col
          }
          if (self$options$Gini) {
            values$Gini <- private$.GINI(data[,col], norm=FALSE)
          }
          if (self$options$norm) {
            values$normalized <- private$.GINI(data[,col], norm=TRUE)
          }
          if (self$options$Herf) {
            values$Herfin <- private$.HERFINDAHL(data[,col])
          }
          table$addRow(rowKey=col, values=values)
        }
      },
      
      .preparePlotData=function(data){
        if (!self$options$Lorenz) {
          return()
        }
        n <- nrow(data)-1
        m <- ncol(data)
        s <- rep(c(0,(1:n)/n), m)
        t <- c()
        by <- c()
        # loop through each column of the data and calculate the cumulative sum for the Lorenz curve
        for (j in 1:m) {
          t <- c(t, c(0,cumsum(sort(data[1:n,j]))/data[n+1,j]))
          if (!is.null(self$options$plotby)) {
            by <- c(by, rep(colnames(data)[j], n+1))
          } else {
            by <- c(by, rep(1, n+1))
          }
        }
        plotData <- data.frame(si=s, ti=t, by=as.factor(by))
        image <- self$results$Lor
        image$setState(list(plotbyvar = self$options$plotby, data = plotData))
      },
      
      .plot=function(image, ggtheme, theme, ...){
        if (is.null(image$state)) {
          return(FALSE)
        }
        
        plotData <- image$state$data
        plotbyvar <- image$state$plotbyvar
        
        Plot <- ggplot(plotData, aes(x=si, y=ti, col=by)) + 
          geom_point() + geom_line() +
          geom_segment(aes(x=0, y=0, xend=1, yend=1), col="black", size=0.1) +
          labs(x="s", y="t") +
          ggtheme
        
        if(is.null(plotbyvar)){
          Plot <- Plot + guides(col="none")
        } else {
          Plot <- Plot + guides(col=guide_legend(title=plotbyvar))
        }
        print(Plot)
        
        TRUE
        
      }
      
      )
)
