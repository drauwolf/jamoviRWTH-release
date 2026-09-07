
# This file is a generated template, your changes will not be overwritten

a7correlationmeasuresClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a7correlationmeasuresClass",
    inherit = a7correlationmeasuresBase,
    private = list(
      .transformedVars = c(),
      .missingvaluesVars = c(),
      .noticeInsertPosition = 1L,
      
      .init = function() {
        nominalcont1 <- self$options$nominalcont1
        nominalcont2 <- self$options$nominalcont2
        countsName <- self$options$contCounts
        nominal1 <- self$options$nominal1
        nominal2 <- self$options$nominal2
        metric1 <- self$options$metric1
        metric2 <- self$options$metric2
        ordinal1 <- self$options$ordinal1
        ordinal2 <- self$options$ordinal2
        
        # hide tables if no variables are selected
        if(is.null(nominalcont1) || is.null(nominalcont2) || is.null(countsName)){
          self$results$cont_table$setVisible(FALSE)
        }
        if(is.null(nominal1) || is.null(nominal2)){
          self$results$nominal_table$setVisible(FALSE)
        }
        if(is.null(metric1) || is.null(metric2)){
          self$results$metric_table$setVisible(FALSE)
          self$results$scatterplot$setVisible(FALSE)
        }
        if(is.null(ordinal1) || is.null(ordinal2)){
          self$results$ordinal_table$setVisible(FALSE)
        }
      },
      .run = function() {
        # nominal data - input as contingency table
        
        nominalcont1 <- self$options$nominalcont1
        nominalcont2 <- self$options$nominalcont2
        countsName <- self$options$contCounts
        
        # nominal data
        
        nominal1 <- self$options$nominal1
        nominal2 <- self$options$nominal2
        
        # metric data
        
        metric1 <- self$options$metric1
        metric2 <- self$options$metric2
        
        # ordinal data
        ordinal1 <- self$options$ordinal1
        ordinal2 <- self$options$ordinal2
        
        # check if anything is selected
        
        check = (is.null(nominalcont1) || is.null(nominalcont2) || is.null(countsName)) && 
          (is.null(nominal1) || is.null(nominal2)) && 
          (is.null(metric1) || is.null(metric2)) && 
          (is.null(ordinal1) || is.null(ordinal2))
        
        if(check){
          return()
        }
        
        ########################### nominal data - input contingency table
        
        if(!any(is.null(nominalcont1), is.null(nominalcont2), is.null(countsName))) {
          cont_table <- private$.contTable()
          if(is.null(cont_table)) {
            self$results$cont_table$setVisible(FALSE)
          } else {
            private$.handleNominal('cont', cont_table, nominalcont1, nominalcont2)
          }
        }
        
        ########################### nominal data
        if(!is.null(nominal1) && !is.null(nominal2)) {          
          nominal_table <- private$.nominalTable()
          if(is.null(nominal_table)) {
            self$results$nominal_table$setVisible(FALSE)
          } else {
            private$.handleNominal('nominal', nominal_table, nominal1, nominal2)
          }
        }
        
        ########################### metric data
        if(!is.null(metric1) && !is.null(metric2)) {
          private$.handleMetric()
        }
        
        ########################### ordinal data
        if(!is.null(ordinal1) && !is.null(ordinal2)) {
          private$.handleOrdinal()
        }
        
        private$.showFinalNotices()
      },
      
      # Chi^2 from given contingency table
      .CHISQUARED_TABLE = function(table) {
        table.rand = addmargins(table)
        r = length(table[1,]) # num columns
        s = length(table[,1]) # num rows
        
        zero_cols = table.rand[s+1,1:r] != numeric(r)
        zero_rows = table.rand[1:s,r+1] != numeric(s)
        if (mean(zero_rows) == 1 && mean(zero_cols) == 1) {
          chi.matrix = (table.rand[1:s,1:r])^2 /
            (table.rand[1:s,r+1] %*% t(table.rand[s+1,1:r]))
          chi.sqrd = table.rand[s+1,r+1] * (sum(chi.matrix)-1)
          return(chi.sqrd)
        } else {
          # zero rows or columns exist, remove them and recalculate
          table.nozeroes = table[,t(as.vector(zero_cols))]
          table.nozeroes = table.nozeroes[t(as.vector(zero_rows)),]
          
          table.no.rand = addmargins(as.table(table.nozeroes))
          r_new = sum(zero_cols)
          s_new = sum(zero_rows)
          if(r_new>1 && s_new>1) {
            chi.matrix = (table.no.rand[1:s_new,1:r_new])^2 /
              (table.no.rand[1:s_new,r_new+1] %*% t(table.no.rand[s_new+1,1:r_new]))
            chi.sqrd = table.no.rand[s_new+1,r_new+1] * (sum(chi.matrix)-1)
            return(chi.sqrd)
          } else {
            return("Not enough data because of removal of zero rows/columns")
          }
        }
      },
      
      # C Pearson from given contingency table
      .C_PEARSON_TABLE = function(table) {
        n = sum(table)
        chi.sqrd = private$.CHISQUARED_TABLE(table)
        if(is.character(chi.sqrd)) {
          return(chi.sqrd)
        }
        C = sqrt(chi.sqrd/(n+chi.sqrd))
        return(C)
      },
      
      # C* Pearson from given contingency table
      .Cstern_PEARSON_TABLE = function(table) {
        table.rand = addmargins(table)
        
        C = private$.C_PEARSON_TABLE(table)
        if(is.character(C)) {
          return(C)
        }
        
        r = length(table[1,])
        s = length(table[,1])
        
        zero_cols = table.rand[s+1,1:r] != numeric(r)
        zero_rows = table.rand[1:s,r+1] != numeric(s)
        r_new = sum(zero_cols)
        s_new = sum(zero_rows)
        if(r_new>1 && s_new>1) {
          Cstern = sqrt(min(r_new,s_new)/(min(r_new,s_new)-1)) * C
          return(Cstern)
        } else {
          # error message: table too small
          return("Not enough data because of removal of zero rows/columns")
        }
      },
      
      #Empirical Covariance
      .EMPIRICALCOVARIANCE = function(x,y) {
        if (length(x) ==  length(y)) {
          
          sxy = mean(x*y) - mean(x) * mean(y)
          
          return(sxy)
        } else {
          # error message: vectors have different length
          jmvcore::reject(jmvcore::format("Vectors have different length"))
        }
      },
      
      #Bravais-Pearson Correlationcoefficient
      .BRAVAISPEARSON = function(x,y) {
        if (length(x) ==  length(y)) {
          
          y_mean = mean(y)
          x_mean = mean(x)
          sxy = mean(x*y) - x_mean * y_mean
          sx = sqrt(mean(x*x) - x_mean^2)
          sy = sqrt(mean(y*y) - y_mean^2)
          
          if(sx > 0 && sy > 0){
            rxy = sxy/(sx*sy) 
            return(rxy)
          } else {
            # error message: empirical covariance of x is not positive
            return("Data do not have positive empirical covariance")
          }
          
        } else {
          # error message: vectors have different length
          jmvcore::reject(jmvcore::format("Vectors have different length"))
        }
      },
      
      # Spearman's rank correlation coefficient
      .SPEARMAN = function(x,y) {
        if (length(x) ==  length(y)) {
          rank.x = as.numeric(rank(x))
          rank.y = as.numeric(rank(y))
          
          rSP = private$.BRAVAISPEARSON(rank.x,rank.y)
          return(rSP)
          
        } else {
          # error message: vectors have different length
          jmvcore::reject(jmvcore::format("Vectors have different length"))
        }
      },
      
      # helper function to add a notice to the results
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
          if(any(is.na(x) | is.nan(x))) {
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
      
      # helper function to fill a table with contingency data
      .fillTable = function(table, data, rowVarName, colVarName, showRowTotal=TRUE, showColTotal=TRUE) {  
        type <- ifelse(is.integer(data), 'integer', 'number')      
        table$addColumn(
          name=rowVarName,
          title=rowVarName,
          type='text'
        )
        
        for (level in colnames(data)) {
          table$addColumn(
            name=level,
            title=level,
            superTitle=colVarName,
            type=type
          )
        }
        if (showRowTotal) {
          table$addColumn(
            name='.total',
            title='Total',
            type=type
          )
        }
        
        # fill the table with data
        for (i in seq_len(nrow(data))) {
          values <- as.list(data[i,])
          names(values) <- colnames(data)
          values[[rowVarName]] <- rownames(data)[i]
          if (showRowTotal) {
            values[['.total']] <- sum(data[i,]) # add total for the row
          }
          table$addRow(rowKey=i, values=values)
        }
        
        # add row for the total of each column
        if (showColTotal) {
          totalRow <- as.list(colSums(data))
          names(totalRow) <- colnames(data)
          totalRow[[rowVarName]] <- 'Total'
          if (showRowTotal) {
            totalRow[['.total']] <- sum(data)
          }
          table$addRow(rowKey=nrow(data)+1, values=totalRow)
        }
      },
      
      # helper function to create a contingency table from the contingency data
      .contTable=function() {
        data <- self$data
        
        rowVarName <- self$options$nominalcont1
        colVarName <- self$options$nominalcont2
        countsName <- self$options$contCounts
        
        # check if the variables are factors
        columns <- jmvcore::select(data, c(rowVarName, colVarName))
        if ( !is.factor(columns[[rowVarName]])) {
          private$.createNotice(
            name = paste0("analysesNotApplicableCont", "_", rowVarName),
            message = paste0("Input Contingency Table: Input variable '", rowVarName, "' is not nominal."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          return(NULL)
        }
        
        if ( !is.factor(columns[[colVarName]])) {
          private$.createNotice(
            name = paste0("analysesNotApplicableCont", "_", colVarName),
            message = paste0("Input Contingency Table: Input variable '", colVarName, "' is not nominal."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          return(NULL)
        }
        
        # select the counts variable and check if it is numeric
        if ( !is.null(countsName)) {
          if ( ! jmvcore::canBeNumeric(data[[countsName]])) {
            private$.createNotice(
              name = paste0("analysesNotApplicableCont", "_", countsName),
              message = paste0("Input Contingency Table: Input variable '", countsName, "' is not numeric."),
              type = jmvcore::NoticeType$STRONG_WARNING
            )
            return(NULL)
          }
          columns$counts <- private$.toNumeric(countsName)
          message <- jmvcore::format("The data is weighted by the variable '{var}'.", var=countsName)
          private$.createNotice('weights', message)
        } else if ( !is.null(attr(data, "jmv-weights"))) {
          columns$counts <- jmvcore::toNumeric(attr(data, "jmv-weights"))
          message <- jmvcore::format("The data is weighted by the variable '{var}'.", var=attr(data, "jmv-weights-name"))
          private$.createNotice('weights', message)
        } else {
          columns$counts <- as.integer(rep(1, nrow(data)))
        }
        
        # check for missing values in the counts variable and set them to 0
        na.values <- is.na(columns$counts)
        if (any(na.values)) {
          private$.createNotice('weights_na', "The counts variable contains missing values. Missing values will be set to 0.")
          columns$counts[na.values] <- 0L
        }
        
        private$.noteMissingValues(rowVarName, columns[[rowVarName]])
        private$.noteMissingValues(colVarName, columns[[colVarName]])
        
        #if (B64)
        #  names(columns) <- jmvcore::toB64(names(columns))
        return(xtabs(counts ~ ., data=columns))
      },
      
      # helper function to create a contingency table from the nominal data
      .nominalTable=function() {
        data <- self$data
        
        rowVarName <- self$options$nominal1
        colVarName <- self$options$nominal2
        
        columns <- jmvcore::select(data, c(rowVarName, colVarName))
        
        if ( !is.factor(columns[[rowVarName]])) {
          private$.createNotice(
            name = paste0("analysesNotApplicableNominal", "_", rowVarName),
            message = paste0("Categorical Data: Input variable '", rowVarName, "' is not nominal."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          return(NULL)
        }
        if ( !is.factor(columns[[colVarName]])) {
          private$.createNotice(
            name = paste0("analysesNotApplicableNominal", "_", colVarName),
            message = paste0("Categorical Data: Input variable '", colVarName, "' is not nominal."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          return(NULL)
        }
        
        private$.noteMissingValues(rowVarName, columns[[rowVarName]])
        private$.noteMissingValues(colVarName, columns[[colVarName]])
        
        return(table(columns))
      },
      
      # helper function to handle nominal data and fill the corresponding tables
      .handleNominal=function(name, data, rowVarName, colVarName) {
        if(nrow(data) < 2 || ncol(data) < 2) {
          private$.createNotice(
            name = paste0("analysesNotApplicableLevelsNom"),
            message = paste0("Categorical Data: Row variable '", rowVarName, "' or column variable '", colVarName, "' contains fewer than 2 levels."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          self$results[[paste0(name, "_table")]]$setVisible(FALSE)
          return()
        }
        if (any(data < 0)) {
          private$.createNotice(
            name = paste0("analysesNotApplicableNegativeCounts"),
            message = paste0("Categorical Data: Counts may not be negative."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          self$results[[paste0(name, "_table")]]$setVisible(FALSE)
          return()
        }
        if (any(is.infinite(data))) {
          private$.createNotice(
            name = paste0("analysesNotApplicableInfiniteCounts"),
            message = paste0("Categorical Data: Counts may not be infinite."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          self$results[[paste0(name, "_table")]]$setVisible(FALSE)
          return()
        }
        
        if(self$options[[paste0(name, "_abs_table")]]) {
          table <- self$results[[paste0(name, "_abs_table")]]
          abs_data <- data
          private$.fillTable(table, abs_data, rowVarName, colVarName)
        }
        if(self$options[[paste0(name, "_rel_table")]]) {
          table <- self$results[[paste0(name, "_rel_table")]]
          rel_data <- data / sum(data)
          private$.fillTable(table, rel_data, rowVarName, colVarName)
        }
        if(self$options[[paste0(name, "_row_table")]]) {
          table <- self$results[[paste0(name, "_row_table")]]
          row_data <- data / rowSums(data)
          private$.fillTable(table, row_data, rowVarName, colVarName, showRowTotal=TRUE, showColTotal=FALSE)
        }
        if(self$options[[paste0(name, "_col_table")]]) {
          table <- self$results[[paste0(name, "_col_table")]]
          col_data <- t(t(data) / colSums(data))
          private$.fillTable(table, col_data, rowVarName, colVarName, showRowTotal=FALSE, showColTotal=TRUE)
        }
        
        # calculate and add chi-squared, C Pearson, and C* Pearson if requested
        table <- self$results[[paste0(name, "_table")]]
        values <- list(var = toString(c(rowVarName, colVarName)))
        if(self$options[[paste0(name, "_chi2")]]) {
          chi.2 <- private$.CHISQUARED_TABLE(data)
          if(is.character(chi.2)) {
            private$.createNotice(paste0("analysesNotApplicableChi2", "_", name),
                                  paste0("Categorical Data: ", chi.2, ". Chi-squared test cannot be performed."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
          } else {
            values$chi.2 <- chi.2
          }
        }
        if(self$options[[paste0(name, "_Ccor")]]) {
          C <- private$.C_PEARSON_TABLE(data)
          if(is.character(C)) {
            private$.createNotice(paste0("analysesNotApplicableCcor", "_", name),
                                  paste0("Categorical Data: ", C, ". C Pearson cannot be calculated."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
          } else {
            values$C <- C
          }
        }
        if(self$options[[paste0(name, "_Cstern")]]) {
          CStern <- private$.Cstern_PEARSON_TABLE(data)
          if(is.character(CStern)) {
            private$.createNotice(paste0("analysesNotApplicableCstern", "_", name),
                                  paste0("Categorical Data: ", CStern, ". C* Pearson cannot be calculated."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
          } else {
            values$CStern <- CStern
          }
        }
        table$addRow(rowKey=1, values=values)
      },
      
      # helper function to handle metric data and fill the corresponding tables
      .handleMetric=function() {
        metric1 <- self$options$metric1
        metric2 <- self$options$metric2
        data <- jmvcore::select(self$data, c(metric1, metric2))
        
        # check if the variables are numeric or can be converted to numeric
        if(!jmvcore::canBeNumeric(data[[metric1]])) {
          private$.createNotice(paste0("analysesNotApplicableMetric", "_", metric1),
                                paste0("Metric Data: Input variable '", metric1, "' is not numeric."),
                                type = jmvcore::NoticeType$STRONG_WARNING)
          self$results$metric_table$setVisible(FALSE)
          self$results$scatterplot$setVisible(FALSE)
          return()
        }
        
        data[[metric1]] = private$.toNumeric(metric1)
        
        if(!jmvcore::canBeNumeric(data[[metric2]])) {
          private$.createNotice(paste0("analysesNotApplicableMetric", "_", metric2),
                                paste0("Metric Data: Input variable '", metric2, "' is not numeric."),
                                type = jmvcore::NoticeType$STRONG_WARNING)
          self$results$metric_table$setVisible(FALSE)
          self$results$scatterplot$setVisible(FALSE)
          return()
        }
        
        data[[metric2]] = private$.toNumeric(metric2)
        
        private$.noteMissingValues(metric1, data[[metric1]])
        private$.noteMissingValues(metric2, data[[metric2]])
        
        data <- na.omit(data) # remove rows with NA values
        
        # calculate and add empirical covariance and Bravais-Pearson correlation coefficient if requested
        sxy_result = ''
        r_result = ''
        
        if(self$options$metric_empKov) {
          sxy_result = private$.EMPIRICALCOVARIANCE(data[[metric1]], data[[metric2]])
        }
        
        if(self$options$metric_bpKor) {
          r_result = private$.BRAVAISPEARSON(data[[metric1]], data[[metric2]])
          if(is.character(r_result)) {
            private$.createNotice(paste0("analysesNotApplicableBP", "_", metric1, "_", metric2),
                                  paste0("Metric Data: ", r_result, ". Bravais-Pearson correlation coefficient cannot be calculated."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
            r_result = ''
          }
        }
        
        metrictable <- self$results$metric_table
        
        metrictable$addRow(rowKey=1, values=list(
          var = toString(c(metric1, metric2)),
          sxy = sxy_result,
          r = r_result
        ))
        
        # create scatterplot if requested
        if(self$options$metric_streu) {
          plotData <- data.frame(x=data[[metric1]], y=data[[metric2]])
          
          image = self$results$scatterplot
          image$setState(list(var1 = metric1, var2 = metric2, data = plotData))
        }
      },
      
      # helper function to handle ordinal data and fill the corresponding tables
      .handleOrdinal=function() {
        ordinal1 <- self$options$ordinal1
        ordinal2 <- self$options$ordinal2
        data <- jmvcore::select(self$data, c(ordinal1, ordinal2))
        
        private$.noteMissingValues(ordinal1, data[[ordinal1]])
        private$.noteMissingValues(ordinal2, data[[ordinal2]])
        data <- na.omit(data)
        
        rSP_result = ''
        
        # check if the variables are ordered factors or can be converted to ordered factors
        transformedVars = c()
        if(!is.ordered(data[[ordinal1]])) {
          if(jmvcore::canBeNumeric(data[[ordinal1]])) {
            transformedVars = c(transformedVars, ordinal1)
            data[[ordinal1]] = as.ordered(jmvcore::toNumeric(data[[ordinal1]]))
          } else {
            private$.createNotice(paste0("analysesNotApplicableOrdinal", "_", ordinal1),
                                  paste0("Ordinal Data: Input variable '", ordinal1, "' is not ordered."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
            self$results$ordinal_table$setVisible(FALSE)
            return()
          }
        }
        
        if(!is.ordered(data[[ordinal2]])) {
          if(jmvcore::canBeNumeric(data[[ordinal2]])) {
            transformedVars = c(transformedVars, ordinal2)
            data[[ordinal2]] = as.ordered(jmvcore::toNumeric(data[[ordinal2]]))
          } else {
            private$.createNotice(paste0("analysesNotApplicableOrdinal", "_", ordinal2),
                                  paste0("Ordinal Data: Input variable '", ordinal2, "' is not ordered."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
            self$results$ordinal_table$setVisible(FALSE)
            return()
          }
        }
        
        if(length(transformedVars) > 0) {
          private$.createNotice(paste0("variablesNotOrdered_", paste(transformedVars, collapse = ", ")), 
                                paste0("Treating variable(s) '", paste(transformedVars, collapse = ", "), "' as ordered."),
                                type=jmvcore::NoticeType$INFO)
        }
        
        # calculate and add Spearman's rank correlation coefficient if requested
        if(self$options$ordinal_spearman) {
          rSP_result = private$.SPEARMAN(data[[ordinal1]],data[[ordinal2]])
          if(is.character(rSP_result)) {
            private$.createNotice(paste0("analysesNotApplicableSpearman", "_", ordinal1, "_", ordinal2),
                                  paste0("Ordinal Data: ", rSP_result, ". Spearman's rank correlation coefficient cannot be calculated."),
                                  type = jmvcore::NoticeType$STRONG_WARNING)
            self$results$ordinal_table$setVisible(FALSE)
            return()
          }
        }
        
        ordinaltable <- self$results$ordinal_table
        
        ordinaltable$addRow(rowKey=1, values=list(
          var = toString(c(ordinal1, ordinal2)),
          rSP = rSP_result
        ))
      },
      
      .scatplot=function(image, ggtheme, theme, ...){
        if (is.null(image$state)) {
          return(FALSE)
        }
        plotData <- image$state$data
        var1 <- image$state$var1
        var2 <- image$state$var2
        
        # simple scatterplot with ggplot2
        SCATTERPLOT = ggplot(plotData, aes(x=x, y=y)) + 
          geom_point() +
          labs(x=var1, y=var2) +
          ggtheme
        
        print(SCATTERPLOT)
        TRUE
        
      }
      
      )
)
