
# This file is a generated template, your changes will not be overwritten

a2tablesgraphsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a2tablesgraphsClass",
    inherit = a2tablesgraphsBase,
    private = list(
      .charttypes = c("stick", "vbar", "hbar", "line"),
      .freqData = list(),
      .ecdfData = list(),
      .transformedVars = c(),
      .missingvaluesVars = c(),
      .noUsableValuesVars = c(),
      .notNominalOrOrdinalVars = c(),
      .noticeInsertPosition = 1L,
      .init = function() {
        vars = self$options$vars
        
        varsecdf = self$options$ecdf
        
        if(is.null(vars) && is.null(varsecdf)){
          return()
        }
        
        if(any(c(self$options$absH, self$options$stick, self$options$vbar, self$options$hbar, self$options$pie, self$options$line)) && !is.null(vars)) {
          for(i in seq_along(vars)) {
            var = vars[i]
            
            ### Frequency Table
            private$.initFrequencyTable(var) 
            
            ### Pie chart
            private$.initPieChartTable(var)
          }
        }
      },
      .run = function() {
        private$.freqData <- list()
        private$.ecdfData <- list()
        private$.transformedVars <- c()
        private$.missingvaluesVars <- c()
        private$.noUsableValuesVars <- c()
        private$.notNominalOrOrdinalVars <- c()
        private$.noticeInsertPosition <- 1L
        
        
        vars = self$options$vars
        varsecdf = self$options$ecdf
        
        if(is.null(vars) && is.null(varsecdf)){
          return()
        }
        
        if(any(c(self$options$absH, self$options$stick, self$options$vbar, self$options$hbar, self$options$pie, self$options$line)) && !is.null(vars)) {
          for(i in seq_along(vars)) {
            var = vars[i]
            data_var <- self$data[[var]]
            
            private$.noteMissingValues(var, data_var)
            
            # remove missing values before analyses
            column <- data_var[!is.na(data_var)]
            
            # check if at least one non-missing value is left
            if(!private$.checkNumberOfValues(var, column)) {
              private$.freqData[[var]] <- NULL
              next
            }
            
            column <- private$.formatColumn(var, column)
            if(is.null(column)) {
              private$.freqData[[var]] <- NULL
              next
            }
            
            private$.freqData[[var]] <- private$.computeFrequencies(column)
            
            ### Frequency Table
            private$.fillFrequencyTable(var)  
            
            ### Charts
            private$.prepareCharts(var)
            
            ### Pie chart
            private$.preparePieChart(var)
            private$.fillPieChartTable(var)
          }
        }
        
        data <- self$data
        if(!is.null(varsecdf) && self$options$empV) {
          for (i in seq_along(varsecdf)) {
            var = varsecdf[i]
            data_var <- data[[var]]
            
            private$.noteMissingValues(var, data_var)
            
            if(!jmvcore::canBeNumeric(data_var)) {
              private$.createNotice(
                paste0("varNotNumeric_", var),
                paste0("Variable '", var, "' is not numeric. Analyses will ignore it."),
                type = jmvcore::NoticeType$STRONG_WARNING
              )
              private$.ecdfData[[var]] <- NULL
              next
            }
            
            # convert to numeric, then remove missing values
            data_var <- private$.toNumeric(var)
            data_var <- data_var[!is.na(data_var)]
            
            # check if at least one non-missing value is left
            if(!private$.checkNumberOfValues(var, data_var)) {
              private$.ecdfData[[var]] <- NULL
              next
            }
            
            ecdata <- private$.computeFrequencies(data_var, cumH = TRUE)
            ecdata$levels <- as.numeric(ecdata$levels)
            private$.ecdfData[[var]] <- ecdata
            
            ### ECDF
            private$.prepareECDF(var)
          }
        } 
        
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
          private$.createNotice(
            paste0("missingValues_", paste(private$.missingvaluesVars, collapse = ", ")),
            paste0("Variable(s) '", paste(private$.missingvaluesVars, collapse = ", "), "' contain(s) missing value(s). Analyses will ignore missing values."),
            type = jmvcore::NoticeType$INFO
          )
        }
        
        if(length(private$.noUsableValuesVars)>0) {
          private$.createNotice(
            paste0("noUsableValues_", paste(private$.noUsableValuesVars, collapse = ", ")),
            paste0("Variable(s) '", paste(private$.noUsableValuesVars, collapse = ", "), "' have no usable observations after removing missing values. Analyses cannot be computed."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
        }
        
        if(length(private$.notNominalOrOrdinalVars)>0) {
          private$.createNotice(
            paste0("notNominalOrOrdinal_", paste(private$.notNominalOrOrdinalVars, collapse = ", ")),
            paste0("Variable(s) '", paste(private$.notNominalOrOrdinalVars, collapse = ", "), "' are not nominal or ordinal (more than 30 distinct values). Analyses will ignore them."),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
        }
        
        if(length(private$.transformedVars)>0) {
          private$.createNotice(
            paste0("variablesNotNumeric_", paste(private$.transformedVars, collapse = ", ")), 
            paste0("Treating variable(s) '", paste(private$.transformedVars, collapse = ", "), "' as numeric."),
            type = jmvcore::NoticeType$INFO
          )
        }
      },
      
      .noteNotNominalOrOrdinal = function(var) {
        if (!(var %in% private$.notNominalOrOrdinalVars)) {
          private$.notNominalOrOrdinalVars <- c(private$.notNominalOrOrdinalVars, var)
        }
        return(invisible(NULL))
      },
      
      .noteNoUsableValues = function(var) {
        if (!(var %in% private$.noUsableValuesVars)) {
          private$.noUsableValuesVars <- c(private$.noUsableValuesVars, var)
        }
        return(invisible(NULL))
      },
      
      .checkNumberOfValues = function(var, x) {
        if (length(x) == 0) {
          private$.noteNoUsableValues(var)
          return(FALSE)
        }
        return(TRUE)
      },
      
      .formatColumn = function(var, column) {
        if (is.factor(column)) {
          lev <- levels(column)
          lev_num <- suppressWarnings(as.numeric(lev))
          # sort levels in case the are not sorted in ascending order
          if (length(lev) > 0 && all(!is.na(lev_num))) {
            lev_sorted <- lev[order(lev_num)]
          } else {
            # if levels are non-numeric, sort alphabetically
            lev_sorted <- sort(lev)
          }
          return(factor(column, levels = lev_sorted, ordered = is.ordered(column)))
        } else if (length(unique(column)) <= 30) {
          if (jmvcore::canBeNumeric(column)) {
            return(as.factor(jmvcore::toNumeric(column)))
          } else {
            return(as.factor(as.character(column)))
          }
        } else {
          private$.noteNotNominalOrOrdinal(var)
          return(NULL)
        }
      },
      
      .computeFrequencies = function(column, cumH = self$options$cumH) {
        absfreqresults <- table(column)
        relfreqresults <- prop.table(absfreqresults)
        df <- data.frame(levels=names(absfreqresults), abs=as.numeric(absfreqresults), rel=as.numeric(relfreqresults))
        if (cumH) {
          df$cumulrel <- cumsum(relfreqresults)
        } 
        return(df)
      },
      
      .initFrequencyTable = function(var) {
        if(!self$options$absH) {
          return()
        }
        table <- self$results$frequencies$get(var)
        
        # passende Tabelle erstellen
        table$addColumn(name='levels', title = 'Levels', type='text')
        table$addColumn(name='abs', title = 'Abs. frequency', type='integer')
        table$addColumn(name='rel', title = 'Rel. frequency', type='number')
        
        if (self$options$cumH) {
          table$addColumn(name='cumulrel', title = 'Cumulative rel. frequency', type='number')
        }
      },
      
      .fillFrequencyTable = function(var) {
        if(!self$options$absH) {
          return()
        }
        table <- self$results$frequencies$get(var)
        data <- private$.freqData[[var]]
        for (k in seq_along(data$levels)) {
          table$addRow(data$levels[k], values = data[k,])
        }
      },
      
      .prepareCharts = function(var) {
        data <- private$.freqData[[var]]
        for (k in seq_along(private$.charttypes)) {
          type <- private$.charttypes[k]
          if(self$options[[type]]) {
            image <- self$results[[paste0(type, "charts")]]$get(var)
            relabs <- if (self$options[[paste0(type, "rel")]]) "rel" else "abs"
            y_label <- if (self$options[[paste0(type, "rel")]]) "Rel. frequency" else "Abs. frequency"
            plotData <- data.frame(levels=data$levels, y=data[[relabs]])
            image$setState(list(data=plotData, var=var, y_label=y_label, type=type))
          }
        }
      },
      
      .preparePieChart = function(var) {
        if(!self$options$pie) {
          return()
        }
        data <- private$.freqData[[var]]
        image <- self$results$piecharts$get(var)$piechartplot
        image$setState(list(data=data[,c("levels","rel")], var=var))
      },
      
      .initPieChartTable = function(var) {
        if(!self$options$pie) {
          return()
        }
        table <- self$results$piecharts$get(var)$piechartangle
        
        # passende Tabelle erstellen
        table$addColumn(name='levels', title = 'Levels', type='text')
        table$addColumn(name='rel', title = 'Rel. frequency', type='number')
        table$addColumn(name='angles', title = 'Angles (in deg.)', type='number')
      },
      
      .fillPieChartTable = function(var) {
        if(!self$options$pie) {
          return()
        }
        table <- self$results$piecharts$get(var)$piechartangle
        data <- private$.freqData[[var]]
        for (k in seq_along(data$levels)) {
          table$addRow(data$levels[k], values = list(levels = data$levels[k], rel = data$rel[k], angles = data$rel[k]*360))
        }
      },
      
      .chart=function(image_abs, ggtheme, theme, ...){
        
        if (is.null(image_abs$state)) {
          return(FALSE)
        }
        plotData <- image_abs$state$data
        var <- image_abs$state$var
        y_label <- image_abs$state$y_label
        type <- image_abs$state$type
        
        CHART = ggplot(plotData, aes(x=levels, y=y))
        if(type == "stick") {
          CHART = CHART + geom_segment(aes(x=levels, xend=levels, y=0, yend=y)) + geom_point()
        } else if(type == "vbar") {
          CHART = CHART + geom_bar(stat="identity", fill=theme$fill[2])
        } else if(type == "hbar") {
          CHART = CHART + geom_bar(stat="identity", fill=theme$fill[2]) + coord_flip()
        } else if(type == "line") {
          CHART = CHART + geom_line(aes(group = 1),col=theme$color[1]) + geom_point(col=theme$color[1])
        }
        CHART = CHART + 
          scale_x_discrete(limits=plotData$levels, guide = guide_axis(angle = ifelse(type == "hbar", 0, 90))) + 
          labs(title="", x=var, y=y_label) + 
          ggtheme
        print(CHART)
        TRUE
      },
      
      .piechartplot=function(image, ggtheme, theme, ...){
        
        if (is.null(image$state)) {
          return(FALSE)
        }
        plotData <- image$state$data
        var <- image$state$var
        
        PIECHART = ggplot(plotData, aes(x="", y=rel, fill=levels))+
          geom_bar(stat="identity", color="white")+
          coord_polar(theta="y")+
          labs(title="", x="", y="")+
          theme_void()+
          scale_fill_discrete(name=var) 
        
        print(PIECHART)
        TRUE
        
      },
      
      .prepareECDF = function(var) {
        if(!self$options$empV) {
          return()
        }
        data <- private$.ecdfData[[var]]
        # prepare data
        plotData <- data.frame(x=c(-Inf, data$levels), y=c(0, data$cumulrel), xend=c(data$levels, Inf))
        image <- self$results$ecdfs$get(var)
        image$setState(list(data=plotData))
      },
      
      .ecdf = function(image, ggtheme, theme, ...){
        if (is.null(image$state)) {
          return(FALSE)
        }
        plotData <- image$state$data
        
        ECDFPLOT = ggplot(plotData, aes(x=x,y=y,xend=xend, yend=y)) +
          geom_segment(col = theme$color[1]) +
          labs(x="x", y="F_n(x)") +
          ggtheme
        if (nrow(plotData) <= 30) {
          ECDFPLOT = ECDFPLOT + geom_point(data=plotData[-1,])
        }
        if (nrow(plotData) > 10) {
          ECDFPLOT = ECDFPLOT + scale_x_continuous(breaks=private$.get_breaks(10))
        } else {
          ECDFPLOT = ECDFPLOT + scale_x_continuous(breaks=plotData$x, labels=private$.round_breaks)
        }
        print(ECDFPLOT)
        TRUE
      },
      
      .round_breaks=function(x) {
        finite_x <- x[is.finite(x)]
        if (length(finite_x) == 0) {
          return(character(length(x)))
        }
        
        span <- max(finite_x) - min(finite_x)
        
        if (span > 100) {
          return(ifelse(is.finite(x), format(round(x, 0), scientific = FALSE, trim = TRUE), ""))
        } else {
          return(ifelse(is.finite(x), format(signif(x, 3), scientific = FALSE, trim = TRUE), ""))
        }
      },
      
      .get_breaks=function(n = 10, include_zero = FALSE) {
        function(x) {
          
          rng <- range(x, finite = TRUE)
          dmin <- rng[1]
          dmax <- rng[2]
          
          if (include_zero) {
            dmin <- min(dmin, 0)
            dmax <- max(dmax, 0)
          }
          
          Q <- c(1, 2, 2.5, 5, 10)
          
          raw_step <- (dmax - dmin) / n
          mag <- 10^floor(log10(raw_step))
          steps <- Q * mag
          
          best_score <- -Inf
          best_breaks <- NULL
          
          for (s in steps) {
            
            if (s <= 0) next
            
            br <- seq(
              floor(dmin / s) * s,
              ceiling(dmax / s) * s,
              by = s
            )
            
            n_breaks <- length(br)
            
            # HARD constraint: avoid too many breaks
            if (n_breaks > n * 1.5) next
            
            score <- -abs(n_breaks - n)
            
            if (score > best_score) {
              best_score <- score
              best_breaks <- br
            }
          }
          
          best_breaks
        }
      }
      
      )
)
