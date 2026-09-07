
# This file is a generated template, your changes will not be overwritten

a3descriptivemeasuresClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a3descriptivemeasuresClass",
    inherit = a3descriptivemeasuresBase,
    private = list(
      .dispersionfuns = NULL, # later list of functions for dispersion measures
      .tendencyfuns = NULL, # later list of functions for central tendency measures
      
      # functions for central tendency measures that are allowed
      .allowed = list(
        nominal = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        ordinal = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
        numeric = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
      ),
      
      # names of the columns in the results table
      .tendTableNames = c('min', 'uQuart', 'mod', 'med', 'mean', 'geomean', 'harmmean', 'oQuart', 'max', 'quant'),
      .dispTableNames = c('range', 'QAbst', 'varianz', 'stAbw', 'absAbw', 'varkoeff'),
      
      # list of weights (transformed to numeric)
      .weights = list(),
      # list of arguments for the functions (x and w) in .dispersionfuns and .tendencyfuns
      .funArgs = list(),
      
      # store the names of variables that have been transformed to numeric with a notice
      .transformedVars = c(),
      .missingvaluesVars = c(),
      .noticeInsertPosition = 1L,
      
      .init=function(){
        if(is.null(self$options$vars) && is.null(self$options$varrank)){
          return()
        }
        private$.initTendTable()
        private$.initDispTable()
      },
      .run = function() {
        
        # leere Eingabe liefert leere Ausgabe
        if(is.null(self$options$vars) && is.null(self$options$varrank)){
          private$.hideAll()
          return()
        }

        private$.orderStats()
        
        # check if inputs are valid (quant, weights, group variable)
        if(!private$.checkInputs()){
          private$.hideAll()
          private$.showFinalNotices()
          return()
        }
        # fill funclists for central tendency and dispersion measures
        private$.createFunLists()

        res = private$.sortVars()
        data = res$data
        vartypes = res$vartypes

        if(is.null(data) || is.null(vartypes) || length(vartypes) == 0){
          private$.hideAll()
          private$.showFinalNotices()
          return()
        }
        
        private$.fillTendTable(vartypes, data)
        private$.fillDispTable(vartypes, data)
        private$.preparePlot(vartypes, data)

        private$.showFinalNotices()
      },
      
      .hideAll = function() {
        self$results$Central$setVisible(FALSE)
        self$results$Disp$setVisible(FALSE)
        self$results$boxplot$setVisible(FALSE)
      },
      
      # calculate quantile for numeric and ordinal data
      .quantile=function(x,qt=0.5){
        if(is.factor(x) & is.ordered(x)){
          # ordinale Daten
          np=length(x)*qt
          sortiert=sort(x)
          
          if(ceiling(np)!=floor(np)){
            # np keine natuerliche Zahl
            return(as.character(sortiert[ceiling(np)]))
          }else{
            # np eine natuerliche Zahl
            quantil=as.character(unique(sortiert[c(np, np+1)]))
            # schoenere Ausgabe fuer den Fall zweier Mediane
            return(paste(quantil, collapse=", "))
          }
        }
        return(quantile(x, type = 2, probs = qt, names = FALSE))
      },
      
      # calculate mode for numeric, ordinal and nominal data
      .mode=function(x){
        ux=unique(x)
        tab=tabulate(match(x, ux))
        values=ux[tab == max(tab)]
        return(paste(values, collapse="; "))
      },
      
      # calculate mean for numeric data, weighted or unweighted
      .mean=function(x, w, useWeights=FALSE){
        if(useWeights){
          return(weighted.mean(x, w))
        } else {
          return(mean(x))
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
      
      .checkInputs=function(){
        if(is.null(self$options$vars)){
          return(TRUE)
        }
        p = self$options$pquant
        # check if valid quantile values are given
        if(self$options$quant){
          if(p == ""){
            private$.createNotice("emptyQuantile", "Value for %-quantile is empty", type = jmvcore::NoticeType$ERROR)
            return(FALSE)
          }
          p=as.numeric(strsplit(self$options$pquant, split=",")[[1]])
          if(any(is.na(p))){
            private$.createNotice("nonNumericQuantile", "Value for %-quantile is not numeric", type = jmvcore::NoticeType$ERROR)
            return(FALSE)
          } else if (any(p < 0) || any(p > 100)){
            private$.createNotice("outOfBoundsQuantile", "Value for %-quantile is not between 0 and 100", type = jmvcore::NoticeType$ERROR)
            return(FALSE)
          }
        }
        
        # check if weights are valid
        if(self$options$wmean || self$options$wgeomean || self$options$wharmmean){
          if(!is.null(self$options$Gew)) {
            if(!jmvcore::canBeNumeric(self$data[[self$options$Gew]])){
              private$.createNotice("nonNumericWeight", "Weight variable is not numeric", type = jmvcore::NoticeType$ERROR)
              return(FALSE)
            }
            private$.noteMissingValues(self$options$Gew, self$data[[self$options$Gew]])
            private$.weights = private$.toNumeric(self$options$Gew)
            private$.weights[is.na(private$.weights)] = 0 # set missing weights to 0
            if(sum(private$.weights) != 1){ # weights must sum up to 1
              private$.createNotice("weightsNotSumToOne", "Weights do not sum up to 1", type = jmvcore::NoticeType$ERROR)
              return(FALSE)
            }
            if(any(private$.weights < 0)){ # weights must be non-negative
              private$.createNotice("negativeWeights", "Weights cannot be negative", type = jmvcore::NoticeType$ERROR)
              return(FALSE)
            }
          } else {
            private$.createNotice("noWeightVariable", "No weight variable specified", type = jmvcore::NoticeType$ERROR)
            return(FALSE)
          }
        }
        
        # check if group variable is not numeric
        if(!is.null(self$options$group)){
          if(is.numeric(self$data[[self$options$group]])){
            private$.createNotice("numericGroup", "Grouping variable cannot be numeric!", type = jmvcore::NoticeType$ERROR)
            return(FALSE)
          }
          if(all(is.na(self$data[[self$options$group]]))){
            private$.createNotice("allMissingGroup", "Grouping variable contains only missing values!", type = jmvcore::NoticeType$ERROR)
            return(FALSE)
          }
        }
        return(TRUE)
      },
      
      # create lists of functions for central tendency and dispersion measures
      .createFunLists = function(){
        private$.tendencyfuns = list(
          MINIMUM = function() as.character(min(private$.funArgs$x)),
          LOWQUARTILE = function() private$.quantile(private$.funArgs$x, 0.25),
          MODE = function() private$.mode(private$.funArgs$x),
          MEDIAN = function() private$.quantile(private$.funArgs$x, 0.5),
          AMEAN = function() private$.mean(private$.funArgs$x, private$.funArgs$w, self$options$wmean),
          GEOMEAN = function() exp(private$.mean(log(private$.funArgs$x), private$.funArgs$w, self$options$wgeomean)),
          HMEAN = function() 1/private$.mean(1/private$.funArgs$x, private$.funArgs$w, self$options$wharmmean),
          UPQUARTILE = function() private$.quantile(private$.funArgs$x, 0.75),
          MAXIMUM = function() as.character(max(private$.funArgs$x)),
          EMPQUANTILE = function() {
            p = as.numeric(strsplit(self$options$pquant, split=",")[[1]])/100
            qs = list()
            for(i in seq_along(p)){
              qs[[i]] = private$.quantile(private$.funArgs$x, p[i])
            }
            paste(qs, collapse=" : ")
          }
        )
        
        private$.dispersionfuns = list(
          RANGE = function() max(private$.funArgs$x)-min(private$.funArgs$x),
          IQR = function() private$.quantile(private$.funArgs$x, 0.75) - private$.quantile(private$.funArgs$x, 0.25),
          VARIANCE = function() mean((private$.funArgs$x - mean(private$.funArgs$x))^2),
          SD = function() sqrt(mean((private$.funArgs$x - mean(private$.funArgs$x))^2)),
          MAD = function() median(abs(private$.funArgs$x - quantile(private$.funArgs$x, 0.5))),
          CV = function() mean((private$.funArgs$x - mean(private$.funArgs$x))^2)/mean(private$.funArgs$x)
        )
      },
      
      .orderStats = function(){
        varrank <- self$options$varrank
        if(is.null(varrank)){
          return()
        }
        if(!self$options$order && !self$options$rank){
          return()
        }
        private$.noteMissingValues(varrank, self$data[[varrank]])
        if(jmvcore::canBeNumeric(self$data[[varrank]])){
          data <- private$.toNumeric(varrank)
        } else {
          data <- self$data[[varrank]]
        }
        # Output order
        if(self$options$order && self$results$order$isNotFilled()) {
          os=sort(data)
          title=paste("Order statistics", varrank)
          
          self$results$order$setRowNums(rownames(self$data))
          self$results$order$setTitle(title)
          self$results$order$setValues(os)
        }
        
        # Output Rank
        if(self$options$rank && self$results$rank$isNotFilled()) {
          ranks=rank(data, na.last="keep")
          title=paste("Rank", varrank)
          
          self$results$rank$setRowNums(rownames(self$data))
          self$results$rank$setTitle(title)
          self$results$rank$setValues(ranks)
        }
      },

      .sortVars = function(){
        centralselected = c(self$options$min, self$options$uQuart, self$options$mod, 
                            self$options$med, self$options$mean, self$options$geomean, self$options$harmmean, 
                            self$options$oQuart, self$options$max, self$options$quant)
        dispersionselected = c(self$options$range, self$options$QAbst, self$options$varianz, 
                               self$options$stAbw, self$options$absAbw, self$options$varkoeff)
        if(!any(centralselected) && !any(dispersionselected)){
          return()
        } 
        if(is.null(self$options$vars)){
          return()
        }
        vartypes = list()
        data = jmvcore::select(self$data, self$options$vars)
        for(var in self$options$vars){
          if(all(is.na(data[[var]]))){
            private$.createNotice(paste0("allMissing_", var), 
                                  paste0("Variable '", var, "' contains only missing values. It will be ignored in the analysis."),
                                  type=jmvcore::NoticeType$STRONG_WARNING)
            next
          }
          if (jmvcore::canBeNumeric(data[[var]])) {
            vartypes[[var]] = "numeric"
            data[[var]] = private$.toNumeric(var)
          } else if (is.factor(data[[var]]) & is.ordered(data[[var]])) {
            vartypes[[var]] = "ordinal"
          } else if (is.factor(data[[var]]) & !is.ordered(data[[var]])) {
            vartypes[[var]] = "nominal"
          }
          private$.noteMissingValues(var, data[[var]])
        }

        if(any(vartypes == "ordinal")){
          if(any(centralselected & !private$.allowed$ordinal)){
            private$.createNotice("selectedMeasuresNotApplicable_ordinal", 
                                  paste0("Some of the selected measures of central tendency are not applicable to the ordinal data: ", paste(names(vartypes[vartypes == "ordinal"]), collapse = ", "), "."),
                                  type=jmvcore::NoticeType$STRONG_WARNING)
          }
        }
        if(any(vartypes == "nominal")){
          if(any(centralselected & !private$.allowed$nominal)){
            private$.createNotice("selectedMeasuresNotApplicable_nominal", 
                                  paste0("Some of the selected measures of central tendency are not applicable to the nominal data: ", paste(names(vartypes[vartypes == "nominal"]), collapse = ", "), "."),
                                  type=jmvcore::NoticeType$STRONG_WARNING)
          }
        }
        if(any(vartypes != "numeric") && any(dispersionselected)){
          private$.createNotice("measuresOfDispersionNotApplicable_nominal", 
                                paste0("Measures of dispersion are not applicable to the nominal and ordinal data: ", paste(names(vartypes[vartypes == "nominal" | vartypes == "ordinal"]), collapse = ", "), "."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
        }
        if((any(vartypes != "numeric")) && (self$options$box)){
          private$.createNotice("boxplotsNotApplicable", 
                                paste0("Boxplots can only be created for numeric data, not for: ", paste(names(vartypes[vartypes != "numeric"]), collapse = ", "), "."),
                                type=jmvcore::NoticeType$STRONG_WARNING)
        }

        if(!is.null(self$options$group)){
          data[[self$options$group]] = self$data[[self$options$group]]
          private$.noteMissingValues(self$options$group, data[[self$options$group]])
        }

        return(list(data = data, vartypes = vartypes))
      },
      
      .initTendTable = function(){
        centralselected = c(self$options$min, self$options$uQuart, self$options$mod, 
                            self$options$med, self$options$mean, self$options$geomean, self$options$harmmean, 
                            self$options$oQuart, self$options$max, self$options$quant)
        if(!any(centralselected)){
          return()
        }
        table=self$results$Central
        table$addColumn(name='Attribute', title = 'Attribute', type='text')
        if(!is.null(self$options$group)) table$addColumn(name='Group', title = 'Group', type='text')
        if(self$options$min) table$addColumn(name=private$.tendTableNames[1], title = 'Minimum', type='number')
        if(self$options$uQuart) table$addColumn(name=private$.tendTableNames[2], title = 'Lower quartile', type='number')
        if(self$options$mod) table$addColumn(name=private$.tendTableNames[3], title = 'Mode', type='number')
        if(self$options$med) table$addColumn(name=private$.tendTableNames[4], title = 'Median', type='number')
        if(self$options$mean) table$addColumn(name=private$.tendTableNames[5], title = 'Arithm. mean', type='number')
        if(self$options$geomean) table$addColumn(name=private$.tendTableNames[6], title = 'Geom. mean', type='number')
        if(self$options$harmmean) table$addColumn(name=private$.tendTableNames[7], title = 'Harm. mean', type='number')
        if(self$options$oQuart) table$addColumn(name=private$.tendTableNames[8], title = 'Upper quartile', type='number')
        if(self$options$max) table$addColumn(name=private$.tendTableNames[9], title = 'Maximum', type='number')
        if(self$options$quant) table$addColumn(name=private$.tendTableNames[10], title = 'Quantiles', type='number')
      },
      
      .fillTendTable = function(vartypes, data){
        centralselected = c(self$options$min, self$options$uQuart, self$options$mod, 
                            self$options$med, self$options$mean, self$options$geomean, self$options$harmmean, 
                            self$options$oQuart, self$options$max, self$options$quant)
        if(!any(centralselected)){
          return()
        }
        table=self$results$Central
        group=self$options$group
        for(i in seq_along(vartypes)){
          var=names(vartypes)[i]

          funs = private$.tendencyfuns[centralselected & private$.allowed[[vartypes[[var]]]]]
          colnames = private$.tendTableNames[centralselected & private$.allowed[[vartypes[[var]]]]]
          
          if(is.null(group)) {
            keep = !is.na(data[[var]])
            splitted = list(data[[var]][keep])
            w_splitted = list(private$.weights[keep])
          } else {
            keep = !is.na(data[[var]]) & !is.na(data[[group]])
            splitted = split(data[[var]][keep], data[[group]][keep])
            w_splitted = split(private$.weights[keep], data[[group]][keep])
          }
          for(j in seq_along(splitted)){
            private$.funArgs$x = splitted[[j]]
            private$.funArgs$w = w_splitted[[j]]
            tendvals = lapply(funs, function(f) f())
            row = list()
            row[['Attribute']] = var
            if(!is.null(group)) row[['Group']] = names(splitted)[j]
            for(k in seq_along(tendvals)){
              row[[colnames[k]]] = tendvals[[k]]
            }
            table$addRow(rowKey=i, values=row)
          }
        }
      },
      
      .initDispTable = function(){
        dispersionselected = c(self$options$range, self$options$QAbst, self$options$varianz, 
                               self$options$stAbw, self$options$absAbw, self$options$varkoeff)
        if(!any(dispersionselected)){
          return()
        }
        table=self$results$Disp
        table$addColumn(name='Attribute', title = 'Attribute', type='text')
        if(!is.null(self$options$group)) table$addColumn(name='Group', title = 'Group', type='text')
        if(self$options$range) table$addColumn(name=private$.dispTableNames[1], title = 'Range', type='number')
        if(self$options$QAbst) table$addColumn(name=private$.dispTableNames[2], title = 'Interquartile range', type='number')
        if(self$options$varianz) table$addColumn(name=private$.dispTableNames[3], title = 'Variance', type='number')
        if(self$options$stAbw) table$addColumn(name=private$.dispTableNames[4], title = 'Standard deviation', type='number')
        if(self$options$absAbw) table$addColumn(name=private$.dispTableNames[5], title = 'Median absolute deviation', type='number')
        if(self$options$varkoeff) table$addColumn(name=private$.dispTableNames[6], title = 'Coefficient of variation', type='number')
      },
      
      .fillDispTable = function(vartypes, data){
        dispersionselected = c(self$options$range, self$options$QAbst, self$options$varianz, 
                               self$options$stAbw, self$options$absAbw, self$options$varkoeff)
        if(!any(dispersionselected)){
          return()
        }
        table=self$results$Disp
        group = self$options$group
        funs = private$.dispersionfuns[dispersionselected]
        colnames = private$.dispTableNames[dispersionselected]
        for(i in seq_along(vartypes)){
          var=names(vartypes)[i]
          
          if(vartypes[[var]] == "numeric") {
            private$.noteMissingValues(var, data[[var]])
            if(is.null(group)) {
              keep = !is.na(data[[var]])
              splitted = list(data[[var]][keep])
            } else {
              keep = !is.na(data[[var]]) & !is.na(data[[group]])
              splitted = split(data[[var]], data[[group]][keep])
            }
            for(j in seq_along(splitted)){
              private$.funArgs$x = splitted[[j]]
              dispvals = lapply(funs, function(f) f())
              row = list()
              row[['Attribute']] = var
              if(!is.null(group)) row[['Group']] = names(splitted)[j]
              for(k in 1:sum(dispersionselected)){
                row[[colnames[k]]] = dispvals[[k]]
              }
              table$addRow(rowKey=i, values=row)
            }
          }
        }
      },
      
      .preparePlot = function(vartypes, data){
        if(!self$options$box){
          return()
        }
        group=self$options$group

        plotData = c()
        varnames = c()
        n.vars = 0
        n.entries = length(data[,1])
        
        for(i in seq_along(vartypes)){
          var=names(vartypes)[i]
          if(vartypes[[var]] != "numeric"){
            next
          }
          column=data[[var]]
          private$.noteMissingValues(var, column)
          n.vars = n.vars + 1
          varnames = c(varnames, rep(var, n.entries))
          plotData = c(plotData, column)
        }
        
        image = self$results$boxplot
        if (is.null(group)){
          image$setState(data.frame(data=plotData, vars=varnames))
        } else {
          keep <- !is.na(data[[group]])
          image$setState(data.frame(data=plotData[rep(keep, n.vars)], group=rep(data[[group]][keep], n.vars), vars=varnames))
        }
      },
      
      # plot function for the boxplot
      .plot=function(image, ggtheme, theme, ...){
        # leere Eingabe liefert leere Ausgabe
        if (is.null(image$state)) {
          return(FALSE)
        }
        
        # define the summary function
        boxplot = function(x) {
          q = c(0.25, 0.5, 0.75)
          
          d = c(min(x), quantile(x, type=2, probs = q), max(x))
          names(d) = c("ymin", "lower", "middle", "upper", "ymax")
          return(d)
        }
        
        plot.data=image$state
        
        aes <- NULL
        x_lab <- ""
        y_lab <- ""
        n_col <- 1
        
        # calculate the aesthetics, labels and columns for the plot depending on the options
        if(self$options$Richtung=="vertical"){
          if(is.null(self$options$group)){
            aes <- aes(x=0, y=data)
            n_col <- 3
          } else {
            aes <- aes(x=group, y=data)
            x_lab <- self$options$group
          }
        } else {
          if(is.null(self$options$group)){
            if(self$options$variante=="base"){
              aes <- aes(x=0, y=data)
            } else {
              aes <- aes(y=0, x=data)
            }
          } else {
            aes <- aes(y=group, x=data)
            y_lab <- self$options$group
          }
        }
        
        plot <- ggplot(plot.data, aes) + labs(x=x_lab, y=y_lab) + facet_wrap(~vars, scale = "free", ncol = n_col) + ggtheme
        if(self$options$variante=="base"){
          plot <- plot + stat_summary(fun.data = boxplot, geom="boxplot", fill=theme$fill[2]) +
            stat_summary(fun.data = boxplot, geom="errorbar", width=0.5)
        } else {
          plot <- plot + geom_boxplot(fill=theme$fill[2]) + 
            stat_boxplot(geom = 'errorbar', width=0.5)
        }
        
        if(is.null(self$options$group) && self$options$Richtung=="horizontal"){
          plot <- plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.line.y = element_blank())
        } else if(is.null(self$options$group)){
          plot <- plot + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.line.x = element_blank())
        }
        
        if(self$options$variante=="base" && self$options$Richtung=="horizontal" && is.null(self$options$group)){
          plot <- plot + coord_flip()
        }
        
        print(plot)
        TRUE
        
      })
    
)
