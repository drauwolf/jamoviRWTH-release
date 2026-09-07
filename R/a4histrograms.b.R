
# This file is a generated template, your changes will not be overwritten

a4histrogramsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a4histrogramsClass",
    inherit = a4histrogramsBase,
    private = list(
      .transformedVars = c(),
      .missingvaluesVars = c(),
      .noticeInsertPosition = 1L,
      
      .init = function() { # init function for preparing tables
        vars = self$options$deps
        categvar = self$options$categvar
        
        # check for empty input
        if(is.null(vars) & is.null(categvar)){
          return()
        }
        
        if(!is.null(vars)) {
          for(i in 1:length(vars)) {
            var <- vars[i]
            
            private$.initPreviewTable(var)
            private$.initEquiTable(var)
          }
        }
        
        if(!is.null(categvar)) {
          private$.initCategTable(categvar)
        }
      },
      
      .run = function() {
        
        vars = self$options$deps
        categvar = self$options$categvar
        
        # check for empty input
        if(is.null(vars) & is.null(categvar)){
          return()
        }
        
        if(!is.null(vars)) {
          for(i in 1:length(vars)) {
            var <- vars[i]
            
            private$.noteMissingValues(var, self$data[[var]])
            
            # check if data is numbers
            if(!jmvcore::canBeNumeric(self$data[[var]])) {
              private$.createNotice(paste0("varNotNumeric_", var), 
                                    paste0("Variable '", var, "' is not numeric. Analyses will ignore it."),
                                    type=jmvcore::NoticeType$STRONG_WARNING)
              next
            }
            
            data <- private$.toNumeric(var)
            data <- data[!is.na(data)]
            
            private$.fillSummaryTable(var, data)
            breaks <- private$.computePreviewData(var, data)
            private$.preparePreviewPlot(var, data, breaks)
            private$.fillPreviewTable(var, data, breaks)
            equiData <- private$.computeEquiData(var, data)
            private$.prepareEquiPlot(var, data, equiData)
            private$.fillEquiTable(var, data, equiData)
          }
        }
        
        if(!is.null(categvar)) {
          private$.noteMissingValues(categvar, self$data[[categvar]])
          
          # check if data is numbers
          if(!jmvcore::canBeNumeric(self$data[[categvar]])) {
            private$.createNotice(paste0("varNotNumeric_", categvar), 
                                  paste0("Variable '", categvar, "' is not numeric. Analyses will ignore it."),
                                  type=jmvcore::NoticeType$STRONG_WARNING)
          } else {
            data <- private$.toNumeric(categvar)
            data <- data[!is.na(data)]
            
            categData <- private$.computeCategData(categvar, data)
            private$.prepareCategPlot(categvar, data, categData)
            private$.fillCategTable(categvar, data, categData)
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
      
      # summaryTable with suggestions for parameters
      .fillSummaryTable = function(var, column){  
        if (!self$options$previewplot) {
          return()
        }
        summarytable <- self$results$hists$get(var)$summarytable
        
        n <- length(column)
        
        nunique <- length(unique(column))
        if (nunique >= 30) {
          nunique = 30
        }
        
        nbins <- min(sqrt(n), 10*log10(n)) # suggested number of bins
        
        minmax = range(column)
        
        aequi <- (minmax[2]-minmax[1])/nunique # width of bins
        
        summarytable$addRow(rowKey = 1, values = list(
          minimum = minmax[1],
          maximum = minmax[2],
          datalength = n,
          noofbins = as.integer(nbins),
          aequi = aequi
        ))
      },
      
      ### Preview Table
      # 30 bins or number of unique if less than 30
      .computePreviewData = function(var, column) {
        if (!(self$options$previewplot | self$options$previewtable)) {
          return()
        }
        minmax <- range(column)
        
        nunique <- min(length(unique(column)), 30)
        
        aequi <- (minmax[2]-minmax[1])/(nunique)
        breaks <- seq(from = minmax[1], to = minmax[2], by = aequi)
        
        return(breaks)
      },
      
      .preparePreviewPlot = function(var, data, breaks){
        # basic histogram with maximum 30 bins
        if (!self$options$previewplot) {
          return()
        }
        
        image <- self$results$hists$get(var)$previewplot
        
        plotData = data.frame(x=data)
        # pass data to function that creates plot
        image$setState(list(
          data=plotData, 
          var=var, 
          breaks=breaks,
          c=1
        ))
      },
      
      # table for preview plot
      .initPreviewTable = function(var) {
        if (!self$options$previewtable) {
          return()
        }
        
        tableprev <- self$results$hists$get(var)$previewtable
        
        tableprev$addColumn(name='nr', title = 'No. j', type='number')
        tableprev$addColumn(name='klassen', title = 'Category K\U2C7C', type='string')
        tableprev$addColumn(name='abs', title = 'Abs. frequency n(K\U2C7C)', type='integer')
        tableprev$addColumn(name='rel', title = 'Rel. frequency f(K\U2C7C)', type='number')
        tableprev$addColumn(name='klassenbreite', title = 'Width b\U2C7C', type='number')
        tableprev$addColumn(name='klassenhoehe', title = 'Height h\U2C7C', type='number')
      },
      
      .fillPreviewTable = function(var, data, breaks) {
        if (!self$options$previewtable) {
          return()
        }
        tableprev <- self$results$hists$get(var)$previewtable
        
        klassen <- cut(data, breaks=breaks, include.lowest = T, dig.lab = 12)
        levels <- data.frame(table(klassen))[,1]
        absfreqresults <- data.frame(table(klassen))[,2]
        relfreqresults <- as.numeric(absfreqresults)/sum(as.numeric(absfreqresults))
        
        # calculate binwidth
        labs <- levels(klassen)
        breite1 <- cbind(as.numeric( sub("\\[(.+),.*", "\\1", labs[1]) ),
                         as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs[1]) ))
        labs2 <- labs[2:length(labs)]
        breite2 <- cbind(as.numeric( sub("\\((.+),.*", "\\1", labs2) ),
                         as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs2) ))
        breite <- cbind(lower = c(breite1[1,1],breite2[,1]), upper = c(breite1[1,2],breite2[,2]))
        
        klassenbreiteresults <- breite[,2] - breite[,1]
        
        # calculate binheight
        klassenhoeheresults <- relfreqresults/klassenbreiteresults
        
        # fill table
        for (k in seq_along(levels)) {
          tableprev$addRow(levels[k], values = list(levels = levels[k]))
          
          tableprev$setRow(
            rowNo=k,
            values=list(
              nr=k,
              klassen=toString(levels[k]),
              abs=absfreqresults[k],
              rel=relfreqresults[k],
              klassenbreite=klassenbreiteresults[k],
              klassenhoehe=klassenhoeheresults[k]
            )
          )
        }
      },
      
      ###### Equidistant ########
      # histogram with number of bins specified by user
      .computeEquiData = function(var, data) {
        if (!self$options$checkboxequi) {
          return()
        }
        
        minmax = range(data)
        
        nbins <- self$options$equibins
        
        if(nbins %% 1 != 0) {
          # error message: non-integer input for number of bins
          jmvcore::reject(jmvcore::format("The number of bins must be an integer!"))
        }
        if(nbins < 1) {
          jmvcore::reject(jmvcore::format("Number of equidistant bins must be between 1 and Inf (is {nbins})!", nbins=nbins))
        }
        
        aequi <- (minmax[2]-minmax[1])/(nbins)
        breaks = seq(from = minmax[1], to = minmax[2], by = aequi)
        
        # optional factor for scaling the y values
        c <- 1
        if (self$options$propfactorequi) {
          c <- self$options$cequi
        }
        
        return(list(breaks=breaks, aequi=aequi, range=minmax, c=c))
      },
      
      .prepareEquiPlot = function(var, data, equiData) {
        # equidistant histogram
        if (!self$options$checkboxequi) {
          return()
        }
        
        image <- self$results$hists$get(var)$equiplot
        
        plotData <- data.frame(x=data)
        aequi <- equiData$aequi
        minmax <- equiData$range
        
        # number of digits for displayed labels
        noofdigits <- max(ceiling(-log10(aequi)), 0) + 1
        
        breaknames <- as.character(round(seq(from = minmax[1], to = minmax[2], by = aequi), digits = noofdigits))
        
        
        image$setState(list(
          data=plotData, 
          var=var, 
          breaks=equiData$breaks,
          breaknames=breaknames,
          c=equiData$c
        ))
      },
      
      .initEquiTable = function(var) {
        ### Frequency Table for custom equidistant plot (initialze)
        if (!self$options$checkboxequi | !self$options$equitable) {
          return()
        }
        
        tableeq <- self$results$hists$get(var)$equitable
        
        tableeq$addColumn(name='nr', title = 'No. j', type='number')
        tableeq$addColumn(name='klassen', title = 'Category K\U2C7C', type='string')
        tableeq$addColumn(name='abs', title = 'Abs. frequency n(K\U2C7C)', type='integer')
        tableeq$addColumn(name='rel', title = 'Rel. frequency f(K\U2C7C)', type='number')
        tableeq$addColumn(name='klassenbreite', title = 'Width b\U2C7C', type='number')
        tableeq$addColumn(name='klassenhoehe', title = 'Height h\U2C7C', type='number')
      },
      
      .fillEquiTable = function(var, data, equiData) {
        ### Frequency Table for custom equidistant plot (fill)
        if (!self$options$checkboxequi | !self$options$equitable) {
          return()
        }
        
        tableeq <- self$results$hists$get(var)$equitable
        
        klassen <- cut(data, breaks=equiData$breaks, include.lowest = T, dig.lab = 12)
        
        levels <- data.frame(table(klassen))[,1]
        absfreqresults <- data.frame(table(klassen))[,2]
        relfreqresults <- as.numeric(absfreqresults)/sum(as.numeric(absfreqresults))
        
        # calculate binwidth
        labs <- levels(klassen)
        breite1 <- cbind(as.numeric( sub("\\[(.+),.*", "\\1", labs[1]) ),
                         as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs[1]) ))
        labs2 <- labs[2:length(labs)]
        breite2 <- cbind(as.numeric( sub("\\((.+),.*", "\\1", labs2) ),
                         as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs2) ))
        breite <- cbind(lower = c(breite1[1,1],breite2[,1]), upper = c(breite1[1,2],breite2[,2]))
        
        klassenbreiteresults <- breite[,2] - breite[,1]
        
        # calculate binheight
        klassenhoeheresults <- equiData$c * relfreqresults/klassenbreiteresults
        
        # fill table
        for (k in seq_along(levels)) {
          tableeq$addRow(levels[k], values = list(levels = levels[k]))
          
          tableeq$setRow(
            rowNo=k,
            values=list(
              nr=k,
              klassen=toString(levels[k]),
              abs=absfreqresults[k],
              rel=relfreqresults[k],
              klassenbreite=klassenbreiteresults[k],
              klassenhoehe=klassenhoeheresults[k]
            )
          )
        }
      },
      
      ###### Categories ########
      # histogram with breaks specified by the user
      .computeCategData = function(var, data) {
        if (!self$options$checkboxcateg) {
          return()
        }
        
        # scaling factor for y-axis
        c <- 1
        if (self$options$propfactorcateg) {
          c <- self$options$ccateg
        }
        
        # interpret user input
        breaksstring <- self$options$breakValues
        
        if(breaksstring == ""){
          jmvcore::reject(jmvcore::format("No breaks specified"))
        }
        
        breaksnumeric <- as.numeric(strsplit(breaksstring, split=",")[[1]])
        # check if valid input
        if(any(is.na(breaksnumeric))){
          jmvcore::reject(jmvcore::format("Custom categories must be numeric values. Input value for 'breaks at' is not numeric."))
        }
        breaks <- sort(breaksnumeric)
        
        # add minimum and maximum to breaks if not included
        minmax = range(data)
        if (min(breaks) > minmax[1]) {
          breaks = c(minmax[1],breaks)
        }
        if (max(breaks) < minmax[2]) {
          breaks = c(breaks,minmax[2])
        }
        
        return(list(breaks=breaks, c=c))
      },
      
      .prepareCategPlot = function(var, data, categData) {
        # Categories Histogram
        if (!self$options$checkboxcateg) {
          return()
        }
        
        image <- self$results$categhist$categplot
        
        plotData <- data.frame(x = data)
        breaknames <- as.character(round(categData$breaks, digits = 2))
        
        # data for function that creates plot
        image$setState(list(
          data=plotData, 
          var=var, 
          breaks=categData$breaks,
          breaknames=breaknames,
          c=categData$c
        ))
      },
      
      .initCategTable = function(var) {
        # frequency table for histogram with user specified breaks (initialize)
        if (!self$options$checkboxcateg | !self$options$categtable) {
          return()
        }
        
        table <- self$results$categhist$categtable
        
        table$addColumn(name='nr', title = 'No. j', type='number')
        table$addColumn(name='klassen', title = 'Category K\U2C7C', type='string')
        table$addColumn(name='abs', title = 'Abs. frequency n(K\U2C7C)', type='integer')
        table$addColumn(name='rel', title = 'Rel. frequency f(K\U2C7C)', type='number')
        table$addColumn(name='klassenbreite', title = 'Width b\U2C7C', type='number')
        table$addColumn(name='klassenhoehe', title = 'Height h\U2C7C', type='number')
      },
      
      .fillCategTable = function(var, data, categData) {
        # frequency table for histogram with user specified breaks (fill)
        if (!self$options$checkboxcateg | !self$options$categtable) {
          return()
        }
        
        table <- self$results$categhist$categtable
        
        klassen <- cut(data, breaks=categData$breaks, include.lowest = T, dig.lab = 4)
        
        levels <- data.frame(table(klassen))[,1]
        absfreqresults <- data.frame(table(klassen))[,2]
        relfreqresults <- as.numeric(absfreqresults)/sum(as.numeric(absfreqresults))
        
        # calculate binwidth
        labs <- levels(klassen)
        breite1 <- cbind(as.numeric( sub("\\[(.+),.*", "\\1", labs[1]) ),
                         as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs[1]) ))
        labs2 <- labs[2:length(labs)]
        breite2 <- cbind(as.numeric( sub("\\((.+),.*", "\\1", labs2) ),
                         as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs2) ))
        breite <- cbind(lower = c(breite1[1,1],breite2[,1]), upper = c(breite1[1,2],breite2[,2]))
        
        klassenbreiteresults <- breite[,2] - breite[,1]
        
        # calculate binheight
        klassenhoeheresults <- categData$c * relfreqresults/klassenbreiteresults
        
        for (k in seq_along(levels)) {
          table$addRow(levels[k], values = list(levels = levels[k]))
          
          table$setRow(
            rowNo=k,
            values=list(
              nr=k,
              klassen=toString(levels[k]),
              abs=absfreqresults[k],
              rel=relfreqresults[k],
              klassenbreite=klassenbreiteresults[k],
              klassenhoehe=klassenhoeheresults[k]
            )
          )
        }
      },
      
      # function for creating the histograms
      .hist = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) {
          return(FALSE)
        }
        data <- image$state$data
        var <- image$state$var
        breaks <- image$state$breaks
        c <- image$state$c
        
        HISTOGRAM = ggplot(data, aes(x=x)) +
          geom_histogram(aes(y=..density..*c), breaks = breaks,  col = theme$color[1], fill = theme$fill[2]) +
          labs(title="", x=var, y="")
        
        # option to specify breaknames
        if ("breaknames" %in% names(image$state)) {
          HISTOGRAM = HISTOGRAM + scale_x_continuous(breaks = breaks, labels = image$state$breaknames,
                                                     guide = guide_axis(angle = 90))
        }
        HISTOGRAM = HISTOGRAM + ggtheme
        print(HISTOGRAM)
        TRUE
      }
      
      )
)
