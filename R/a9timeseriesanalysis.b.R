
# This file is a generated template, your changes will not be overwritten

a9timeseriesanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "a9timeseriesanalysisClass",
    inherit = a9timeseriesanalysisBase,
    private = list(
      .transformedVars = c(),
      .missingvaluesVars = c(),
      .noticeInsertPosition = 1L,
      
      .run = function() {
        if(is.null(self$options$var)){
          return()
        }
        
        # preparation of data
        ord <- private$.getOrd()
        time <- private$.getTime()
        data <- private$.getData()
        n.entries <- length(data)
        
        # estimation of regression
        model=lm(data~time)
        y.hat=predict(model)
        
        # estimation of moving averages (without and with seasonality)
        y.stern1 <- matrix(ncol=length(ord), nrow=n.entries)
        y.stern2 <- matrix(ncol=length(ord), nrow=n.entries)
        for(j in 1:length(ord)){
          y.stern1[,j] = private$.MA(data, as.numeric(ord[j]))
          y.stern2[,j] = private$.MAS(data, as.numeric(ord[j]))
        }
        
        # add results to data
        private$.fillCols(ord, y.hat, y.stern1, y.stern2)
        # prepare plot
        private$.preparePlot(data, time, y.hat, y.stern1, y.stern2, ord)
        
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
      },
      
      # moving average
      .MA=function(y, ord){
        n=length(y)
        M=rep(NA, n)
        y.stern=rep(NA,n)
        # k ungerade
        if(ord%%2 == 1){
          k=(ord-1)/2
          M[k+1] = sum(y[1:(2*k+1)])
          for(j in (k+2):(n-k)){
            M[j] = M[j-1] - y[j-k-1] + y[j+k]
          }
          y.stern=M/(2*k+1)
        } else {
          k=ord/2
          M[k] = sum(y[1:(2*k)])
          for(j in (k+1):(n-k)){
            M[j] = M[j-1] - y[j-k] + y[j+k]
          }
          for(i in (k+1):(n-k)){
            y.stern[i] = 1/(4*k)*(M[i-1] + M[i])
          }
        }
        return(y.stern)
      },
      
      # moving average with seasonality
      .MAS=function(y, p){
        n=length(y)
        y.stern=private$.MA(y, p)
        yt=y-y.stern
        
        # choose k depending on p (odd or even)
        if(p%%2 == 1){
          k=(p-1)/2
        } else {
          k=p/2
        }
        
        # estimation of seasonal components
        s.tilde=rep(0, p)
        for(i in 1:p){
          m=floor((n-k-i)/p)
          l=ceiling((k+1-i)/p)
          for(j in l:m){
            s.tilde[i] = s.tilde[i] + yt[i+j*p]
          }
          s.tilde[i] = 1/(m-l+1)*s.tilde[i]
        }
        s.hat=s.tilde - mean(s.tilde)
        
        n.Wdh=ceiling(n/p)
        s=rep(s.hat, n.Wdh)
        s=s[1:n]
        
        y.res=y.stern+s
        return(y.res)
      },
      
      # get order of moving average and check if it is numeric
      .getOrd=function(){
        ord=self$options$Ordnung
        if(ord == ""){
          jmvcore::reject(jmvcore::format("Order or period length is empty"))
        }
        ord=as.numeric(strsplit(self$options$Ordnung, split=",")[[1]])
        if(any(is.na(ord))){
          jmvcore::reject(jmvcore::format("Order or period length is not numeric"))
        }
        return(ord)
      },
      
      # get time variable and check if it is numeric and equidistant
      .getTime=function(){
        opttime <- self$options$time
        data <- self$data
        n.entries <- length(data[[self$options$var]])
        if(is.null(opttime)){
          time <- 1:n.entries
        } else if(any(is.na(data[[opttime]]))){
          time <- 1:n.entries
          private$.createNotice("timeMissingValues", jmvcore::format("Time variable '{}' contains missing values. Time will be treated as equidistant.", opttime))
        } else {
          if(!jmvcore::canBeNumeric(data[[opttime]])){
            jmvcore::reject(jmvcore::format("Time variable is not numeric"))
          } else {
            time <- private$.toNumeric(opttime)
          }
          # Abfrage, ob Zeit aequidistant
          d=time[-1] - time[-length(time)]
          if(any(d!=d[1])){
            #Fehlermeldung: Zeiten nicht aequidistant
            jmvcore::reject(jmvcore::format("Time is not equidistant"))
          }
        }
        return(time)
      },
      
      # get data variable and check if it is numeric
      .getData=function(){
        var <- self$options$var
        if(jmvcore::canBeNumeric(self$data[[var]])){
          data <- private$.toNumeric(var)
        } else {
          jmvcore::reject(jmvcore::format("Variable is not numeric"))
        }
        if(any(is.na(data))){
          private$.createNotice("dataMissingValues", jmvcore::format("Variable '{}' contains missing values. Analyses will ignore missing values.", var))
        }
        return(data)
      },
      
      # fill results to data columns
      .fillCols=function(ord, y.hat, y.stern1, y.stern2){
        if(self$options$reg && self$results$reg$isNotFilled()){
          self$results$reg$setRowNums(rownames(self$data))
          self$results$reg$setValues(y.hat)
        }
        
        if(self$options$movavOS && self$results$movavOS$isNotFilled()){
          self$results$movavOS$setRowNums(rownames(self$data))
          self$results$movavOS$setValues(y.stern1[,1])
        }
        
        if(self$options$movavMS && self$results$movavMS$isNotFilled()){
          self$results$movavMS$setRowNums(rownames(self$data))
          self$results$movavMS$setValues(y.stern2[,1])
        }
      },
      
      # prepare plot data and set state for plot
      .preparePlot=function(data, time, y.hat, y.stern1, y.stern2, ord){
        # Plotdaten
        if(!self$options$runchart){
          return()
        }
        n.entries <- length(data)
        # different colors for original data, regression and moving averages (with and without seasonality)
        # different linetypes for different orders of moving averages
        df.plot <- data.frame(y=data, time=time, color=rep("variable", n.entries), 
                              ord=rep("none", n.entries))
        if(self$options$addest){
          if(self$options$reg){
            df.new=data.frame(y=y.hat, time=time, color=rep("regression", n.entries), 
                              ord=rep("none", n.entries))
            df.plot=rbind(df.plot, df.new)
          }
          for(j in 1:length(ord)){
            if(self$options$movavOS){
              df.new=data.frame(y=y.stern1[,j], time=time, color=rep("without seasonality", n.entries), 
                                ord=rep(ord[j], n.entries))
              df.plot=rbind(df.plot, df.new)
            } 
            if(self$options$movavMS){
              df.new=data.frame(y=y.stern2[,j], time=time, color=rep("with seasonality", n.entries),
                                ord=rep(ord[j], n.entries))
              df.plot=rbind(df.plot, df.new)
            }
          }
        }
        df.plot$ord=factor(df.plot$ord, levels=c("none", ord))
        image=self$results$run
        image$setState(df.plot)
      },
      
      
      .plot=function(image, ggtheme, theme, ...){
        if (is.null(image$state)) {
          return(FALSE)
        }
        
        plotData <- image$state
        
        runchart=ggplot(plotData) +
          geom_point(aes(x=time, y=y, color=color)) + 
          geom_line(aes(x=time, y=y, color=color, linetype=ord)) +
          labs(x=self$options$time, y=self$options$var) +
          guides(linetype=guide_legend(title="order")) +
          ggtheme
        
        print(runchart)
        
        TRUE
        
      }
      
      )
)
