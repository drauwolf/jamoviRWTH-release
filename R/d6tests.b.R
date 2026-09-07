# This file is a generated template, your changes will not be overwritten

d6testsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "d6testsClass",
  inherit = d6testsBase,
  private = list(
    # store the names of variables that have been transformed to numeric with a notice
    .transformedVars = c(),
    # store the names of variables that have missing values
    .missingvaluesVars = c(),
    .noticeInsertPosition = 1L,
    .run = function() {
      private$.noticeInsertPosition <- 1L
      
      # return nothing if input is empty
      numberOfVariables <- private$.numberOfVariables()
      if(numberOfVariables == 0) {
        return()
      }
      
      # run analyses
      private$.runOneSampleNormalTest()
      private$.runPariedSamplesNormalTest()
      private$.runTwoSampleNormalTest()
      private$.runOneSampleBinomialTest()
      private$.runTwoSampleBinomialTest()
      private$.runOneSampleNonparametricTest()
      private$.runTwoSampleNonparametricTest()
      private$.runContingencyAnalysis()
      
      # give notice for all transformed variables and variables with missing values (if applicable)
      private$.showFinalNotices()
    },
    
    # Normal distribution - one sample
    .runOneSampleNormalTest = function() {
      
      varnormalones <- self$options$varnormalones
      normalphaones <- self$options$normalphaones
      
      testname <- "Normal distribution - one sample"
      testnameshort <- "NORMone"
      
      selectedTableNames <- private$.selectedTables(c(ztest = self$options$ztest,ttest = self$options$ttest,
                                                      chitestknownmu = self$options$chitestknownmu,chitest = self$options$chitest))
      anyTestSelected <- private$.anySelected(c(self$options$ztest,self$options$ttest,self$options$chitestknownmu,self$options$chitest))
      
      private$.withOneSampleAnalysis(
        var = varnormalones,
        alpha = normalphaones,
        option = anyTestSelected,
        selectedTableNames = selectedTableNames,
        testname = testname,
        testnameshort = testnameshort,
        callback = function(testdata) {
          
          distinctValuesValid <- private$.checkAtLeastTwoDistinctValues(varname = varnormalones,x = testdata,tableNames = selectedTableNames,
                                                                        testname = testname,testnameshort = testnameshort)
          
          if (!isTRUE(distinctValuesValid)) {
            return(invisible(NULL))
          }
          
          inputStatus <- private$.checkOneSampleNormalInputs(testname = testname,testnameshort = testnameshort)
          zStatus <- list(typeIInH0 = FALSE,typeIIInH1 = FALSE,minnCanCompute = FALSE)
          
          if (isTRUE(self$options$ztest) && isTRUE(inputStatus$zSigmaValid)) {
            zStatus <- private$.checkOneSampleZAdditionalInputs(mu0 = self$options$ztestvalue,side = self$options$ztestside,
                                                                betastar = self$options$ztestexpdesignbeta,delta0 = self$options$ztestexpdesigndelta,
                                                                typeImu = self$options$ztesttypeIerrorvalue,typeIImu = self$options$ztesttypeIIerrorvalue,
                                                                typeIOption = self$options$ztesttypeIerror,typeIIOption = self$options$ztesttypeIIerror,
                                                                expDesignOption = isTRUE(self$options$ztestexpdesign),testname = testname,testnameshort = testnameshort)
          }
          
          tests <- list(
            list(
              enabled = isTRUE(self$options$ztest) && isTRUE(inputStatus$zSigmaValid),
              table = self$results$ztest,
              validate = NULL,
              compute = function() {
                private$.computeZTestOneSample(x = testdata,alpha = normalphaones,mu0 = self$options$ztestvalue,sigma2 = self$options$ztestsigma,
                                               side = self$options$ztestside,powermu = self$options$ztestpowervalue,typeImu = self$options$ztesttypeIerrorvalue,
                                               typeIImu = self$options$ztesttypeIIerrorvalue,betastar = self$options$ztestexpdesignbeta,delta0 = self$options$ztestexpdesigndelta,
                                               typeIInH0 = zStatus$typeIInH0,typeIIInH1 = zStatus$typeIIInH1,minnCanCompute = zStatus$minnCanCompute)
              }
            ),
            list(
              enabled = isTRUE(self$options$ttest),
              table = self$results$ttest,
              validate = NULL,
              compute = function() {
                private$.computeTTestOneSample(x = testdata,alpha = normalphaones,mu0 = self$options$ttestvalue,side = self$options$ttestside)
              }
            ),
            list(
              enabled = isTRUE(self$options$chitestknownmu) &&
                isTRUE(inputStatus$chiKnownSigmaValid),
              table = self$results$chitestknownmu,
              validate = NULL,
              compute = function() {
                private$.computeChiSquareVarianceTestOneSample(x = testdata,alpha = normalphaones,
                                                               sigma0 = self$options$chitestvalueknownmu,side = self$options$chitestsideknownmu,
                                                               knownMu = TRUE,mu = self$options$chitestmu)
              }
            ),
            list(
              enabled = isTRUE(self$options$chitest) &&
                isTRUE(inputStatus$chiSigmaValid),
              table = self$results$chitest,
              validate = NULL,
              compute = function() {
                private$.computeChiSquareVarianceTestOneSample(x = testdata,alpha = normalphaones,sigma0 = self$options$chitestvalue,
                                                               side = self$options$chitestside,knownMu = FALSE,mu = NULL)
              }
            )
          )
          
          private$.runEnabledTests(tests)
          
          return(invisible(NULL))
        }
      )
      
      return(invisible(NULL))
    },
    
    # Normal distribution - two paired samples
    .runPariedSamplesNormalTest = function() {
      
      varnormalpaired1 <- self$options$varnormalpaired1
      varnormalpaired2 <- self$options$varnormalpaired2
      normalphapaired <- self$options$normalphapaired
      
      testname <- "Normal distribution - two paired samples"
      testnameshort <- "NORMpaired"
      
      selectedTableNames <- private$.selectedTables(c(ztestpaired = self$options$ztestpaired,ttestpaired = self$options$ttestpaired,
                                                      indeptestpaired = self$options$indeptestpaired))
      anyTestSelected <- private$.anySelected(c(self$options$ztestpaired,self$options$ttestpaired,self$options$indeptestpaired))
      
      private$.withTwoPairedSamplesAnalysis(
        var1 = varnormalpaired1,
        var2 = varnormalpaired2,
        alpha = normalphapaired,
        option = anyTestSelected,
        selectedTableNames = selectedTableNames,
        testname = testname,
        testnameshort = testnameshort,
        callback = function(x1, x2, n) {
          
          distinctValuesValid <- private$.checkAtLeastTwoDistinctValuesInPairedSamples(varname1 = varnormalpaired1,varname2 = varnormalpaired2,
                                                                                       x1 = x1,x2 = x2,tableNames = selectedTableNames,testname = testname,
                                                                                       testnameshort = testnameshort)
          
          if (!isTRUE(distinctValuesValid)) {
            return(invisible(NULL))
          }
          
          independenceRxy <- NULL
          
          tests <- list(
            list(
              enabled = isTRUE(self$options$ztestpaired),
              table = self$results$ztestpaired,
              validate = function() {
                private$.checkPairedZTestInputs(sigma12 = self$options$ztestpairedsigma1,sigma22 = self$options$ztestpairedsigma2,
                                                rho = self$options$ztestpairedrho,tableNames = "ztestpaired",testname = testname,
                                                testnameshort = testnameshort,option = self$options$ztestpaired)
              },
              compute = function() {
                private$.computeZTestPaired(x1 = x1,x2 = x2,n = n,alpha = normalphapaired,testvalue = self$options$ztestpairedvalue,
                                            sigma12 = self$options$ztestpairedsigma1,sigma22 = self$options$ztestpairedsigma2,
                                            rho = self$options$ztestpairedrho,side = self$options$ztestpairedside)
              }
            ),
            list(
              enabled = isTRUE(self$options$ttestpaired),
              table = self$results$ttestpaired,
              validate = function() {
                private$.checkPairedDifferencesVariancePositive(x1 = x1,x2 = x2,tableNames = "ttestpaired",testname = testname,testnameshort = testnameshort)
              },
              compute = function() {
                private$.computeTTestPaired(x1 = x1,x2 = x2,n = n,alpha = normalphapaired,testvalue = self$options$ttestpairedvalue,side = self$options$ttestpairedside)
              }
            ),
            list(
              enabled = isTRUE(self$options$indeptestpaired),
              table = self$results$indeptestpaired,
              validate = function() {
                independenceStatus <- private$.checkPairedIndependenceInputs(x1 = x1,x2 = x2,n = n,tableNames = "indeptestpaired",
                                                                             testname = testname,testnameshort = testnameshort)
                if (!isTRUE(independenceStatus$valid)) {
                  return(FALSE)
                }
                independenceRxy <<- independenceStatus$rxy
                return(TRUE)
              },
              compute = function() {
                private$.computeIndependenceTestPaired(rxy = independenceRxy,n = n,alpha = normalphapaired,side = self$options$indeptestpairedside)
              }
            )
          )
          
          private$.runEnabledTests(tests)
          
          return(invisible(NULL))
        }
      )
      
      return(invisible(NULL))
    },
    
    # Normal distribution - two independent samples
    .runTwoSampleNormalTest = function() {
      
      varnormalindep <- self$options$varnormalindep
      splitnormal <- self$options$splitnormal
      normalphaindep <- self$options$normalphaindep
      
      testname <- "Normal distribution - two independent samples"
      testnameshort <-"NORMtwo"
      
      selectedTableNames <- private$.selectedTables(c(ftest = self$options$ftest,ztestindep = self$options$ztestindep,ttestindep = self$options$ttestindep))
      anyTestSelected <- private$.anySelected(c(self$options$ftest,self$options$ztestindep,self$options$ttestindep))
      
      private$.withTwoIndependentSamplesAnalysis(
        var = varnormalindep,
        splitVar = splitnormal,
        alpha = normalphaindep,
        option = anyTestSelected,
        selectedTableNames = selectedTableNames,
        testname = testname,
        testnameshort = testnameshort,
        callback = function(x1, x2, n1, n2, preparedData) {
          
          distinctValuesValid <- private$.checkAtLeastTwoDistinctValuesInEachGroup(varname = varnormalindep,splitvarname = splitnormal,
                                                                                   x1 = x1,x2 = x2,tableNames = selectedTableNames,
                                                                                   testname = testname,testnameshort = testnameshort)
          if (!isTRUE(distinctValuesValid)) {
            return(invisible(NULL))
          }
          
          tests <- list(
            list(
              enabled = self$options$ftest,
              table = self$results$ftest,
              validate = NULL,
              compute = function() {
                private$.computeFTestIndep(x1 = x1,x2 = x2,n1 = n1,n2 = n2,alpha = normalphaindep,side = self$options$ftestside)
              }
            ),
            list(
              enabled = self$options$ttestindep,
              table = self$results$ttestindep,
              validate = NULL,
              compute = function() {
                private$.computeTTestIndep(x1 = x1,x2 = x2,n1 = n1,n2 = n2,alpha = normalphaindep,side = self$options$ttestindepside)
              }
            ),
            list(
              enabled = self$options$ztestindep,
              table = self$results$ztestindep,
              validate = function() {
                valid <- private$.checkIfInputValuePositive(valuename = "\U03C3\U00B2",value = self$options$ztestindepsigma,
                                                            testname = testname,testnameshort = paste0(testnameshort, "_ztestindep_sigma2"))
                if (!isTRUE(valid)) {
                  private$.hideTables("ztestindep")
                  return(FALSE)
                }
                return(TRUE)
              },
              compute = function() {
                private$.computeZTestIndep(x1 = x1,x2 = x2,n1 = n1,n2 = n2,alpha = normalphaindep,
                                           sigma2 = self$options$ztestindepsigma,side = self$options$ztestindepside)
              }
            )
          )
          
          private$.runEnabledTests(tests)
        }
      )
      
      return(invisible(NULL))
    },
    
    # Binomial distribution - one sample
    .runOneSampleBinomialTest = function() {
      
      varbinones <- self$options$varbinones
      binalphaones <- self$options$binalphaones
      
      testname <- "Binomial distribution - one sample"
      testnameshort <- "BINone"
      
      selectedTableNames <- private$.selectedTables(c(exactbintest = self$options$exactbintest,approxbintest = self$options$approxbintest))
      anyTestSelected <- private$.anySelected(c(self$options$exactbintest,self$options$approxbintest))
      
      private$.withOneSampleAnalysis(
        var = varbinones,
        alpha = binalphaones,
        option = anyTestSelected,
        selectedTableNames = selectedTableNames,
        testname = testname,
        testnameshort = testnameshort,
        callback = function(testdata) {
          
          databinomial <- private$.checkBinomialDistribution(varbin = varbinones,bindata = testdata,tablename = selectedTableNames,testname = testname)
          
          if (!isTRUE(databinomial)) {
            return(invisible(NULL))
          }
          
          testvalueexact <- self$options$exactbintestvalue
          powerp <- self$options$exactbintestpowervalue
          typeIp <- self$options$exactbintesttypeIerrorvalue
          typeIIp <- self$options$exactbintesttypeIIerrorvalue
          testvalueapprox <- self$options$approxbintestvalue
          sumtestdata <- sum(testdata)
          n <- length(testdata)
          tests <- list()
          
          if (isTRUE(self$options$exactbintest)) {
            exactInputStatus <- private$.checkOneSampleExactBinomialInputs(testvalueexact = testvalueexact,powerp = powerp,
                                                                            typeIp = typeIp,typeIIp = typeIIp,
                                                                            side = self$options$exactbintestside,
                                                                            powerOption = self$options$exactbintestpower,
                                                                            typeIOption = self$options$exactbintesttypeIerror,
                                                                            typeIIOption = self$options$exactbintesttypeIIerror,
                                                                            testname = testname,testnameshort = testnameshort)
            
            if (isTRUE(exactInputStatus$exactTestCanRun)) {
              tests[[length(tests) + 1]] <- list(
                enabled = TRUE,
                table = self$results$exactbintest,
                validate = NULL,
                compute = function() {
                  
                  safePowerp <- if (isTRUE(exactInputStatus$powerCanCompute)) {powerp} else {testvalueexact}
                  safeTypeIp <- if (isTRUE(exactInputStatus$typeICanCompute)) {typeIp} else {testvalueexact}
                  safeTypeIIp <- if (isTRUE(exactInputStatus$typeIICanCompute)) {typeIIp} else {testvalueexact}
                  
                  values <- private$.computeExactBinomialTest(sumtestdata = sumtestdata,n = n,alpha = binalphaones,
                                                              p0 = testvalueexact,side = self$options$exactbintestside,
                                                              powerp = safePowerp,typeIp = safeTypeIp,typeIIp = safeTypeIIp)
                  
                  if (!isTRUE(exactInputStatus$powerCanCompute)) {
                    values$power <- ""
                  }
                  
                  if (!isTRUE(exactInputStatus$typeICanCompute)) {
                    values$typeI <- ""
                  }
                  
                  if (!isTRUE(exactInputStatus$typeIICanCompute)) {
                    values$typeII <- ""
                  }
                  
                  values
                }
              )
            }
          }
          
          if (isTRUE(self$options$approxbintest)) {
            approxDistinctValuesValid <- private$.checkAtLeastTwoDistinctValues(varname = varbinones,x = testdata,tableNames = "approxbintest",
                                                                                testname = testname,testnameshort = paste0(testnameshort, "_approx"))
            if (isTRUE(approxDistinctValuesValid)) {
              approxInputsValid <- private$.checkOneSampleApproxBinomialInputs(testvalueapprox = testvalueapprox,testname = testname,testnameshort = testnameshort)
              
              if (isTRUE(approxInputsValid)) {
                tests[[length(tests) + 1]] <- list(
                  enabled = TRUE,
                  table = self$results$approxbintest,
                  validate = NULL,
                  compute = function() {
                    private$.computeApproxOneSampleBinomialTest(sumtestdata = sumtestdata,n = n,alpha = binalphaones,p0 = testvalueapprox,
                                                                side = self$options$approxbintestside)
                  })
              }
            }
          }
          
          private$.runEnabledTests(tests)
          
          return(invisible(NULL))
        }
      )
      
      return(invisible(NULL))
    },
    
    
    # Binomial distribution - two independent samples
    .runTwoSampleBinomialTest = function() {
      
      varbintwos <- self$options$varbintwos
      splitbin <- self$options$splitbin
      binalphaindep <- self$options$binalphaindep
      
      testname <- "Binomial distribution - two independent samples"
      testnameshort <-"BINtwo"
      
      selectedTableNames <- private$.selectedTables(c(approxtwosamplebintest = self$options$twosamplesbintest))
      anyTestSelected <- private$.anySelected(c(self$options$twosamplesbintest))
      
      if (any(is.null(varbintwos), is.null(splitbin))) {
        return(invisible(NULL))
      }
      
      if (!isTRUE(anyTestSelected)) {
        return(invisible(NULL))
      }
      
      runalanysis <- private$.checkVariableInput(selectedTableNames,binalphaindep,self$options$twosamplesbintest,testname,testnameshort)
      
      if (!runalanysis) {
        return(invisible(NULL))
      }
      
      preparedData <- private$.prepareTwoIndependentSamplesInput(varbintwos,splitbin,testname,testnameshort,self$options$twosamplesbintest)
      
      if (is.null(preparedData)) {
        private$.hideTables(selectedTableNames)
        return(invisible(NULL))
      }
      
      databinomial <- private$.checkBinomialDistribution(varbintwos,preparedData[[1]],selectedTableNames,testname)
      
      if (!databinomial) {
        return(invisible(NULL))
      }
      
      numberOfNonMissingLevels <- preparedData[[3]]
      
      if (!isTRUE(numberOfNonMissingLevels == 2)) {
        private$.hideTables(selectedTableNames)
        return(invisible(NULL))
      }
      
      testdata <- private$.splitTwoIndependentSamplesData(varbintwos,splitbin,preparedData$x,preparedData$group,testname,testnameshort)
      
      if (is.null(testdata)) {
        return(invisible(NULL))
      }
      
      x1 <- testdata[[1]]
      x2 <- testdata[[2]]
      n1 <- testdata[[3]]
      n2 <- testdata[[4]]
      
      distinctValuesValid <- private$.checkAtLeastTwoDistinctValuesInEachGroup(varname = varbintwos,splitvarname = splitbin,
                                                                               x1 = x1,x2 = x2,tableNames = selectedTableNames,
                                                                               testname = testname,testnameshort = testnameshort)
      
      if (!isTRUE(distinctValuesValid)) {
        return(invisible(NULL))
      }
      
      tests <- list(
        list(
          enabled = self$options$twosamplesbintest,
          table = self$results$approxtwosamplebintest,
          validate = NULL,
          compute = function() {
            private$.computeTwoSampleBinomialApprox(x1 = x1,x2 = x2,n1 = n1,n2 = n2,alpha = binalphaindep,side = self$options$twosamplesbintestside)
          }))
      
      private$.runEnabledTests(tests)
      
      return(invisible(NULL))
    },
    
    # nonparametric tests - one sample
    .runOneSampleNonparametricTest = function() {
      
      varnonparamones <- self$options$varnonparamones
      nonparamalphaones <- self$options$nonparamalphaones
      
      testname <- "Nonparametric tests - one sample"
      testnameshort <-"NONPARone"
      
      selectedTableNames <- private$.selectedTables(c(
        signtest = self$options$signtest
      ))
      
      anyTestSelected <- private$.anySelected(c(
        self$options$signtest
      ))
      
      private$.withOneSampleAnalysis(
        var = varnonparamones,
        alpha = nonparamalphaones,
        option = anyTestSelected,
        selectedTableNames = selectedTableNames,
        testname = testname,
        testnameshort = testnameshort,
        callback = function(testdata) {
          
          distinctValuesValid <- private$.checkAtLeastTwoDistinctValues(varname = varnonparamones,x = testdata,tableNames = selectedTableNames,
                                                                        testname = testname,testnameshort = testnameshort)
          
          if (!isTRUE(distinctValuesValid)) {
            return(invisible(NULL))
          }
          
          tests <- list(
            list(
              enabled = self$options$signtest,
              table = self$results$signtest,
              validate = NULL,
              compute = function() {
                private$.computeSignTest(testdata = testdata,alpha = nonparamalphaones,testvalue = self$options$signtestvalue,side = self$options$signtestside)
              }))
          
          private$.runEnabledTests(tests)
        }
      )
      return(invisible(NULL))
    },
    
    # nonparametric tests - two independent samples
    .runTwoSampleNonparametricTest = function() {
      
      varnonparamtwos <- self$options$varnonparamtwos
      splitnonparam <- self$options$splitnonparam
      nonparamalphatwos <- self$options$nonparamalphatwos
      
      testname <- "Nonparametric tests - two independent samples"
      testnameshort <-"NONPARtwo"
      
      selectedTableNames <- private$.selectedTables(c(utest = self$options$utest))
      anyTestSelected <- private$.anySelected(c(self$options$utest))
      
      private$.withTwoIndependentSamplesAnalysis(
        var = varnonparamtwos,
        splitVar = splitnonparam,
        alpha = nonparamalphatwos,
        option = anyTestSelected,
        selectedTableNames = selectedTableNames,
        testname = testname,
        testnameshort = testnameshort,
        callback = function(x1, x2, n1, n2, preparedData) {
          
          distinctValuesValid <- private$.checkAtLeastTwoDistinctValuesInEachGroup(varname = varnonparamtwos,splitvarname = splitnonparam,
                                                                                   x1 = x1,x2 = x2,tableNames = selectedTableNames,
                                                                                   testname = testname,testnameshort = testnameshort)
          
          if (!isTRUE(distinctValuesValid)) {
            return(invisible(NULL))
          }
          
          tests <- list(
            list(
              enabled = self$options$utest,
              table = self$results$utest,
              validate = NULL,
              compute = function() {
                private$.computeUTestIndep(x1 = x1,x2 = x2,n1 = n1,n2 = n2,alpha = nonparamalphatwos,side = self$options$utestside)
              }))
          
          private$.runEnabledTests(tests)
        }
      )
      
      return(invisible(NULL))
    },
    
    # test for independence in contingency tables
    .runContingencyAnalysis = function() {
      
      nominal1 <- self$options$nominal1
      nominal2 <- self$options$nominal2
      countsName <- self$options$counts
      
      testname <-"Test for independence in contingency tables"
      testnameshort <- "CONT"
      
      # list of all selected tables
      selectedTableNames <- c()
      if(self$options$contabs){
        selectedTableNames <- c(selectedTableNames, "absfreqstable")
      }
      if(self$options$conttest) {
        selectedTableNames <- c(selectedTableNames, "contindependencetest")
      }
      
      # return nothing if nominal input is incomplete
      if(!private$.hasContingencyInput()) {
        return()
      }
      
      rowVarName <- nominal1
      colVarName <- nominal2
      
      # check if nominal1 and nominal2 have missing values
      private$.excludeMissingValues(nominal1, self$data[[nominal1]])
      private$.excludeMissingValues(nominal2, self$data[[nominal2]])
      
      # check if nominal1 and nominal2 are factors
      columns <- jmvcore::select(self$data, c(rowVarName, colVarName))
      
      if((self$options$contabs || self$options$conttest) && (!is.factor(columns[[rowVarName]]) || !is.factor(columns[[colVarName]]))) {
        private$.hideTables(selectedTableNames)
        private$.handleAnalysisIssue(
          name = paste0("notFactor", testnameshort, "_", colVarName, "_", rowVarName),
          message = paste0(testname, ": Row variable '",rowVarName,"' and/or column variable '",colVarName,"' is not a factor."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return()
      }
      
      # check if nominal1 and nominal2 have at least two levels
      contingencytable <- table(columns)
      
      if((self$options$contabs || self$options$conttest) && (nrow(contingencytable) < 2 || ncol(contingencytable) < 2)) {
        private$.hideTables(selectedTableNames)
        private$.handleAnalysisIssue(
          name = paste0("numberOfLevels", testnameshort, "_", rowVarName, "_", colVarName),
          message = paste0(testname, ": Row variable '",rowVarName,"' or column variable '",colVarName,"' contains fewer than 2 levels."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return()
      }
      
      # check significance level
      alphaValid <- private$.checkSignificanceLevel(alpha = self$options$conttestalpha,
                                                    option = self$options$conttest,
                                                    testname = testname,
                                                    testnameshort = testnameshort,
                                                    type = jmvcore::NoticeType$STRONG_WARNING)
      
      # hide table if alpha is not valid
      if(!alphaValid && self$options$conttest) {
        private$.hideTables("contindependencetest")
      }
      
      # create contingency table depending on whether counts are given
      contingencytableForAnalysis <- if(is.null(countsName)) {
        private$.tableWithoutCounts(countsName = countsName,contingencytable = contingencytable,selectedTableNames = selectedTableNames)
      } else {
        private$.tableWithCounts(columns = columns,countsName = countsName,rowVarName = rowVarName,colVarName = colVarName,
                                 testname = testname,selectedTableNames = selectedTableNames)
      }
      
      if(is.null(contingencytableForAnalysis)) {
        return()
      }
      
      # populate output tables
      private$.renderContingencyResults(contingencytable = contingencytableForAnalysis,rowVarName = rowVarName,colVarName = colVarName,
                                        alphaValid = alphaValid,testname = testname)
    },
    
    # general helper functions
    
    .numberOfVariables=function() {
      n <- as.numeric(!is.null(self$options$varnormalones))+ 
        as.numeric(!is.null(self$options$varnormalpaired1))*as.numeric(!is.null(self$options$varnormalpaired2))+ 
        as.numeric(!is.null(self$options$varnormalindep))*as.numeric(!is.null(self$options$splitnormal))+ 
        as.numeric(!is.null(self$options$varbinones)) +
        as.numeric(!is.null(self$options$varbintwos))*as.numeric(!is.null(self$options$splitbin))+  
        as.numeric(!is.null(self$options$varnonparamones)) +
        as.numeric(!is.null(self$options$varnonparamtwos))*as.numeric(!is.null(self$options$splitnonparam)) +
        as.numeric(!is.null(self$options$nominal1))*as.numeric(!is.null(self$options$nominal2))
      return(n)
    },
    
    .handleAnalysisIssue = function(name, message, type=jmvcore::NoticeType$WARNING) {
      private$.createNotice(name, message, type=type)
      return(invisible(NULL))
    },
    
    .createNotice = function(name, message, type=jmvcore::NoticeType$WARNING){
      notice <- jmvcore::Notice$new(options=self$options, name=name, type=type)
      notice$setContent(message)
      self$results$insert(private$.noticeInsertPosition, notice)
      private$.noticeInsertPosition <- private$.noticeInsertPosition + 1L
    },
    
    .toNumericWithoutExplicitNotice=function(var){
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
    
    .excludeMissingValues=function(var, x){
      # check if data have missing values
      if(!(var %in% private$.missingvaluesVars)) {
        nMissing <- sum(is.na(x))
        if (nMissing > 0) {
          private$.missingvaluesVars <- c(private$.missingvaluesVars, var)
        }
      }
      return(x[!is.na(x)]) #ignore missing values
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
                              paste0("Variable(s) '", paste(private$.missingvaluesVars, collapse = ", "), "' contain(s) missing value(s). Analyses will ignore 
                                         missing values."),
                              type = jmvcore::NoticeType$INFO)
      }
      
      if(length(private$.transformedVars)>0) {
        private$.createNotice(paste0("variablesNotNumeric_", paste(private$.transformedVars, collapse = ", ")), 
                              paste0("Treating variable(s) '", paste(private$.transformedVars, collapse = ", "), "' as numeric."),
                              type=jmvcore::NoticeType$INFO)
      }
    },
    
    .selectedTables = function(tableOptions) {
      names(tableOptions)[as.logical(tableOptions)]
    },
    
    .anySelected = function(options) {
      any(as.logical(options))
    },
    
    .addTestResult = function(table, values) {
      if (is.null(values)) {
        return(invisible(NULL))
      }
      table$addRow(rowKey = 1, values = values)
      return(invisible(NULL))
    },
    
    # test-specific helper functions
    
    .runEnabledTests = function(tests) {
      for (test in tests) {
        if (!isTRUE(test$enabled)) {
          next
        }
        if (!is.null(test$validate) && !isTRUE(test$validate())) {
          next
        }
        values <- test$compute()
        if (is.null(values)) {
          next
        }
        private$.addTestResult(test$table, values)
      }
      return(invisible(NULL))
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
    
    .withOneSampleAnalysis = function(var, alpha, option, selectedTableNames,testname, testnameshort, callback) {
      if (is.null(var)) {
        return(invisible(NULL))
      }
      
      if (!isTRUE(option)) {
        return(invisible(NULL))
      }
      
      runAnalysis <- private$.checkVariableInput(selectedTableNames = selectedTableNames,alpha = alpha,option = option,testname = testname,testnameshort = testnameshort)
      
      if (!isTRUE(runAnalysis)) {
        return(invisible(NULL))
      }
      
      testdata <- private$.prepareOneSampleInput(var = var,testname = testname,testnameshort = testnameshort,option = option)
      
      if (is.null(testdata)) {
        return(invisible(NULL))
      }
      callback(testdata)
      return(invisible(NULL))
    },
    
    .withTwoIndependentSamplesAnalysis = function(var, splitVar, alpha, option,selectedTableNames, testname,
                                                  testnameshort, callback) {
      
      if (any(is.null(var), is.null(splitVar))) {
        return(invisible(NULL))
      }
      if (!isTRUE(option)) {
        return(invisible(NULL))
      }
      
      runAnalysis <- private$.checkVariableInput(selectedTableNames = selectedTableNames,alpha = alpha,
                                                 option = option,testname = testname,testnameshort = testnameshort)
      
      if (!isTRUE(runAnalysis)) {
        return(invisible(NULL))
      }
      
      preparedData <- private$.prepareTwoIndependentSamplesInput(var = var,splitVar = splitVar,
                                                                 testname = testname,testnameshort = testnameshort,option = option)
      
      if (is.null(preparedData)) {
        private$.hideTables(selectedTableNames)
        return(invisible(NULL))
      }
      
      numberOfNonMissingLevels <- preparedData[[3]]
      
      if (!isTRUE(numberOfNonMissingLevels == 2)) {
        private$.hideTables(selectedTableNames)
        return(invisible(NULL))
      }
      
      testdata <- private$.splitTwoIndependentSamplesData(varname = var,splitvarname = splitVar,
                                                          preparedx = preparedData$x,preparedgroup = preparedData$group,
                                                          testname = testname,testnameshort = testnameshort)
      
      if (is.null(testdata)) {
        return(invisible(NULL))
      }
      callback(x1 = testdata[[1]],x2 = testdata[[2]],n1 = testdata[[3]],n2 = testdata[[4]],preparedData = preparedData)
      return(invisible(NULL))
    },
    
    .withTwoPairedSamplesAnalysis = function(var1, var2,alpha,option,selectedTableNames,testname,testnameshort,callback) {
      if (any(is.null(var1), is.null(var2))) {
        return(invisible(NULL))
      }
      if (!isTRUE(option)) {
        return(invisible(NULL))
      }
      
      runAnalysis <- private$.checkVariableInput(selectedTableNames = selectedTableNames,alpha = alpha,option = option,
                                                 testname = testname,testnameshort = testnameshort)
      
      if (!isTRUE(runAnalysis)) {
        return(invisible(NULL))
      }
      
      preparedData <- private$.prepareTwoPairedSamplesInput(var1 = var1,var2 = var2,
                                                            testname = testname,testnameshort = testnameshort,option = option)
      
      if (is.null(preparedData)) {
        private$.hideTables(selectedTableNames)
        return(invisible(NULL))
      }
      callback(x1 = preparedData$x1,x2 = preparedData$x2,n = preparedData$n)
      return(invisible(NULL))
    },
    
    # helper functions for input checks
    
    .checkSignificanceLevel = function(alpha, option=TRUE, testname, testnameshort, type=jmvcore::NoticeType$INFO) {
      if(!(alpha <= 0 || alpha >= 1) && option) {
        return(TRUE)
      } else if (option) {
        private$.handleAnalysisIssue(
          name = paste0("SignLevel", testnameshort, "_", alpha),
          message = paste0(testname, ": Significance level must be between 0 and 1!"),
          type = type
        )
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkIfInputValuePositive = function(valuename,value,tablename,testname,testnameshort,option=TRUE) {
      if(isTRUE(option) && !isTRUE(value > 0)) {
        private$.handleAnalysisIssue(
          name = paste0("valueNotPositive", testnameshort, "_", valuename),
          message = paste0(testname, ": Test only applicable for positive input value ",valuename,", but ",valuename,"  = ", value, "."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkIfInputValue01 = function(valuename,value,testname,testnameshort, option=TRUE) {
      if(!(value>0 && value<1) && option) {
        private$.handleAnalysisIssue(
          name = paste0("valueNot01", testnameshort, "_", valuename),
          message = paste0(testname, ": Test only applicable for input value ",valuename," between 0 and 1, but ",valuename,"  = ", value, "."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkRhoInput = function(rho, testname, testnameshort, option = TRUE) {
      if (!isTRUE(option)) {
        return(TRUE)
      }
      
      rhoValid <- length(rho) == 1 && is.finite(rho) && rho >= -1 && rho <= 1
      if (!isTRUE(rhoValid)) {
        private$.handleAnalysisIssue(
          name = paste0("rhoNotBetweenMinusOneAndOne", testnameshort, "_rho"),
          message = paste0(testname,": Correlation coefficient \U03C1 must be between -1 and 1, but \U03C1 = ",rho,"."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    # helper functions checking input data
    
    .countDistinctNonMissingValues = function(x) {
      x <- x[!is.na(x)]
      return(length(unique(x)))
    },
    
    .checkAtLeastTwoDistinctValues = function(varname, x, tableNames, testname, testnameshort) {
      numberOfDistinctValues <- private$.countDistinctNonMissingValues(x)
      if (numberOfDistinctValues < 2) {
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("notEnoughDistinctValues", testnameshort, "_", varname),
          message = paste0(testname, ": Input variable '",varname,"' must contain at least two distinct non-missing values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkAtLeastTwoDistinctValuesInPairedSamples = function(varname1,varname2,x1,x2,tableNames,testname,testnameshort) {
      x1Valid <- private$.countDistinctNonMissingValues(x1) >= 2
      x2Valid <- private$.countDistinctNonMissingValues(x2) >= 2
      
      if (!isTRUE(x1Valid) || !isTRUE(x2Valid)) {
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("notEnoughDistinctValuesPaired",testnameshort,"_",varname1,"_",varname2),
          message = paste0(testname,": Each paired input variable must contain at least two distinct values ","after removing rows with missing values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkAtLeastTwoDistinctValuesInEachGroup = function(varname, splitvarname, x1, x2,tableNames, testname, testnameshort) {
      group1Valid <- private$.countDistinctNonMissingValues(x1) >= 2
      group2Valid <- private$.countDistinctNonMissingValues(x2) >= 2
      
      if (!isTRUE(group1Valid) || !isTRUE(group2Valid)) {
        invalidGroups <- c()
        if (!isTRUE(group1Valid)) {
          invalidGroups <- c(invalidGroups, "group 1")
        }
        if (!isTRUE(group2Valid)) {
          invalidGroups <- c(invalidGroups, "group 2")
        }
        
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("notEnoughDistinctValuesByGroup", testnameshort, "_", varname, "_", splitvarname),
          message = paste0(testname,": Each group of input variable '",varname,"' split by '",splitvarname,"' must contain at least two distinct non-missing values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    # helper functions for specific test input checks
    
    .checkOneSampleNormalInputs = function(testname, testnameshort) {
     zSigmaValid <- TRUE
      chiKnownSigmaValid <- TRUE
      chiSigmaValid <- TRUE
      
      if (isTRUE(self$options$ztest)) {
        zSigmaValid <- private$.checkIfInputValuePositive(valuename = "\U03C3\U00B2",value = self$options$ztestsigma,
                                                          testname = paste0(testname, " (Z-Test)"),
                                                          testnameshort = paste0(testnameshort, "_ztest_sigma2"))
        if (!isTRUE(zSigmaValid)) {
          private$.hideTables("ztest")
        }
      }
      
      if (isTRUE(self$options$chitestknownmu)) {
        chiKnownSigmaValid <- private$.checkIfInputValuePositive(valuename = "\U03C3\U2080",value = self$options$chitestvalueknownmu, 
                                                                 testname = paste0(testname, " (\U03C7\U00B2-Test with known \U03BC)"),
                                                                 testnameshort = paste0(testnameshort, "_chitestknownmu_sigma0"))
        if (!isTRUE(chiKnownSigmaValid)) {
          private$.hideTables("chitestknownmu")
        }
      }
      
      if (isTRUE(self$options$chitest)) {
        chiSigmaValid <- private$.checkIfInputValuePositive(valuename = "\U03C3\U2080",value = self$options$chitestvalue,
                                                            testname = paste0(testname, " (\U03C7\U00B2-Test with unknown \U03BC)"),
                                                            testnameshort = paste0(testnameshort, "_chitest_sigma0"))
        if (!isTRUE(chiSigmaValid)) {
          private$.hideTables("chitest")
        }
      }
      
      return(list(zSigmaValid = zSigmaValid,chiKnownSigmaValid = chiKnownSigmaValid,chiSigmaValid = chiSigmaValid))
    },
    
    .checkOneSampleZAdditionalInputs = function(mu0, side, betastar, delta0,typeImu, typeIImu,typeIOption, typeIIOption,expDesignOption,testname, testnameshort) {
      betaValid <- private$.checkIfInputValue01(valuename = "\U03B2*",value = betastar,testname = paste0(testname, " (Z-Test)"),
                                                testnameshort = paste0(testnameshort, "_ztest_beta"),option = expDesignOption)
      
      deltaValid <- private$.checkIfInputValuePositive(valuename = "\U03B4\U2080",value = delta0,testname = paste0(testname, " (Z-Test)"),
                                                       testnameshort = paste0(testnameshort, "_ztest_delta0"),option = expDesignOption)
      
      errorStatus <- private$.checkOneSampleZErrorInputs(mu0 = mu0,side = side,typeImu = typeImu,typeIImu = typeIImu,
                                                         typeIOption = typeIOption,typeIIOption = typeIIOption,
                                                         testname = testname,testnameshort = testnameshort)
      
      return(list(betaValid = betaValid,deltaValid = deltaValid,
                  minnCanCompute = isTRUE(expDesignOption) && isTRUE(betaValid) && isTRUE(deltaValid),
                  typeIInH0 = errorStatus$typeIInH0,typeIIInH1 = errorStatus$typeIIInH1))
    },
    
    .checkOneSampleZErrorInputs = function(mu0, side, typeImu, typeIImu,typeIOption, typeIIOption,testname, testnameshort) {
      side <- private$.normalizeSide(side)
      
      if (side == "two") {
        typeIInH0 <- typeImu == mu0
        typeIIInH1 <- typeIImu != mu0
      } else if (side == "lower") {
        typeIInH0 <- typeImu >= mu0
        typeIIInH1 <- typeIImu < mu0
      } else if (side == "upper") {
        typeIInH0 <- typeImu <= mu0
        typeIIInH1 <- typeIImu > mu0
      }
      
      if (isTRUE(typeIOption) && !isTRUE(typeIInH0)) {
        private$.handleAnalysisIssue(
          name = paste0("typeIError", testnameshort, "_ztest_", side),
          message = paste0(testname,": Type I error: \U03BC = ",typeImu," is not in H\U2080!"),
          type = jmvcore::NoticeType$STRONG_WARNING)
      }
      
      if (isTRUE(typeIIOption) && !isTRUE(typeIIInH1)) {
        private$.handleAnalysisIssue(
          name = paste0("typeIIError", testnameshort, "_ztest_", side),
          message = paste0(testname,": Type II error: \U03BC = ",typeIImu," is not in H\U2081!"),
          type = jmvcore::NoticeType$STRONG_WARNING)
      }
      return(list(typeIInH0 = typeIInH0,typeIIInH1 = typeIIInH1))
    },
    
    .checkPairedZTestInputs = function(sigma12,sigma22,rho,tableNames,testname,testnameshort,option = TRUE) {
      if (!isTRUE(option)) {
        return(TRUE)
      }
      
      sigma1Valid <- private$.checkIfInputValuePositive(valuename = "\U03C3\U2081\U00B2",value = sigma12,
                                                        testname = paste0(testname, " (paired Z-Test)"),
                                                        testnameshort = paste0(testnameshort, "_ztestpaired_sigma1"),option = option)
      sigma2Valid <- private$.checkIfInputValuePositive(valuename = "\U03C3\U2082\U00B2",value = sigma22,
                                                        testname = paste0(testname, " (paired Z-Test)"),
                                                        testnameshort = paste0(testnameshort, "_ztestpaired_sigma2"),option = option)
      rhoValid <- private$.checkRhoInput(rho = rho,testname = paste0(testname, " (paired Z-Test)"),
                                         testnameshort = paste0(testnameshort, "_ztestpaired"),option = option)
      
      sigmaDeltaValid <- TRUE
      if (isTRUE(sigma1Valid) && isTRUE(sigma2Valid) && isTRUE(rhoValid)) {
        sigmaDelta2 <- sigma12 + sigma22 - 2 * rho * sqrt(sigma12 * sigma22)
        sigmaDeltaValid <- TRUE
        if(isTRUE(option) && !isTRUE(sigmaDelta2 > 0)) {
          private$.handleAnalysisIssue(
            name = paste0("InternalvalueNotPositive", testnameshort, "_", "\U03C3\U03B4\U00B2"),
            message = paste0(testname, " (paired Z-Test)", ": Internal computation error for these input values and data."),
            type = jmvcore::NoticeType$STRONG_WARNING)
          sigmaDeltaValid <- FALSE
        }
      }
      
      valid <- isTRUE(sigma1Valid) && isTRUE(sigma2Valid) && isTRUE(rhoValid) && isTRUE(sigmaDeltaValid)
      
      if (!isTRUE(valid)) {
        private$.hideTables(tableNames)
      }
      return(valid)
    },
    
    .checkPairedDifferencesVariancePositive = function(x1,x2,tableNames,testname,testnameshort) {
      differences <- x1 - x2
      differencesValid <- private$.countDistinctNonMissingValues(differences) >= 2
      
      if (!isTRUE(differencesValid)) {
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("pairedDifferencesNoVariance", testnameshort, "_ttestpaired"),
          message = paste0(testname,": The paired t-test is not applicable because the paired differences ","do not contain at least two distinct values."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkPairedIndependenceInputs = function(x1,x2,n,tableNames,testname,testnameshort) {
      if (!isTRUE(n > 2)) {
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("insufficientObservations", testnameshort, "_indeptestpaired"),
          message = paste0(testname,": At least three complete paired observations are required ","for the independence test."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(list(valid = FALSE,rxy = NULL))
      }
      
      rxy <- private$.computeBravaisPearson(x1, x2)
      
      if (!is.finite(rxy)) {
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("correlationNotFinite", testnameshort, "_indeptestpaired"),
          message = paste0(testname,": The Bravais-Pearson correlation coefficient could not be computed."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(list(valid = FALSE,rxy = NULL))
      }
      
      if (isTRUE(abs(rxy) >= 1)) {
        private$.hideTables(tableNames)
        private$.handleAnalysisIssue(
          name = paste0("perfectCorrelation", testnameshort, "_indeptestpaired"),
          message = paste0(testname,": The independence test is not applicable because ","|r| = 1 and the test statistic is not finite."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(list(valid = FALSE,rxy = NULL))
      }
      
      return(list(valid = TRUE,rxy = rxy))
    },
    
    .checkExactBinomialErrorInputs = function(p0, side, typeIp, typeIIp, typeIOption, typeIIOption, testname, testnameshort) {
      side <- private$.normalizeSide(side)
      typeIValid <- TRUE
      typeIIValid <- TRUE
      
      if (side == "two") {
        if (isTRUE(typeIOption) && typeIp != p0) {
          private$.handleAnalysisIssue(
            name = paste0("typeIError", testnameshort, "_", side),
            message = paste0(testname, ": Type I error: p = ", typeIp, " is not in H\U2080!"),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          typeIValid <- FALSE
        }
        if (isTRUE(typeIIOption) && typeIIp == p0) {
          private$.handleAnalysisIssue(
            name = paste0("typeIIError", testnameshort, "_", side),
            message = paste0(testname, ": Type II error: p = ", typeIIp, " is not in H\U2081!"),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          typeIIValid <- FALSE
        }
      }
      
      if (side == "lower") {
        if (isTRUE(typeIOption) && typeIp < p0) {
          private$.handleAnalysisIssue(
            name = paste0("typeIError", testnameshort, "_", side),
            message = paste0(testname, ": Type I error: p = ", typeIp, " is not in H\U2080!"),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          typeIValid <- FALSE
        }
        if (isTRUE(typeIIOption) && typeIIp >= p0) {
          private$.handleAnalysisIssue(
            name = paste0("typeIIError", testnameshort, "_", side),
            message = paste0(testname, ": Type II error: p = ", typeIIp, "  is not in H\U2081!"),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          typeIIValid <- FALSE
        }
      }
      
      if (side == "upper") {
        if (isTRUE(typeIOption) && typeIp > p0) {
          private$.handleAnalysisIssue(
            name = paste0("typeIError", testnameshort, "_", side),
            message = paste0(testname, ": Type I error: p = ", typeIp, " is not in H\U2080!"),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          typeIValid <- FALSE
        }
        if (isTRUE(typeIIOption) && typeIIp <= p0) {
          private$.handleAnalysisIssue(
            name = paste0("typeIIError", testnameshort, "_", side),
            message = paste0(testname, ": Type II error: p = ", typeIIp, "  is not in H\U2081!"),
            type = jmvcore::NoticeType$STRONG_WARNING
          )
          typeIIValid <- FALSE
        }
      }
      
      return(list(typeIValid = typeIValid,typeIIValid = typeIIValid,allValid = isTRUE(typeIValid) && isTRUE(typeIIValid)))
    },
    
    .checkOneSampleExactBinomialInputs = function(testvalueexact, powerp, typeIp, typeIIp,side, powerOption, typeIOption,
                                                  typeIIOption, testname, testnameshort) {
      
      p0Valid <- private$.checkIfInputValue01(valuename = "p\U2080",value = testvalueexact,testname = testname,
                                              testnameshort = paste0(testnameshort, "_", "p0_exact"),option = TRUE)
      
      powerValid <- TRUE
      if (isTRUE(powerOption)) {
        powerValid <- private$.checkIfInputValue01(valuename = "p for power", value = powerp,testname = testname,
                                                   testnameshort = paste0(testnameshort, "_", "p_power"),option = powerOption)
      }
      
      typeIValueValid <- TRUE
      if (isTRUE(typeIOption)) {
        typeIValueValid <- private$.checkIfInputValue01(valuename = "p for Type I error",value = typeIp,testname = testname,
                                                        testnameshort = paste0(testnameshort, "_", "p_typeI"),option = typeIOption)
      }
      
      typeIIValueValid <- TRUE
      if (isTRUE(typeIIOption)) {
        typeIIValueValid <- private$.checkIfInputValue01(valuename = "p for Type II error", value = typeIIp,testname = testname,
                                                         testnameshort = paste0(testnameshort, "_", "p_typeII"),option = typeIIOption)
      }
      
      typeIErrorOption <- isTRUE(typeIOption) && isTRUE(p0Valid) && isTRUE(typeIValueValid)
      typeIIErrorOption <- isTRUE(typeIIOption) && isTRUE(p0Valid) && isTRUE(typeIIValueValid)
      errorInputStatus <- private$.checkExactBinomialErrorInputs(p0 = testvalueexact,side = side,typeIp = typeIp,typeIIp = typeIIp,
                                                                 typeIOption = typeIErrorOption,typeIIOption = typeIIErrorOption,
                                                                 testname = testname,testnameshort = testnameshort)
      
      if (!isTRUE(p0Valid)) {
        private$.hideTables("exactbintest")
      }
      
      return(list(
        exactTestCanRun = isTRUE(p0Valid),
        p0Valid = isTRUE(p0Valid),
        powerValid = isTRUE(powerValid),
        typeIValueValid = isTRUE(typeIValueValid),
        typeIIValueValid = isTRUE(typeIIValueValid),
        typeIErrorValid = isTRUE(errorInputStatus$typeIValid),
        typeIIErrorValid = isTRUE(errorInputStatus$typeIIValid),
        powerCanCompute = !isTRUE(powerOption) || isTRUE(powerValid),
        typeICanCompute = !isTRUE(typeIOption) || (isTRUE(typeIValueValid) &&isTRUE(errorInputStatus$typeIValid)),
        typeIICanCompute = !isTRUE(typeIIOption) || (isTRUE(typeIIValueValid) &&isTRUE(errorInputStatus$typeIIValid))
      ))
    },
    
    .checkOneSampleApproxBinomialInputs = function(testvalueapprox, testname, testnameshort) {
      approxInputsValid <- private$.checkIfInputValue01(valuename = "p\U2080",value = testvalueapprox,testname = testname,
                                                        testnameshort = paste0(testnameshort, "_", "p0_approx"),option = TRUE)
      if (!isTRUE(approxInputsValid)) {
        private$.hideTables("approxbintest")
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkBinomialDistribution = function(varbin,bindata,tablename,testname) {
      if (!all(bindata == 0 | bindata == 1)) {
        private$.hideTables(tablename)
        private$.handleAnalysisIssue(
          name = paste0("dataNotBinomial", varbin),
          message = paste0(testname, ": Test only applicable for data from a Binomial distribution, but '",varbin,"' contains values other than 0 and 1!"),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(FALSE)
      }
      return(TRUE)
    },
    
    .checkVariableInput = function(selectedTableNames,alpha,option,testname,testnameshort) {
      # check significance level
      alphaValid <- private$.checkSignificanceLevel(
        alpha = alpha,
        option = option,
        testname = testname,
        testnameshort = testnameshort,
        type = jmvcore::NoticeType$STRONG_WARNING)
      
      # hide table(s) if alpha is not valid
      if(!alphaValid) {
        private$.hideTables(selectedTableNames)
        return(FALSE)
      }
      return(TRUE)
    },
    
    # helper functions to preprare inputs for further analyses
    
    .prepareOneSampleInput = function(var,testname,testnameshort,option=TRUE) {
      if (is.null(var) || !option) {
        return(NULL)
      }
      if (!jmvcore::canBeNumeric(self$data[[var]]) && option) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable", testnameshort, "_", var),
          message = paste0(testname, ": Input variable '",var,"' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(NULL)
      } 
      private$.excludeMissingValues(var, private$.toNumericWithoutExplicitNotice(var))
    },
    
    .prepareTwoPairedSamplesInput = function(var1,var2,testname,testnameshort,option = TRUE) {
      if (any(is.null(var1), is.null(var2))) {
        return(NULL)
      }
      if (!isTRUE(option)) {
        return(NULL)
      }
      
      if (!jmvcore::canBeNumeric(self$data[[var1]])) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable", testnameshort, "_", var1),
          message = paste0(testname, ": Input variable '", var1, "' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(NULL)
      }
      if (!jmvcore::canBeNumeric(self$data[[var2]])) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable", testnameshort, "_", var2),
          message = paste0(testname, ": Input variable '", var2, "' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(NULL)
      }
      
      x1Raw <- private$.toNumericWithoutExplicitNotice(var1)
      x2Raw <- private$.toNumericWithoutExplicitNotice(var2)
      
      private$.noteMissingValues(var1, x1Raw)
      private$.noteMissingValues(var2, x2Raw)
      
      if (!isTRUE(length(x1Raw) == length(x2Raw))) {
        private$.handleAnalysisIssue(
          name = paste0("pairedSamplesDifferentLength", testnameshort, "_", var1, "_", var2),
          message = paste0(testname,": Both samples must have the same length for paired samples tests."),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        return(NULL)
      }
      
      pairedData <- data.frame(.x1 = x1Raw,.x2 = x2Raw)
      pairedData <- pairedData[complete.cases(pairedData), , drop = FALSE]
      x1 <- pairedData$.x1
      x2 <- pairedData$.x2
      n <- length(x1)
      
      return(list(x1 = x1,x2 = x2,n = n))
    },
    
    .prepareTwoIndependentSamplesInput = function(var,splitVar,testname,testnameshort,option=TRUE) {
      if (is.null(var) || is.null(splitVar)) {
        return(NULL)
      }
      if(!option) {
        return(NULL)
      }
      if (!jmvcore::canBeNumeric(self$data[[var]]) && option) {
        private$.handleAnalysisIssue(
          name = paste0("analysesNotApplicable", testnameshort, "_", var),
          message = paste0(testname, ": Input variable '",var,"' is not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(NULL)
      } 
        
      xRaw <- private$.toNumericWithoutExplicitNotice(var)
      groupRaw <- self$data[[splitVar]]
        
      private$.noteMissingValues(var, xRaw)
      private$.noteMissingValues(splitVar, groupRaw)
        
      dftwosample <- data.frame(.x = xRaw,.group = groupRaw,stringsAsFactors = FALSE)
      dftwosample <- dftwosample[complete.cases(dftwosample), , drop = FALSE]
        
      x <- dftwosample$.x
      group <- dftwosample$.group
        
      observedGroups <- private$.observedGroups(group)
        
      if (length(observedGroups) != 2) {
        private$.handleAnalysisIssue(
          name = paste0("splitVariableLevels", testnameshort, "_", splitVar),
          message = paste0(testname, ": 'Split by' variable '",splitVar,"' does not have exactly two levels after removing all rows with missing values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        
      }
      return(list(x = x,group = group, length(observedGroups)))
    },
    
    .observedGroups = function(group) {
      group <- group[!is.na(group)]
      if (is.factor(group)) {
        group <- droplevels(group)
      }
      return(unique(group))
    },
    
    .asObservedGroupFactor = function(group) {
      if (is.factor(group)) {
        return(droplevels(group))
      }
      return(factor(group))
    },
    
    .splitTwoIndependentSamplesData = function(varname,splitvarname,preparedx,preparedgroup,testname,testnameshort) {
      xtwosample <- preparedx
      group <- preparedgroup
      groupForSplit <- private$.asObservedGroupFactor(group)
      x <- split(xtwosample, groupForSplit)
      
      if (length(x) != 2) {
        private$.handleAnalysisIssue(
          name = paste0("splitVariableLevels", testnameshort, "_", splitvarname),
          message = paste0(testname, ": 'Split by' variable '",splitvarname,"' must have exactly two levels after removing rows with missing values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(invisible(NULL))
      }
      
      x1 <- x[[1]]
      x2 <- x[[2]]
      n1 <- length(x1)
      n2 <- length(x2)
      
      if ((n1 + n2 - 2) <= 0) {
        private$.handleAnalysisIssue(
          name = paste0("insufficientObservations", testnameshort, "_", varname),
          message = paste0(testname, ": At least three complete observations are required for the test."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(invisible(NULL))
      }
      return(list(x1,x2,n1,n2))
    },
    
    # helper functions for contingency analysis
    
    .hasContingencyInput = function() {
      nominal1 <- self$options$nominal1
      nominal2 <- self$options$nominal2
      return(!any(is.null(nominal1), is.null(nominal2)))
    },
    
    .tableWithoutCounts = function(countsName, contingencytable,testnameshort="CONT",selectedTableNames) {
      # check if all counts are positive and finite
      if(!any(contingencytable < 0) & !any(is.infinite(contingencytable))) {
        return(contingencytable)
      }
      private$.hideTables(selectedTableNames)
      private$.handleAnalysisIssue(
        name = paste0("negCounts", testnameshort, "_", countsName),
        message = "Test for independence in contingency tables: Some counts are negative and/or infinite, but all counts are required to be non-negative and finite.",
        type = jmvcore::NoticeType$STRONG_WARNING
      )
      
      return(NULL)
    },
    
    .tableWithCounts = function(columns, countsName, rowVarName, colVarName,testname, testnameshort="CONT",selectedTableNames) {
      # check if all counts are numeric
      if(!jmvcore::canBeNumeric(self$data[[countsName]])) {
        private$.hideTables(selectedTableNames)
        private$.handleAnalysisIssue(
          name = paste0("CountsNotNumeric", testnameshort, "_", countsName),
          message = paste0(testname,": Counts must be numeric, but '",countsName,"' contains values that are not numeric."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(NULL)
      }
      
      columns$counts <- private$.toNumericWithoutExplicitNotice(countsName)
      
      # check if all counts are non-negative
      if(any(columns$counts < 0, na.rm = TRUE)) {
        private$.hideTables(selectedTableNames)
        private$.handleAnalysisIssue(
          name = paste0("CountsNegative", testnameshort, "_", countsName),
          message = paste0(testname,": Counts must be non-negative, but '",countsName,"' contains negative values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(NULL)
      }
      
      # check if all counts are finite
      if(any(is.infinite(columns$counts), na.rm = TRUE)) {
        private$.hideTables(selectedTableNames)
        private$.handleAnalysisIssue(
          name = paste0("CountsInfinite", testnameshort, "_", countsName),
          message = paste0(testname,": Counts must be finite, but '",countsName,"' contains infinite values."),
          type = jmvcore::NoticeType$STRONG_WARNING)
        return(NULL)
      }
      
      # no missing count values
      if(!any(is.na(columns$counts))) {
        contingencytableFromCounts <- xtabs(counts ~ ., data=columns)
        return(contingencytableFromCounts)
      }
      
      # ignore any counts with missing values
      df = data.frame(a=self$data[[rowVarName]], b=self$data[[colVarName]], c=columns$counts)
      
      # only keep data where no counts are missing
      df = df[!is.na(df$c),]
      contingencytableFromCounts <- xtabs(c~a+b, data=df)
      
      # add countsName to list of variables with missing values for the notice
      private$.noteMissingValues(countsName, self$data)
      
      return(contingencytableFromCounts)
    },
    
    .renderContingencyResults = function(contingencytable, rowVarName, colVarName, alphaValid, testname) {
      # populate contingency table with absolute frequencies
      if(self$options$contabs) {
        private$.fillContingencyTable(self$results$absfreqstable, contingencytable, rowVarName, colVarName)
      }
      # populate test table
      if(self$options$conttest && alphaValid) {
        private$.renderChiSquaredTest(
          contingencytable = contingencytable,
          testname = testname
        )
      }
    },
    
    .renderChiSquaredTest = function(contingencytable, testname) {
      alpha <- self$options$conttestalpha
      
      # calculate test statistic and critical value
      r <- dim(contingencytable)[1]
      s <- dim(contingencytable)[2]
      statistic_value <- private$.computeChisquaredStatistic(contingencytable, testname = testname)
      c_value <- qchisq(1-alpha,(r-1)*(s-1))
      dec = 'Reject H\U2080 if Statistic > c'
      
      # populate results table
      self$results$contindependencetest$addRow(rowKey=1, values=list(
        signlevel=alpha,
        statistic=statistic_value,
        c=c_value,
        decision=dec
      ))
    },
    
    .fillContingencyTable = function(table, data, rowVarName, colVarName) {  
      type <- ifelse(is.integer(data), 'integer', 'number')      
      table$addColumn(
        name=rowVarName,
        title=rowVarName,
        type='text')
      
      for (level in colnames(data)) {
        table$addColumn(
          name=level,
          title=level,
          superTitle=colVarName,
          type=type)
      }
      table$addColumn(
        name='.total',
        title='Total',
        type=type)
      
      # fill the table with data
      for (i in seq_len(nrow(data))) {
        values <- as.list(data[i,])
        names(values) <- colnames(data)
        values[[rowVarName]] <- rownames(data)[i]
        values[['.total']] <- sum(data[i,]) # add total for the row
        table$addRow(rowKey=i, values=values)
      }
      
      # add row for the total of each column
      totalRow <- as.list(colSums(data))
      names(totalRow) <- colnames(data)
      totalRow[[rowVarName]] <- 'Total'
      totalRow[['.total']] <- sum(data)
      table$addRow(rowKey=nrow(data)+1, values=totalRow)
    },
    
    # compute functions for all tests
    
    .oneSampleZSideSpec = function(alpha, n, mu0, sigma2, side) {
      side <- private$.normalizeSide(side)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "normal")
      
      if (side == "two") {
        return(list(side = side,c = crit$c,decision = crit$decision,
          powerFunction = function(mu) {
            delta <- sqrt(n) * (mu - mu0) / sqrt(sigma2)
            pnorm(qnorm(alpha / 2) + delta) +
              pnorm(qnorm(alpha / 2) - delta)
          },
          minNQuantile = qnorm(1 - alpha / 2)))
      }
      
      if (side == "lower") {
        return(list(side = side,c = crit$c,decision = crit$decision,
          powerFunction = function(mu) {
            delta <- sqrt(n) * (mu - mu0) / sqrt(sigma2)
            pnorm(qnorm(alpha) - delta)
          },
          minNQuantile = qnorm(1 - alpha)))
      }
      
      if (side == "upper") {
        return(list(side = side,c = crit$c,decision = crit$decision,
          powerFunction = function(mu) {
            delta <- sqrt(n) * (mu - mu0) / sqrt(sigma2)
            pnorm(qnorm(alpha) + delta)
          },
          minNQuantile = qnorm(1 - alpha)))
      }
    },
    
    .computeZTestOneSample = function(x,alpha,mu0,sigma2,side,powermu,typeImu,typeIImu,betastar,
                                      delta0,typeIInH0,typeIIInH1,minnCanCompute) {
      n <- length(x)
      statistic_value <- sqrt(n) * (mean(x) - mu0) / sqrt(sigma2)
      spec <- private$.oneSampleZSideSpec(alpha = alpha,n = n,mu0 = mu0,sigma2 = sigma2,side = side)
      power_value <- spec$powerFunction(powermu)
      
      typeI_value <- ""
      if (isTRUE(typeIInH0)) {
        typeI_value <- spec$powerFunction(typeImu)
      }
      
      typeII_value <- ""
      if (isTRUE(typeIIInH1)) {
        typeII_value <- 1 - spec$powerFunction(typeIImu)
      }
      
      minn_value <- ""
      if (isTRUE(minnCanCompute)) {
        minn_value <- sigma2 / delta0^2 * (spec$minNQuantile + qnorm(1 - betastar))^2
      }
      
      return(list(signlevel = alpha,statistic = statistic_value,c = spec$c,decision = spec$decision,
                  power = power_value,typeI = typeI_value,typeII = typeII_value,minn = minn_value))
    },
    
    .computeTTestOneSample = function(x, alpha, mu0, side) {
      n <- length(x)
      x_mean <- mean(x)
      sigmadach <- 1 / (n - 1) * sum((x - x_mean)^2)
      statistic_value <- sqrt(n) * (x_mean - mu0) / sqrt(sigmadach)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "t",df = n - 1)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeCriticalChiSquareVariance = function(alpha, side, sigma0, df, divisor) {
      side <- private$.normalizeSide(side)
      
      if (side == "two") {
        return(list(c = sigma0^2 / divisor * qchisq(1 - alpha / 2, df),d = sigma0^2 / divisor * qchisq(alpha / 2, df),
                    decision = 'Reject H\U2080 if Statistic > c or Statistic < d'))
      }
      
      if (side == "lower") {
        return(list(c = sigma0^2 / divisor * qchisq(alpha, df),d = "",
                    decision = 'Reject H\U2080 if Statistic < c'))
      }
      
      if (side == "upper") {
        return(list(c = sigma0^2 / divisor * qchisq(1 - alpha, df),d = "",
                    decision = 'Reject H\U2080 if Statistic > c '))
      }
    },
    
    .computeChiSquareVarianceTestOneSample = function(x,alpha,sigma0,side,knownMu = FALSE,mu = NULL) {
      n <- length(x)
      
      if (isTRUE(knownMu)) {
        center <- mu
        df <- n
        divisor <- n
      } else {
        center <- mean(x)
        df <- n - 1
        divisor <- n - 1
      }
      statistic_value <- 1 / divisor * sum((x - center)^2)
      crit <- private$.computeCriticalChiSquareVariance(alpha = alpha,side = side,sigma0 = sigma0,df = df,divisor = divisor)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,d = crit$d,decision = crit$decision))
    },
    
    .computeZTestPaired = function(x1,x2,n,alpha,testvalue,sigma12,sigma22,rho,side) {
      deltaBar <- mean(x1 - x2)
      sigmaDelta2 <- sigma12 + sigma22 - 2 * rho * sqrt(sigma12 * sigma22)
      statistic_value <- sqrt(n) * (deltaBar - testvalue) / sqrt(sigmaDelta2)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "normal")
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeTTestPaired = function(x1,x2,n,alpha,testvalue,side) {
      differences <- x1 - x2
      differencesMean <- mean(differences)
      differencesVariance <- 1 / (n - 1) * sum((differences - differencesMean)^2)
      statistic_value <- sqrt(n) * (differencesMean - testvalue) / sqrt(differencesVariance)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "t",df = n - 1)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeIndependenceTestPaired = function(rxy,n,alpha,side) {
      statistic_value <- rxy * sqrt(n - 2) / sqrt(1 - rxy^2)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "t",df = n - 2)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeCriticalZTLike = function(alpha, side, distribution = c("normal", "t"), df = NULL) {
      side <- private$.normalizeSide(side)
      distribution <- match.arg(distribution)
      
      q <- function(p) {
        if (distribution == "normal") {
          return(qnorm(p))
        }
        return(qt(p, df))
      }
      if (side == "two") {
        return(list(c = q(1 - alpha / 2),d = "",decision = 'Reject H\U2080 if |Statistic| > c'))
      }
      if (side == "lower") {
        return(list(c = -q(1 - alpha),d = "",decision = 'Reject H\U2080 if Statistic < c'))
      }
      if (side == "upper") {
        return(list(c = q(1 - alpha),d = "",decision = 'Reject H\U2080 if Statistic > c '))
      }
    },
    
    .computeCriticalFTest = function(alpha, side, df1, df2) {
      side <- private$.normalizeSide(side)
      
      if (side == "two") {
        return(list(c = qf(1 - alpha / 2, df1, df2),d = qf(alpha / 2, df1, df2),
          decision = 'Reject H\U2080 if Statistic > c or Statistic < d'))
      }
      if (side == "lower") {
        return(list(c = qf(alpha, df1, df2),d = "",decision = 'Reject H\U2080 if Statistic < c'))
      }
      if (side == "upper") {
        return(list(c = qf(1 - alpha, df1, df2),d = "",decision = 'Reject H\U2080 if Statistic > c '))
      }
    },
    
    .computeCriticalSignTest = function(alpha, side, n) {
      side <- private$.normalizeSide(side)
      
      if (side == "two") {
        return(list(c = private$.computeCO(alpha / 2, n, 0.5),d = private$.computeCU(alpha / 2, n, 0.5),
                    decision = 'Reject H\U2080 if Statistic > c or Statistic < d'))
      }
      if (side == "lower") {
        return(list(c = private$.computeCU(alpha, n, 0.5),d = "",decision = 'Reject H\U2080 if Statistic < c'))
      }
      if (side == "upper") {
        return(list(c = private$.computeCO(alpha, n, 0.5),d = "",decision = 'Reject H\U2080 if Statistic > c'))
      }
    },
    
    .computeFTestIndep = function(x1, x2, n1, n2, alpha, side) {
      sigmadach12 <- 1 / (n1 - 1) * sum((x1 - mean(x1))^2)
      sigmadach22 <- 1 / (n2 - 1) * sum((x2 - mean(x2))^2)
      statistic_value <- sigmadach12 / sigmadach22
      crit <- private$.computeCriticalFTest(alpha = alpha,side = side,df1 = n1 - 1,df2 = n2 - 1)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,d = crit$d,decision = crit$decision))
    },
    
    .computeTTestIndep = function(x1, x2, n1, n2, alpha, side) {
      sigmadach12 <- 1 / (n1 - 1) * sum((x1 - mean(x1))^2)
      sigmadach22 <- 1 / (n2 - 1) * sum((x2 - mean(x2))^2)
      sigma2pool <- (n1 - 1) / (n1 + n2 - 2) * sigmadach12 + (n2 - 1) / (n1 + n2 - 2) * sigmadach22
      deltabar <- mean(x1) - mean(x2)
      statistic_value <- deltabar / sqrt((1 / n1 + 1 / n2) * sigma2pool)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "t",df = n1 + n2 - 2)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeZTestIndep = function(x1, x2, n1, n2, alpha, sigma2, side) {
      deltabar <- mean(x1) - mean(x2)
      sigmadeltadach2 <- (1 / n1 + 1 / n2) * sigma2
      statistic_value <- deltabar / sqrt(sigmadeltadach2)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "normal")
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeCriticalExactBinomialTest = function(alpha, side, n, p0) {
      side <- private$.normalizeSide(side)
      
      if (side == "two") {
        c_value <- private$.computeCO(alpha / 2, n, p0)
        d_value <- private$.computeCU(alpha / 2, n, p0)
        return(list(c = c_value,d = d_value,decision = 'Reject H\U2080 if Statistic > c or Statistic < d',
          powerFunction = function(x) {
            1 - pbinom(c_value, n, x) + pbinom(d_value - 1, n, x)
          }))
      }
      
      if (side == "lower") {
        c_value <- private$.computeCU(alpha, n, p0)
        return(list(c = c_value,d = "",decision = 'Reject H\U2080 if Statistic < c',
          powerFunction = function(x) {
            pbinom(c_value - 1, n, x)
          }))
      }
      
      if (side == "upper") {
        c_value <- private$.computeCO(alpha, n, p0)
        return(list(c = c_value,d = "",decision = 'Reject H\U2080 if Statistic > c ',
          powerFunction = function(x) {
            1 - pbinom(c_value, n, x)
          }))
      }
    },
    
    .computeExactBinomialTest = function(sumtestdata, n, alpha, p0, side,powerp, typeIp, typeIIp) {
      statistic_value <- sumtestdata
      power_value <- ""
      typeI_value <- ""
      typeII_value <- ""
      crit <- private$.computeCriticalExactBinomialTest(alpha = alpha,side = side,n = n,p0 = p0)
      sideNormalized <- private$.normalizeSide(side)
      power_value <- crit$powerFunction(powerp)
      
      if (sideNormalized == "two") {
        if (typeIp == p0) {
          typeI_value <- crit$powerFunction(typeIp)
        }
        if (typeIIp != p0) {
          typeII_value <- 1 - crit$powerFunction(typeIIp)
        }
      }
      if (sideNormalized == "lower") {
        if (typeIp >= p0) {
          typeI_value <- crit$powerFunction(typeIp)
        }
        if (typeIIp < p0) {
          typeII_value <- 1 - crit$powerFunction(typeIIp)
        }
      }
      if (sideNormalized == "upper") {
        if (typeIp <= p0) {
          typeI_value <- crit$powerFunction(typeIp)
        }
        if (typeIIp > p0) {
          typeII_value <- 1 - crit$powerFunction(typeIIp)
        }
      }
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,d = crit$d,
        decision = crit$decision,power = power_value,typeI = typeI_value,typeII = typeII_value))
    },
    
    .computeApproxOneSampleBinomialTest = function(sumtestdata, n, alpha, p0, side) {
      pdach <- 1 / n * sumtestdata
      statistic_value <- sqrt(n) * (pdach - p0) * 1 / sqrt(pdach * (1 - pdach))
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "normal") ##TODO binomial
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeTwoSampleBinomialApprox = function(x1, x2, n1, n2, alpha, side) {
      p1dach <- 1 / n1 * sum(x1)
      p2dach <- 1 / n2 * sum(x2)
      p12dach <- n1 / (n1 + n2) * p1dach + n2 / (n1 + n2) * p2dach
      statistic_value <- (p1dach - p2dach) / sqrt((1 / n1 + 1 / n2) * p12dach * (1 - p12dach))
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "normal")
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeSignTest = function(testdata, alpha, testvalue, side) {
      n <- length(testdata)
      statistic_value <- n * (1 - private$.computeECDF(testdata, testvalue))
      crit <- private$.computeCriticalSignTest(alpha = alpha,side = side,n = n)
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,d = crit$d,decision = crit$decision))
    },
    
    .computeUTestIndep = function(x1, x2, n1, n2, alpha, side) {
      W1 <- sum(rank(x1))    # sum of all ranks for the first sample
      statistic_value <- (W1 - n1 * n2 - n1 * (n1 + 1) / 2 + n1 * n2 / 2) / sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
      crit <- private$.computeCriticalZTLike(alpha = alpha,side = side,distribution = "normal")
      
      return(list(signlevel = alpha,statistic = statistic_value,c = crit$c,decision = crit$decision))
    },
    
    .computeChisquaredStatistic = function(table, testname, testnameshort="CONT") {
      table.rand <- addmargins(table)
      r <- length(table[1,]) # num columns
      s <- length(table[,1]) # num rows
      
      zero_cols <- table.rand[s+1,1:r] != numeric(r)
      zero_rows <- table.rand[1:s,r+1] != numeric(s)
      
      if(mean(zero_rows) == 1 & mean(zero_cols) == 1) {
        chi.matrix <- (table.rand[1:s,1:r])^2 /
          (table.rand[1:s,r+1] %*% t(table.rand[s+1,1:r]))
        chi.sqrd <- table.rand[s+1,r+1] * (sum(chi.matrix)-1)
        return(chi.sqrd)
      }
      
      # zero rows or columns exist, remove them and recalculate
      r_new <- sum(zero_cols)
      s_new <- sum(zero_rows)
      
      if(!(r_new > 1 && s_new > 1)) {
        private$.handleAnalysisIssue(
          name = paste0("notEnoughCountsData", testnameshort),
          message = paste0(testname, ": Too many missing values and not enough data for calculating the test statistic!"),
          type = jmvcore::NoticeType$STRONG_WARNING
        )
        private$.hideTables("contindependencetest")
        return(NaN)
      }
      
      table.nozeroes <- table[,t(as.vector(zero_cols))]
      table.nozeroes <- table.nozeroes[t(as.vector(zero_rows)),]
      table.no.rand <- addmargins(as.table(table.nozeroes))
      
      chi.matrix <- (table.no.rand[1:s_new,1:r_new])^2 /
        (table.no.rand[1:s_new,r_new+1] %*% t(table.no.rand[s_new+1,1:r_new]))
      chi.sqrd <- table.no.rand[s_new+1,r_new+1] * (sum(chi.matrix)-1)
      
      return(chi.sqrd)
    },
    
    .computeECDF = function(x,t) {
      # only for numerical inputs
      absfrequencies <- data.frame(table(x))[,2]
      relfrequencies <- absfrequencies/sum(absfrequencies)
      cumulativerelfrequencies <- cumsum(c(0,relfrequencies))
      ecdfvalues <- numeric(length(unique(t)))
      for (i in seq_along(unique(t))) {
        k <- sum(unique(x) <= sort(unique(t))[i])
        ecdfvalues[i] <- cumulativerelfrequencies[k+1]
      }
      return(ecdfvalues)
    },
    
    .computeCU = function(alpha, n, p0) {
      Fx <- pbinom(seq(-1, n, 0.1), size = n, prob = p0)
      if(Fx[min(which(Fx>alpha))-1] <= alpha && alpha < Fx[min(which(Fx>alpha))]) {
        return(qbinom(Fx[min(which(Fx>alpha))],size = n, prob = p0))
      }
    },
    
    .computeCO = function(alpha, n, p0) {
      Fx <- pbinom(seq(-1, n, 0.1), size = n, prob = p0)
      if(Fx[min(which(Fx>1-alpha))-1] < 1-alpha && 1-alpha <= Fx[min(which(Fx>1-alpha))]) {
        return(qbinom(Fx[min(which(Fx>1-alpha))-1],size = n, prob = p0) + 1)
      }
    },
    
    .computeBravaisPearson = function(x,y) {
      y_mean <- mean(y)
      x_mean <- mean(x)
      sxy <- mean(x*y) - x_mean * y_mean
      sx <- sqrt(mean(x*x) - mean(x)^2)
      sy <- sqrt(mean(y*y) - mean(y)^2)
      rxy <- sxy/(sx*sy) 
      return(rxy)
    }
  )
)