library(GA)
library(staTools)
library(shinyWidgets)
library(shinyalert)
library(writexl)
library(rlang)
library(DT)
library(modelr)
library(tidyr)

library(jsonlite)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(readxl)
library(scales)
library(optimx)



source(file = "src/Rechenkern.R")
source(file = "src/createLandkreisR0_no_erfasstDf.R")
source(file = "src/createDfBundLandKreis.R")
source(file = "src/optimizerLoopingR0N0.R")
source(file = "src/helperForCovid19.R")

source(file = "src/Rechenkern.R")
source(file = "src/createLandkreisR0_no_erfasstDf.R")
source(file = "src/createDfBundLandKreis.R")
source(file = "src/optimizerLoopingR0N0.R")
source(file = "src/helperForCovid19.R")
load("data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))

vals <- list(Flag = "Bundesland")
input$BundeslandSelected <- "Thüringen"
r0_no_erfasstDf <- createLandkreisR0_no_erfasstDf(historyDfBundesLand, historyDfBund, regionSelected, vals, input,session)

dfRoNo <- r0_no_erfasstDf[[1]]
n0_erfasst_nom_min_max <- r0_no_erfasstDf[[2]]
R0_conf_nom_min_max <- r0_no_erfasstDf[[3]]
startDate <- r0_no_erfasstDf[[4]]
rechenDf_nom <- cbind(dfRoNo,n0_erfasst=n0_erfasst_nom_min_max$n0_erfasst_nom, R0 =R0_conf_nom_min_max$R0_nom)
calcPredictionsForOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, R0Opt, n0Opt, startDate, rechenDf_nom, input) {
  #browser()
  inputForOptimization <- input# to make setting reduzierung_rtx easy and fast
  inputForOptimization$reduzierung_rt1 <- reduzierung_rt1
  inputForOptimization$reduzierung_rt2 <- reduzierung_rt2
  inputForOptimization$reduzierung_rt3 <- reduzierung_rt3
  
  dfRechenKern <- (Rechenkern(rechenDf_nom, inputForOptimization, startDate))
  metric <- calcMetric(dfRechenKern, rechenDf_nom)
  cat("metric: ", metric)
  return(metric) # minus because GA maximizes
} 



calcMetric <- function(dfRechenKern, data){
  res <- left_join(data, dfRechenKern %>% select(c(Tag, ErfassteInfizierteBerechnet)), by = c("MeldeDate" = "Tag")) %>% filter(!is.na(ErfassteInfizierteBerechnet))
  #   (sum(log10(res$SumAnzahl) 
  #        - log10(res$ErfassteInfizierteBerechnet) )^2)/(nrow(data)-1)^0.5
  
  # metric <- MPE((res$SumAnzahl), (res$ErfassteInfizierteBerechnet))
  metric <- (sum(res$SumAnzahl   - res$ErfassteInfizierteBerechnet )^2)^0.5
}


lower = c(10, 10,-20 )
upper = c(30, 30, 20)
# https://bergant.github.io/nlexperiment/flocking_bfgs.html
tic()
optimrRes <- optimr(c(30, 20,-0 ), function(x) calcPredictionsForOptimization(x[1], x[2], x[3], R0_conf_nom_min_max,  n0_erfasst_nom_min_max, startDate, rechenDf_nom, input),
                    lower = lower, upper = upper, method="L-BFGS-B",control = list(maxit = 1, trace = 1))
toc()
proptimr(optimrRes)
optimrRes
optimrRes$par
GA <- ga(type = "real-valued", 
         fitness =  function(x) calcPredictionsForOptimization(x[1], x[2], x[3], R0_conf_nom_min_max,  n0_erfasst_nom_min_max, startDate, rechenDf_nom, input),
         seed = 2020,
         lower = lower, upper = upper, 
         popSize = 10, maxiter = 30, run = 5)
# browser()
