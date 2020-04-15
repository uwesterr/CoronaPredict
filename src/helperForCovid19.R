# helper functions for shiny app Covid19

library(tictoc)
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

calcPredictionsForOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, R0Opt, n0Opt, startDate, rechenDf_nom, input) {
  #browser()
  inputForOptimization <- isolate(reactiveValuesToList(input)) # to make setting reduzierung_rtx easy and fast
  inputForOptimization$reduzierung_rt1 <- reduzierung_rt1
  inputForOptimization$reduzierung_rt2 <- reduzierung_rt2
  inputForOptimization$reduzierung_rt3 <- reduzierung_rt3
  
  dfRechenKern <- (Rechenkern(rechenDf_nom, inputForOptimization, startDate))
  metric <- calcMetric(dfRechenKern, rechenDf_nom)
  cat("metric: ", metric)
  return(-metric) # minus because GA maximizes
} 

calcMetric <- function(dfRechenKern, data){
  res <- left_join(data, dfRechenKern %>% select(c(Tag, ErfassteInfizierteBerechnet)), by = c("MeldeDate" = "Tag")) %>% filter(!is.na(ErfassteInfizierteBerechnet)) %>% 
  rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  metric <- (sum(log10(res$SumAnzahl)   - log10(res$ErfassteInfizierteBerechnet) )^2)/(nrow(data)-1)^0.5
}

################    optimizing reduzierung  #################

optimizerGeneticAlgorithmRedReduction <- function(RkiDataWithR0N0, input) {
  # optimizer using genetic algorithm to optimize reduzierungsmaßnahmen und R0 n0
  # dfRoNoOpt should be dataframe starting with reduzierung_datum1
  dateOfFirstAction <- input$reduzierung_datum1
   RkiDataWithR0N0 <- RkiDataWithR0N0 %>% filter(MeldeDate >= dateOfFirstAction) # only consider values after first reduction action
  
  calcPredictionsForGaOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, RkiDataWithR0N0, input) {
    
    

    inputForOptimization <- input# to make setting reduzierung_rtx easy and fast
    inputForOptimization$reduzierung_rt1 <- reduzierung_rt1
    inputForOptimization$reduzierung_rt2 <- reduzierung_rt2
    inputForOptimization$reduzierung_rt3 <- reduzierung_rt3
    
    inputForOptimization$dateInput[2] = RkiDataWithR0N0$MeldeDate %>% max() # set endDate to date of last MeldeDate
    
  # browser()
    dfRechenKern <-   Rechenkern(RkiDataWithR0N0, inputForOptimization)
    dfRechenKern <- dfRechenKern %>% filter(Tag  %in% RkiDataWithR0N0$MeldeDate)
    RkiDataWithR0N0 <- RkiDataWithR0N0 %>% filter(MeldeDate  %in% dfRechenKern$Tag)
    res <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,RkiDataWithR0N0$SumAnzahl)
    res <- (sum(log10(RkiDataWithR0N0$SumAnzahl)   - log10(dfRechenKern$ErfassteInfizierteBerechnet) )^2)/(nrow(dfRechenKern)-1)^0.5

    # cat("res is :", res , "redu1 = ", reduzierung_rt1, "\n")
    # res <- sqrt(mean((log10(dfRechenKern$ErfassteInfizierteBerechnet)-log10(RkiDataWithR0N0$SumAnzahl))^2))
    return(-res)
  } 
  
  suggestions <- c( 0, 0, -20)
  GA <- ga(type = "real-valued", 
           fitness =  function(x) calcPredictionsForGaOptimization(x[1], x[2], x[3], RkiDataWithR0N0, input),
           suggestions =suggestions,
           lower = c(0, 0, -40), upper = c(60, 60, 30), 
           popSize = 10, maxiter = 30, run = 5, seed = 2020)
  
  input$reduzierung_rt1 <- GA@solution[[1]]
  input$reduzierung_rt2 <- GA@solution[[2]]
  input$reduzierung_rt3 <- GA@solution[[3]]
  reduzierungsOptResult <- tibble("reduzierung_rt1" = GA@solution[[1]], "reduzierung_rt2" = GA@solution[[2]],
                                "reduzierung_rt3" = GA@solution[[3]], "GaPara" = list(GA))
  return(list("reduzierungsOptResult" = reduzierungsOptResult))
  
}

calcReduziertOptPredictions <- function(R0, n0, dfRoNoOpt, input, startDate){
  # browser()
  dfRoNoOpt$R0 <-  R0
  dfRoNoOpt$n0_erfasst <- n0
  dfRoNoOpt <- dfRoNoOpt %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  dfRechenKern <- (Rechenkern(dfRoNoOpt, input$input, startDate))
  
  return(dfRechenKern) 
}   