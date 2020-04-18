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

createOptParmeters <- function(){
  
  parameter_tibble <- tribble(
    ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
    "reduzierung_rt1", 0         ,  0,        60,        "TRUE",
    "reduzierung_rt2", 0         ,  0,        60,        "TRUE",
    "reduzierung_rt3", -20       ,  -40,      30,        "TRUE")
  
  parameter_tibble$var_selected <- TRUE
  # parameter_tibble[parameter_tibble$var_name =="TNOM", "var_selected"] <- "FALSE"
  
  parameter_tibble$new_sol <- NA 
  
  parameter_tibble <- parameter_tibble %>% mutate(start_value = var_value)
  
  parameter_tibble <- parameter_tibble %>% mutate(var_value_norm = (var_value - var_min) /(var_max-var_min))
  
  optPara <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_value) %>% unlist
  names(optPara) <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_name) %>% unlist
  nonOptPara <- parameter_tibble %>% filter(var_selected == FALSE) %>% 
    select(var_value) %>% unlist
  names(nonOptPara) <-parameter_tibble %>% filter(var_selected == FALSE) %>% 
    select(var_name) %>% unlist
  allPara <- c(optPara , nonOptPara )
  maxOpt <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_max) %>% unlist
  minOpt <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_min) %>% unlist
  parameter_tibble <-  parameter_tibble %>% filter(var_selected == TRUE) %>% 
    mutate( suggestions_norm =  (var_value - var_min) /(var_max-var_min))
  
  suggestions <- parameter_tibble  %>% filter(var_selected == TRUE)%>%
    select(suggestions_norm) %>%unlist
  
  minOpt <- rep(0, length(minOpt))
  maxOpt <- rep(1, length(minOpt))
  # browser()
  return(list(allPara, optPara, minOpt, maxOpt, suggestions, parameter_tibble  ))
}

denormalizePara <- function(optPara, parameter_tibble, para) {
  denormPara <- list(NULL)
  # browser()
  for(i in 1 : length(optPara)){
    denormPara[[i]] <- optPara[[i]] * (parameter_tibble$var_max[i]-parameter_tibble$var_min[i]) +
      parameter_tibble$var_min[i]
  }
  
  # browser()
  names(denormPara) <- parameter_tibble$var_name
  denormPara
}
createRkiRegOptFrame <- function(RkiDataWithRoNoAndReduzierungOpimized, regionSelected, input ){
  # browser()
  RkiDataWithR0N0 <- RkiDataWithRoNoAndReduzierungOpimized %>%  filter(whichRegion == regionSelected) %>% unnest(data)
  res <- optimizerGeneticAlgorithmRedReduction(RkiDataWithR0N0, input)
  indexEntitiy <- which(RkiDataWithRoNoAndReduzierungOpimized$whichRegion == regionSelected)
  RkiDataWithRoNoAndReduzierungOpimized$reduzierungsOptResult[indexEntitiy] <-list(res$reduzierungsOptResult )
  
  return(list( "RkiDataWithRoNoAndReduzierungOpimized" = RkiDataWithRoNoAndReduzierungOpimized))
}
optimizerGeneticAlgorithmRedReduction <- function(RkiDataWithR0N0, input) {
  # optimizer using genetic algorithm to optimize reduzierungsmaßnahmen und R0 n0
  # dfRoNoOpt should be dataframe starting with reduzierung_datum1
  dateOfFirstAction <- input$reduzierung_datum1
  RkiDataWithR0N0 <- RkiDataWithR0N0 %>% filter(MeldeDate >= dateOfFirstAction) # only consider values after first reduction action
  
  res <- createOptParmeters()
  allPara          <- res[[1]]
  optPara          <- res[[2]]
  minOpt           <- res[[3]]
  maxOpt           <- res[[4]]
  suggestions      <- res[[5]]
  parameter_tibble <- res[[6]]
  
  GA <-  ga(type = "real-valued",
            suggestions = suggestions,
            fitness = calcPredictionsForGaOptimization,
            lower = minOpt,
            upper = maxOpt,
            popSize = 10, maxiter = 30,
            seed = 2020,
            allPara = allPara, parameter_tibble = parameter_tibble, RkiDataWithR0N0 = RkiDataWithR0N0, input = input,
            keepBest = FALSE
  )
  denormPara <- denormalizePara(GA@solution, parameter_tibble, para)
  
  print(GA@solution)
  for(i in seq(1, length(denormPara)))  {
    print(i)
    GA@solution[i] <- denormPara[[i]]
  }
  print(GA@solution)
  cat("denormPara", unlist(denormPara), "\n")  
  # browser()
  
  
  
  #  ############## non normalized optimization
  #  suggestions <- c( 0, 0, -20)
  #  GA <- ga(type = "real-valued", 
  #           fitness =  function(x) calcPredictionsForGaOptimization(x[1], x[2], x[3], RkiDataWithR0N0, input),
  #           suggestions =suggestions,
  #           lower = c(0, 0, -40), upper = c(60, 60, 30), 
  #           popSize = 10, maxiter = 30, run = 5, seed = 2020,
  #           keepBest = TRUE) #a logical argument specifying if best solutions at each iteration should be saved in a slot called bestSol
  #  
  #  input$reduzierung_rt1 <- GA@solution[[1]]
  #  input$reduzierung_rt2 <- GA@solution[[2]]
  #  input$reduzierung_rt3 <- GA@solution[[3]]
  #  print(GA@solution)
  #  browser()
  #########################################
  reduzierungsOptResult <- tibble("reduzierung_rt1" = denormPara[["reduzierung_rt1"]], "reduzierung_rt2" = denormPara[["reduzierung_rt2"]],
                                  "reduzierung_rt3" = denormPara[["reduzierung_rt3"]], "GaFitnessValue" = GA@fitnessValue, "GaPara" = list(summary(GA)))
  return(list("reduzierungsOptResult" = reduzierungsOptResult))
  
}
calcPredictionsForGaOptimization = function(optPara, allPara, parameter_tibble, RkiDataWithR0N0,input) {   
  # calculate the predictions for the optimzaition loop, i.e. GA algorithm
  denormPara <- denormalizePara(optPara, parameter_tibble)
  #  cat("para",unlist(denormPara,"\n"))
  for(i in 1 : length(optPara)){
    allPara[[i]] <- denormPara[[i]]
  }
  
  inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
  inputVarNames <- names(allPara)
  for (inputVarName in   inputVarNames ) {
    inputForOptimization[[inputVarName]]<- allPara[[inputVarName]]
  }
  
  inputForOptimization$dateInput[2] = RkiDataWithR0N0$MeldeDate %>% max() # set endDate to date of last MeldeDate
    dfRechenKern <-   Rechenkern(RkiDataWithR0N0, inputForOptimization)

  dfRechenKern <- dfRechenKern %>% filter(Tag  %in% RkiDataWithR0N0$MeldeDate)
  RkiDataWithR0N0 <- RkiDataWithR0N0 %>% filter(MeldeDate  %in% dfRechenKern$Tag)
  # res <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,RkiDataWithR0N0$SumAnzahl)
  # browser()
  #    res <- (
  #      (sum(
  #      (log10(RkiDataWithR0N0$SumAnzahl)   - log10(dfRechenKern$ErfassteInfizierteBerechnet))^2)
  #      )/(nrow(dfRechenKern)-1)
  #      )^0.5
  #  res <- sqrt(mean((log10(RkiDataWithR0N0$SumAnzahl)-log10(dfRechenKern$ErfassteInfizierteBerechnet))^2))
  res <- MAPE(log10(RkiDataWithR0N0$SumAnzahl),log10(dfRechenKern$ErfassteInfizierteBerechnet))
  # cat("res is :", res , "redu1 = ", reduzierung_rt1, "\n")
  # res <- sqrt(mean((log10(dfRechenKern$ErfassteInfizierteBerechnet)-log10(RkiDataWithR0N0$SumAnzahl))^2))
  return(-res)
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



