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




createRkiRegOptFrame <- function(dfNested, regionSelected, parameter_tibble, optFunction, resultColumnName, input){
  # browser()
  dfUnNested <- dfNested %>%  filter(whichRegion == regionSelected) %>% unnest(data)
  res <- optimizerGeneticAlgorithmRedReduction(dfUnNested, parameter_tibble, optFunction, input)
  indexEntitiy <- which(dfNested$whichRegion == regionSelected)
 # dfNested$reduzierungsOptResult[indexEntitiy] <- list(res$OptResult)
  #dfNested[[resultColumnName]][indexEntitiy]
  dfNested[[indexEntitiy, resultColumnName]] <- list(res$OptResult)
  
  return(list( "dfNested" = dfNested))
}
optimizerGeneticAlgorithmRedReduction <- function(dfUnNested, parameter_tibble, optFunction, input) {
  # optimizer using genetic algorithm to optimize reduzierungsmaßnahmen und R0 n0
  # dfRoNoOpt should be dataframe starting with reduzierung_datum1
  dateOfFirstAction <- input$reduzierung_datum1
  dfUnNested <- dfUnNested %>% filter(MeldeDate >= dateOfFirstAction) # only consider values after first reduction action
  
  res <- createOptParmeters(parameter_tibble)
  allPara          <- res[[1]]
  optPara          <- res[[2]]
  minOpt           <- res[[3]]
  maxOpt           <- res[[4]]
  suggestions      <- res[[5]]
  parameter_tibble <- res[[6]]
  
  GA <-  ga(type = "real-valued",
            suggestions = suggestions,
            fitness = optFunction,
            lower = minOpt,
            upper = maxOpt,
            popSize = 10, maxiter = 3,
            seed = 2020,
            allPara = allPara, parameter_tibble = parameter_tibble, dfUnNested = dfUnNested, input = input,
            keepBest = FALSE
  )
  
  print(GA@solution)
  #browser()
  # using always the first line of the solution output, TODO: understand why at times there are several 
  # output lines
  denormPara <- denormalizePara(GA@solution[1,], parameter_tibble, para)
  cat("denormPara: ", unlist(denormPara %>% names), "\n") 
  cat("denormPara: ", unlist(denormPara), "\n")  

  #  ############## non normalized optimization
  #  suggestions <- c( 0, 0, -20)
  #  GA <- ga(type = "real-valued", 
  #           fitness =  function(x) calcPredictionsForGaOptimization(x[1], x[2], x[3], dfUnNested, input),
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
 
  OptResult <- tibble("reduzierung_rt1" = denormPara[["reduzierung_rt1"]], "reduzierung_rt2" = denormPara[["reduzierung_rt2"]],
                                  "reduzierung_rt3" = denormPara[["reduzierung_rt3"]], "GaFitnessValue" = GA@fitnessValue, "GaPara" = list(summary(GA)))
  return(list("OptResult" = OptResult))
  
}

createOptParmeters <- function(parameter_tibble){
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
calcPredictionsForGaOptimization = function(optPara, allPara, parameter_tibble, dfUnNested,input) {   
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
  
  inputForOptimization$dateInput[2] = dfUnNested$MeldeDate %>% max() # set endDate to date of last MeldeDate
    dfRechenKern <-   Rechenkern(dfUnNested, inputForOptimization)

  dfRechenKern <- dfRechenKern %>% filter(Tag  %in% dfUnNested$MeldeDate)
  dfUnNested <- dfUnNested %>% filter(MeldeDate  %in% dfRechenKern$Tag)
  # res <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,dfUnNested$SumAnzahl)
  # browser()
  #    res <- (
  #      (sum(
  #      (log10(dfUnNested$SumAnzahl)   - log10(dfRechenKern$ErfassteInfizierteBerechnet))^2)
  #      )/(nrow(dfRechenKern)-1)
  #      )^0.5
  #  res <- sqrt(mean((log10(dfUnNested$SumAnzahl)-log10(dfRechenKern$ErfassteInfizierteBerechnet))^2))
  res <- MAPE(log10(dfUnNested$SumAnzahl),log10(dfRechenKern$ErfassteInfizierteBerechnet))
  # cat("res is :", res , "redu1 = ", reduzierung_rt1, "\n")
  # res <- sqrt(mean((log10(dfRechenKern$ErfassteInfizierteBerechnet)-log10(dfUnNested$SumAnzahl))^2))
  return(-res)
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


###############  error function for krankenhausdaten optimization ###################

calcOptimizationKrankenhausDaten = function(optPara, allPara, parameter_tibble, dfUnNested,input) {   
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
  
  inputForOptimization$dateInput[2] = dfUnNested$MeldeDate %>% max() # set endDate to date of last MeldeDate
  dfRechenKern <-   Rechenkern(dfUnNested, inputForOptimization)
  
  dfRechenKern <- dfRechenKern %>% filter(Tag  %in% dfUnNested$MeldeDate)
  dfUnNested <- dfUnNested %>% filter(MeldeDate  %in% dfRechenKern$Tag)
  # res <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,dfUnNested$SumAnzahl)
  # browser()
  #    res <- (
  #      (sum(
  #      (log10(dfUnNested$SumAnzahl)   - log10(dfRechenKern$ErfassteInfizierteBerechnet))^2)
  #      )/(nrow(dfRechenKern)-1)
  #      )^0.5
  #  res <- sqrt(mean((log10(dfUnNested$SumAnzahl)-log10(dfRechenKern$ErfassteInfizierteBerechnet))^2))
  res <- MAPE(log10(dfUnNested$SumAnzahl),log10(dfRechenKern$ErfassteInfizierteBerechnet))
  # cat("res is :", res , "redu1 = ", reduzierung_rt1, "\n")
  # res <- sqrt(mean((log10(dfRechenKern$ErfassteInfizierteBerechnet)-log10(dfUnNested$SumAnzahl))^2))
  return(-res)
} 
############# ValidationsScriptBundeslaender ##############

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



