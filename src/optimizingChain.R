# Concatenating optimizations of
# reduzierung
# Stationaer
# ICU_Beatmet
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
#setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
load("../data/landkreiseBadenWuerttemberg.RData")

input <- isolate(reactiveValuesToList(inputExample20200422))
#load("../data/createDfBundLandKreisOutput.RData") 
######### needs work to implement krankenhaus data read in   #############
RkiDataWithRoNoOpimizedUpToDate <- createDfBundLandKreis()

save(RkiDataWithRoNoOpimizedUpToDate, file = "../data/RkiDataWithRoNoOpimizedUpToDate.RData")
#  loads 
# dataframe RkiDataWithRoNoOpimizedUpToDate 
# from  file createDfBundLandKreisOutput.RData created by 
# cronjob running createDfBundLandKreis.R every day at 0.01am 

plotCreate <- 0


############ Optimize Reduzierung ##############################

############ define optimization parameters, optFunction and resultColumnName
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  "reduzierung_rt1", 0         ,  0,        60,        "TRUE",
  "reduzierung_rt2", 0         ,  0,        60,        "TRUE",
  "reduzierung_rt3", -20       ,  -40,      30,        "TRUE",
  "reduzierung_rt4", -20       ,  -40,      30,        "TRUE")
# function to be used to calculate metric for optimizer
optFunction <- calcPredictionsForGaOptimization
resultColumnName <- "reduzierungsOptResult"
gaPara <- list("popSize" = 25, "maxiter" = 40, run = 8)
if(!"reduzierungsOptResult" %in% colnames(RkiDataWithRoNoOpimizedUpToDate)){
RkiDataWithRoNoOpimizedUpToDate <- RkiDataWithRoNoOpimizedUpToDate %>%  as_tibble() %>%  add_column("reduzierungsOptResult" = list("a"),
                                           "optimizedInput" = list("OptimizedInputValues" = 0)) # %>% filter(whichRegion == "Brandenburg")
}

################## for tests #########
# RkiDataWithRoNoOpimizedUpToDate <- RkiDataWithRoNoOpimizedUpToDate %>%
#   filter(whichRegion %in% c("Deutschland", "Baden-Württemberg" , landkreiseBadenWuerttemberg)) %>% 
#   head(2)
############################## conduct optimization #######################
source(file = "helperForCovid19.R")
source(file = "Rechenkern.R")

RkiDataWithRoNoAndReduzierungOpimized <- appendOpt(RkiDataWithRoNoOpimizedUpToDate, parameter_tibble, optFunction, resultColumnName, gaPara) 

 save(RkiDataWithRoNoAndReduzierungOpimized, file  = "../data/RkiReduzierungOptFrameDeutschland.RData")

if(plotCreate){
  plotReduOpt <- createPlotReduOpt(RkiDataWithRoNoAndReduzierungOpimized, input)
  plotReduOpt
  
}

################ optimize Sationaer ###############################

############ define optimization parameters, optFunction and resultColumnName #################
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  "kh_normal",         0.5         ,  1,        20,        "TRUE", # Anteil an aktuellen Infizierten [%]
 # "kh_intensiv",       25          ,  10,       40,        "TRUE", # Anteil Intensivstation [%]
  "t_kh",              10          ,  5,        30,        "TRUE", # Dauer in Krankenhaus
 # "t_intensiv",        10          ,  5,        30,        "TRUE", # Dauer Intensivstation
  "dt_inf_kh",          8          ,  3,        14,        "TRUE", # Versatz nach Infektion
 # "dt_kh_int",          1          ,  1,        15,        "TRUE", # Versatz Krankenhaus - Intensivstations
)

optFunction <- calcOptimizationStationaerDaten # function to be used to calculate metric for optimizer
resultColumnName <- "StationaerOptResult" # column where result of optimizer is stored
gaPara <- list("popSize" = 25, "maxiter" = 40, run = 8, paralel = 4, errorFunc = "RMS",  "ReportedVar" = "Stationaer", "CalculatedVar" = "KhBerechnet")
# load("../data/RkiReduzierungOptFrameDeutschland.RData")


if(!"StationaerOptResult" %in% colnames(RkiDataWithRoNoAndReduzierungOpimized)){
RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>%  
  as_tibble()  %>% add_column("StationaerOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")
}

################## only Baden-Württemberg   because we only have krankenhausdata for Baden-Württemberg #########
RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% 
  filter(whichRegion %in% c("Baden-Württemberg" ,landkreiseBadenWuerttemberg)) 

#####################################################


############################## conduct optimization #######################

  source(file = "helperForCovid19.R")
  optFunction <- calcOptimizationStationaerDaten # function to be used to calculate metric for optimizer
RkiDataStationaerOpti <- appendOpt(RkiDataWithRoNoAndReduzierungOpimized, parameter_tibble, optFunction, resultColumnName, gaPara) 

save(RkiDataStationaerOpti, file  = "../data/RkiDataStationaerOpti.RData")

plots <- createPlotStationaerOpt(input)
plots




################ optimize ICU_beatmet ###############################

############ define optimization parameters, optFunction and resultColumnName #################
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  #"kh_normal",         0.5         ,  1,        20,        "TRUE", # Anteil an aktuellen Infizierten [%]
   "kh_intensiv",       25          ,  10,       70,        "TRUE", # Anteil Intensivstation [%]
  #"t_kh",              10          ,  5,        30,        "TRUE", # Dauer in Krankenhaus
   "t_intensiv",        10          ,  5,        30,        "TRUE", # Dauer Intensivstation
  #"dt_inf_kh",          8          ,  3,        14,        "TRUE", # Versatz nach Infektion
   "dt_kh_int",          1          ,  1,        15,        "TRUE", # Versatz Krankenhaus - Intensivstations
)

optFunction <- calcOptimizationStationaerDaten # function to be used to calculate metric for optimizer
resultColumnName <- "ICU_beatmetOptResult" # column where result of optimizer is stored
gaPara <- list("popSize" = 25, "maxiter" = 40, run = 8,  paralel = 4, errorFunc = "RMS", "ReportedVar" = "ICU_Beatmet", "CalculatedVar" = "IntensivBerechnet")
# load("../data/RkiDataStationaerOpti.RData")


if(!"ICU_BeatmetOptResult" %in% colnames(RkiDataStationaerOpti)){
RkiDataStationaerOpti <- RkiDataStationaerOpti %>% 
  as_tibble()  %>% add_column("ICU_BeatmetOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")
}

################## only Baden-Württemberg   because we only have krankenhausdata for Baden-Württemberg #########
RkiDataStationaerOpti <- RkiDataStationaerOpti %>% 
  filter(whichRegion %in% c("Baden-Württemberg" ,landkreiseBadenWuerttemberg))

#####################################################


############################## conduct optimization #######################


RkiDataICU_BeatmetOpti <- appendOpt(RkiDataStationaerOpti, parameter_tibble, optFunction, resultColumnName, gaPara) 


 save(RkiDataICU_BeatmetOpti, file  = "../data/RkiDataICU_BeatmetOpti.RData")

plots <- createPlotICU_BeatmetOpt(input)
plots



############ write BW optimising results to all non BW entities  ############

load("../data/RkiReduzierungOptFrameDeutschland.RData")
load("../data/RkiDataICU_BeatmetOpti.RData")
source(file = "helperForCovid19.R")
load("../data/landkreiseBadenWuerttemberg.RData")

nonBwRegions <-  RkiDataWithRoNoAndReduzierungOpimized %>% filter(!whichRegion %in% c("Baden-Württemberg" ,landkreiseBadenWuerttemberg)) 
BwBeatmetOpt <- RkiDataICU_BeatmetOpti[[which(RkiDataICU_BeatmetOpti$whichRegion == "Baden-Württemberg"),"optimizedInput"]]
BwBeatmetNames <- BwBeatmetOpt[[1]] %>% names
for (region in (nonBwRegions$whichRegion %>% unlist)) {
  indexEntitiy <- which(nonBwRegions$whichRegion == region)
  ReduzierungOpt <- nonBwRegions[[indexEntitiy,"optimizedInput"]] 
  redNames <- ReduzierungOpt[[1]] %>% names # get names of already optmised parameters
  extraNames <-  BwBeatmetNames[!BwBeatmetNames %in% redNames] # get names which are not already optimised for non BW regions
  for (extraName in extraNames) {
    ReduzierungOpt[[1]][[extraName]]<- BwBeatmetOpt[[1]][[extraName]] 
  }
  nonBwRegions[[indexEntitiy,"optimizedInput"]] <- ReduzierungOpt
  nonBwRegions[[indexEntitiy,"optimizedInput"]]
}

RkiDataICU_BeatmetOptiTotal <- bind_rows(nonBwRegions,RkiDataICU_BeatmetOpti)

path <- "../data/InputFileForAppFolder/"

## move old opt file and save new one #####

newFolder <-  "../data/ArchieveInputFileForAppFolder/"
oldFile <- list.files(path, full.names = TRUE)
file.copy(oldFile, newFolder, overwrite = TRUE)

# copy the files to the new folder

do.call(file.remove, list(list.files(path, full.names = TRUE)))

save(RkiDataICU_BeatmetOptiTotal, file  = paste0(path,"RkiDataICU_BeatmetOptiTotal", Sys.time(), ".RData"))


# save(RkiDataICU_BeatmetOptiTotal, file  = "../data/RkiDataICU_BeatmetOptiTotal.RData")


############ create statistic over optimized parameter

optStat <- function(optStat){
 list( as_tibble(optStat %>% t))

}

tmp <- bind_rows(RkiDataICU_BeatmetOpti,nonBwRegions) %>% select(whichRegion,optimizedInput) %>% mutate(optWide= map(optimizedInput, optStat))
optWideDf <- tmp %>% unnest(optWide)%>% unnest(optWide)
optLongDf <- optWideDf %>% select(-optimizedInput) %>% pivot_longer(-whichRegion, names_to = "OptParameter")  %>% 
  filter(str_detect(OptParameter, c("red")))

optLongDf %>% ggplot(aes(OptParameter, value, color = OptParameter)) +geom_boxplot() + 
  coord_flip() + labs(title = "Optimised reduzierungs paramter", x = "Optimised parameter")  + theme(legend.position = "none")#+ facet_wrap(vars(OptParameter), scales = "free")



tmp <- RkiDataICU_BeatmetOpti %>% select(whichRegion, optimizedInput) %>% mutate(optWide= map(optimizedInput, optStat))
optWideDf <- tmp %>% unnest(optWide)%>% unnest(optWide)
optLongDf <- optWideDf %>% select(-optimizedInput) %>% pivot_longer(-whichRegion, names_to = "OptParameter") %>% 
    filter(str_detect(OptParameter, c("inten", "kh", "dt")))
optLongDf %>% ggplot(aes(OptParameter, value)) +geom_boxplot() + coord_flip() + facet_wrap(vars(OptParameter), scales = "free")

