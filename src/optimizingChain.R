# Concatenating optimizations of
# reduzierung
# Stationaer
# ICU_Beatmet

setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
load("../data/landkreiseBadenWuerttemberg.RData")

input <- isolate(reactiveValuesToList(inputExample))
#load("../data/createDfBundLandKreisOutput.RData") 
######### needs work to implement krankenhaus data read in   #############
RkiDataWithRoNoOpimizedUpToDate <- createDfBundLandKreis()

save(RkiDataWithRoNoOpimizedUpToDate, file = "RkiDataWithRoNoOpimizedUpToDate.RData")
#  loads 
# dataframe RkiDataWithRoNoOpimizedUpToDate 
# from  file createDfBundLandKreisOutput.RData created by 
# cronjob running createDfBundLandKreis.R every day at 0.01am 

plotCreate <- 1


############ Optimize Reduzierung ##############################

############ define optimization parameters, optFunction and resultColumnName
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  "reduzierung_rt1", 0         ,  0,        60,        "TRUE",
  "reduzierung_rt2", 0         ,  0,        60,        "TRUE",
  "reduzierung_rt3", -20       ,  -40,      30,        "TRUE")
# function to be used to calculate metric for optimizer
optFunction <- calcPredictionsForGaOptimization
resultColumnName <- "reduzierungsOptResult"
gaPara <- list("popSize" = 15, "maxiter" = 40, run = 5)
RkiDataWithRoNoOpimizedUpToDate <- RkiDataWithRoNoOpimizedUpToDate %>%  as_tibble() %>%   add_column("reduzierungsOptResult" = list("a"),
                                           "optimizedInput" = list("OptimizedInputValues" = 0)) # %>% filter(whichRegion == "Brandenburg")

################## for tests #########
 RkiDataWithRoNoOpimizedUpToDate <- RkiDataWithRoNoOpimizedUpToDate %>%
   filter(whichRegion %in% c("Deutschland", "Baden-Württemberg" , landkreiseBadenWuerttemberg)) %>% head(2)

############################## conduct optimization #######################

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
gaPara <- list("popSize" = 14, "maxiter" = 20, run = 5, "ReportedVar" = "Stationaer", "CalculatedVar" = "StationaerBerechnet")
load("../data/RkiReduzierungOptFrameDeutschland.RData")

RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% 
  as_tibble()  %>% add_column("StationaerOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")


################## only Baden-Württemberg   because we only have krankenhausdata for Baden-Württemberg #########
RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% 
  filter(whichRegion %in% c("Baden-Württemberg" ,landkreiseBadenWuerttemberg)) %>% head(1)

#####################################################


############################## conduct optimization #######################


RkiDataStationaerOpti <- appendOpt(RkiDataWithRoNoAndReduzierungOpimized, parameter_tibble, optFunction, resultColumnName, gaPara) 
browser()

toc()
save(RkiDataStationaerOpti, file  = "../data/RkiDataStationaerOpti.RData")
browser()

plots <- createPlotStationaerOpt(input)
plots




################ optimize ICU_beatmet ###############################

############ define optimization parameters, optFunction and resultColumnName #################
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  #"kh_normal",         0.5         ,  1,        20,        "TRUE", # Anteil an aktuellen Infizierten [%]
   "kh_intensiv",       25          ,  10,       40,        "TRUE", # Anteil Intensivstation [%]
  #"t_kh",              10          ,  5,        30,        "TRUE", # Dauer in Krankenhaus
   "t_intensiv",        10          ,  5,        30,        "TRUE", # Dauer Intensivstation
  #"dt_inf_kh",          8          ,  3,        14,        "TRUE", # Versatz nach Infektion
   "dt_kh_int",          1          ,  1,        15,        "TRUE", # Versatz Krankenhaus - Intensivstations
)

optFunction <- calcOptimizationStationaerDaten # function to be used to calculate metric for optimizer
resultColumnName <- "ICU_beatmetOptResult" # column where result of optimizer is stored
gaPara <- list("popSize" = 14, "maxiter" = 20, run = 5, "ReportedVar" = "ICU_Beatmet", "CalculatedVar" = "IntensivBerechnet")
load("../data/RkiDataStationaerOpti.RData")

RkiDataStationaerOpti <- RkiDataStationaerOpti %>% 
  as_tibble()  %>% add_column("ICU_BeatmetOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")


################## only Baden-Württemberg   because we only have krankenhausdata for Baden-Württemberg #########
RkiDataStationaerOpti <- RkiDataStationaerOpti %>% 
  filter(whichRegion %in% c("Baden-Württemberg" ,landkreiseBadenWuerttemberg))

#####################################################


############################## conduct optimization #######################


RkiDataICU_BeatmetOpti <- appendOpt(RkiDataStationaerOpti, parameter_tibble, optFunction, resultColumnName, gaPara) 
browser()


save(RkiDataICU_BeatmetOpti, file  = "../data/RkiDataICU_BeatmetOpti.RData")
browser()

plots <- createPlotStationaerOpt(input)
plots

