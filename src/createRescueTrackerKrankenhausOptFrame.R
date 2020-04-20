## script to optimize krankenhaus values


setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))

############### load data of other optimizations
load("../data/landkreiseBadenWuerttemberg.RData")
# load("../data/RkiReduzierungOptFrameDeutschland.RData")
load("../data/RkiReduzierungOptFrameDeutschland.RData")

#  loads 
# dataframe RkiDataWithRoNoAndReduzierungOpimized 
# from  file RkiReduzierungOpt.RData created by 
# cronjob running createRkiReduzierungOptFrame.R every day at 0.01am 


################ optimize Sationaer ###############################

############ define optimization parameters, optFunction and resultColumnName #################
parameter_tibble <- tribble(
  ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
  "kh_normal",         4.5         ,  1,        20,        "TRUE", # Anteil an aktuellen Infizierten [%]
  "kh_intensiv",       25          ,  10,       40,        "TRUE", # Anteil Intensivstation [%]
  "t_kh",              10          ,  5,        30,        "TRUE", # Dauer in Krankenhaus
  "t_intensiv",        10          ,  5,        30,        "TRUE", # Dauer Intensivstation
  "dt_inf_kh",          8          ,  3,        14,        "TRUE", # Versatz nach Infektion
  "dt_kh_int",          1          ,  1,        15,        "TRUE", # Versatz Krankenhaus - Intensivstations
  )

optFunction <- calcOptimizationStationaerDaten # function to be used to calculate metric for optimizer
resultColumnName <- "StationaerOptResult" # column where result of optimizer is stored
gaPara <- list("popSize" = 4, "maxiter" = 2, run = 5, "ReportedVar" = "Stationaer", "CalculatedVar" = "StationaerBerechnet")

RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% 
  as_tibble()  %>% add_column("StationaerOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")

###################################################################################################
index <- 0

################## only Baden-Württemberg   because we only have krankenhausdata for Baden-Württemberg #########
RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% 
  filter(whichRegion %in% c("Baden-Württemberg" ,landkreiseBadenWuerttemberg))

#####################################################
tictoc::tic()


  RkiDataStationaerOpti <- appendOpt(RkiDataWithRoNoAndReduzierungOpimized, parameter_tibble, optFunction, resultColumnName, gaPara) 
  browser()

toc()
save(RkiDataStationaerOpti, file  = "../data/RkiDataStationaerOpti.RData")
browser()

#browser()
############### create compare plots  #######################


# load("../data/RkiReduzierungOptFrameServer0416.RData")
plotCreate <- 1
#load("../data/RkiReduzierungOptFrameServerMPEItr500417.RData")


if(plotCreate){
  # plot the worst n regions depending on optimizer fitness
  # a <- RkiDataStationaerOpti %>% unnest(reduzierungsOptResult) %>% arrange(GaFitnessValue)
  # load("../data/RkiDataStationaerOpti.RData")
  RkiDataStationaerOpti <- RkiDataStationaerOpti %>% 
    as_tibble()  %>% add_column("dfRechenKern" = 0)
  

  tictoc::tic()
  dfRechenkernAndRki <- tibble()
  for (regionSelected in RkiDataStationaerOpti$whichRegion %>% head(10)) {
    ##### create input vector based on optimisaton results
    inputForOptimization <- input
    tmp  <- RkiDataStationaerOpti %>% filter(whichRegion == regionSelected)
    reduzierungsOptResultDf <- tmp %>% select(reduzierungsOptResult) %>% unnest(reduzierungsOptResult)
    for (paraName in reduzierungsOptResultDf$OptParaNames[[1]]) {
      inputForOptimization[[paraName]] <-  reduzierungsOptResultDf[[paraName]]
    }
   # browser()
    reduzierungsOptResultDf <- tmp %>% select(KrankenhausOptResult) %>% unnest(KrankenhausOptResult)
    for (paraName in reduzierungsOptResultDf$OptParaNames[[1]]) {
      inputForOptimization[[paraName]] <-  reduzierungsOptResultDf[[paraName]]
    }
    #######################
    RkiDataWithR0N0 <- tmp %>% unnest(data)
    
    df_nom <-  Rechenkern(RkiDataWithR0N0, inputForOptimization)
    tmp<-  Rechenkern(RkiDataWithR0N0, inputForOptimization) %>% mutate(whichRegion = regionSelected) %>% 
      group_by(whichRegion) %>% nest()
    dfRechenkernAndRki <- bind_rows(dfRechenkernAndRki,tmp)
    
    
  }
  
  dfRechenkernAndRkiUnnest <-  dfRechenkernAndRki %>% unnest(data) 
  redDate1 <- input$reduzierung_datum1
  redDate2 <- input$reduzierung_datum2
  redDate3 <- input$reduzierung_datum3
  #maxMeldeDate <- max(dfRechenkernAndRkiUnnest$MeldeDate)
  tmp <- dfRechenkernAndRkiUnnest %>%  filter(!is.na(SumAnzahl))
  tmp %>%  group_by(whichRegion)  %>% filter(Tag >= redDate1 & !is_na(SumAnzahl)) %>% ggplot(aes(Tag, AnzahlFall)) + geom_point() + 
    geom_line(aes(Tag, NeuInfizierteBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 5, label = "Reduzierungsmaßnahme 1", angle=90) 
  
  
  tmp %>%  group_by(whichRegion)  %>% filter(Tag >= redDate1 & !is_na(ICU_Beatmet)) %>% ggplot(aes(Tag, ICU_Beatmet)) + geom_point() + 
    geom_line(aes(Tag, IntensivBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 5, label = "Reduzierungsmaßnahme 1", angle=90) 
  
}



