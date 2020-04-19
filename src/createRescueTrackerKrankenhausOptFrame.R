## script to optimize reduzierungs values


setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))

############### load data of other optimizations
load("../data/landkreiseBadenWuerttemberg.RData")
# load("../data/RkiReduzierungOptFrameDeutschland.RData")
load("../data/RkiReduzierungOptServer20200419.RData")

#  loads dataframe RkiDataWithRoNoAndReduzierungOpimized from  file RkiReduzierungOpt.RData created by 
# cronjob running createRkiReduzierungOptFrame.R every day at 0.01am 

RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized <- RkiDataWithRoNoAndReduzierungOpimized

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

optFunction <- calcOptimizationKrankenhausDaten # function to be used to calculate metric for optimizer
resultColumnName <- "KrankenhausOptResult" # column where result of optimizer is stored
gaPara <- list("popSize" = 5, "maxiter" = 2, run = 5)
RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized %>% 
  as_tibble()  %>% add_column("KrankenhausOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")

###################################################################################################
index <- 0

################## only Baden-Württemberg   because we only have krankenhausdata for Baden-Württemberg #########
RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized 
 # filter(whichRegion %in% c(landkreiseBadenWuerttemberg, "Baden-Württemberg")) %>% head(2)

#####################################################
tictoc::tic()

for (regionSelected in RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized$whichRegion) {
  index <- index +1
  print(index)
  print(regionSelected)#
  ###### set input parameter according to RkiDataWithRoNoAndReduzierungOpimized ######
  
  tmp  <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized %>% filter(whichRegion == regionSelected)
  reduzierungsOptResultDf <- tmp %>% select(reduzierungsOptResult) %>% unnest(reduzierungsOptResult)
  inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
  for (inputVarName in reduzierungsOptResultDf$OptParaNames[[1]]) {
    inputForOptimization[[inputVarName]]<- reduzierungsOptResultDf[[inputVarName]]
  }
#  browser()
  #############################
  tmp <-   createRkiRegOptFrame(RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized, regionSelected, parameter_tibble, 
                                optFunction, resultColumnName, gaPara, inputForOptimization)
  regionSelectedDf <- tmp[["dfNested"]] %>% filter(whichRegion == regionSelected)
  RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized[match(regionSelectedDf$whichRegion, RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized$whichRegion), ] <- regionSelectedDf
}
toc()
save(RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized, file  = "../data/RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized.RData")
browser()

#browser()
############### create compare plots  #######################


# load("../data/RkiReduzierungOptFrameServer0416.RData")
plotCreate <- 1
#load("../data/RkiReduzierungOptFrameServerMPEItr500417.RData")


if(plotCreate){
  # plot the worst n regions depending on optimizer fitness
  # a <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized %>% unnest(reduzierungsOptResult) %>% arrange(GaFitnessValue)
  RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized %>% 
    as_tibble()  %>% add_column("dfRechenKern" = 0)
  
  createRkiRegOptPlots<- function(RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized, regionSelected, input ){
    
    RkiDataWithR0N0 <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized %>% filter(whichRegion == regionSelected) %>% unnest(data)
    df_nom <-  Rechenkern(RkiDataWithR0N0, input) %>% as_tibble()
    df_nom$whichRegion = regionSelected
    df_nomNetest <- df_nom %>% group_by(whichRegion) %>% nest()
    
    indexEntitiy <- which(RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized$whichRegion == regionSelected)
    df_nom$whichRegion
    RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized$dfRechenKern[indexEntitiy] <- df_nom 
    return( RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized)
  }
  tictoc::tic()
  dfRechenkernAndRki <- tibble()
  for (regionSelected in RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized$whichRegion %>% head(10)) {
    RkiDataWithR0N0 <- RkiDataWithRoNoAndReduzierungAndKrankenhausOpimized %>% filter(whichRegion == regionSelected) %>% unnest(data) %>% unnest(reduzierungsOptResult)
    inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
    inputForOptimization$reduzierung_rt1 <- RkiDataWithR0N0$reduzierung_rt1 %>% unique()
    inputForOptimization$reduzierung_rt2 <- RkiDataWithR0N0$reduzierung_rt2 %>% unique()
    inputForOptimization$reduzierung_rt3 <- RkiDataWithR0N0$reduzierung_rt3 %>% unique()
    
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
  tmp %>%  group_by(whichRegion)  %>% filter(Tag >= redDate1 & !is_na(SumAnzahl)) %>% ggplot(aes(Tag, SumAnzahl)) + geom_point() + 
    geom_line(aes(Tag, ErfassteInfizierteBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  scale_y_log10(label = label_number_si()) + 
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 300, label = "Reduzierungsmaßnahme 1", angle=90) 
  
}

