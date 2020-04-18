## script to optimize reduzierungs values


setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))
load("../data/createDfBundLandKreisOutput.RData") 
#  loads dataframe RkiData from  file createDfBundLandKreisOutput.RData created by 
# cronjob running createDfBundLandKreis.R every day at 0.01am 
load("../data/R0n0OptimizedStep0.0120200418.RData") 
#  loads dataframe RkiDataWithRoNoOpimized from  file R0n0OptimizedStep0.0120200418.RData created by 
#  running createRkiRegOptFrame.R on 2020.04.18
# join with up to date data from RKI and throwing old data away
RkiDataWithRoNoOpimizedUpToDate <- left_join(RkiData,RkiDataWithRoNoOpimized %>% select(-data))
RkiDataWithRoNoOpimizedUpToDate<- left_join(RkiData %>% 
                                    select(-c(R0Start, R0Opt, n0Start, n0Opt,  RegStartDate, groupedBy, predictedValues)),
                                  RkiDataWithRoNoOpimized %>% select(-data))
 
 load("../data/landkreiseBadenWuerttemberg.RData")
 RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoOpimizedUpToDate
 RkiDataWithRoNoAndReduzierungOpimized$inputOrig <- list(input)
 RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% 
   as_tibble() %>% 
   select(!contains("redu")) %>% add_column("reduzierungsOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")
 ############ define optimization parameters
 parameter_tibble <- tribble(
   ~var_name,         ~var_value, ~var_min,  ~var_max,  ~var_selected,
   "reduzierung_rt1", 0         ,  0,        60,        "TRUE",
   "reduzierung_rt2", 0         ,  0,        60,        "TRUE",
   "reduzierung_rt3", -20       ,  -40,      30,        "TRUE")
 optFunction <- calcPredictionsForGaOptimization
 
 index <- 0
 
 ################## only baden-württemberg  #########
 RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>%
   filter(whichRegion %in% c(landkreiseBadenWuerttemberg, "Baden-Württemberg")) %>% head(2)
 
 #####################################################
 tictoc::tic()
 source(file = "helperForCovid19.R")
 
 for (regionSelected in RkiDataWithRoNoAndReduzierungOpimized$whichRegion) {
   index <- index +1
   print(index)
 
   print(regionSelected)#
   tmp <-   createRkiRegOptFrame(RkiDataWithRoNoAndReduzierungOpimized, regionSelected, parameter_tibble, optFunction, input)

   regionSelectedDf <- tmp[["dfNested"]] %>% filter(whichRegion == regionSelected)
  # browser()
   RkiDataWithRoNoAndReduzierungOpimized[match(regionSelectedDf$whichRegion, RkiDataWithRoNoAndReduzierungOpimized$whichRegion), ] <- regionSelectedDf
   
 }
browser()
# RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>% unnest(reduzierungsOptResult)
 toc()
#browser()
save(RkiDataWithRoNoAndReduzierungOpimized, file  = "../data/RkiReduzierungOptFrameBW200417.RData")
#browser()
############### create compare plots  #######################

 
# load("../data/RkiReduzierungOptFrameServer0416.RData")
plotCreate <- 1
#load("../data/RkiReduzierungOptFrameServerMPEItr500417.RData")


if(plotCreate){
  # plot the worst n regions depending on optimizer fitness
 # a <- RkiDataWithRoNoAndReduzierungOpimized %>% unnest(reduzierungsOptResult) %>% arrange(GaFitnessValue)
  RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>%    as_tibble()  %>% add_column("dfRechenKern" = 0)
  
  createRkiRegOptPlots<- function(RkiDataWithRoNoAndReduzierungOpimized, regionSelected, input ){
    
    RkiDataWithR0N0 <- RkiDataWithRoNoAndReduzierungOpimized %>% filter(whichRegion == regionSelected) %>% unnest(data)
    df_nom <-  Rechenkern(RkiDataWithR0N0, input) %>% as_tibble()
    df_nom$whichRegion = regionSelected
    df_nomNetest <- df_nom %>% group_by(whichRegion) %>% nest()
    
    indexEntitiy <- which(RkiDataWithRoNoAndReduzierungOpimized$whichRegion == regionSelected)
    df_nom$whichRegion
    RkiDataWithRoNoAndReduzierungOpimized$dfRechenKern[indexEntitiy] <- df_nom 
    return( RkiDataWithRoNoAndReduzierungOpimized)
  }
  tictoc::tic()
  dfRechenkernAndRki <- tibble()
  for (regionSelected in RkiDataWithRoNoAndReduzierungOpimized$whichRegion %>% head(10)) {
    RkiDataWithR0N0 <- RkiDataWithRoNoAndReduzierungOpimized %>% filter(whichRegion == regionSelected) %>% unnest(data) %>% unnest(reduzierungsOptResult)
    inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
    inputForOptimization$reduzierung_rt1 <- RkiDataWithR0N0$reduzierung_rt1 %>% unique()
    inputForOptimization$reduzierung_rt2 <- RkiDataWithR0N0$reduzierung_rt2 %>% unique()
    inputForOptimization$reduzierung_rt3 <- RkiDataWithR0N0$reduzierung_rt3 %>% unique()
   
    tmp<-  Rechenkern(RkiDataWithR0N0, inputForOptimization) %>% mutate(whichRegion = regionSelected) %>% 
      group_by(whichRegion) %>% nest()
    dfRechenkernAndRki <- bind_rows(dfRechenkernAndRki,tmp)

    
  }
  browser()
  
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

