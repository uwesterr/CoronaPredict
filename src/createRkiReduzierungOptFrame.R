## script to calculate reduzierungs values

createRkiRegOptFrame <- function(RkiDataWithSumsNested, regionSelected, input ){
  
  RkiDataWithR0N0 <- RkiDataWithSumsNested %>%  filter(whichRegion == regionSelected) %>% unnest(data)
  res <- optimizerGeneticAlgorithmRedReduction(RkiDataWithR0N0, input)
  indexEntitiy <- which(RkiDataWithSumsNested$whichRegion == regionSelected)
  RkiDataWithSumsNested$reduzierungsOptResult[indexEntitiy] <-list(res$reduzierungsOptResult )
  
  return(list( "RkiDataWithSumsNested" = RkiDataWithSumsNested))
}


setwd("~/CloudProjectsUnderWork/ProjectsUnderWork/PredCo/CoronaPredict/src")
source(file = "Rechenkern.R")
# source(file = "createLandkreisR0_no_erfasstDf.R")
# source(file = "createDfBundLandKreis.R")
# source(file = "optimizerLoopingR0N0.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))
 load("../data/createDfBundLandKreisOutput.RData")
 load("../data/landkreiseBadenWuerttemberg.RData")
 RkiDataWithSumsNested$inputOrig <- list(input)
 RkiDataWithSumsNested <- RkiDataWithSumsNested %>% 
   as_tibble() %>% 
   select(!contains("redu")) %>% add_column("reduzierungsOptResult" = list("a")) # %>% filter(whichRegion == "Brandenburg")
 index <- 0
 
 ################## only baden-württemberg  #########
 RkiDataWithSumsNestedBW <- RkiDataWithSumsNested %>% filter(whichRegion %in% c(landkreiseBadenWuerttemberg, "Baden-Württemberg")) 
 
 #####################################################
 tictoc::tic()
 for (regionSelected in RkiDataWithSumsNestedBW$whichRegion) {
   index <- index +1
   print(index)
   print(regionSelected)
   tmp <-   createRkiRegOptFrame(RkiDataWithSumsNested, regionSelected, input )
   regionSelectedDf <- tmp[["RkiDataWithSumsNested"]] %>% filter(whichRegion == regionSelected)
   RkiDataWithSumsNested[match(regionSelectedDf$whichRegion, RkiDataWithSumsNested$whichRegion), ] <- regionSelectedDf
   
 }

# RkiDataWithSumsNested <- RkiDataWithSumsNested %>% unnest(reduzierungsOptResult)
 toc()
#browser()
save(RkiDataWithSumsNested, file  = "../data/RkiReduzierungOptFrameBW200417.RData")

############### create compare plots  #######################

 
 load("../data/RkiReduzierungOptFrameServer0416.RData")
plotCreate <- 1
load("../data/RkiReduzierungOptFrameServerMPEItr500417.RData")


if(plotCreate){
  RkiDataWithSumsNested <- RkiDataWithSumsNested %>%    as_tibble()  %>% add_column("dfRechenKern" = 0)
  
  createRkiRegOptPlots<- function(RkiDataWithSumsNested, regionSelected, input ){
    
    RkiDataWithR0N0 <- RkiDataWithSumsNested %>% filter(whichRegion == regionSelected) %>% unnest(data)
    #browser()
    
    df_nom <-  Rechenkern(RkiDataWithR0N0, input) %>% as_tibble()
    df_nom$whichRegion = regionSelected
    df_nomNetest <- df_nom %>% group_by(whichRegion) %>% nest()
    
    indexEntitiy <- which(RkiDataWithSumsNested$whichRegion == regionSelected)
    df_nom$whichRegion
    browser()
    RkiDataWithSumsNested$dfRechenKern[indexEntitiy] <- df_nom 
    
    
    return( RkiDataWithSumsNested)
  }
  tictoc::tic()
  dfRechenkernAndRki <- tibble()
  for (regionSelected in RkiDataWithSumsNested$whichRegion %>% head(10)) {
    RkiDataWithR0N0 <- RkiDataWithSumsNested %>% filter(whichRegion == regionSelected) %>% unnest(data)
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
  maxMeldeDate <- max(dfRechenkernAndRkiUnnest$MeldeDate)
  a <- dfRechenkernAndRkiUnnest %>%  filter(!is.na(SumAnzahl))
  a %>%  group_by(whichRegion)  %>% filter(Tag >= redDate1 & !is_na(SumAnzahl)) %>% ggplot(aes(Tag, SumAnzahl)) + geom_point() + 
    geom_line(aes(Tag, ErfassteInfizierteBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  scale_y_log10(label = label_number_si()) + 
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 300, label = "Reduzierungsmaßnahme 1", angle=90) 
  
}

