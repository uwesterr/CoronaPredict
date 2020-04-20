library(jsonlite)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(readxl)
library(writexl)
library(scales)
library(tidyr)
library(broom)
library(modelr)
library(DT)
library(shinyalert)


createDfBundLandKreisOld <- function() {
 
  historyData <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")


  historyDf <- historyData[["features"]][["properties"]]
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  
  
  ## read population file from thonmas
  bundesLandPopulation <- read_excel("../data/bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("../data/bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  # browser()
  BundFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% 
    mutate(Bundesland = "NA", Landkreis = NA, SumAnzahl = cumsum(AnzahlFall), sumTote = cumsum(AnzahlTodesfall), whichRegion = "Deutschland",
           Index = as.numeric(MeldeDate- min(MeldeDate))) %>% mutate(Einwohner = bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland)) %>% unlist) 
  historyDfBund$FirstMelde <- BundFirstMeldung %>% unlist %>% as.Date()
  
  
  
  BundesLandFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Bundesland) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBundesLand  <- historyDf %>% dplyr::ungroup() %>% group_by(Bundesland, MeldeDate)  %>% summarise_if(is.numeric, sum, na.rm = TRUE)  %>% 
    mutate(Landkreis = NA, SumAnzahl = cumsum(AnzahlFall), sumTote = cumsum(AnzahlTodesfall),whichRegion = Bundesland,
           SumAnzahl = ifelse(SumAnzahl <1, 0.1,SumAnzahl),
           Index = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(BundesLandFirstMeldung) %>% left_join(bundesLandPopulation)  %>% 
        rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) 
  
  
  
  
  LandkreisFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Landkreis) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfLandkreis <- historyDf  %>% dplyr::group_by(Bundesland,Landkreis, MeldeDate)%>% dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
    mutate(SumAnzahl = cumsum(AnzahlFall), sumTote = cumsum(AnzahlTodesfall), whichRegion = Landkreis,
           SumAnzahl = ifelse(SumAnzahl <1, 0.1,SumAnzahl),
           Index = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(LandkreisFirstMeldung) %>%  left_join(landKreisPopulation)  %>% 
     rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) 
  ############# add krankenhaus daten
  Land_BW_Data <-  readRescueTrackerData()
  # load("../data/LK_KH_Data.RData")
  tmp <- bind_rows(historyDfBund, historyDfBundesLand, historyDfLandkreis) %>%
    left_join(Land_BW_Data, by = c("whichRegion" = "Landkreis", "MeldeDate" = "Date"))
  
  
  RkiData <- tmp %>% group_by(whichRegion) %>% nest() %>% 
    add_column("R0Start"= -1e7, "R0Opt"= -1e7, "n0Start" = -1e7, "n0Opt" = -1e7, "RegStartDate" = as.Date('1966-05-10'), 
               "groupedBy" ="", "predictedValues" = "NULL", "NotEnoughDataFlag" = 0) %>% mutate( 
               groupedBy = ifelse(whichRegion == "Deutschland", "Deutschland", 
                                  ifelse(whichRegion %in% historyDf$Bundesland, "Bundesland",
                                         ifelse(whichRegion %in% historyDf$Landkreis, "Landkreis",""))))
  
  
  load("../data/R0n0OptimizedStep0.0120200418.RData") 
  #  loads 
  # dataframe RkiDataWithRoNoOpimized 
  # from  file R0n0OptimizedStep0.0120200418.RData created by 
  #  running createRkiRegOptFrame.R on 2020.04.18
  # join with up to date data from RKI and throwing old data away
  RkiDataWithRoNoOpimizedUpToDate<- left_join(RkiData %>% 
                                                select(-c(R0Start, R0Opt, n0Start, n0Opt,  RegStartDate, groupedBy, 
                                                          predictedValues, NotEnoughDataFlag)),
                                              RkiDataWithRoNoOpimized %>% select(-c(data)))
  


  save(RkiDataWithRoNoOpimizedUpToDate, file = "../data/createDfBundLandKreisOutput.RData")
  return(list(RkiDataWithRoNoOpimizedUpToDate))
}




