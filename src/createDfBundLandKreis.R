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




createDfBundLandKreis <- function() {
  
  historyData <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  historyDf <- historyData[["features"]][["properties"]]
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  
  
  ## read population file from thonmas
  bundesLandPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  # browser()
  BundFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% 
    mutate(sumAnzahlFallBund = cumsum(AnzahlFall), sumToteBund = cumsum(AnzahlTodesfall),
           BundIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% mutate(EinwohnerBund = bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland)) %>% unlist) 
  historyDfBund$FirstMelde <- BundFirstMeldung %>% unlist %>% as.Date()
  
  
  
  BundesLandFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Bundesland) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBundesLand  <- historyDf %>% dplyr::ungroup() %>% group_by(Bundesland, MeldeDate)  %>% summarise_if(is.numeric, sum, na.rm = TRUE)  %>% 
    mutate(sumAnzahlFallBundesland = cumsum(AnzahlFall), sumToteBundesland = cumsum(AnzahlTodesfall),
           sumAnzahlFallBundesland = ifelse(sumAnzahlFallBundesland <1, 0.1,sumAnzahlFallBundesland),
           LandIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(BundesLandFirstMeldung) %>% left_join(bundesLandPopulation)
  
  
  
  
  LandkreisFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Landkreis) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfLandkreis <- historyDf  %>% dplyr::group_by(Bundesland,Landkreis, MeldeDate)%>% dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
    mutate(sumAnzahlFallLandkreis = cumsum(AnzahlFall), sumToteLandkreis = cumsum(AnzahlTodesfall),
           sumAnzahlFallLandkreis = ifelse(sumAnzahlFallLandkreis <1, 0.1,sumAnzahlFallLandkreis),
           KreisIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(LandkreisFirstMeldung) %>%  left_join(landKreisPopulation)
  
  return(list(historyDfBund, historyDfBundesLand, historyDfLandkreis))
}