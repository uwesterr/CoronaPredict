# Files to supply functions to other programs

library(jsonlite)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(readxl)
library(scales)
library(tidyr)
library(broom)
library(modelr)
library(DT)


createDfBundLandKreis <- function() {
  
  historyData <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  historyDf <- historyData[["features"]][["properties"]]
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  
  historyDf %>% filter(str_detect(Landkreis,"oburg")) %>% select(Landkreis) %>% unique()
  
  ## read population file from thonmas
  bundesLandPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  
  
  historyDf <- left_join(historyDf,bundesLandPopulation)
  historyDf <- left_join(historyDf,landKreisPopulation)
  
  
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% mutate(sumAnzahlFallBund = cumsum(AnzahlFall),
                                                                                                                      BundIndex = as.numeric(MeldeDate- min(MeldeDate))) 
  historyDfBundesLand  <- historyDf %>% dplyr::ungroup() %>% group_by(Bundesland, MeldeDate)  %>% summarise_if(is.numeric, sum, na.rm = TRUE)  %>% mutate(sumAnzahlFallBundesland = cumsum(AnzahlFall),
                                                                                                                                                          LandIndex = as.numeric(MeldeDate- min(MeldeDate)))  
  historyDfLandkreis <- historyDf %>% dplyr::group_by(Bundesland,Landkreis, MeldeDate)%>% dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)  %>% mutate(sumAnzahlFallLandkreis = cumsum(AnzahlFall),
                                                                                                                                                         KreisIndex = as.numeric(MeldeDate- min(MeldeDate))) 

  return(list(historyDfBund, historyDfBundesLand, historyDfLandkreis))
}



testFunc <- function(){
  
  a <- 5
  }