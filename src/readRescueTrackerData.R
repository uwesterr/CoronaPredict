# reads data from rescuetracker api and stores data in data.frame
library(httr)
library(tidyverse)
library(lubridate)
library(readxl)
library(mefa4)
library(dplyr)

# tmp
#setwd("D:/thomas/Dropbox/Intern/LGA/R/app/kh_import/src")
setwd("c:/Users/thomas.gneiting/Dropbox/Intern/LGA/R/app/kh_import/src")

# get data from rescuetrack
rescuetrackData <- GET("https://apps.rescuetrack.com/rrb/api/v1/getCapacityDump", add_headers(Cookie = "rt-sso-sid=094eb893-5d4c-4640-9e92-5a795ede04d2"))
rescuetrackDataContent <- content(rescuetrackData)#  %>% as_tibble() %>% unnest()
foldersDf <- rescuetrackDataContent[["folders"]]  #%>% as_tibble(.name_repair = c("universal")) %>% transpose() %>% as_tibble() 

# read structure file for Bundesland, RP and ILS
BL_names  <- read_excel("../data/struktur_BL_RP_ILS_KH.xlsx", "BL",  col_names = TRUE)
RP_names  <- read_excel("../data/struktur_BL_RP_ILS_KH.xlsx", "RP",  col_names = TRUE)
ILS_names <- read_excel("../data/struktur_BL_RP_ILS_KH.xlsx", "ILS", col_names = TRUE) %>% as_tibble()
LK_ILS    <- read_excel("../data/struktur_BL_RP_ILS_KH.xlsx", "LK_ILS", col_names = TRUE)



# read out required data fields
index <- 0
krankenhausData <- tibble(id = double(),  
                          folderId = character(), 
                          folderName = character(), 
                          parentId = character(), 
                          resourceType = character(),
                          free = double(),  
                          total = double(), 
                          timestamp = ymd_hms())
krankenhausDataLoop <- tibble(id = 1,  
                              folderId = "character()", 
                              folderName = "character()", 
                              parentId = "character()", 
                              resourceType = "character()",
                              free = 1e7,  
                              total = 1e7, 
                              timestamp = ymd_hms('2020-04-17T12:12:56'))

# Assign raw data on level "Krankenhaus"
print ("----- Assign raw data on level Krankenhaus -------")
for (folderIndex in seq(1,length(foldersDf))) {
  folderIndex
  # exclude higher levels BL, RP and ILS from data import
  if ( (length(foldersDf[[folderIndex]][["resources"]])>0) 
       && !str_detect(foldersDf[[folderIndex]][["folderName"]], c("ILS ", "RP ", "Baden-W")) %>% sum()
       )  {
    print(foldersDf[[folderIndex]][["folderName"]])
    # browser()
    for (resourceIndex in seq(1,length(foldersDf[[folderIndex]][["resources"]]))) {
      for (capacitiesIndex in seq(1,length(foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]]))) {
        index <- index + 1
        krankenhausDataLoop$folderId[1]   <- foldersDf[[folderIndex]][["folderId"]]
        krankenhausDataLoop$folderName[1] <- foldersDf[[folderIndex]][["folderName"]]
        krankenhausDataLoop$parentId[1]   <- foldersDf[[folderIndex]][["parentId"]]
        krankenhausDataLoop$resourceType  <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["resourceTypeName"]]
        krankenhausDataLoop$free          <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]][[capacitiesIndex]][["free"]]
        krankenhausDataLoop$total         <- foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]][[capacitiesIndex]][["total"]]
        krankenhausDataLoop$timestamp     <- ymd_hms(substr(foldersDf[[folderIndex]][["resources"]][[resourceIndex]][["capacities"]][[capacitiesIndex]][["timestamp"]], 1, 19))
        krankenhausData <- bind_rows(krankenhausData,krankenhausDataLoop)
        index
      }
    }  
  }
}

# fill up empty day with value of previous day(s) and delete multiple entries per day
kh_names=sort(unique(krankenhausData$folderName))

for (KH_Index in seq(1,length(kh_names))) {
  KH_Index
  
  tmp_filter_werte <- c("COVID-Patienten beatmet" , "COVID-Patienten gesamt")

  for (fi_index in seq(1,length(tmp_filter_werte))) {
    act_filter_wert <- tmp_filter_werte[fi_index]
    # entry of this kh
    act_kh <- filter(krankenhausData, (krankenhausData$folderName==kh_names[[KH_Index]] &  krankenhausData$resourceType==act_filter_wert))
    act_kh <- arrange(act_kh, timestamp)
  
    # time frame
    start_date = min(date(act_kh$timestamp))
    end_date   = max(date(act_kh$timestamp))
  
    # check for doubles and missing values in this time frame
    if (end_date > start_date) {
      for (day_index in seq(start_date, end_date , by = 1)) {
        tmp_day <- filter(act_kh, as.numeric(date(act_kh$timestamp))==day_index )
        nrow_tmp_day <- nrow(tmp_day)
        if (nrow_tmp_day==0) {
          # generate a new dataset entry with content of previous day and the actual date
          print ("anlegen")
          act_timestamp <- as_datetime(day_index*86400+36000)
          krankenhausDataLoop            <- previous_day
          krankenhausDataLoop$timestamp  <- act_timestamp
          krankenhausData <- bind_rows(krankenhausData,krankenhausDataLoop)
      
        }  else if (nrow_tmp_day>1) {
          #keep only newest entry (=last in act_kh) and delete others in krankenhausData
          print (paste("Laenge krankenhausData vor Loeschen:", nrow(krankenhausData)))
          # jetzt nur noch das letzte behalten
          krankenhausData <- anti_join(krankenhausData, head(tmp_day,n=nrow_tmp_day-1))
          print (paste("Laenge krankenhausData nach Loeschen sollte kleiner sein:", nrow(krankenhausData)))
          previous_day=tail(tmp_day,n=1)
        }  else {
          # is ok as the first date is always populated
          previous_day=tmp_day
        }
      }
    }
    # 
    #break()                                           
  }
}


# combine data to landkreis, date
print ("----- Combine data to landkreis, date, ... -------")
start_date = min(date(krankenhausData$timestamp))
end_date   = max(date(krankenhausData$timestamp))

index <- 0
# data for landkreise
LK_KH_Data <- tibble(id = double(),  
                          Landkreis = character(), 
                          Date = ymd(), 
                          Stationaer  = double(),
                          ICU_Beatmet = double())
# data for BW
Land_BW_Data <- LK_KH_Data                          
                          
LK_KH_DataLoop <- tibble(id = 1,  
                         Landkreis = "character()", 
                         Date = ymd('2020-04-17'), 
                         Stationaer = 0,
                         ICU_Beatmet = 0)

# Loop through all Landkreises and dates

for (LK_Index in seq(1,length(LK_ILS$Landkreis))) {
  LK_Index
  print(LK_ILS$Landkreis[[LK_Index]])
  
  day_index=0
  for (day_index in seq(start_date, end_date, by = 1)) {
    day_index
    #print (day_index)
    
    targetId = LK_ILS$folderId[[LK_Index]]
    anteil_resourcen = LK_ILS$Anteil_Resourcen[[LK_Index]]
    
    #kh_covid_beatmet
    tmp_kh_covid_beatmet <- filter(krankenhausData, krankenhausData$parentId==targetId 
                     & as.numeric(date(krankenhausData$timestamp))==day_index 
                     & krankenhausData$resourceType=="COVID-Patienten beatmet")
    tmp_sum_kh_covid_beatmet = round(anteil_resourcen * sum(tmp_kh_covid_beatmet$total), digits = 0)
    
    #kh_covid_gesamt
    tmp_kh_covid_gesamt <- filter(krankenhausData, krankenhausData$parentId==targetId 
                                   & as.numeric(date(krankenhausData$timestamp))==day_index 
                                   & krankenhausData$resourceType=="COVID-Patienten gesamt")
    tmp_sum_kh_covid_gesamt = round(anteil_resourcen * sum(tmp_kh_covid_gesamt$total), digits = 0)

    #kh_covid_stationaer
    tmp_sum_kh_covid_stationaer=max(0,tmp_sum_kh_covid_gesamt-tmp_sum_kh_covid_beatmet)
    
    LK_KH_DataLoop$Landkreis   <- LK_ILS$Landkreis[[LK_Index]]
    LK_KH_DataLoop$Date        <- as_date(day_index)
    LK_KH_DataLoop$Stationaer  <- tmp_sum_kh_covid_stationaer
    LK_KH_DataLoop$ICU_Beatmet <- tmp_sum_kh_covid_beatmet
    LK_KH_Data <- bind_rows(LK_KH_Data,LK_KH_DataLoop)
  }
}  

# Loop all dates for baden-wuerttemberg


day_index=0
for (day_index in seq(start_date, end_date, by = 1)) {
  day_index

  #kh_covid_beatmet
  tmp_kh_covid_beatmet <- filter(krankenhausData, as.numeric(date(krankenhausData$timestamp))==day_index 
                                   & krankenhausData$resourceType=="COVID-Patienten beatmet")
  tmp_sum_kh_covid_beatmet = round(anteil_resourcen * sum(tmp_kh_covid_beatmet$total), digits = 0)
    
  #kh_covid_gesamt
  tmp_kh_covid_gesamt <- filter(krankenhausData, as.numeric(date(krankenhausData$timestamp))==day_index 
                                  & krankenhausData$resourceType=="COVID-Patienten gesamt")
  tmp_sum_kh_covid_gesamt = round(anteil_resourcen * sum(tmp_kh_covid_gesamt$total), digits = 0)
    
  #kh_covid_stationaer
  tmp_sum_kh_covid_stationaer=max(0,tmp_sum_kh_covid_gesamt-tmp_sum_kh_covid_beatmet)
    
  LK_KH_DataLoop$Landkreis   <- "Baden-Württemberg"
  LK_KH_DataLoop$Date        <- as_date(day_index)
  LK_KH_DataLoop$Stationaer  <- tmp_sum_kh_covid_stationaer
  LK_KH_DataLoop$ICU_Beatmet <- tmp_sum_kh_covid_beatmet
  Land_BW_Data <- bind_rows(Land_BW_Data,LK_KH_DataLoop)

}  





