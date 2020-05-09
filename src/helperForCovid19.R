# helper functions for shiny app Covid19

library(httr)
library(tictoc)
library(GA)
library(jsonlite)
library(staTools)
library(writexl)
library(readxl)
library(rlang)
library(DT)
library(modelr)
library(tidyverse)
library(lubridate)
library(zoo)

createDfBundLandKreis <- function() {
  # https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
  historyData <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  
  historyDf <- historyData[["features"]][["properties"]]
  cat("Number of rows: ", nrow(historyDf))
  maxTries <- 5  # number of tries to poll server and get at least minimum rows of data
  waitingTime <- 600 # wait ten minutes before polling server again
  minDataSize <- 123396 # minimum number of rows to be considered a full download
  counter <- 0
  while ((nrow(historyDf) < minDataSize ) & (counter < maxTries) ) {
    counter <- counter + 1
    cat("Download too small: ", nrow(historyDf), " try again, counter: ", counter)
    print("")
    Sys.sleep(waitingTime)
    historyData <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
    historyDf <- historyData[["features"]][["properties"]]
    
  }
  
  if (counter == maxTries) {
    abort(message = "Not enough data in download, abort")
    
  }
  
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  
  
  ## read population file from thomas
  bundesLandPopulation <- read_excel("../data/bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("../data/bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  # browser()
  BundFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% 
    mutate(Bundesland = "NA", Landkreis = NA, SumAnzahl = cumsum(AnzahlFall), sumTote = cumsum(AnzahlTodesfall), whichRegion = "Deutschland",
           Index = as.numeric(MeldeDate- min(MeldeDate))) %>% mutate(Einwohner = bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland)) %>% unlist) 
  historyDfBund$FirstMelde <- BundFirstMeldung %>% unlist %>% as.Date()
  
  ######## calculate all meldungen within week per 100.000 inhabitants
  historyDfBund <- historyDfBund %>% arrange(MeldeDate) %>% mutate(MeldeWithinWeek = pmap_dbl(list(MeldeDate, Einwohner, whichRegion), sumFallForWeek, historyDfBund),
                                                                   MeldeWithinWeekPer100kInhabitants = MeldeWithinWeek/Einwohner*1e5 )
  
  

  
  BundesLandFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Bundesland) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBundesLand  <- historyDf %>% dplyr::ungroup() %>% group_by(Bundesland, MeldeDate)  %>% summarise_if(is.numeric, sum, na.rm = TRUE)  %>% 
    mutate(Landkreis = NA, SumAnzahl = cumsum(AnzahlFall), sumTote = cumsum(AnzahlTodesfall),whichRegion = Bundesland,
           SumAnzahl = ifelse(SumAnzahl <1, 0.1,SumAnzahl),
           Index = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(BundesLandFirstMeldung) %>% left_join(bundesLandPopulation)  %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) 
  ######## calculate all meldungen within week per 100.000 inhabitants
  historyDfBundesLand <- historyDfBundesLand %>% group_by(whichRegion) %>% arrange(MeldeDate) %>% mutate(MeldeWithinWeek = pmap_dbl(list(MeldeDate, Einwohner, whichRegion), sumFallForWeek, historyDfBundesLand),
  MeldeWithinWeekPer100kInhabitants = MeldeWithinWeek/Einwohner*1e5 )
  
  
  LandkreisFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Landkreis) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfLandkreis <- historyDf  %>% dplyr::group_by(Bundesland,Landkreis, MeldeDate)%>% dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
    mutate(SumAnzahl = cumsum(AnzahlFall), sumTote = cumsum(AnzahlTodesfall), whichRegion = Landkreis,
           SumAnzahl = ifelse(SumAnzahl <1, 0.1,SumAnzahl),
           Index = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(LandkreisFirstMeldung) %>%  left_join(landKreisPopulation)  %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) 
  
  ######## calculate all meldungen within week per 100.000 inhabitants
  historyDfLandkreis <- historyDfLandkreis %>% group_by(whichRegion)  %>% arrange(MeldeDate) %>% mutate(MeldeWithinWeek = pmap_dbl(list(MeldeDate, Einwohner, whichRegion), sumFallForWeek, historyDfLandkreis),
                                                                             MeldeWithinWeekPer100kInhabitants = MeldeWithinWeek/Einwohner*1e5 )
  
  
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
  return(RkiDataWithRoNoOpimizedUpToDate)
}

######## calculate all meldungen within week per 100.000 inhabitants forecast ############
sumFallForWeek <- function(MeldeDate, Einwohner, selectedRegion, df){
  df <- df %>% filter(whichRegion == selectedRegion)
  IndexToday <- which(df$MeldeDate == MeldeDate)
  IndexWeekBefore <- min(which(df$MeldeDate >= MeldeDate - 6)) # find the day not more than a week ago but as much in the past as possible
  if (is_empty(IndexWeekBefore)) {
    IndexWeekBefore <- 1
    
  }
  
  tmp <- df$AnzahlFall[IndexWeekBefore:IndexToday] %>% sum
  res <- tmp%>% as.numeric() # gemeldete within a week per 100.000 inhabitants
  return(res)
  
}



######## calculate all meldungen within week per 100.000 inhabitants  ############
sumFallForWeekForecast <- function(MeldeDate, df){
  IndexToday <- which(df$Tag == MeldeDate)
  IndexWeekBefore <- min(which(df$Tag >= MeldeDate - 6)) # find the day not more than a week ago but as much in the past as possible
  if (is_empty(IndexWeekBefore)) {
    IndexWeekBefore <- 1
    
  }
  
  tmp <- df$NeuInfizierteBerechnet[IndexWeekBefore:IndexToday] %>% sum
  res <- tmp%>% as.numeric() # gemeldete within a week per 100.000 inhabitants
  return(res)
  
}


### read krankenhaus data ######
# reads data from rescuetracker api and stores data in data.frame
readRescueTrackerData <- function() {
  library(httr)
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(mefa4)
  library(dplyr)
  
  # tmp
  #setwd("D:/thomas/Dropbox/Intern/LGA/R/app/kh_import/src")
  
  # get data from rescuetrack
  load("../secure/cookieForReutlingerData.RData")
  rescuetrackData <- GET("https://apps.rescuetrack.com/rrb/api/v1/getCapacityDump", add_headers(Cookie = Cookie))
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
  # print ("----- Assign raw data on level Krankenhaus -------")
  for (folderIndex in seq(1,length(foldersDf))) {
    folderIndex
    # exclude higher levels BL, RP and ILS from data import
    if ( (length(foldersDf[[folderIndex]][["resources"]])>0) 
         && !str_detect(foldersDf[[folderIndex]][["folderName"]], c("ILS ", "RP ", "Baden-W")) %>% sum()
    )  {
      #  print(foldersDf[[folderIndex]][["folderName"]])
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
            #  print ("anlegen")
            act_timestamp <- as_datetime(day_index*86400+36000)
            krankenhausDataLoop            <- previous_day
            krankenhausDataLoop$timestamp  <- act_timestamp
            krankenhausData <- bind_rows(krankenhausData,krankenhausDataLoop)
            
          }  else if (nrow_tmp_day>1) {
            #keep only newest entry (=last in act_kh) and delete others in krankenhausData
            #  print (paste("Laenge krankenhausData vor Loeschen:", nrow(krankenhausData)))
            # jetzt nur noch das letzte behalten
            krankenhausData <- anti_join(krankenhausData, head(tmp_day,n=nrow_tmp_day-1))
            #  print (paste("Laenge krankenhausData nach Loeschen sollte kleiner sein:", nrow(krankenhausData)))
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
  #  print ("----- Combine data to landkreis, date, ... -------")
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
    #   print(LK_ILS$Landkreis[[LK_Index]])
    
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
    
    LK_KH_DataLoop$Landkreis   <- "Baden-W?rttemberg"
    LK_KH_DataLoop$Date        <- as_date(day_index)
    LK_KH_DataLoop$Stationaer  <- tmp_sum_kh_covid_stationaer
    LK_KH_DataLoop$ICU_Beatmet <- tmp_sum_kh_covid_beatmet
    Land_BW_Data <- bind_rows(Land_BW_Data,LK_KH_DataLoop)
    
  } 
  
  Land_BW_Data <- Land_BW_Data %>% mutate(Landkreis = ifelse(Landkreis =="Baden-W?rttemberg", "Baden-Württemberg", Landkreis))
  KrankenDaten <- bind_rows(Land_BW_Data,LK_KH_Data)
  return(KrankenDaten)
}







calcPredictionsForOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, R0Opt, n0Opt, startDate, rechenDf_nom, input) {
  #browser()
  inputForOptimization <- isolate(reactiveValuesToList(input)) # to make setting reduzierung_rtx easy and fast
  inputForOptimization$reduzierung_rt1 <- reduzierung_rt1
  inputForOptimization$reduzierung_rt2 <- reduzierung_rt2
  inputForOptimization$reduzierung_rt3 <- reduzierung_rt3
  
  dfRechenKern <- (Rechenkern(rechenDf_nom, inputForOptimization, startDate))
  metric <- calcMetric(dfRechenKern, rechenDf_nom)
  cat("metric: ", metric)
  return(-metric) # minus because GA maximizes
} 

calcMetric <- function(dfRechenKern, data){
  res <- left_join(data, dfRechenKern %>% select(c(Tag, ErfassteInfizierteBerechnet)), by = c("MeldeDate" = "Tag")) %>% filter(!is.na(ErfassteInfizierteBerechnet)) %>% 
    rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  metric <- (sum(log10(res$SumAnzahl)   - log10(res$ErfassteInfizierteBerechnet) )^2)/(nrow(data)-1)^0.5
}

################    optimizing reduzierung  #################
appendOpt <- function(df, parameter_tibble, optFunction, resultColumnName, gaPara) {
  index <- 0
  for (regionSelected in df$whichRegion) {
    index <- index +1
    print(index)
    print(regionSelected)#
    tmp  <- df %>% filter(whichRegion == regionSelected)
    inputForOptimization <- setInputAccordingToPreviousOpt(tmp) 
    df <-   createRkiRegOptFrame(df, regionSelected, parameter_tibble, 
                                 optFunction, resultColumnName, gaPara, inputForOptimization)
    
  }
  return(df)
}

setInputAccordingToPreviousOpt <- function(tmp) {
  OptResultDf <- tmp[[1,"optimizedInput"]]
  inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
  for (inputVarName in OptResultDf[[1]] %>% names) {
    if (inputVarName %in% (input %>% names)) {
      inputForOptimization[[inputVarName]]<- OptResultDf[[1]][[inputVarName]] 
    }
  }
  return(inputForOptimization)
}


createRkiRegOptFrame <- function(dfNested, regionSelected, parameter_tibble, optFunction, resultColumnName,  gaPara, input){
  
  # set suggestions values according to values of previous run
  tmp <- dfNested %>%  filter(whichRegion == regionSelected) 
  OptResultDf <- tmp[[1,"optimizedInput"]]
  for (inputVarName in OptResultDf[[1]] %>% names) {
    if (inputVarName %in% (parameter_tibble$var_name)) {
      index <- which(parameter_tibble$var_name == inputVarName)
      parameter_tibble[index,"var_value"] <- OptResultDf[[1]][[inputVarName]] 
    }
  }
  dfUnNested <- tmp  %>% unnest(data)
  res <- optimizerGeneticAlgorithmRedReduction(dfUnNested, parameter_tibble, optFunction,  gaPara, input)
  indexEntitiy <- which(dfNested$whichRegion == regionSelected)
  dfNested[[indexEntitiy, resultColumnName]] <- list(res$OptResult)
  optimizedInput <- dfNested[[indexEntitiy, "optimizedInput"]][[1]]
  # update optimized values to list
  
  for (parameter in parameter_tibble$var_name) {
    
    optimizedInput[[parameter]] <- res[["OptResult"]][[parameter]][[1]]
  }
  
  dfNested[[indexEntitiy, "optimizedInput"]] <- list(optimizedInput)
  return( "dfNested" = dfNested)
}
optimizerGeneticAlgorithmRedReduction <- function(dfUnNested, parameter_tibble, optFunction,  gaPara, input) {
  # optimizer using genetic algorithm to optimize reduzierungsmaßnahmen und R0 n0
  # dfRoNoOpt should be dataframe starting with reduzierung_datum1
  dateOfFirstAction <- input$reduzierung_datum1
  dfUnNested <- dfUnNested %>% filter(MeldeDate >= dateOfFirstAction) # only consider values after first reduction action
  
  
  res <- createOptParmeters(parameter_tibble)
  allPara          <- res[[1]]
  optPara          <- res[[2]]
  minOpt           <- res[[3]]
  maxOpt           <- res[[4]]
  suggestions      <- res[[5]]
  parameter_tibble <- res[[6]]
  GA <-  ga(type = "real-valued",
            suggestions = suggestions,
            fitness = optFunction,
            lower = minOpt,
            upper = maxOpt,
            popSize =  gaPara$popSize, 
            maxiter = gaPara$maxiter,
            parallel = gaPara$parallel,
            run = gaPara$run,
            seed = 2020,
            allPara = allPara, parameter_tibble = parameter_tibble, dfUnNested = dfUnNested, gaPara, input = input,
            keepBest = FALSE
  )
  
  print(GA@solution)
  # using always the first line of the solution output, TODO: understand why at times there are several 
  # output lines
  denormPara <- denormalizePara(GA@solution[1,], parameter_tibble, para)
  cat("denormPara: ", unlist(denormPara %>% names), "\n") 
  cat("denormPara: ", unlist(denormPara), "\n")  
  
  denormParaNames <- denormPara %>% names
  
  # create output frame 
  OptResult <- tibble(timeStamp = now())
  for(i in 1 : length(denormPara)){
    OptResult[[denormParaNames[[i]]]] <- denormPara[[i]]
  }
  OptResult[["GaFitnessValue"]] <- GA@fitnessValue
  OptResult[["OptParaNames"]] <- list("denormParaNames" = denormParaNames)
  OptResult[["GaPara"]] <- list( "summmaryGA" = summary(GA))
  
  return(list("OptResult" = OptResult))
  
}

createOptParmeters <- function(parameter_tibble){
  parameter_tibble$new_sol <- NA 
  
  parameter_tibble <- parameter_tibble %>% mutate(start_value = var_value)
  
  parameter_tibble <- parameter_tibble %>% mutate(var_value_norm = (var_value - var_min) /(var_max-var_min))
  
  optPara <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_value) %>% unlist
  names(optPara) <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_name) %>% unlist
  nonOptPara <- parameter_tibble %>% filter(var_selected == FALSE) %>% 
    select(var_value) %>% unlist
  names(nonOptPara) <-parameter_tibble %>% filter(var_selected == FALSE) %>% 
    select(var_name) %>% unlist
  allPara <- c(optPara , nonOptPara )
  maxOpt <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_max) %>% unlist
  minOpt <- parameter_tibble %>% filter(var_selected == TRUE) %>% 
    select(var_min) %>% unlist
  parameter_tibble <-  parameter_tibble %>% filter(var_selected == TRUE) %>% 
    mutate( suggestions_norm =  (var_value - var_min) /(var_max-var_min))
  
  suggestions <- parameter_tibble  %>% filter(var_selected == TRUE)%>%
    select(suggestions_norm) %>%unlist
  
  minOpt <- rep(0, length(minOpt))
  maxOpt <- rep(1, length(minOpt))
  # browser()
  return(list(allPara, optPara, minOpt, maxOpt, suggestions, parameter_tibble  ))
}
calcPredictionsForGaOptimization = function(optPara, allPara, parameter_tibble, dfUnNested, gaPara, input) {   
  # calculate the predictions for the optimzaition loop, i.e. GA algorithm
  denormPara <- denormalizePara(optPara, parameter_tibble)
  #  cat("para",unlist(denormPara,"\n"))
  for(i in 1 : length(optPara)){
    allPara[[i]] <- denormPara[[i]]
  }
  inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
  inputVarNames <- names(allPara)
  for (inputVarName in   inputVarNames ) {
    inputForOptimization[[inputVarName]]<- allPara[[inputVarName]]
  }
  
  inputForOptimization$dateInput[2] = dfUnNested$MeldeDate %>% max() # set endDate to date of last MeldeDate
  
  dfRechenKern <-   Rechenkern(dfUnNested, inputForOptimization)
  
  dfRechenKern <- dfRechenKern %>% filter(Tag  %in% dfUnNested$MeldeDate)
  dfUnNested <- dfUnNested %>% filter(MeldeDate  %in% dfRechenKern$Tag)
  # res <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,dfUnNested$SumAnzahl)
  # browser()
  #    res <- (
  #      (sum(
  #      (log10(dfUnNested$SumAnzahl)   - log10(dfRechenKern$ErfassteInfizierteBerechnet))^2)
  #      )/(nrow(dfRechenKern)-1)
  #      )^0.5
  #  res <- sqrt(mean((log10(dfUnNested$SumAnzahl)-log10(dfRechenKern$ErfassteInfizierteBerechnet))^2))
  res <- MAPE(log10(dfUnNested$SumAnzahl),log10(dfRechenKern$ErfassteInfizierteBerechnet))
  # cat("res is :", res , "redu1 = ", reduzierung_rt1, "\n")
  # res <- sqrt(mean((log10(dfRechenKern$ErfassteInfizierteBerechnet)-log10(dfUnNested$SumAnzahl))^2))
  return(-res)
} 
denormalizePara <- function(optPara, parameter_tibble, para) {
  denormPara <- list(NULL)
  # browser()
  for(i in 1 : length(optPara)){
    denormPara[[i]] <- optPara[[i]] * (parameter_tibble$var_max[i]-parameter_tibble$var_min[i]) +
      parameter_tibble$var_min[i]
  }
  
  # browser()
  names(denormPara) <- parameter_tibble$var_name
  denormPara
}




createPlotReduOpt <- function(RkiDataWithRoNoAndReduzierungOpimized,input) {
  
  load("../data/RkiReduzierungOptFrameDeutschland.RData")
  RkiDataWithRoNoAndReduzierungOpimized <- RkiDataWithRoNoAndReduzierungOpimized %>%    as_tibble()  %>% add_column("dfRechenKern" = 0)
  
  
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
  
  dfRechenkernAndRkiUnnest <-  dfRechenkernAndRki %>% unnest(data) 
  redDate1 <- input$reduzierung_datum1
  redDate2 <- input$reduzierung_datum2
  redDate3 <- input$reduzierung_datum3
  #maxMeldeDate <- max(dfRechenkernAndRkiUnnest$MeldeDate)
  tmp <- dfRechenkernAndRkiUnnest %>%  filter(!is.na(SumAnzahl))
  p <- tmp %>%  group_by(whichRegion)  %>% filter(Tag >= redDate1 & !is_na(SumAnzahl)) %>% ggplot(aes(Tag, SumAnzahl)) + geom_point() + 
    geom_line(aes(Tag, ErfassteInfizierteBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  scale_y_log10(label = label_number_si()) + 
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 300, label = "Reduzierungsmaßnahme 1", angle=90) 
  return(p)
}






###############  error function for krankenhausdaten optimization ###################
calcOptimizationStationaerDaten = function(optPara, allPara, parameter_tibble, dfUnNested, gaPara, input) {   
  # calculate the predictions for krankenhaus data
  denormPara <- denormalizePara(optPara, parameter_tibble)
  #  cat("para",unlist(denormPara,"\n"))
  for(i in 1 : length(optPara)){
    allPara[[i]] <- denormPara[[i]]
  }
  inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
  inputVarNames <- names(allPara)
  for (inputVarName in   inputVarNames ) {
    inputForOptimization[[inputVarName]]<- allPara[[inputVarName]]
  }
  
  inputForOptimization$dateInput[2] = dfUnNested$MeldeDate %>% max() # set endDate to date of last MeldeDate
  dfRechenKern <-   Rechenkern(dfUnNested, inputForOptimization)
  # browser()
  tmp <- dfUnNested[!is.na(dfUnNested[[gaPara$ReportedVar]]),]
  tmp <- tmp[tmp[[gaPara$ReportedVar]] != 0,]
  if(nrow(tmp) == 0){
    res= 0
  } else{
    
    dfRechenKern <- dfRechenKern %>% filter((Tag  %in% tmp$MeldeDate)) # & (Tag > as.Date("2020-03-30")))
    
    
    tmp <- tmp %>% filter(MeldeDate  %in% dfRechenKern$Tag)
    #  
    if (gaPara$errorFunc == "RMS") {
      res <- sqrt(mean((tmp[[gaPara$ReportedVar]]-dfRechenKern[[gaPara$CalculatedVar]])^2)) 
    } else if (gaPara$errorFunc == "MAPE") {
      res <- MAPE(tmp[[gaPara$ReportedVar]],dfRechenKern[[gaPara$CalculatedVar]])
    }  else{
      print("Forgot to define errorfunction for GA calcOptimizationStationaerDaten")
    }
  }
  return(-res)
} 



createPlotStationaerOpt <- function(input) {
  
  load("../data/RkiDataStationaerOpti.RData")
  RkiDataStationaerOpti <- RkiDataStationaerOpti %>% 
    as_tibble()  %>% add_column("dfRechenKern" = 0)
  
  
  dfRechenkernAndRki <- tibble()
  for (regionSelected in RkiDataStationaerOpti$whichRegion %>% head(10)) {
    
    
    ##### create input vector based on optimisaton results
    inputForOptimization <- input
    tmp  <- RkiDataStationaerOpti %>% filter(whichRegion == regionSelected)
    
    inputForOptimization <- setInputAccordingToPreviousOpt(tmp)
    
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
  plotNeuInfizierteBerechnet<-  tmp %>%  group_by(whichRegion)  %>% filter(Tag >= redDate1 & !is_na(SumAnzahl)) %>% ggplot(aes(Tag, AnzahlFall)) + geom_point() + 
    geom_line(aes(Tag, NeuInfizierteBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 5, label = "Reduzierungsmaßnahme 1", angle=90) 
  
  
  plotStationaer <-  tmp %>%  group_by(whichRegion)  %>% 
    filter(Tag >= redDate1 & !is_na(KhBerechnet)) %>% ggplot(aes(Tag, Stationaer)) + geom_point() + 
    geom_line(aes(Tag, KhBerechnet)) + scale_y_log10() +
    facet_wrap(vars(whichRegion), scales="free") +  
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 5, label = "Reduzierungsmaßnahme 1", angle=90) 
  
  return(list("plotNeuInfizierteBerechnet" = plotNeuInfizierteBerechnet, "plotStationaer" = plotStationaer ))
}

createPlotICU_BeatmetOpt <- function(input) {
  load("../data/RkiDataICU_BeatmetOpti.RData")
  RkiDataICU_BeatmetOpti <- RkiDataICU_BeatmetOpti %>% 
    as_tibble()  %>% add_column("dfRechenKern" = 0)
  
  dfRechenkernAndRki <- tibble()
  for (regionSelected in RkiDataICU_BeatmetOpti$whichRegion %>% head(10)) {
    
    
    ##### create input vector based on optimisaton results
    inputForOptimization <- input
    tmp  <- RkiDataICU_BeatmetOpti %>% filter(whichRegion == regionSelected)
    
    inputForOptimization <- setInputAccordingToPreviousOpt(tmp)
    
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
  
  plotIntensivBerechnet <-  tmp %>%  group_by(whichRegion)  %>% 
    filter(Tag >= redDate1 & !is_na(ICU_Beatmet)) %>% ggplot(aes(Tag, ICU_Beatmet)) + geom_point() + 
    geom_line(aes(Tag, IntensivBerechnet))  +
    facet_wrap(vars(whichRegion), scales="free") +  
    geom_vline(xintercept = redDate1, color = "green") + 
    geom_vline(xintercept = redDate2, color = "blue") +  
    geom_vline(xintercept = redDate3, color = "red") +
    annotate("text", x = redDate1, y = 5, label = "Reduzierungsmaßnahme 1", angle=90) 
  
  return(list("plotIntensivBerechnet" = plotIntensivBerechnet))
}

### create leaflet map #######
createMap <- function(RkiDataWithSumsNested) {
  tmp <- RkiDataWithSumsNested %>% unnest(data)
  landkreisMeldeProWocheAktuell <- tmp %>% group_by(Landkreis) %>% filter(MeldeDate == max(MeldeDate)) %>% select(Landkreis, MeldeWithinWeekPer100kInhabitants)
  
  DeutschlandLkSk <-geojsonio::geojson_read("../data/landkreise-in-germany.geojson", what = "sp")
  tmp <- DeutschlandLkSk@data
  tmp <- tmp %>% mutate(Landkreis = ifelse(str_detect(type_2, "Stadt"), paste0("SK ",name_2), paste0("LK ",name_2)),
                        Landkreis = ifelse(str_detect(Landkreis, "Freiburg"), "SK Freiburg i.Breisgau", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Dillingen"), "LK Dillingen a.d.Donau", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Neumarkt"), "LK Neumarkt i.d.OPf.", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Weiden"), "SK Weiden i.d.OPf.", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Wendel"), "LK Sankt Wendel", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "SK Neustadt"), "SK Neustadt a.d.Weinstraße", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Aachen"), "StadtRegion Aachen", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Wunsiedel"), "LK Wunsiedel i.Fichtelgebirge", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Hannover"), "Region Hannover", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Ludwigshafen"), "SK Ludwigshafen", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Landau"), "SK Landau i.d.Pfalz" , Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "SK Offenbach"), "SK Offenbach", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Lindau"), "LK Lindau", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Saarbrücken"), "LK Stadtverband Saarbrücken", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Frankenthal"), "SK Frankenthal", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Halle"), "SK Halle", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Aisch"), "LK Neustadt a.d.Aisch-Bad Windsheim", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Prüm"), "LK Bitburg-Prüm", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Kempten"), "SK Kempten", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Brandenburg"), "SK Brandenburg a.d.Havel", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Pfaffenhofen"), "LK Pfaffenhofen a.d.Ilm", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Landsberg"), "LK Landsberg a.Lech", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Mühldorf"), "LK Mühldorf a.Inn", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Mülheim"), "SK Mülheim a.d.Ruhr", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Berlin"), "SK Berlin Mitte ", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Altenkirchen"), "LK Altenkirchen", Landkreis),
                        Landkreis = ifelse(str_detect(Landkreis, "Waldnaab"), "LK Neustadt a.d.Waldnaab", Landkreis))
  
  
  DeutschlandLkSk@data <- left_join(tmp,  landkreisMeldeProWocheAktuell )#,by = c("Landkreis1" = "Landkreis"))
  mybins <- c(0,5, 10, 15, 20,40, 50, 100, 200, Inf)
  mypalette <- colorBin( palette="YlOrBr", domain=DeutschlandLkSk@data$MeldeWithinWeekPer100kInhabitants, na.color="transparent", bins=mybins)
  
  # Prepare the text for tooltips:
  mytext <- paste(
    "Kreis: ", DeutschlandLkSk@data$Landkreis, "<br/>", 
    "Meldungen letze Woche pro 100.000 Einwohner: ", round(DeutschlandLkSk@data$MeldeWithinWeekPer100kInhabitants, 0), 
    sep="") %>%
    lapply(htmltools::HTML)
  
  # Final Map
  MeldeMap <- leaflet(DeutschlandLkSk) %>% 
    addTiles()  %>% 
    setView( lat=51, lng=6 , zoom=5) %>%
    addPolygons( 
      layerId=~Landkreis,
      fillColor = ~mypalette(MeldeWithinWeekPer100kInhabitants), 
      stroke=TRUE, 
      fillOpacity = 0.6, 
      color="white", 
      weight=0.3,
      label = mytext,
      labelOptions = labelOptions( 
        style = list("font-weight" = "normal", padding = "3px 8px"), 
        textsize = "13px", 
        direction = "auto"
      )
    ) %>%
    addLegend( pal=mypalette, values=~MeldeWithinWeekPer100kInhabitants, opacity=0.9, title = "Meldungen", position = "bottomleft" )
  
 return(MeldeMap)
}



############# ValidationsScriptBundeslaender ##############

calcReduziertOptPredictions <- function(R0, n0, dfRoNoOpt, input, startDate){
  # browser()
  dfRoNoOpt$R0 <-  R0
  dfRoNoOpt$n0_erfasst <- n0
  dfRoNoOpt <- dfRoNoOpt %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  dfRechenKern <- (Rechenkern(dfRoNoOpt, input$input, startDate))
  
  return(dfRechenKern) 
}   



