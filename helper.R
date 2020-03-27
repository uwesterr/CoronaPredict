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



createBundesLandR0_no_erfasstDf <- function(historyDfBundesLand, input){
  

  
  if(input$regionSelected == 2 &input$filterRegion != "Deutschland") {

    nestedHistoryDfBundesLand <- historyDfBundesLand %>% filter(Bundesland ==  input$filterRegion) %>% filter(MeldeDate <= as.Date('2020-03-16')) %>% group_by(Bundesland) %>% nest()
    # browser()
    # Define function to calculate regression
    expoModel <- function(df) {
      
      df <- df %>% filter(MeldeDate >= FirstMelde)
      lm(log10(sumAnzahlFallBundesland) ~ MeldeDate, data = df)
    }
    
    
    predictLm <- function( model, data){
      startDate <- data$FirstMelde %>% unique()
      endDate <- as.Date('2020-03-16')  
      data <- data.frame(MeldeDate = seq(startDate, endDate,by =1))
      
      add_predictions(data, model)
      
    }
    nestedHistoryDfBundesLandModel <- nestedHistoryDfBundesLand %>% 
      mutate(model = map(data, expoModel),
             predictionsRegressionPeriode  = map2(model,data, predictLm),
             predictions  = map2(data, model, add_predictions), # https://r4ds.had.co.nz/many-models.html
             tidiedFit = map(model,tidy)) 
    
    predictedHistoryDfBundesLandModel <- nestedHistoryDfBundesLandModel %>% unnest(c(predictions), .sep ="_") %>% unnest(data) 
    
    #### calculate R0 und no_erfasst
    
    
    predictedHistoryDfBundesLandR0 <- nestedHistoryDfBundesLandModel %>% unnest(c(predictions), .sep ="_") %>% unnest(tidiedFit)
    
    r0Df <- predictedHistoryDfBundesLandR0 %>% mutate(R0 = ifelse(term == "MeldeDate", 10^estimate, NA))
    
    r0Df <- r0Df %>% group_by(Bundesland) %>% select(-c(std.error, statistic)) %>% summarise_if(is.numeric, max, na.rm = TRUE) 
    n0_erfasstDf <- predictedHistoryDfBundesLandModel %>% select(Bundesland, predictions_MeldeDate, predictions_pred)  %>% filter(predictions_MeldeDate == as.Date('2020-03-01')) %>% unique() %>% mutate(n0_erfasst = 10^predictions_pred) %>% 
      
      select(Bundesland, n0_erfasst, predictions_MeldeDate) 
    
    r0_no_erfasstDf <- left_join(r0Df  ,n0_erfasstDf) %>% select(Bundesland, p.value, R0, n0_erfasst) 
   # browser()
    
    
    dfRoNo <- left_join(historyDfBundesLand %>% filter(Bundesland ==  input$filterRegion), r0_no_erfasstDf) %>% 
      mutate(sumAnzahlFall = sumAnzahlFallBundesland, Ygesamt = EinwohnerBundesland)
    

  }
  dfRoNo
}

createDfBundLandKreis <- function() {
  
  historyData <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  historyDf <- historyData[["features"]][["properties"]]
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  

  ## read population file from thonmas
  bundesLandPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  
  
  #historyDf <- left_join(historyDf,bundesLandPopulation)
  #historyDf <- left_join(historyDf,landKreisPopulation)
  
  
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% 
    mutate(sumAnzahlFallBund = cumsum(AnzahlFall),
           BundIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% mutate(Ygesamt = bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland)) %>% unlist)
  
  BundesLandFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Bundesland) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBundesLand  <- historyDf %>% dplyr::ungroup() %>% group_by(Bundesland, MeldeDate)  %>% summarise_if(is.numeric, sum, na.rm = TRUE)  %>% 
    mutate(sumAnzahlFallBundesland = cumsum(AnzahlFall), sumAnzahlFallBundesland = ifelse(sumAnzahlFallBundesland <1, 0.1,sumAnzahlFallBundesland),
           LandIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(BundesLandFirstMeldung) %>% left_join(bundesLandPopulation)
  
  
  
  
  LandkreisFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Landkreis) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfLandkreis <- historyDf  %>% dplyr::group_by(Bundesland,Landkreis, MeldeDate)%>% dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
    mutate(sumAnzahlFallLandkreis = cumsum(AnzahlFall), sumAnzahlFallLandkreis = ifelse(sumAnzahlFallLandkreis <1, 0.1,sumAnzahlFallLandkreis),
           KreisIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(LandkreisFirstMeldung) %>%  left_join(landKreisPopulation)
  
  return(list(historyDfBund, historyDfBundesLand, historyDfLandkreis))
}


Rechenkern <- function(r0_no_erfasstDf, input) {

  ### rechenkern
  
  
  
  # Betroffene
  
  Ygesamt	<- r0_no_erfasstDf$Ygesamt # Gesamtmenge
  n0_erfasst <- 	r0_no_erfasstDf$n0_erfasst # Anzahl erfasster Infizierter am Beginn 
  beginn_date	<- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) # Datum Beginn

  
  #Krankenhausaufenthalt
  kh_normal	<-input$kh_normal/100 # Anteil an aktuellen Infizierten [percent]
  t_kh	<- input$t_kh # Dauer [tage]
  dt_inf_kh	<- input$dt_inf_kh # Versatz nach Infektion [tage]
  kh_intensiv	<- input$kh_intensiv/100 #  Anteil Intensivstation [percent]
  t_intensiv	<- input$t_intensiv # Dauer Intensivstation [tage]
  dt_kh_int	<- input$dt_kh_int # Versatz Krankenhaus - Intensivstation [tage]
  
  # Expertenparameter für Infektionsverlauf
  
  
  ges_inf_rate <- 	input$ges_inf_rate/100 # Gesättige Infektionsrate [percent]
  faktor_n_inf <- 	input$faktor_n_inf # Faktor der nicht erfassten Infizierten
  ta	<- input$ta # Dauer Ansteckbarkeit  [tage]
  r0	<- input$r0 # Neuansteckung durch einen Infizierten
  tod_rate <-  input$tod_rate/100 # Sterblichkeit
  td_tod <- 	input$td_tod # Dauer Infektion bis Tod  [tage]
  reduzierung_datum	<- input$reduzierung_datum # Datum Reduktionsmassnahme
  reduzierung_rt <- 	input$reduzierung_rt/100 # Reduktion der Repr.rate/Tag
  
  # Ausgabe
  
  
  Y_inf_limit <- Ygesamt*ges_inf_rate/faktor_n_inf
  Rt <- r0^(1/ta)
  
  
  
  # functions
  
  calcWirksamkeitReduktion <- function(calcDf, reduzierung_datum, ta) {
    if (calcDf$Tag < reduzierung_datum){
      
      WirksamkeitReduktion <- 0
      
    }  else {
      calcDf <- calcDf %>% tail(1)
      WirksamkeitReduktion <-min(1,(as.numeric(calcDf$Tag - reduzierung_datum)+1)/ta)
      
    }
    WirksamkeitReduktion
  }
  
  
  calcReduzierteRt <-  function(df){
    
    df <- df %>% tail(1)
    ReduzierteRt <- df$TaeglichReproduktionsRateRt-df$WirksamkeitReduktion * (df$TaeglichReproduktionsRateRt-1) * reduzierung_rt
    ReduzierteRt
  }
  
  # max(0,n0_erfasst*(ta - as.numeric(calcDf$Tag - startDate)+2 )/ta)
  
  
  
  calcNeuGesamtInfizierteBerechnet <- function(calcDf){
    
    max(0.1,(calcDf$ReduzierteRt-1)*calcDf$GesamtInfizierteBerechnet)
  }
  
  
  # Initialize the dataframe
  startDate <- as.Date('2020-03-01')
  endDate <- as.Date('2020-05-31')
  calcDf <- tibble(Tag                     = startDate,
                   TaeglichReproduktionsRateRt       = Rt,
                   AktuellInfizierteBerechnet        = n0_erfasst,
                   RestanteilStartwert               = NA,
                   NeuInfizierteBerechnet            = NA,
                   ErfassteInfizierteBerechnet       = AktuellInfizierteBerechnet,
                   GesamtInfizierteBerechnet         = AktuellInfizierteBerechnet*faktor_n_inf,
                   NeuGesamtInfizierteBerechnet      = NA,
                   KhBerechnet                       = NA,
                   IntensivBerechnet                 = 0,
                   NeueToteBerechnet                 = 0,
                   ToteBerechnet                     = 0,
                   ReduktionAbDatum                  = 0,
                   WirksamkeitReduktion              = 0,
                   ReduzierteRt                      = 0,
                   MaxKhBerechnet                    = 0,
                   MaxIntBerechnet                   = 0,
                   
  )
  
  initCalcDf <- function(calcDf, reduzierung_datum, ta, n0_erfasst, startDate, faktor_n_inf) {
    calcDf$WirksamkeitReduktion<- calcWirksamkeitReduktion(calcDf, reduzierung_datum, ta)  
    calcDf$ReduzierteRt<- calcReduzierteRt(calcDf)
    calcDf$NeuGesamtInfizierteBerechnet<- calcNeuGesamtInfizierteBerechnet(calcDf)
    calcDf$NeuInfizierteBerechnet <- max(.1,calcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf)
    return(calcDf)
  }
  
  initCalcDf <- initCalcDf(calcDf, reduzierung_datum, ta, n0_erfasst, startDate, faktor_n_inf)
  
  
  
  lengthOfTail <- 1
  calcDf <- initCalcDf
  
  
  calcTaeglichReproduktionsRateRt <- function(Rt, calcDf, Y_inf_limit) {
    Rt-(tailCalcDf$ErfassteInfizierteBerechnet*(Rt-1))/Y_inf_limit
  }
  
  calcRestanteilStartwert <- function(tailCalcDf, n0_erfasst, ta, startDate, date) {
    
    max(0,n0_erfasst*(ta - as.numeric(date - startDate) )/ta)
  }
  
  
  calcGesamtInfizierteBerechnet <- function(calcDf){
    
    calcDf$GesamtInfizierteBerechnet+calcDf$NeuGesamtInfizierteBerechnet
  }
  
  calcErfassteInfizierteBerechnet <- function(tailCalcDf){
    tailCalcDf$NeuInfizierteBerechnet   + tailCalcDf$ErfassteInfizierteBerechnet
  }
  
  startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) # Datum Beginn
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")) # Datum Beginn
  
  
  for (i in seq(startDate, endDate,by = 1)) {
    tailCalcDf <- tail(calcDf,lengthOfTail)
    date <- tailCalcDf$Tag +1
    updatecalcDf <- tibble(
      Tag                               = tailCalcDf$Tag+1,
      TaeglichReproduktionsRateRt       = calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit),
      AktuellInfizierteBerechnet        = n0_erfasst,
      RestanteilStartwert               = calcRestanteilStartwert(tailCalcDf, n0_erfasst, ta, startDate, date),
      NeuInfizierteBerechnet            = NA,
      ErfassteInfizierteBerechnet       = NA,
      GesamtInfizierteBerechnet         = NA,
      NeuGesamtInfizierteBerechnet      = NA,
      KhBerechnet                       = NA,
      IntensivBerechnet                 = 0,
      NeueToteBerechnet                 = 0,
      ToteBerechnet                     = 0,
      ReduktionAbDatum                  = 0,
      WirksamkeitReduktion              = 0,
      ReduzierteRt                      = 0,
      MaxKhBerechnet                    = 0,
      MaxIntBerechnet                   = 0,
      
    )
    
    
    updatecalcDf$WirksamkeitReduktion<- calcWirksamkeitReduktion(updatecalcDf, reduzierung_datum, ta)  
    updatecalcDf$ReduzierteRt<- calcReduzierteRt(updatecalcDf)
    updatecalcDf$GesamtInfizierteBerechnet <-  calcGesamtInfizierteBerechnet(tailCalcDf)
    updatecalcDf$NeuGesamtInfizierteBerechnet<- calcNeuGesamtInfizierteBerechnet(updatecalcDf)
    updatecalcDf$NeuInfizierteBerechnet <- max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf)
    updatecalcDf$ErfassteInfizierteBerechnet<- calcErfassteInfizierteBerechnet(tailCalcDf)
    
    #test <- add_row(test,  Tag = test$Tag[i], TaeglichReproduktionsRateRt = test$TaeglichReproduktionsRateRt[i], 
    #                     GesamtInfizierteBerechnet = test$GesamtInfizierteBerechnet[i], ReduzierteRt = test$ReduzierteRt[i])
    
    calcDf <- rbind(calcDf,updatecalcDf)
    
  }
  calcDf$ID <- seq.int(nrow(calcDf))
  
  
  
  calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = ifelse(ID==1,n0_erfasst,
                                                                  rollapply(NeuInfizierteBerechnet, 10, sum,align = "right", fill = NA, partial =TRUE) + RestanteilStartwert-NeuInfizierteBerechnet))
 
  calcDf <- calcDf %>% mutate(KhBerechnet = kh_normal*  (rollapply(NeuInfizierteBerechnet, 22, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, 8, sum,align = "right", partial = TRUE )) )
  calcDf <- calcDf %>% mutate(IntensivBerechnet = kh_normal * kh_intensiv *  (rollapply(NeuInfizierteBerechnet, 18, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, 9, sum,align = "right", partial = TRUE )))
  
  df <- left_join(calcDf,r0_no_erfasstDf, by =c("Tag" = "MeldeDate"))
  
 #browser()
  return(df)
}
