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




createLandkreisR0_no_erfasstDf <- function(df, historyDfBund, regionSelected, input, session){
  # browser()
  if (regionSelected ==1) {
#    filterVar = "Bund"
#    historyDfBund$Bund = "Deutschland"
#    df <- historyDfBund %>% rename("whichRegion" = "Bund")
#    regionSelected <- "Bund"
#    df$whichRegion ="Bund"  # set all of the entries to be taken care of
  } else if (regionSelected ==2) {
    
    if(input$BundeslandSelected == "Deutschland"){
      filterVar = "Bund"
      historyDfBund$Bund = "Deutschland"
      df <- historyDfBund %>% rename("whichRegion" = "Bund")
      regionSelected <- "Deutschland" 
    } else{
      
    filterVar = "Bundesland"
    df <- df %>% rename("whichRegion" = "Bundesland")
    regionSelected <- input$BundeslandSelected
    }
  } else if(regionSelected ==3) {
    filterVar = "Landkreis"
    df <- df %>% rename("whichRegion" = "Landkreis")
    regionSelected <- input$LandkreiseSelected
  }
  
  df <- df %>% ungroup() %>%  filter(whichRegion == regionSelected)
  
  df <- df %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" )
  
  nesteddf <- df %>% filter(MeldeDate <=  as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d")) )  %>% nest()
  
  # Define function to calculate regression
  expoModel <- function(df) {
    
    #TG: Regression zwischen Startdatum bzw. erstem Meldedatum und erster Massnahme. Annahme: bis dahin exponentieller Verlauf
    #    Wenn Start-Date > erste Massnahme ist: Ausnahme --> 10 Tage nach Start
    endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))
    startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) 
    
    if (endDate > startDate) {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= endDate)
    } else {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= startDate+10)
    }
    
    #TG durch oben ersetzt: df <- df %>% filter(MeldeDate >= FirstMelde)
    
    lm(log10(SumAnzahl) ~ MeldeDate, data = df)
  }
  
  
  predictLm <- function( model, data){
    #browser()
    #TG ersetzt durch unten: startDate <- data$FirstMelde %>% unique()
    #TG ersetzt durch unten: endDate <- as.Date('2020-03-16')  
    
    #TG: Wie in expoModel, wieso 2x ? habe ich noch nicht richtig verstanden
    startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()                      
    endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))  %>% unique() 
    
    if (endDate <= startDate) {
      endDate <- startDate+10
    } 
    
    data <- data.frame(MeldeDate = seq(startDate, endDate,by =1))
    add_predictions(data, model)
    
  }
  nesteddfModel <- nesteddf %>% 
    mutate(model = map(data, expoModel),
           predictionsRegressionPeriode  = map2(model,data, predictLm),
           predictions  = map2(data, model, add_predictions), # https://r4ds.had.co.nz/many-models.html
           tidiedFit = map(model,tidy)) 
  #browser()
  #   predicteddfModel <- nesteddfModel %>% unnest(c(predictions), .sep ="_") %>% unnest(data) 
  
  # US 29.03.2020: Vereinfachuchung der der nest und unnest vorgänge
  
  predicteddfR0 <- nesteddfModel  %>% unnest(data) %>% unnest(c(predictions), .sep ="_")%>% unnest(c(predictionsRegressionPeriode), .sep ="_") %>% unnest(tidiedFit)
  
  r0Df <- predicteddfR0 %>% mutate(R0 = ifelse(term == "MeldeDate", 10^estimate, NA))
  
  r0Df <- r0Df  %>% select(-c(std.error, statistic)) %>% summarise_if(is.numeric, max, na.rm = TRUE) 
  
  # find regression value of first melde day, here is the problem, with first day of melde the results are useless
  #firstMeldeDay <- df$FirstMelde %>% min
  
  n0_erfasstDf <- predicteddfR0 %>% select(whichRegion, predictionsRegressionPeriode_MeldeDate, predictionsRegressionPeriode_pred)  %>% 
    filter(predictionsRegressionPeriode_MeldeDate == as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) ) %>% unique() %>% mutate(n0_erfasst = 10^predictionsRegressionPeriode_pred) 
  
  
  r0_no_erfasstDf <- cbind(r0Df ,n0_erfasstDf %>% select(whichRegion, n0_erfasst) ) %>% select(whichRegion, p.value, R0, n0_erfasst) 
  dfRoNo <- left_join(df , r0_no_erfasstDf) %>%    mutate( Ygesamt = Einwohner)
  
  
  dfRoNo
  #browser()
}

createDfBundLandKreis <- function() {
  
  historyData <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  historyDf <- historyData[["features"]][["properties"]]
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  
  
  ## read population file from thonmas
  bundesLandPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  
  BundFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% 
    mutate(sumAnzahlFallBund = cumsum(AnzahlFall),
           BundIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% mutate(EinwohnerBund = bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland)) %>% unlist) 
  historyDfBund$FirstMelde <- BundFirstMeldung %>% unlist %>% as.Date()
  
  
  
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
  
  # Betroffene
  Ygesamt	<- r0_no_erfasstDf$Einwohner # Gesamtmenge
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
  ta	<- input$ta # Infektiosit?t  [tage]
  ti	<- input$ti # Inkubationszeit [tage]
  
  
  #TG: dieser Parameter wurde entfernt, kann intern aus ta und rt (extrahiert) berechnet werden
  #r0	<- input$r0 # Neuansteckung durch einen Infizierten
  tod_rate <-  input$tod_rate/100 # Sterblichkeit
  td_tod <- 	input$td_tod # Dauer Infektion bis Tod  [tage]
  
  # Auswirkung Massnahmen
  reduzierung_datum1	<- input$reduzierung_datum1   # Datum 1. Reduzierungsmassnahme
  reduzierung_rt1     <- input$reduzierung_rt1/100  # Reduzierung1 der Repr.rate/Tag
  reduzierung_datum2	<- input$reduzierung_datum2   # Datum 2. Reduzierungsmassnahme
  reduzierung_rt2     <- 	input$reduzierung_rt2/100 # Reduzierung2 der Repr.rate/Tag
  reduzierung_datum3	<- input$reduzierung_datum3   # Datum 3. Reduzierungsmassnahme
  reduzierung_rt3     <- input$reduzierung_rt3/100  # Reduzierung3 der Repr.rate/Tag
  
  # Ausgabe
  Y_inf_limit <- Ygesamt*ges_inf_rate/faktor_n_inf
  Rt <- r0_no_erfasstDf$R0
  r0 <- Rt^ta
  
  # functions
  
  # TG, 30.3. : Berechnung 3-stufige Reduzierung Rt
  calcReduzierung <- function(df, red_datum1, red_rt1, red_datum2, red_rt2, red_datum3, red_rt3, ta) {
    # US 29.03.2020: avoid error message Warning in if (calcDf$Tag < red_datum) { :the condition has length > 1 and only the first element will be used
    rt_i <- df$TaeglichReproduktionsRateRt
    
    df <- df %>% tail(1) 
    if (df$Tag < red_datum1){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum1)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt1
    }
    
    if (df$Tag < red_datum2){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum2)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt2
    }
    
    if (df$Tag < red_datum3){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum3)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt3
    }
    rt_i
  }
  
  # max(0,n0_erfasst*(ta - as.numeric(calcDf$Tag - startDate)+2 )/ta)
  
  
  
  calcNeuGesamtInfizierteBerechnet <- function(calcDf){
    
    max(0.1,(calcDf$ReduzierteRt-1)*calcDf$GesamtInfizierteBerechnet)
  }
  
  
  # Initialize the dataframe
  #US 30.03.20202 Startdatum fix gesetzt damit bei verändern des startdatums keine anderen berechnungsergebnisse entstehen
  startDate <- as.Date('2020-03-01', format="%Y-%m-%d")
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d"))
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
                   ReduzierteRt                      = 0,
                   MaxKhBerechnet                    = 0,
                   MaxIntBerechnet                   = 0,
                   
  )
  
  initCalcDf <- function(calcDf, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta, n0_erfasst, startDate, faktor_n_inf) {
    calcDf$ReduzierteRt <- calcReduzierung(calcDf, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
    
    calcDf$NeuGesamtInfizierteBerechnet<- calcNeuGesamtInfizierteBerechnet(calcDf)
    calcDf$NeuInfizierteBerechnet <- max(.1,calcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf)
    return(calcDf)
  }
  initCalcDf <- initCalcDf(calcDf, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta, n0_erfasst, startDate, faktor_n_inf)
  
  
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
      ReduzierteRt                      = 0,
      MaxKhBerechnet                    = 0,
      MaxIntBerechnet                   = 0,
      
    )
    
    # Reduzierung Rt (max. 3x)
    updatecalcDf$ReduzierteRt <- calcReduzierung(updatecalcDf, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
    updatecalcDf$GesamtInfizierteBerechnet <-  round(calcGesamtInfizierteBerechnet(tailCalcDf),digits = 0)
    updatecalcDf$NeuGesamtInfizierteBerechnet<- round(calcNeuGesamtInfizierteBerechnet(updatecalcDf), digits = 0)
    updatecalcDf$NeuInfizierteBerechnet <- round(max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf), digits = 0)
    updatecalcDf$ErfassteInfizierteBerechnet<- round(calcErfassteInfizierteBerechnet(tailCalcDf), digits = 0)
    
    #test <- add_row(test,  Tag = test$Tag[i], TaeglichReproduktionsRateRt = test$TaeglichReproduktionsRateRt[i], 
    #                     GesamtInfizierteBerechnet = test$GesamtInfizierteBerechnet[i], ReduzierteRt = test$ReduzierteRt[i])
    
    calcDf <- rbind(calcDf,updatecalcDf)
    
  }
  calcDf$ID <- seq.int(nrow(calcDf))
  
  #    Infiziert
  ende_inf <- ti+ta
  calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = ifelse(ID==1,n0_erfasst,
                                                                  rollapply(NeuInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) + RestanteilStartwert-NeuInfizierteBerechnet))
  # In Intensiv
  # Diese Formel sollte noch einmal ueberprueft werden
  beginn_intensiv <- dt_inf_kh + dt_kh_int
  ende_intensiv   <- dt_inf_kh + dt_kh_int + t_intensiv
  calcDf <- calcDf %>% mutate(IntensivBerechnet = round(kh_normal * kh_intensiv * (rollapply(NeuInfizierteBerechnet, ende_intensiv, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, beginn_intensiv, sum,align = "left", partial = TRUE ))), digits=0)
  
  # In KH
  beginn_kh <- dt_inf_kh
  ende_kh   <- dt_inf_kh + t_kh
  calcDf <- calcDf %>% mutate(KhBerechnet       = kh_normal * (rollapply(NeuInfizierteBerechnet, ende_kh, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, beginn_kh, sum,align = "left", partial = TRUE )))
  calcDf <- calcDf %>% mutate(KhBerechnet       = round(KhBerechnet-IntensivBerechnet),digits=0)
  
  
  df <- left_join(calcDf,r0_no_erfasstDf, by =c("Tag" = "MeldeDate"))
  
  return(df)
}