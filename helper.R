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




createLandkreisR0_no_erfasstDf <- function(df, input, session){
 # browser()
  if (input$regionSelected ==1) {
    filterVar = "Bund"
    df$Bund = "Deutschland"
    df <- df %>% rename("whichRegion" = "Bund")
    regionSelected <- "Bund"
    df$whichRegion ="Bund"  # set all of the entries to be taken care of
  } else if (input$regionSelected ==2) {
    filterVar = "Bundesland"
    df <- df %>% rename("whichRegion" = "Bundesland")
    regionSelected <- input$BundeslandSelected
  } else if(input$regionSelected ==3) {
    filterVar = "Landkreis"
    df <- df %>% rename("whichRegion" = "Landkreis")
    regionSelected <- input$LandkreiseSelected
  }
  
    df <- df %>% ungroup() %>%  filter(whichRegion == regionSelected)
 
    df <- df %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
      rename_at(vars(contains("Einwohner")), ~ "Einwohner" )
    
    nesteddf <- df %>% filter(MeldeDate <= as.Date('2020-03-16'))  %>% nest()

    # Define function to calculate regression
    expoModel <- function(df) {

      #TG: Regression zwischen Startdatum bzw. erstem Meldedatum und erster Massnahme. Annahme: bis dahin exponentieller Verlauf
      endDate <- as.Date(strptime(input$reduzierung_datum, format="%Y-%m-%d"))
      startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) 

      if (endDate > startDate) {
        df <- df %>% filter(MeldeDate >= startDate)
        df <- df %>% filter(MeldeDate <= endDate)
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
      endDate <- as.Date(strptime(input$reduzierung_datum, format="%Y-%m-%d"))  %>% unique() 

      if (endDate > startDate) {
        data <- data.frame(MeldeDate = seq(startDate, endDate,by =1))
      }    
      add_predictions(data, model)
      
    }
    nesteddfModel <- nesteddf %>% 
      mutate(model = map(data, expoModel),
             predictionsRegressionPeriode  = map2(model,data, predictLm),
             predictions  = map2(data, model, add_predictions), # https://r4ds.had.co.nz/many-models.html
             tidiedFit = map(model,tidy)) 
    
 #   predicteddfModel <- nesteddfModel %>% unnest(c(predictions), .sep ="_") %>% unnest(data) 
    
    # US 29.03.2020: Vereinfachuchung der der nest und unnest vorgänge

    predicteddfR0 <- nesteddfModel  %>% unnest(data) %>% unnest(c(predictions), .sep ="_")%>% unnest(c(predictionsRegressionPeriode), .sep ="_") %>% unnest(tidiedFit)
    
    r0Df <- predicteddfR0 %>% mutate(R0 = ifelse(term == "MeldeDate", 10^estimate, NA))
    
    r0Df <- r0Df  %>% select(-c(std.error, statistic)) %>% summarise_if(is.numeric, max, na.rm = TRUE) 
    
    # find regression value of first melde day, here is the problem, with first day of melde the results are useless
    #firstMeldeDay <- df$FirstMelde %>% min

    n0_erfasstDf <- predicteddfR0 %>% select(whichRegion, predictionsRegressionPeriode_MeldeDate, predictionsRegressionPeriode_pred)  %>% 
      filter(predictionsRegressionPeriode_MeldeDate == as.Date('2020-03-01')) %>% unique() %>% mutate(n0_erfasst = 10^predictionsRegressionPeriode_pred) 
  
    
    r0_no_erfasstDf <- cbind(r0Df ,n0_erfasstDf %>% select(whichRegion, n0_erfasst) ) %>% select(whichRegion, p.value, R0, n0_erfasst) 
    dfRoNo <- left_join(df , r0_no_erfasstDf) %>%    mutate( Ygesamt = Einwohner)


  dfRoNo
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
  reduzierung_datum	<- input$reduzierung_datum # Datum Reduktionsmassnahme
  reduzierung_rt <- 	input$reduzierung_rt/100 # Reduktion der Repr.rate/Tag
  
  # Ausgabe
  
  
  Y_inf_limit <- Ygesamt*ges_inf_rate/faktor_n_inf
  #tg Rt <- r0^(1/ta)
  Rt <- r0_no_erfasstDf$R0
  r0 <- Rt^ta
  
  # functions
  
  calcWirksamkeitReduktion <- function(calcDf, reduzierung_datum, ta) {
    # US 29.03.2020: avoid error message Warning in if (calcDf$Tag < reduzierung_datum) { :the condition has length > 1 and only the first element will be used
    calcDf <- calcDf %>% tail(1) 
    if (calcDf$Tag < reduzierung_datum){
  #browser()    
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
 
  #TG: ersetzen der festen Zahlen durch die Variablen aus Krankenhaus, Korrektur right->left 2.pointer
  #    Infiziert
  ende_inf <- ti+ta
  calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = ifelse(ID==1,n0_erfasst,
                                                                  rollapply(NeuInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) + RestanteilStartwert-NeuInfizierteBerechnet))
  # In KH
  beginn_kh <- dt_inf_kh
  ende_kh   <- dt_inf_kh + t_kh
  calcDf <- calcDf %>% mutate(KhBerechnet = kh_normal * (rollapply(NeuInfizierteBerechnet, ende_kh, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, beginn_kh, sum,align = "left", partial = TRUE )) )

  # In Intensiv
  # TG: hier muss noch ein Fehler stecken, wenn dt_kh_int=0 und kh_intensiv=1 (100%) ist, sind beide Kurven gleich
  #     wenn dt_kh_int = 5 ist, erwarten wir eigentlich einen Versatz von 5 Tagen? Das ist glaube ich ein Denkfehler
  #     Neuer Ansatz: die x% Intensiv m?ssen von den 
  #     Zusaetzlich m?ssen die Intensiv-Betten von den Normalen Betten abgezogen werden. 
  #     das kann getestet werden, wenn die Zahl nicht mehr ansteigt, also 100% reduzierung
  beginn_intensiv <- dt_inf_kh + dt_kh_int
  ende_intensiv   <- dt_inf_kh + dt_kh_int + t_intensiv
  calcDf <- calcDf %>% mutate(IntensivBerechnet = kh_normal * kh_intensiv * (rollapply(NeuInfizierteBerechnet, ende_intensiv, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, beginn_intensiv, sum,align = "left", partial = TRUE )))
  df <- left_join(calcDf,r0_no_erfasstDf, by =c("Tag" = "MeldeDate"))
  
 
  return(df)
}
