

# Files to supply functions to other programs

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

#calcR0ConN0Conf <- function(lmModel, startDate) {
#  confidenceLevel <- 0.95
#  n0_erfasst_conf <- lmModel %>% predict(data.frame(MeldeDate =startDate), interval = "confidence", level = confidenceLevel)
#  n0_erfasst_nom_min_max <- 10^n0_erfasst_conf %>% as_tibble() %>% set_names("n0_erfasst_nom", "n0_erfasst_min", "n0_erfasst_max")
#  R0_nom <- 10^lmModel[["coefficients"]][["MeldeDate"]]
#  R0_min_max <- 10^confint(lmModel, level = confidenceLevel)
#  R0_conf_nom_min_max <- tibble(R0_nom = R0_nom, R0_min = R0_min_max[[2]], R0_max = R0_min_max[[4]] )
#  return(list(n0_erfasst_nom_min_max, R0_conf_nom_min_max))
#}


createLandkreisR0_no_erfasstDf <- function(df, historyDfBund, regionSelected, vals, input,session){
  #browser()
  if (vals$Flag  == "Bundesland") {
    if(input$BundeslandSelected == "---"){input$BundeslandSelected == "Deutschland"}
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
  } else if(vals$Flag  == "Landkreis") {
    filterVar = "Landkreis"
    df <- df %>% rename("whichRegion" = "Landkreis")
    regionSelected <- input$LandkreiseSelected
  }
  # browser()
  if(regionSelected =="---"){
    filterVar = "Bund"
    historyDfBund$Bund = "Deutschland"
    df <- historyDfBund %>% rename("whichRegion" = "Bund")
    regionSelected <- "Deutschland" 
    updateSelectInput(session, "BundeslandSelected",  selected = "Deutschland")
    
  }
  df <- df %>% ungroup() %>%  filter(whichRegion == regionSelected)
  
  df <- df %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  
  startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()                      
  endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))  %>% unique() 
  #browser()
  # Gewährleiste, dass genügend Fälle in der Zeit bis zur Reduzierung liegen:
  mindest_faelle <- 12
  mindest_anzahl_faelle_start <- 10
  tmp <- df %>% filter(SumAnzahl >=  mindest_anzahl_faelle_start)
  startDate <- max(startDate, min(tmp$MeldeDate))
  tmp <- df %>% filter(MeldeDate <=  endDate & AnzahlFall >0 )
  while ((length(unique(tmp$MeldeDate))<mindest_faelle) & (endDate<max(df$MeldeDate))) {
    endDate <- endDate +1
    tmp <- df %>% filter(MeldeDate <=  endDate & AnzahlFall >0 )
  }
  if ((length(unique(tmp$MeldeDate))<mindest_faelle) & (endDate>=max(df$MeldeDate))) {
    showModal(modalDialog(title = "Zu wenige Fallzahlen für eine gute Schätzung des Verlaufs", "Glücklicherweise sind in diesem Kreis bisher nur wenige an COVID 19 erkrankt. Hierdurch ist aber auch keine valide Zukunftsschätzung möglich.",  footer = modalButton("Ok")))
  }
  #  TG: eigentlich sollte lm jetzt gar nicht ausgefuehrt werden sondern z.B. ersatzwert ausgegeben werden
  # browser()
  # used only data until endDate
  #browser()
  df_org <- df %>% mutate( Ygesamt = Einwohner)
  if (endDate > startDate) {
    df <- df %>% filter(MeldeDate >= startDate)
    df <- df %>% filter(MeldeDate <= endDate)
  } else {
    df <- df %>% filter(MeldeDate >= startDate)
    df <- df %>% filter(MeldeDate <= startDate+10)
  }
  
  # Define function to calculate regression
  expoModel <- function(df, startDate, endDate) {
    
    #TG: Regression zwischen Startdatum bzw. erstem Meldedatum und erster Massnahme. Annahme: bis dahin exponentieller Verlauf
    #    Wenn Start-Date > erste Massnahme ist: Ausnahme --> 10 Tage nach Start
    
    
    if (endDate > startDate) {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= endDate)
    } else {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= startDate+10)
    }
    
    #TG: Rt ist jetzt nicht mehr so einfach zu ermitteln, da keine einfache exponentielle Funktion mehr
    
    #lm(log10(SumAnzahl) ~ MeldeDate, data = df)
    
    lmModel <- lm(log10(SumAnzahl) ~ MeldeDate, data = df)
    
  } 
  
  resultDf<- data.frame()
  dfRoNo <- df %>% mutate( Ygesamt = Einwohner)
  dfRoNoOpt <- dfRoNo
  lmModel <-  lm(log10(SumAnzahl) ~ MeldeDate, data = df)
  index <-  0
  # browser()
  rmsValue =1e7
  for (i in seq(1.2,0.9, by = -0.01)) {
    index <- index + 1
    lmModelLoop <- lmModel
    lmModelLoop[["coefficients"]][["MeldeDate"]] <- lmModel[["coefficients"]][["MeldeDate"]]*i
    R0 <- lmModelLoop[["coefficients"]][["MeldeDate"]]
    n0_erfasst <- lmModelLoop %>% predict(data.frame(MeldeDate =startDate))
    
    dfRoNoOpt$n0_erfasst <- n0_erfasst
    dfRoNoOpt$R0<- 10^R0
    
    dfRechenKern <-  Rechenkern(dfRoNoOpt, input, startDate)
    dfRechenKern <- dfRechenKern %>% filter(Tag  %in% df$MeldeDate)
    rms <- sqrt(mean((dfRechenKern$ErfassteInfizierteBerechnet-df$SumAnzahl)^2))

    resultDf <- rbind(resultDf, data.frame(R0 = R0, RoLin = 10^R0, n0_erfasst = n0_erfasst, coefficient = i,  rms = rms))
    if (rms< rmsValue) {
      rmsValue <- rms
    } else {
      break
    }
    
  }
   browser()
  resultDf <- resultDf %>% arrange(rms) %>% head(1)
  n0_erfasst_nom_min_max <- data_frame(n0_erfasst_nom = resultDf$n0_erfasst %>% as.numeric())
  R0_conf_nom_min_max <- data.frame(R0_nom= resultDf$RoLin  %>% as.numeric())
  
  return(list(df_org, n0_erfasst_nom_min_max, R0_conf_nom_min_max, startDate))
}

createDfBundLandKreis <- function() {
  
  historyData <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
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


Rechenkern <- function(r0_no_erfasstDf, input, startDate) {
  
  # Betroffene
  # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
  Ygesamt	<- r0_no_erfasstDf$Einwohner %>% unique() # Gesamtmenge
  # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
  n0_erfasst <- 	r0_no_erfasstDf$n0_erfasst %>% unique() # Anzahl erfasster Infizierter am Beginn 
  beginn_date	<- startDate # Datum Beginn
  
  
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
  # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
  Rt <- r0_no_erfasstDf$R0 %>% unique()
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
    #max(0.1,(calcDf$ReduzierteRt-1)*calcDf$GesamtInfizierteBerechnet)
    #TG: Berechnung der neu infizierten nur aus den gesamten aktuell Infizierten und nicht aus den kumulierten Werten
    # macht sich erst spaeter bemerkbar
    max(1,(calcDf$ReduzierteRt-1)*calcDf$GesamtAktuellInfizierteBerechnet)
  }
  
  
  # Initialize the dataframe
  #US 30.03.20202 Startdatum fix gesetzt damit bei verändern des startdatums keine anderen berechnungsergebnisse entstehen
  
  #startDate <- as.Date('2020-03-01', format="%Y-%m-%d")
  #TG wieder variabel gesetzt, damit Anpassung stimmt
  startDate <- startDate
  
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d"))
  calcDf <- tibble(Tag                     = startDate,
                   TaeglichReproduktionsRateRt       = Rt,
                   AktuellInfizierteBerechnet        = n0_erfasst,
                   RestanteilStartwert               = NA,
                   NeuInfizierteBerechnet            = NA,
                   ErfassteInfizierteBerechnet       = AktuellInfizierteBerechnet,
                   GesamtAktuellInfizierteBerechnet  = AktuellInfizierteBerechnet*faktor_n_inf,
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
  #browser()
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
  
  #US 30.03.20202 Startdatum fix gesetzt damit bei verändern des startdatums keine anderen berechnungsergebnisse entstehen
  
  #startDate <- as.Date('2020-03-01', format="%Y-%m-%d")
  #TG wieder variabel gesetzt, damit Anpassung stimmt
  startDate <- startDate # Datum Beginn
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")) # Datum Ende
  
  
  index <- 0
  for (i in seq(startDate, endDate,by = 1)) {
    index <- index + 1
    tailCalcDf <- tail(calcDf,lengthOfTail)
    date <- tailCalcDf$Tag +1
    updatecalcDf <- tibble(
      Tag                               = tailCalcDf$Tag+1,
      TaeglichReproduktionsRateRt       = calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit),
      AktuellInfizierteBerechnet        = n0_erfasst,
      RestanteilStartwert               = calcRestanteilStartwert(tailCalcDf, n0_erfasst, ta, startDate, date),
      NeuInfizierteBerechnet            = NA,
      ErfassteInfizierteBerechnet       = NA,
      GesamtAktuellInfizierteBerechnet  = 0,
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
    # browser()
    # Reduzierung Rt (max. 3x)
    updatecalcDf$ReduzierteRt <- calcReduzierung(updatecalcDf, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
    updatecalcDf$GesamtInfizierteBerechnet <-  round(calcGesamtInfizierteBerechnet(tailCalcDf),digits = 0)
    updatecalcDf$NeuGesamtInfizierteBerechnet<- round(calcNeuGesamtInfizierteBerechnet(updatecalcDf), digits = 0)
    updatecalcDf$NeuInfizierteBerechnet <- round(max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf), digits = 0)
    updatecalcDf$ErfassteInfizierteBerechnet<- round(calcErfassteInfizierteBerechnet(tailCalcDf), digits = 0)
    
    #test <- add_row(test,  Tag = test$Tag[i], TaeglichReproduktionsRateRt = test$TaeglichReproduktionsRateRt[i], 
    #                     GesamtInfizierteBerechnet = test$GesamtInfizierteBerechnet[i], ReduzierteRt = test$ReduzierteRt[i])
    
    
    
    # build NeuGesamtInfizierteBerechnet for dates before startdate, because if we add up NeuGesamtInfizierteBerechnet to calculate 
    # GesamtAktuellInfizierteBerechnet we need for the beginning data from the past
    ende_inf <- ti+ta
    #browser()
    
    #if (nrow(calcDf) < ende_inf){
    #  NeuGesamtInfizierteBerechnetHistory = StartGesamtAktuellInfizierteBerechnet*(StartReduzierteRt-1)
    #  
    #  for(i in seq(i,nrow(calcDf))){
    #    NeuGesamtInfizierteBerechnetHistory = NeuGesamtInfizierteBerechnetHistory + StartGesamtAktuellInfizierteBerechnet*(StartReduzierteRt-1)^(i)
    #  }
    #} else {
    #  NeuGesamtInfizierteBerechnetHistory = 0
    #}
    # TG: Berechnung der NeuInfizierten vor Startdatum nach linearer Methode (eigentliches Problem ist aber die Extraktion von Rt
    # nicht aus GesamtErfasste sondern nur von aktuell Erfasste, gibt es aber so nicht!) Unterschied zu obiger Berechnung history ist marginal.
    if (nrow(calcDf) < ende_inf){
      NeuGesamtInfizierteBerechnetHistory = updatecalcDf$RestanteilStartwert*faktor_n_inf
    } else {
      NeuGesamtInfizierteBerechnetHistory = 0
    } 
    
    # TG: Ansatz die gesamt neu infizierten aus den GesamtAktuellInfizierten zu berechnen. Siehe hierzu auch geaenderte Funktion 
    
    # browser()  
    updatecalcDf$GesamtAktuellInfizierteBerechnet <-  NeuGesamtInfizierteBerechnetHistory +
      rollapply(calcDf$NeuGesamtInfizierteBerechnet, ende_inf, sum,
                align = "right", partial =TRUE) %>% tail(1)
    
    
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
  calcDf <- calcDf %>% mutate(IntensivBerechnet = round(kh_normal * kh_intensiv * (rollapply(NeuInfizierteBerechnet, ende_intensiv, sum,align = "right", partial = TRUE )- 
                                                                                     rollapply(NeuInfizierteBerechnet, beginn_intensiv, sum,align = "right", partial = TRUE ))), digits=0)
  
  # In KH
  beginn_kh <- dt_inf_kh
  ende_kh   <- dt_inf_kh + t_kh
  calcDf <- calcDf %>% mutate(KhBerechnet       = kh_normal * (rollapply(NeuInfizierteBerechnet, ende_kh, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, beginn_kh, sum,align = "right", partial = TRUE )))
  calcDf <- calcDf %>% mutate(KhBerechnet       = round(KhBerechnet-IntensivBerechnet),digits=0)
  
  # Verstorben
  
  calcDf <- calcDf %>% mutate(NeueToteBerechnet = round(tod_rate* lag(NeuInfizierteBerechnet, td_tod, default = 0),digits=0)) %>% mutate(ToteBerechnet = cumsum(NeueToteBerechnet))
  
  
  df <- left_join(calcDf,r0_no_erfasstDf, by =c("Tag" = "MeldeDate"))
  # browser()
  return(df)
  
}
