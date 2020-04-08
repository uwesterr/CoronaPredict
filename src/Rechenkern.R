# Files calculated the predicted values 

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
  calcDf <- tibble(Tag                               = startDate,
                   TaeglichReproduktionsRateRt       = Rt,
                   AktuellInfizierteBerechnet        = n0_erfasst,
                   # RestanteilStartwert               = NA,
                   NeuInfizierteBerechnet            = NA,
                   ErfassteInfizierteBerechnet       = AktuellInfizierteBerechnet,
                   GesamtAktuellInfizierteBerechnet  = 0, #AktuellInfizierteBerechnet*faktor_n_inf,
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
  
  
  # Init the aktuell infizierte mit den nicht vorhandenen werten vor startdatum

  ende_inf <- ti + ta  
  start_inf = ti 
  Rt_start <- Rt
  # find day on which the first was case would have been reported with given Rt 
  offsetDay <- ceiling(log(n0_erfasst*faktor_n_inf,Rt)) # calculate the day when one case was there 
  Rt_start <- Rt
  calcInit <- data.frame(Tag = seq(startDate- offsetDay, startDate, by = 1)) %>% 
    mutate(indexBack = as.numeric(-(Tag - startDate)),
           TaeglichReproduktionsRateRt       = Rt_start + (Rt_start-1)*indexBack/Y_inf_limit,
           #           RestanteilStartwert               = NA,
           NeuInfizierteBerechnet            = NA,
           GesamtInfizierteBerechnet         = 0,
           GesamtAktuellInfizierteBerechnet  = 0,
           NeuGesamtInfizierteBerechnet      = NA,
           KhBerechnet                       = NA,
           IntensivBerechnet                 = 0,
           NeueToteBerechnet                 = 0,
           ToteBerechnet                     = 0,
           ReduktionAbDatum                  = 0,
           ReduzierteRt                      = 0,
           MaxKhBerechnet                    = 0,
           MaxIntBerechnet                   = 0)
  
  for (i in 1:nrow(calcInit)) {
    calcInit$ReduzierteRt[i] =  calcReduzierung(calcInit[i,], reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
    
  }
  
  calcInit$ReduzierteRt <- calcReduzierung(calcInit, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
  calcInit <- calcInit %>% mutate(GesamtInfizierteBerechnet = n0_erfasst*faktor_n_inf/ReduzierteRt^indexBack,
                                  ErfassteInfizierteBerechnet = GesamtInfizierteBerechnet / faktor_n_inf,
                                  GesamtAktuellInfizierteBerechnet = rollapply(GesamtInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) -
                                    rollapply(GesamtInfizierteBerechnet, start_inf, sum,align = "right", fill = NA, partial =TRUE),
                                  NeuGesamtInfizierteBerechnet = (ReduzierteRt-1)*GesamtAktuellInfizierteBerechnet,
                                  NeuInfizierteBerechnet = NeuGesamtInfizierteBerechnet/faktor_n_inf)
  
  
  ########################################################  Loop propagate infections over time ########################
  lengthOfTail <- 1
  calcDf <- calcInit
  
  
  calcTaeglichReproduktionsRateRt <- function(Rt, calcDf, Y_inf_limit) {
    Rt-(tailCalcDf$ErfassteInfizierteBerechnet*(Rt-1))/Y_inf_limit
  }
  
  # calcRestanteilStartwert <- function(tailCalcDf, n0_erfasst, ta, startDate, date) {
  #   
  #   max(0,n0_erfasst*(ta - as.numeric(date - startDate))/ta)
  
  # }
  # 
  
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
  StartGesamtAktuellInfizierteBerechnet  = calcDf$AktuellInfizierteBerechnet*faktor_n_inf
  StartReduzierteRt <- calcDf$ReduzierteRt
  #  browser() 
  index <- 0
  for (i in seq(startDate, endDate,by = 1)) {
    index <- index + 1
    tailCalcDf <- tail(calcDf,lengthOfTail)
    date <- tailCalcDf$Tag +1
    updatecalcDf <- tibble(
      Tag                               = tailCalcDf$Tag+1,
      TaeglichReproduktionsRateRt       = calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit),
      AktuellInfizierteBerechnet        = n0_erfasst,
      #RestanteilStartwert               = calcRestanteilStartwert(tailCalcDf, n0_erfasst, ta, startDate, date),
      NeuInfizierteBerechnet            = NA,
      ErfassteInfizierteBerechnet       = NA,
      GesamtAktuellInfizierteBerechnet  = 0,
      GesamtInfizierteBerechnet         = NA,
      NeuGesamtInfizierteBerechnet      = 0,
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
    updatecalcDf$ReduzierteRt <- calcReduzierung(updatecalcDf, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
    updatecalcDf$GesamtInfizierteBerechnet <-  round(calcGesamtInfizierteBerechnet(tailCalcDf),digits = 0)
    
    
    #    GesamtAktuellInfizierteBerechnet = rollapply(GesamtInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) -
    #      rollapply(GesamtInfizierteBerechnet, start_inf, sum,align = "right", fill = NA, partial =TRUE),
    #    
    # US 2020.04.07: since the history is looked at in calcDf wich is one day in the past, the look back needs to be one day less into history
    updatecalcDf$GesamtAktuellInfizierteBerechnet <- calcDf %>% filter((Tag <= updatecalcDf$Tag - ti+1) & (Tag > updatecalcDf$Tag - ende_inf+1 )) %>% 
      summarise(sum = sum(NeuGesamtInfizierteBerechnet)) %>% as.numeric()
    
    updatecalcDf$NeuGesamtInfizierteBerechnet<- round(calcNeuGesamtInfizierteBerechnet(updatecalcDf), digits = 0)
    
    updatecalcDf$NeuInfizierteBerechnet <- round(max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf), digits = 0)
    updatecalcDf$ErfassteInfizierteBerechnet<- round(calcErfassteInfizierteBerechnet(tailCalcDf), digits = 0)
    
    
    calcDf <- bind_rows(calcDf,updatecalcDf)
    
  }
  calcDf$ID <- seq.int(nrow(calcDf))
  
  #    Infiziert
  ende_inf <- ti+ta
  
  # US 07.04.2020: following line was replaced since now the progression of the data is done for Gesamt values
  # calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = ifelse(ID==1,n0_erfasst,
  #                                                                  rollapply(NeuInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) -NeuInfizierteBerechnet))
  calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = GesamtAktuellInfizierteBerechnet/faktor_n_inf)
  
  
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
  
  #browser()
  df <- left_join(calcDf,r0_no_erfasstDf, by =c("Tag" = "MeldeDate"))
  return(df)
  
}
