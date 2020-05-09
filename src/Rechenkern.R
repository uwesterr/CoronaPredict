
# Files calculated the predicted values 

Rechenkern <- function(RkiDataWithR0N0, input) {
  # Betroffene
  # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
  Ygesamt	<- RkiDataWithR0N0$Einwohner %>% unique() # Gesamtmenge
  # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
  n0_erfasst <- 	RkiDataWithR0N0$n0Opt %>% unique() # Anzahl erfasster Infizierter am Beginn 
  beginn_date	<- RkiDataWithR0N0$RegStartDate # Datum Beginn
  
  
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
  # Datum i. Reduzierungsmassnahme
  # Reduzierungi der Reproduktionsrate/Tag
  red_data <- cbind(
    rbind(  red_datum1	<- input$reduzierung_datum1,   
            red_datum2	<- input$reduzierung_datum2,   
            red_datum3	<- input$reduzierung_datum3,   
            red_datum4	<- input$reduzierung_datum4,   
            red_datum5	<- input$reduzierung_datum5),
    rbind(  red_rt1     <- input$reduzierung_rt1/100,  
            red_rt2     <- input$reduzierung_rt2/100, 
            red_rt3     <- input$reduzierung_rt3/100,  
            red_rt4     <- input$reduzierung_rt4/100,  
            red_rt5     <- input$reduzierung_rt5/100)  
  )

  
  # Ausgabe
  Y_inf_limit <- Ygesamt*ges_inf_rate/faktor_n_inf
  # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
  Rt <- RkiDataWithR0N0$R0Opt %>% unique()
  r0 <- Rt^ta
  
  # functions
  
  # TG, 10.4. : Berechnung n-stufige Reduzierung Rt
  calcReduzierung <- function(df, red_data, ta) {
    # US 29.03.2020: avoid error message Warning in if (calcDf$Tag < red_datum) { :the condition has length > 1 and only the first element will be used
    rt_i <- df$TaeglichReproduktionsRateRt
    df <- df %>% tail(1) 
    for (i in 1:nrow(red_data)) {
      if (df$Tag < red_data[i,1]){
        WirksamkeitReduktion <- 0 }
      else {
        WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_data[i,1])+1)/ta)
        rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_data[i,2]
      }
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
  
  
  
  RegStartDate <- RkiDataWithR0N0$RegStartDate %>% unique()
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d"))
  ende_inf <- ti + ta  
  start_inf = ti 
  Rt_start <- Rt
  # browser()
  # find day on which the first was case would have been reported with given Rt 
  offsetDay <- ceiling(log(n0_erfasst*faktor_n_inf,Rt)) # calculate the day when one case was there 
  if(offsetDay< ende_inf+2){
  #  browser()
    offsetDay = ende_inf+2 # to avoid error message when low SumAnzahlFall
  }
  #browser()
  calcDf <- tibble(Tag = seq(RegStartDate-offsetDay, endDate, by = 1),
                   TaeglichReproduktionsRateRt       = Rt,
                   AktuellInfizierteBerechnet        = n0_erfasst,
                   # RestanteilStartwert               = NA,
                   NeuInfizierteBerechnet            = -1e7,
                   ErfassteInfizierteBerechnet       = AktuellInfizierteBerechnet,
                   GesamtAktuellInfizierteBerechnet  = -1e7, #AktuellInfizierteBerechnet*faktor_n_inf,
                   GesamtInfizierteBerechnet         = AktuellInfizierteBerechnet*faktor_n_inf,
                   NeuGesamtInfizierteBerechnet      = -1e7,
                   KhBerechnet                       = -1e7,
                   IntensivBerechnet                 = -1e7,
                   NeueToteBerechnet                 = -1e7,
                   ToteBerechnet                     = -1e7,
                   ReduktionAbDatum                  = -1e7,
                   ReduzierteRt                      = -1e7,
                   MaxKhBerechnet                    = -1e7,
                   MaxIntBerechnet                   = -1e7,
                   indexBack                         = -1e7
                   
  )
  
  
  # Init the aktuell infizierte mit den nicht vorhandenen werten vor startdatum
  


  for (day in seq(RegStartDate- offsetDay, RegStartDate, by = 1)) {
    
    day = as.Date(day)
    index <- which(calcDf$Tag == day)
    calcDf[index,"indexBack"] <- as.numeric(-(day - RegStartDate))
    calcDf[index,"TaeglichReproduktionsRateRt"] <- Rt_start + (Rt_start-1)*calcDf$indexBack[index]/Y_inf_limit
    #browser()
    calcDf$ReduzierteRt[index] =  calcReduzierung(calcDf[index,], red_data, ta)
  }
  
  calcDf <- calcDf %>% mutate(GesamtInfizierteBerechnet = n0_erfasst*faktor_n_inf/ReduzierteRt^indexBack,
                              ErfassteInfizierteBerechnet = GesamtInfizierteBerechnet / faktor_n_inf,
                              GesamtAktuellInfizierteBerechnet = rollapply(GesamtInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) -
                                rollapply(GesamtInfizierteBerechnet, start_inf, sum,align = "right", fill = NA, partial =TRUE),
                              NeuGesamtInfizierteBerechnet = (ReduzierteRt-1)*GesamtAktuellInfizierteBerechnet,
                              NeuInfizierteBerechnet = NeuGesamtInfizierteBerechnet/faktor_n_inf)
  # browser()
  
  ########################################################  Loop propagate infections over time ########################
  
  calcTaeglichReproduktionsRateRt <- function(Rt, calcDf, Y_inf_limit) {
    Rt-(calcDf$ErfassteInfizierteBerechnet*(Rt-1))/Y_inf_limit
  }
  
  
  
  calcGesamtInfizierteBerechnet <- function(calcDf){
    
    calcDf$GesamtInfizierteBerechnet+calcDf$NeuGesamtInfizierteBerechnet
  }
  
  calcErfassteInfizierteBerechnet <- function(CalcDf){
    CalcDf$NeuInfizierteBerechnet   + CalcDf$ErfassteInfizierteBerechnet
  }
  
  #US 30.03.20202 Startdatum fix gesetzt damit bei verändern des startdatums keine anderen berechnungsergebnisse entstehen
  
  #startDate <- as.Date('2020-03-01', format="%Y-%m-%d")
  #TG wieder variabel gesetzt, damit Anpassung stimmt
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")) # Datum Ende

  for (dayOfCalculation in seq(RegStartDate+1, endDate,by = 1)) {
    dayOfCalculation = as.Date(dayOfCalculation)
    indexDay <- which(calcDf$Tag == dayOfCalculation)
    
    
    calcDf$Tag[indexDay] <- dayOfCalculation
    calcDf$TaeglichReproduktionsRateRt[indexDay] <- calcTaeglichReproduktionsRateRt(Rt, calcDf[indexDay-1,], Y_inf_limit)
    # browser()
    calcDf$ReduzierteRt[indexDay]  <- calcReduzierung(calcDf[indexDay,], red_data, ta)
    calcDf$GesamtInfizierteBerechnet[indexDay]  <-  round(calcGesamtInfizierteBerechnet(calcDf[indexDay-1,]),digits = 0)
    
    
    
    activeEndDay <- which(calcDf$Tag == dayOfCalculation - ti+1)
    activeStartDay <- which(calcDf$Tag ==  dayOfCalculation - ende_inf+2)
    #   calcDf$GesamtAktuellInfizierteBerechnet[indexDay] <- calcDf %>% filter((Tag <= dayOfCalculation - ti+1) & (Tag > dayOfCalculation - ende_inf+1)) %>% 
    #     summarise(sum = sum(NeuGesamtInfizierteBerechnet)) %>% as.numeric()
    
    
   
    calcDf$GesamtAktuellInfizierteBerechnet[indexDay] <- calcDf$NeuGesamtInfizierteBerechnet[activeStartDay:activeEndDay] %>% sum
    calcDf$NeuGesamtInfizierteBerechnet[indexDay]<- round(calcNeuGesamtInfizierteBerechnet(calcDf[indexDay,]), digits = 0)
    calcDf$NeuInfizierteBerechnet[indexDay]  <- round(max(.1,calcDf$NeuGesamtInfizierteBerechnet[indexDay]/faktor_n_inf), digits = 0)
    calcDf$ErfassteInfizierteBerechnet[indexDay] <- round(calcErfassteInfizierteBerechnet(calcDf[indexDay-1,]), digits = 0)
    
  }
  # browser()
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
  calcDf <- calcDf %>% mutate(NeueToteBerechnet = round(tod_rate* lag(NeuInfizierteBerechnet, td_tod, default = 0),digits=0)) %>% 
    mutate(ToteBerechnet = cumsum(NeueToteBerechnet)) %>% filter(Tag >= RegStartDate)
  
  ######## calculate all meldungen within week per 100.000 inhabitants
  calcDf <- calcDf  %>% arrange(Tag) %>% mutate(MeldeWithinWeekBerechnet = map_dbl(Tag, sumFallForWeekForecast, calcDf),
                                                                                                         MeldeWithinWeekPer100kInhabitantsBerechnet = MeldeWithinWeekBerechnet/Ygesamt*1e5 )
  
  # browser()
  df <- left_join(calcDf,RkiDataWithR0N0, by =c("Tag" = "MeldeDate")) %>% 
    select(Tag, TaeglichReproduktionsRateRt, ReduzierteRt, AktuellInfizierteBerechnet, NeuInfizierteBerechnet, ErfassteInfizierteBerechnet, 
           GesamtAktuellInfizierteBerechnet, GesamtInfizierteBerechnet, NeuGesamtInfizierteBerechnet, everything())
  return(df)
  
}
