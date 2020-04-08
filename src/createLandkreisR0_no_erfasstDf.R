# function to calculate R0 and n0_erfasst

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
  
  df_org <- df %>% mutate( Ygesamt = Einwohner)
  #browser()
  while ((length(unique(tmp$MeldeDate))<mindest_faelle) & (endDate<max(df$MeldeDate))) {
    endDate <- endDate +1
    tmp <- df %>% filter(MeldeDate <=  endDate & AnzahlFall >0 )
  }
  if ((length(unique(tmp$MeldeDate))<mindest_faelle) & (endDate>=max(df$MeldeDate))) {
    
    # Ersatzwerte
    #n0_erfasst_nom_min_max, R0_conf_nom_min_max, startDate
    n0_erfasst_nom_min_max <- data_frame(n0_erfasst_nom = 165*min(df_org$Einwohner)/83000000)
    R0_conf_nom_min_max <- data.frame(R0_nom= 1.32)
    startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()
    #browser()
    
    showModal(modalDialog(title = "Zu wenige Fallzahlen für eine gute Schätzung des Verlaufs", "Glücklicherweise sind in diesem Kreis bisher nur wenige an COVID 19 erkrankt. Hierdurch ist aber auch keine valide Zukunftsschätzung möglich.",  footer = modalButton("Ok")))
  } else 
  {
    # Ausreichende Fallzahlen. Lineare Regression und Optimierung
    # used only data until endDate
    
    if (endDate > startDate) {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= endDate)
    } else {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= startDate+10)
    }
    
    # Calculate regression and optimize daily reproduction rate Rt
    resultDf<- data.frame()
    dfRoNo <- df %>% mutate( Ygesamt = Einwohner)
    dfRoNoOpt <- dfRoNo
    lmModel <-  lm(log10(SumAnzahl) ~ MeldeDate, data = df)
    
    # gives first reasonable fit
    R0_start <- lmModel[["coefficients"]][["MeldeDate"]]
    n0_erfasst_start <- lmModel %>% predict(data.frame(MeldeDate =startDate))
    n0_erfasst_start <- 10^n0_erfasst_start
    #browser()
    
    # lousy trick to improve fit, which is not good due to the discrepancy between the assumed function (no^rt*tage) and the real 
    # implemented complex function. Should be replaced by a real optimizer. 
    # with e.g. a simple levenberg-marquardt optimizer, it should be possible to get a better representation in a reasonable amount of time as the 
    # start parameters are already quit good. 
    
    for (i in seq(1.0,1.2, by = 0.02)) {
      for (k in seq(0.9,1.1, by = 0.1)) {
        
        R0=R0_start*i
        dfRoNoOpt$R0<- 10^(R0)
        
        n0_erfasst <- n0_erfasst_start*k
        dfRoNoOpt$n0_erfasst <- n0_erfasst
        
        #browser()
        
        dfRechenKern <-  isolate(Rechenkern(dfRoNoOpt, input, startDate))
        dfRechenKern <- dfRechenKern %>% filter(Tag  %in% df$MeldeDate)
        rms <- sqrt(mean((dfRechenKern$ErfassteInfizierteBerechnet-df$SumAnzahl)^2))
        
        resultDf <- rbind(resultDf, data.frame(R0 = R0, RoLin = 10^R0, n0_erfasst = n0_erfasst, coefficient = i,  rms = rms))
      }
    }
    #browser()
    resultDf <- resultDf %>% arrange(rms) %>% head(1)
    n0_erfasst_nom_min_max <- data_frame(n0_erfasst_nom = resultDf$n0_erfasst %>% as.numeric())
    R0_conf_nom_min_max <- data.frame(R0_nom= resultDf$RoLin  %>% as.numeric())
    #browser()
  }  
  
  return(list(df_org, n0_erfasst_nom_min_max, R0_conf_nom_min_max, startDate))
}