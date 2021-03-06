# function to calculate R0 and n0_erfasst

createLandkreisR0_no_erfasstDf <- function(RkiDataWithSumsNested, regionSelected, vals, input,session,optimizeFunction = optimizerLoopingR0N0, ...  ){
  # browser()
  if (vals$Flag  == "Bundesland") {
    if(input$BundeslandSelected == "---"){input$BundeslandSelected == "Deutschland"}
    if(input$BundeslandSelected == "Deutschland"){
      regionSelected <- "Deutschland" 
    } else{
      
      regionSelected <- input$BundeslandSelected
    }
  } else if(vals$Flag  == "Landkreis") {
    regionSelected <- input$LandkreiseSelected
  }
  if(regionSelected =="---"){
    regionSelected <- "Deutschland" 
    updateSelectInput(session, "BundeslandSelected",  selected = "Deutschland")
    
  } 

# browser()
  if(regionSelected =="---"){

    regionSelected <- "Deutschland" 
    updateSelectInput(session, "BundeslandSelected",  selected = "Deutschland")
    
  }
 df <- RkiDataWithSumsNested %>%  filter(whichRegion == regionSelected) %>% unnest()

#  df <- df %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
#    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
#    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  
  startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()                      
  endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))  %>% unique() 

  # Gewährleiste, dass genügend Fälle in der Zeit bis zur Reduzierung liegen:
  mindest_faelle <- 12
  mindest_anzahl_faelle_start <- 10

  tmp <- df %>% filter(SumAnzahl >=  mindest_anzahl_faelle_start)
  startDate <- max(startDate, min(tmp$MeldeDate))  
  tmp <- df %>% filter(MeldeDate <=  endDate & AnzahlFall >0 )
  
  df_org <- df %>% mutate( Ygesamt = Einwohner)

  while ((length(unique(tmp$MeldeDate))<mindest_faelle) & (endDate<max(df$MeldeDate))) {
    endDate <- endDate +1
    tmp <- df %>% filter(MeldeDate <=  endDate & AnzahlFall >0 )
  }
  if ((length(unique(tmp$MeldeDate))<mindest_faelle) & (endDate>=max(df$MeldeDate))) {
    
    # Ersatzwerte
    #n0_erfasst_nom_min_max, R0_conf_nom_min_max, startDate
    n0Opt <- data_frame(n0_erfasst_nom = 165*min(df_org$Einwohner)/83000000)
    R0Opt <- data.frame(R0_nom= 1.32)
    startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()

    
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

    lmModel <-  lm(log10(SumAnzahl) ~ MeldeDate, data = df)
    
    # gives first reasonable fit
    R0_start <- lmModel[["coefficients"]][["MeldeDate"]]
    n0_erfasst_start <- lmModel %>% predict(data.frame(MeldeDate =startDate))

     indexEntitiy <- which(RkiDataWithSumsNested$whichRegion == regionSelected)
     RkiDataWithSumsNested$R0Start[indexEntitiy] <- 10^R0_start
     RkiDataWithSumsNested$n0Start[indexEntitiy] <- 10^n0_erfasst_start
     RkiDataWithSumsNested$RegStartDate[indexEntitiy] <- startDate
     RkiDataWithR0N0 <- RkiDataWithSumsNested %>%  filter(whichRegion == regionSelected) %>% unnest()
     
     res <- optimizeFunction(RkiDataWithR0N0, input, ...)
     #browser()
    RkiDataWithSumsNested$R0Opt[indexEntitiy] <- res$R0Opt
    RkiDataWithSumsNested$n0Opt[indexEntitiy] <- res$n0Opt

   
  }  
 #  browser()
  
  return(list( "RkiDataWithSumsNested" = RkiDataWithSumsNested, "regionSelected" = regionSelected))
  
  
}