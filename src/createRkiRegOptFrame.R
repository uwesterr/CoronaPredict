# function to calculate R0 and n0_erfasst

createRkiRegOptFrame <- function(RkiDataWithSumsNested, regionSelected, input,optimizeFunction = optimizerLoopingR0N0, ...  ){
   #browser()
  indexEntitiy <- which(RkiDataWithSumsNested$whichRegion == regionSelected)
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
    RkiDataWithSumsNested$NotEnoughDataFlag[indexEntitiy] <- 1
    RkiDataWithSumsNested$R0Opt[indexEntitiy] <- 1.32
    RkiDataWithSumsNested$n0Opt[indexEntitiy] <- 165*min(df_org$Einwohner)/83000000
    RkiDataWithSumsNested$RegStartDate[indexEntitiy] <- startDate
    
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
library(tictoc)
source(file = "Rechenkern.R")
source(file = "createLandkreisR0_no_erfasstDf.R")
source(file = "createDfBundLandKreis.R")
source(file = "optimizerLoopingR0N0.R")
source(file = "helperForCovid19.R")
load("../data/inputExample.RData")
input <- isolate(reactiveValuesToList(inputExample))
load("../data/createDfBundLandKreisOutput.RData")
tictoc::tic()
RkiDataWithRoNoOpimized <- RkiData # create new dataframe to which the results of the R0n0 optimizsation will be added
for (regionSelected in RkiDataWithRoNoOpimized$whichRegion) {
print(regionSelected)
tmp <-   createRkiRegOptFrame(RkiDataWithRoNoOpimized, regionSelected, input, optimizeFunction = optimizerLoopingR0N0 )
regionSelectedDf <- tmp[["RkiDataWithSumsNested"]] %>% filter(whichRegion == regionSelected)

RkiDataWithRoNoOpimized[match(regionSelectedDf$whichRegion, RkiDataWithRoNoOpimized$whichRegion), ] <- regionSelectedDf

}
toc()
save(RkiDataWithRoNoOpimized, file = "../data/R0n0OptimizedStep0.0120200418.RData")
