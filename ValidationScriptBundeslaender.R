# function to calculate R0 and n0_erfasst
library("tictoc")
library(microbenchmark)
source(file = "src/createDfBundLandKreis.R")
source(file = "src/optimizerLoopingR0N0.R")
source(file = "src/RechenkernSpeedUp.R")
source(file = "src/Rechenkern.R")

load("data/inputExample.RData")

outpput <-  createDfBundLandKreis()
historyDfBund <- outpput[[1]]
historyDfBundesLand <- outpput[[2]]
historyDfLandkreis <- outpput[[3]]
input <- isolate(reactiveValuesToList(inputExample))
optimizeFunction <- optimizerLoopingR0N0

optWrap <- function(df, input) {
  df <- df %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
    rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
    rename_at(vars(contains("sumTote")), ~ "sumTote" )
  
  startDate <- as.Date("2020-03-01")                 
  endDate <- as.Date("2020-03-16") 
  
  
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
    res <- optimizeFunction(R0_start, dfRoNoOpt, n0_erfasst_start, input, startDate, df, resultDf, optsPara = list("iStep" =0.02, "kStep" = 0.1))
    
    n0Opt <- res$n0Opt
    R0Opt <- res$R0Opt
    dfRechenKern <- res$dfRechenKern
  }
  return(list("n0Opt" = n0Opt, "R0Opt" = R0Opt ))
}


calcBerechnetValues <- function(R0, n0, input, startDate){

dfRoNoOpt$R0<- R0
dfRoNoOpt$n0_erfasst <-n0
dfRechenKern <- (Rechenkern(dfRoNoOpt, input, startDate))

return(dfRechenKern) 
}


calcMetric <- function(dfRechenKern, data){
  res <- left_join(data, dfRechenKern, by = c("MeldeDate" = "Tag")) %>% filter(!is.na(ErfassteInfizierteBerechnet))
  (sum(log10(res$sumAnzahlFallBundesland) 
        - log10(res$ErfassteInfizierteBerechnet) )^2)/(nrow(data)-1)^0.5

}

#createLandkreisR0_no_erfasstDf <- function(df, historyDfBund, regionSelected, vals, input,session,optimizeFunction = optimizerLoopingR0N0, ...  ){
  df <- historyDfBundesLand
  dfTotal <- df %>% rename("whichRegion" = "Bundesland") # %>% as_tibble %>%  mutate(a = optimizerLoopingR0N0 )
  dfTotalNested <-  dfTotal %>% group_by(whichRegion) %>% nest
 tic()
#R0N0Optvalues <-  dfTotalNested %>% mutate(optimizedValues = map(data, optWrap, input))
 
#save(R0N0Optvalues, file = "R0N0Optvalues.RData")
 load("a.RData")
 toc()
 MetricDf <- R0N0Optvalues %>% mutate(n0Opt = (optimizedValues[[1]][["n0Opt"]]) %>% as.numeric(),
                   R0Opt = (optimizedValues[[1]][["R0Opt"]]) %>% as.numeric(),
                   dfRechenKern = map2(R0Opt, n0Opt, calcBerechnetValues, input, startDate),
                   metric = map2_dbl(dfRechenKern, data, calcMetric))
 
 MetricDf %>% ungroup() %>% mutate(whichRegion= fct_reorder(whichRegion,metric))  %>%  
   ggplot(aes(whichRegion, metric, color = whichRegion)) + geom_point() + theme(legend.position = "none") +
   coord_flip() + labs(title = expression("Metric"), y = "Metric", x = "")
                       
  
 
################## optimizer #################
 library(GA)
 
 calcPredictionsForOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, R0Opt, n0Opt, startDate) 
 {
   input$reduzierung_rt1 = reduzierung_rt1
   input$reduzierung_rt2 = reduzierung_rt2
   input$reduzierung_rt3 = reduzierung_rt3
  # browser()
   res <- calcBerechnetValues(R0Opt, n0Opt, input, startDate)
   metric <- calcMetric(res, MetricDf$data[[2]])
   cat("metric: ", metric)
return(-metric) # minus because GA maximizes
 } 
 
 tic()
 GA <- ga(type = "real-valued", 
          fitness =  function(x) calcPredictionsForOptimization(x[1], x[2], x[3], MetricDf$R0Opt[[1]],  MetricDf$n0Opt[[1]], startDate),
        
          lower = c(20, 30,-20 ), upper = c(40, 50, 20), 
          popSize = 10, maxiter = 30, run = 5)
 toc()
 # n0_erfasst_start = n0_erfasst_start)
 summary(GA)
 plot(GA)
# 
# 
# 
# 
# 
# 
# 
#   
# dfRoNoOpt$R0<- 1.3
# 
# dfRoNoOpt$n0_erfasst <-200
#
# source(file = "src/RechenkernSpeedUp.R")
#
# dfRechenKernSpeed <- isolate(Rechenkern(dfRoNoOpt, input, startDate)) 
# source(file = "src/Rechenkern.R")
# dfRechenKern <- isolate(Rechenkern(dfRoNoOpt, input, startDate))
#
#
# all.equal( dfRechenKernSpeed, dfRechenKern)
# source(file = "src/RechenkernSpeedUp.R")
# microbenchmark( 
#   
# 
#   dfRechenKernSpeed <- isolate(Rechenkern(dfRoNoOpt, input, startDate))
#   , times = 10)
# 
# source(file = "src/Rechenkern.R")
# microbenchmark( 
#   
#
#   dfRechenKern <- isolate(Rechenkern(dfRoNoOpt, input, startDate))
#   , times = 10)
#            
# #