# function to calculate R0 and n0_erfasst
library("tictoc")
library(microbenchmark)
library(staTools)
source(file = "src/createDfBundLandKreis.R")
source(file = "src/optimizerLoopingR0N0.R")
source(file = "src/RechenkernSpeedUp.R")
source(file = "src/Rechenkern.R")
source(file = "src/optimizerGeneticAlgorithm.R")



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

df <- historyDfBundesLand
dfTotal <- df %>% rename("whichRegion" = "Bundesland") # %>% as_tibble %>%  mutate(a = optimizerLoopingR0N0 )
dfTotalNested <-  dfTotal %>% group_by(whichRegion) %>% nest

#######################  set optimizer function  ##########################
  source(file = "src/optimizerLoopingR0N0.R")
  optimizeFunction <- optimizerLoopingR0N0
 tic()
R0N0Optvalues <-  dfTotalNested %>% mutate(optimizedValues = map(data, optWrap, input))
 
save(R0N0Optvalues, file = "R0N0Optvalues.RData")
# #load("R0N0Optvalues.RData")
# toc()
 MetricDfLoopingR0N0 <- R0N0Optvalues %>% mutate(n0Opt = (optimizedValues[[1]][["n0Opt"]]) %>% as.numeric(),
                                                 R0Opt = (optimizedValues[[1]][["R0Opt"]]) %>% as.numeric(),
                                                 dfRechenKern = map2(R0Opt, n0Opt, calcBerechnetValues, input, startDate),
                                                 metric = map2_dbl(dfRechenKern, data, calcMetric),
                                                 SumAnzahlFall = data[[1]][["AnzahlFall"]] %>% sum,
                                                 OptimizerFunction = "LoopingR0N0")
 
 
 save(MetricDfLoopingR0N0, file = "data/MetricDfLoopingR0N0.RData")
load("data/MetricDfLoopingR0N0.RData")
MetricDfLoopingR0N0 %>% ungroup() %>% mutate(whichRegion= fct_reorder(whichRegion,metricLoopingR0N0))  %>%  
  ggplot(aes(whichRegion, metricLoopingR0N0, color = whichRegion)) + geom_point() + theme(legend.position = "none") +
  coord_flip() + labs(title = expression("Metric"), y = "Metric", x = "")

#######################  set optimizer function  ##########################
source(file = "src/optimizerGeneticAlgorithm.R")
optimizeFunction <- optimizerGeneticAlgorithm
tic()
optimizerGeneticAlgorithmRmsDf <-  dfTotalNested %>% mutate(optimizedValues = map(data, optWrap, input))
save(optimizerGeneticAlgorithmRmsDf, file = "optimizerGeneticAlgorithmRmsDf.RData")
#load("R0N0Optvalues.RData")
toc()  
MetricGeneticAlgorithmRmsDf <- optimizerGeneticAlgorithmRmsDf %>% mutate(n0Opt = (optimizedValues[[1]][["n0Opt"]]) %>% as.numeric(),
                                                                 R0Opt = (optimizedValues[[1]][["R0Opt"]]) %>% as.numeric(),
                                                                 dfRechenKern = map2(R0Opt, n0Opt, calcBerechnetValues, input, startDate),
                                                                 metric = map2_dbl(dfRechenKern, data, calcMetric),
                                                                 SumAnzahlFall = data[[1]][["AnzahlFall"]] %>% sum,
                                                                 OptimizerFunction = "optimizerGeneticAlgorithm")

save(MetricGeneticAlgorithmRmsDf, file = "data/MetricGeneticAlgorithmRmsDf.RData")
load("data/MetricGeneticAlgorithmRmsDf.RData")

MetricGeneticAlgorithmRmsDf %>% ungroup() %>% mutate(whichRegion= fct_reorder(whichRegion,metricGeneticAlgorithm))  %>%  
  ggplot(aes(whichRegion, metricGeneticAlgorithm, color = whichRegion)) + geom_point() + theme(legend.position = "none") +
  coord_flip() + labs(title = expression("optimizerGeneticAlgorithm Metric"), y = "Metric", x = "")




################### compare the two optimizers


compareOptimizerDf <- rbind(MetricDfLoopingR0N0, MetricGeneticAlgorithmRmsDf)
compareOptimizerDfLong <- compareOptimizerDf %>% pivot_longer(OptimizerFunction, names_to = "OptimizingFunction", values_to =  "Function")
compareOptimizerDfLong %>% ggplot(aes(whichRegion,R0Opt, color = Function)) +geom_point() + coord_flip() + 
  labs(title = ("Comparision optimized R0"))

compareOptimizerDfLong %>% ggplot(aes(whichRegion, metric, color = Function)) +geom_point() + coord_flip() + 
  labs(title = ("Comparision metric"))



#################### check MPE vs rms with GA  ######################

#######################  set optimizer function  ##########################
source(file = "src/optimizerGeneticAlgorithm.R")
optimizeFunction <- optimizerGeneticAlgorithm
tic()
optimizerGeneticAlgorithmMPE <-  dfTotalNested %>% mutate(optimizedValues = map(data, optWrap, input))
save(optimizerGeneticAlgorithmMPE, file = "optimizerGeneticAlgorithmMPE.RData")
#load("R0N0Optvalues.RData")
toc()  
MetricGeneticAlgorithmMpeDf <- optimizerGeneticAlgorithmMPE %>% mutate(n0Opt = (optimizedValues[[1]][["n0Opt"]]) %>% as.numeric(),
                                                                 R0Opt = (optimizedValues[[1]][["R0Opt"]]) %>% as.numeric(),
                                                                 dfRechenKern = map2(R0Opt, n0Opt, calcBerechnetValues, input, startDate),
                                                                 metric = map2_dbl(dfRechenKern, data, calcMetric),
                                                                 SumAnzahlFall = data[[1]][["AnzahlFall"]] %>% sum,
                                                                 OptimizerFunction = "optimizerGeneticAlgorithmMPE")

save(MetricGeneticAlgorithmMpeDf, file = "data/MetricGeneticAlgorithmMpeDf.RData")

################### compare MPE vs rms optimizers

#compareOptimizerDf <- left_join(MetricGeneticAlgorithmMpeDf, MetricGeneticAlgorithmRmsDf %>% select(-data), by = "whichRegion", suffix = c("_GA_MPE", "_GA_rms"))
#compareOptimizerDf <- left_join(compareOptimizerDf, MetricDfLoopingR0N0 %>% select(-data), by = "whichRegion", suffix = c("_GA", "_Loop"))
#
compareOptimizerDf <- rbind(MetricGeneticAlgorithmMpeDf, MetricGeneticAlgorithmRmsDf, MetricDfLoopingR0N0)


compareOptimizerDf %>% ungroup() %>% mutate(whichRegion= fct_reorder(whichRegion,R0Opt))  %>% 
  ggplot(aes(whichRegion,R0Opt, color = OptimizerFunction)) +geom_point() + coord_flip() + 
  labs(title = ("Comparision optimized R0"))

compareOptimizerDf %>%  ungroup() %>% mutate(whichRegion= fct_reorder(whichRegion, metric))  %>% 
  ggplot(aes(whichRegion, metric, color = OptimizerFunction)) +geom_point() + coord_flip() + 
  labs(title = ("Comparision metric"))



compareOptimizerUnnestDf <-  compareOptimizerDf %>% unnest(data) %>%  unnest(dfRechenKern, sep = "_")


compareOptimizerUnnestDf  %>% ggplot(aes(MeldeDate, sumAnzahlFallBundesland, color = OptimizerFunction)) + geom_point() + 
 geom_point(aes(Tag, ErfassteInfizierteBerechnet))  +
  facet_wrap(vars(whichRegion)) 

