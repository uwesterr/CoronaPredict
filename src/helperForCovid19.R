# helper functions for shiny app Covid19


calcPredictionsForOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, R0Opt, n0Opt, startDate, rechenDf_nom, input) {
  #browser()
  inputForOptimization <- isolate(reactiveValuesToList(input)) # to make setting reduzierung_rtx easy and fast
  inputForOptimization$reduzierung_rt1 <- reduzierung_rt1
  inputForOptimization$reduzierung_rt2 <- reduzierung_rt2
  inputForOptimization$reduzierung_rt3 <- reduzierung_rt3
  
  dfRechenKern <- (Rechenkern(rechenDf_nom, inputForOptimization, startDate))
  metric <- calcMetric(dfRechenKern, rechenDf_nom)
  cat("metric: ", metric)
  return(-metric) # minus because GA maximizes
} 

calcMetric <- function(dfRechenKern, data){
  res <- left_join(data, dfRechenKern %>% select(c(Tag, ErfassteInfizierteBerechnet)), by = c("MeldeDate" = "Tag")) %>% filter(!is.na(ErfassteInfizierteBerechnet))
  #   (sum(log10(res$SumAnzahl) 
  #        - log10(res$ErfassteInfizierteBerechnet) )^2)/(nrow(data)-1)^0.5
  
 # metric <- MPE(log10(res$SumAnzahl), log10(res$ErfassteInfizierteBerechnet))
  metric <- (sum(log10(res$SumAnzahl)   - log10(res$ErfassteInfizierteBerechnet) )^2)/(nrow(data)-1)^0.5
}