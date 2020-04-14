

optimizerGeneticAlgorithmRedR0No <- function(R0_start, n0_erfasst_start, dfRoNoOpt, startDate, input) {
  # optimizer using genetic algorithm to optimize reduzierungsmaßnahmen und R0 n0
  # dfRoNoOpt should be dataframe starting with reduzierung_datum1
  dfRoNoOpt <- dfRoNoOpt%>% filter(MeldeDate >= input$reduzierung_datum1)
  
  calcPredictionsForGaOptimization = function(reduzierung_rt1, reduzierung_rt2, reduzierung_rt3, R0_start, n0_erfasst_start, startDate, dfRoNoOpt, input) {
    
  #      
    dfRoNoOpt$R0<- R0_start
     
    dfRoNoOpt$n0_erfasst <- n0_erfasst_start
    dfRoNoOpt <-  dfRoNoOpt %>% rename_at(vars(contains("sumAnzahlFall")), ~ "SumAnzahl" ) %>% 
      rename_at(vars(contains("Einwohner")), ~ "Einwohner" ) %>% 
      rename_at(vars(contains("sumTote")), ~ "sumTote" )
    inputForOptimization <- input# to make setting reduzierung_rtx easy and fast
    inputForOptimization$reduzierung_rt1 <- reduzierung_rt1
    inputForOptimization$reduzierung_rt2 <- reduzierung_rt2
    inputForOptimization$reduzierung_rt3 <- reduzierung_rt3
    inputForOptimization$dateInput[2] = dfRoNoOpt$MeldeDate %>% max() # set endDate to date of last MeldeDate
    dfRechenKern <-  isolate(Rechenkern(dfRoNoOpt, inputForOptimization, startDate))
    dfRechenKern <- dfRechenKern %>% filter(Tag  %in% dfRoNoOpt$MeldeDate)
    dfRoNoOpt <- dfRoNoOpt %>% filter(MeldeDate  %in% dfRechenKern$Tag)
    dfRechenKern$ErfassteInfizierteBerechnet %>% sum()
    res <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,dfRoNoOpt$SumAnzahl)
   # cat("res is :", res , "redu1 = ", reduzierung_rt1, "\n")
      res <-    rms <- sqrt(mean((log10(dfRechenKern$ErfassteInfizierteBerechnet)-log10(dfRoNoOpt$SumAnzahl))^2))
    return(-res)
  } 
  
  suggestions <- c( 0, 0, -20)
  GA <- ga(type = "real-valued", 
           fitness =  function(x) calcPredictionsForGaOptimization(x[1], x[2], x[3], R0_start,  n0_erfasst_start, startDate, dfRoNoOpt, input),
           suggestions =suggestions,
           lower = c(0, 0, -40), upper = c(60, 60, 30), 
           popSize = 10, maxiter = 30, run = 5, seed = 2020)

  input$reduzierung_rt1 <- GA@solution[[1]]
  input$reduzierung_rt2 <- GA@solution[[2]]
  input$reduzierung_rt3 <- GA@solution[[3]]
  return(list("input" = input))
  
}