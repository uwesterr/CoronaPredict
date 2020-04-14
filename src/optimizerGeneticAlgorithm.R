# optimizer using genetic algorithm
library(GA)

optimizerGeneticAlgorithm <- function(R0_start, dfRoNoOpt, n0_erfasst_start, input, startDate, df, resultDf, optsPara = list("iStep" =0.02, "kStep" = 0.1)) {


calcPredictionsForGaOptimization = function(R0Coefficient, n0Coefficient, R0_start, n0_erfasst_start, startDate) {
 
  #browser()    
      R0=R0_start*R0Coefficient
      dfRoNoOpt$R0<- 10^(R0)
 #     browser()
      n0_erfasst <- n0_erfasst_start*n0Coefficient
      dfRoNoOpt$n0_erfasst <- n0_erfasst
      dfRechenKern <-  isolate(Rechenkern(dfRoNoOpt, input, startDate))
      dfRechenKern <- dfRechenKern %>% filter(Tag  %in% df$MeldeDate)
      MPE <- MPE(dfRechenKern$ErfassteInfizierteBerechnet,df$SumAnzahl)
#      rms <- sqrt(mean((dfRechenKern$ErfassteInfizierteBerechnet-df$SumAnzahl)^2))
      return(-MPE)
} 

tic()
suggestions <- c(1.1, 1.0)
GA <- ga(type = "real-valued", 
         fitness =  function(x) calcPredictionsForGaOptimization(x[1], x[2], R0_start,  n0_erfasst_start, startDate),
         suggestions =suggestions,
         lower = c(1.0, 0.9), upper = c(1.2, 1.1), 
         popSize = 10, maxiter = 30, run = 5)
toc()
#browser()
# n0_erfasst_start = n0_erfasst_start)
# summary(GA)
# plot(GA)

R0Opt <- 10^(R0_start*GA@solution[[1]])
n0Opt <- n0_erfasst_start * GA@solution[[2]]

return(list("n0Opt" = n0Opt, "R0Opt" = R0Opt))

}