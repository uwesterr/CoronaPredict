# optimizer as imnplemented by thomas april 2020

optimizerLoopingR0N0 <- function(R0_start, dfRoNoOpt, n0_erfasst_start, input, startDate, df, resultDf) {
  
  # lShould be replaced by a real optimizer. 
  # with e.g. a simple levenberg-marquardt optimizer
  
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
  n0Opt <- data_frame(n0_erfasst_nom = resultDf$n0_erfasst %>% as.numeric())
  R0Opt <- data.frame(R0_nom= resultDf$RoLin  %>% as.numeric())
  return(list("n0Opt" = n0Opt, "R0Opt" = R0Opt))
}