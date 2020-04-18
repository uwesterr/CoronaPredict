# optimizer as imnplemented by thomas april 2020

optimizerLoopingR0N0 <- function(RkiDataWithR0N0, input, optsPara = list("iStep" =0.2, "kStep" = 0.1)) {
  #browser()
  # lShould be replaced by a real optimizer. 
  # with e.g. a simple levenberg-marquardt optimizer
  resultDf <- data.frame(RoLin = double(), n0_erfasst = double(), coefficient = double(),  rms = double())
  for (i in seq(1.0,1.2, by =optsPara$iStep)) {
    for (k in seq(0.9,1.1, by = optsPara$kStep)) {
      
      RkiDataWithR0N0$n0Opt=RkiDataWithR0N0$n0Start*k

      RkiDataWithR0N0$R0Opt=10^(log10(RkiDataWithR0N0$R0Start)*i)

      dfRechenKern <-  isolate(Rechenkern(RkiDataWithR0N0, input))
      dfRechenKern <- dfRechenKern %>% filter(!is.na(SumAnzahl))
     
      rms <- sqrt(mean((dfRechenKern$ErfassteInfizierteBerechnet-dfRechenKern$SumAnzahl)^2))
      
      resultDf <- rbind(resultDf, data.frame(RoLin = RkiDataWithR0N0$R0Opt, n0_erfasst = RkiDataWithR0N0$n0Opt, coefficient = i,  rms = rms))
    }
  }
  # browser()
  
  resultDf <- resultDf %>% arrange(rms) %>% head(1)
  n0Opt <- resultDf$n0_erfasst %>% as.numeric()
  R0Opt <- resultDf$RoLin  %>% as.numeric()
  return(list("n0Opt" = n0Opt, "R0Opt" = R0Opt))
}