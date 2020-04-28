

calcRKI_R <- function(tmpNeuInfizierte){
  r_rki <- 0*tmpNeuInfizierte
  max_tage = NROW(tmpNeuInfizierte)
  timeSpan <- 7
  if (max_tage>=2*timeSpan) {
    for (i in (2*timeSpan):max_tage) {
      # erste 4 tage 
      ViererGruppe1 <- mean(tmpNeuInfizierte[(i-2*timeSpan+1):(i-timeSpan)], na.rm = TRUE)
      # 2te 4 tage
      ViererGruppe2 <- mean(tmpNeuInfizierte[(i-timeSpan+1):i], na.rm = TRUE)
      if ((ViererGruppe1>0) && !is.nan(ViererGruppe1) && !is.nan(ViererGruppe2)) {r_rki[i] <- ViererGruppe2/ViererGruppe1}
    }
  }
  #print(r_rki)
  r_rki <- round(r_rki, digits = 3)
  return(r_rki)
}