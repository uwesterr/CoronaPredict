# Berechnung in CoPE Stand 31.03.2020
Das Dokument beruht auf der Software [link zum Github repo commit](https://github.com/uwesterr/CoronaPredict/commit/2adb04beb1648197c441eefcfc1ed6303cda2736)


## Ermittlung von no und RT

Die beiden Werte für den Start der Vorhersageberechnung ist die Reproduktionsrate zum Zeitpunkt `startDate` und die Anzahl der Infizierten zum gleichen Zeitpunkt gemäß der Regression

$$SumAnzahl=10^{R\_t*MeldeDate}$$

- n_0: Anzahl erfasster Infizierter am Beginn  startDate [n0_erfasst]
- Rt: Reproduktionsrate pro Tag [R0]


```r
 startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()                      
  endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))  %>% unique() 
  #browser()
  # Gewährleiste, dass genügend Fälle in der Zeit bis zur Reduzierung liegen:
  mindest_faelle <- 12
  mindest_anzahl_faelle_start <- 10
  tmp <- df %>% filter(SumAnzahl >=  mindest_anzahl_faelle_start)
  startDate <- max(startDate, min(tmp$MeldeDate))  
  tmp <- df %>% filter(MeldeDate <=  endDate & AnzahlFall >0 )
  
  df_org <- df %>% mutate( Ygesamt = Einwohner)
  #browser()
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
    #browser()
    
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
    resultDf<- data.frame()
    dfRoNo <- df %>% mutate( Ygesamt = Einwohner)
    dfRoNoOpt <- dfRoNo
    lmModel <-  lm(log10(SumAnzahl) ~ MeldeDate, data = df)
    
    # gives first reasonable fit
    R0_start <- lmModel[["coefficients"]][["MeldeDate"]]
    n0_erfasst_start <- lmModel %>% predict(data.frame(MeldeDate =startDate))
    n0_erfasst_start <- 10^n0_erfasst_start
    res <- optimizerLoopingR0N0(R0_start, dfRoNoOpt, n0_erfasst_start, input, startDate, df, resultDf)
    
    n0Opt <- res$n0Opt
    R0Opt <- res$R0Opt

  }  

  return(list(df_org, n0Opt, R0Opt, startDate))
    
``` 

## The optimizer 
The optimizer is invoked by  `optimizerLoopingR0N0(R0_start, dfRoNoOpt, n0_erfasst_start, input, startDate, df, resultDf)`

A iterative approach for the optimizer is given below

```r
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
```


Die Regression berechnet 

- n: Anzahl erfasster Infizierter am Beginn  startDate [n0_erfasst]
- Rt: Reproduktionsrate pro Tag [R0]

Die Werte sind in `tidiedFit` abgelegt.

Diese Werte werden an die Funktion `Rechenkern <- function(r0_no_erfasstDf, input)` übergeben

## Einlesen der RKI date
Die RKI daten werden von dem Server "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson" eingelesen

```r

createDfBundLandKreis <- function() {
  
  historyData <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  historyDf <- historyData[["features"]][["properties"]]
  historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
  
  
  ## read population file from thonmas
  bundesLandPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
  landKreisPopulation <- read_excel("bundesland_landkreis_200326_2.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))
  # browser()
  BundFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBund <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, list(sum), na.rm = TRUE) %>% 
    mutate(sumAnzahlFallBund = cumsum(AnzahlFall), sumToteBund = cumsum(AnzahlTodesfall),
           BundIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% mutate(EinwohnerBund = bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland)) %>% unlist) 
  historyDfBund$FirstMelde <- BundFirstMeldung %>% unlist %>% as.Date()
  
  
  
  BundesLandFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Bundesland) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfBundesLand  <- historyDf %>% dplyr::ungroup() %>% group_by(Bundesland, MeldeDate)  %>% summarise_if(is.numeric, sum, na.rm = TRUE)  %>% 
    mutate(sumAnzahlFallBundesland = cumsum(AnzahlFall), sumToteBundesland = cumsum(AnzahlTodesfall),
           sumAnzahlFallBundesland = ifelse(sumAnzahlFallBundesland <1, 0.1,sumAnzahlFallBundesland),
           LandIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(BundesLandFirstMeldung) %>% left_join(bundesLandPopulation)
  
  
  
  
  LandkreisFirstMeldung  <- historyDf %>% filter(AnzahlFall>0) %>% dplyr::ungroup() %>% group_by(Landkreis) %>% summarise(FirstMelde = min(MeldeDate))
  historyDfLandkreis <- historyDf  %>% dplyr::group_by(Bundesland,Landkreis, MeldeDate)%>% dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)  %>%
    mutate(sumAnzahlFallLandkreis = cumsum(AnzahlFall), sumToteLandkreis = cumsum(AnzahlTodesfall),
           sumAnzahlFallLandkreis = ifelse(sumAnzahlFallLandkreis <1, 0.1,sumAnzahlFallLandkreis),
           KreisIndex = as.numeric(MeldeDate- min(MeldeDate))) %>% left_join(LandkreisFirstMeldung) %>%  left_join(landKreisPopulation)
  
  return(list(historyDfBund, historyDfBundesLand, historyDfLandkreis))

```

## Berechnung der Vorhersagewerte
In der Funktion `Rechenkern <- functionr0_no_erfasstDf, input, startDate)` werden die Vorhersagewerte für den Zeitraum 

```r 
  startDate 
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d"))
```  

 berechnet, als Input von der Regression werden folgende Werte verwendet

```r
# US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
Rt <- r0_no_erfasstDf$R0 %>% unique()
  
 # US 31.03.2020: use only one value, before the whole column was used this lead to a init CalcDf with many rows instead of one which could screw up the rollapply later on
n0_erfasst <- 	r0_no_erfasstDf$n0_erfasst %>% unique() # Anzahl erfasster Infizierter am Beginn 
```

### Initalisieren des dataframes

Zuerst wird die erste Zeile des dataframes berechnet

```r
  calcDf <- tibble(Tag                               = startDate,
                   TaeglichReproduktionsRateRt       = Rt,
                   AktuellInfizierteBerechnet        = n0_erfasst,
                   # RestanteilStartwert               = NA,
                   NeuInfizierteBerechnet            = NA,
                   ErfassteInfizierteBerechnet       = AktuellInfizierteBerechnet,
                   GesamtAktuellInfizierteBerechnet  = 0, #AktuellInfizierteBerechnet*faktor_n_inf,
                   GesamtInfizierteBerechnet         = AktuellInfizierteBerechnet*faktor_n_inf,
                   NeuGesamtInfizierteBerechnet      = NA,
                   KhBerechnet                       = NA,
                   IntensivBerechnet                 = 0,
                   NeueToteBerechnet                 = 0,
                   ToteBerechnet                     = 0,
                   ReduktionAbDatum                  = 0,
                   ReduzierteRt                      = 0,
                   MaxKhBerechnet                    = 0,
                   MaxIntBerechnet                   = 0,
                   
  )
```


#### Berechnung `ReduzierteRt`

```r
calcDf$ReduzierteRt <- 
  calcReduzierung <- function(df, red_datum1, red_rt1, red_datum2, red_rt2, red_datum3, red_rt3, ta) {
    # US 29.03.2020: avoid error message Warning in if (calcDf$Tag < red_datum) { :the condition has length > 1 and only the first element will be used
    rt_i <- df$TaeglichReproduktionsRateRt
    
    df <- df %>% tail(1) 
    if (df$Tag < red_datum1){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum1)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt1
    }
    
    if (df$Tag < red_datum2){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum2)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt2
    }
    
    if (df$Tag < red_datum3){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum3)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt3
    }
    rt_i
  }
```
#### Berechnung `NeuGesamtInfizierteBerechnet`

```r
calcDf$NeuGesamtInfizierteBerechnet<- 
calcNeuGesamtInfizierteBerechnet(calcDf)    
    max(0.1,(calcDf$ReduzierteRt-1)*calcDf$GesamtInfizierteBerechnet)
  }
  
```

#### Berechnung `NeuInfizierteBerechnet`

`calcDf$NeuInfizierteBerechnet <- max(.1,calcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf)`

### Schleife zur Berechnung der Werte für den Vorhersagezeitraum
In dieser Schleife werden die Werte für den Vorhersagezeitraum berechnet

Before we can use the loop we need to generate data for the past so that the `GesamtAktuellInfizierteBerechnet` can be calculated

```r
 Rt_start <- Rt
 offsetDay <- ceiling(log(n0_erfasst*faktor_n_inf,Rt)) # calculate the day when one case was there 
  calcInit <- data.frame(Tag = seq(startDate- offsetDay, startDate, by = 1)) %>% 
    mutate(indexBack = as.numeric(-(Tag - startDate)),
           TaeglichReproduktionsRateRt       = Rt_start + (Rt_start-1)*indexBack/Y_inf_limit,
           #           RestanteilStartwert               = NA,
           NeuInfizierteBerechnet            = NA,
           GesamtInfizierteBerechnet         = 0,
           GesamtAktuellInfizierteBerechnet  = 0,
           NeuGesamtInfizierteBerechnet      = NA,
           KhBerechnet                       = NA,
           IntensivBerechnet                 = 0,
           NeueToteBerechnet                 = 0,
           ToteBerechnet                     = 0,
           ReduktionAbDatum                  = 0,
           ReduzierteRt                      = 0,
           MaxKhBerechnet                    = 0,
           MaxIntBerechnet                   = 0)
  
  for (i in 1:nrow(calcInit)) {
    calcInit$ReduzierteRt[i] =  calcReduzierung(calcInit[i,], reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
    
  }
  
  calcInit$ReduzierteRt <- calcReduzierung(calcInit, reduzierung_datum1, reduzierung_rt1, reduzierung_datum2, reduzierung_rt2, reduzierung_datum3, reduzierung_rt3, ta)
  calcInit <- calcInit %>% mutate(GesamtInfizierteBerechnet = n0_erfasst*faktor_n_inf/ReduzierteRt^indexBack,
                                  ErfassteInfizierteBerechnet = GesamtInfizierteBerechnet / faktor_n_inf,
                                  GesamtAktuellInfizierteBerechnet = rollapply(GesamtInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) -
                                    rollapply(GesamtInfizierteBerechnet, start_inf, sum,align = "right", fill = NA, partial =TRUE),
                                  NeuGesamtInfizierteBerechnet = (ReduzierteRt-1)*GesamtAktuellInfizierteBerechnet,
                                  NeuInfizierteBerechnet = NeuGesamtInfizierteBerechnet/faktor_n_inf)
  
```

And then the first row of the loop can be calculated

```r 
  for (i in seq(startDate, endDate,by = 1)) {
    index <- index + 1
    tailCalcDf <- tail(calcDf,lengthOfTail)
    date <- tailCalcDf$Tag +1
    updatecalcDf <- tibble(
      Tag                               = tailCalcDf$Tag+1,
      TaeglichReproduktionsRateRt       = calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit),
      AktuellInfizierteBerechnet        = n0_erfasst,
      #RestanteilStartwert               = calcRestanteilStartwert(tailCalcDf, n0_erfasst, ta, startDate, date),
      NeuInfizierteBerechnet            = NA,
      ErfassteInfizierteBerechnet       = NA,
      GesamtAktuellInfizierteBerechnet  = 0,
      GesamtInfizierteBerechnet         = NA,
      NeuGesamtInfizierteBerechnet      = 0,
      KhBerechnet                       = NA,
      IntensivBerechnet                 = 0,
      NeueToteBerechnet                 = 0,
      ToteBerechnet                     = 0,
      ReduktionAbDatum                  = 0,
      ReduzierteRt                      = 0,
      MaxKhBerechnet                    = 0,
      MaxIntBerechnet                   = 0,
      
    )

```

#### TaeglichReproduktionsRateRt
Reproduction rate without any counter measures of day T0

$$Rt_i = Rt_{i-1} -\frac{ErfassteInfizierteBerechnet*(Rt_{i-1}-1)}{Y\_inf\_limit}$$

```r 
calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit)
 calcTaeglichReproduktionsRateRt <- function(Rt, calcDf, Y_inf_limit) {
    Rt-(tailCalcDf$ErfassteInfizierteBerechnet*(Rt-1))/Y_inf_limit
```    


#### Update `ReduzierteRt`


```r
updatecalcDf$ReduzierteRt <- calcReduzierung(updatecalcDf
 calcReduzierung <- function(df, red_datum1, red_rt1, red_datum2, red_rt2, red_datum3, red_rt3, ta) {
    # US 29.03.2020: avoid error message Warning in if (calcDf$Tag < red_datum) { :the condition has length > 1 and only the first element will be used
    rt_i <- df$TaeglichReproduktionsRateRt
    
    df <- df %>% tail(1) 
    if (df$Tag < red_datum1){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum1)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt1
    }
    
    if (df$Tag < red_datum2){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum2)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt2
    }
    
    if (df$Tag < red_datum3){
      WirksamkeitReduktion <- 0 }
    else {
      WirksamkeitReduktion <-min(1,(as.numeric(df$Tag - red_datum3)+1)/ta)
      rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt3
    }
    rt_i
  }

```

#### Update `GesamtInfizierteBerechnet`

$$GesamtInfizierteBerechnet_i = GesamtInfizierteBerechnet_{i-1} + NeuGesamtInfizierteBerechnet_{i-1}$$


```r
updatecalcDf$GesamtInfizierteBerechnet <-  round(calcGesamtInfizierteBerechnet(tailCalcDf), digits = 0)
calcDf$GesamtInfizierteBerechnet+calcDf$NeuGesamtInfizierteBerechnet
```

#### Update `GesamtAktuellInfizierteBerechnet`
$$ GesamtAktuellInfizierteBerechnet_i  = \sum_{i=-ti}^{i=-(ti+ta)}{NeuGesamtInfizierteBerechnet}$$

```r
  updatecalcDf$GesamtAktuellInfizierteBerechnet <- calcDf %>% filter((Tag <= updatecalcDf$Tag - ti+1) & (Tag > updatecalcDf$Tag - ende_inf+1 )) %>% 
      summarise(sum = sum(NeuGesamtInfizierteBerechnet)) %>% as.numeric()
```      

#### Update `NeuGesamtInfizierteBerechnet`

$$NeuGesamtInfizierteBerechnet_i = (ReduzierteRt_i-1)* GesamtAktuellInfizierteBerechnet_i$$

```r
updatecalcDf$NeuGesamtInfizierteBerechnet<- round(calcNeuGesamtInfizierteBerechnet(updatecalcDf), digits = 0)
max(1,(calcDf$ReduzierteRt-1)*calcDf$GesamtAktuellInfizierteBerechnet))
```

#### Update `NeuInfizierteBerechnet`

$$NeuInfizierteBerechnet_i = \frac{NeuGesamtInfizierteBerechnet_i}{faktor\_n\_inf} =
\frac{NeuGesamtInfizierteBerechnet_i}{Dunkelziffer}$$

```r
updatecalcDf$NeuInfizierteBerechnet <- round(max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf), digits = 0)
```

#### Update `ErfassteInfizierteBerechnet`

$$ErfassteInfizierteBerechnet_i = ErfassteInfizierteBerechnet_{i-1} + NeuInfizierteBerechnet_{i-1}$$ 

```r
 updatecalcDf$ErfassteInfizierteBerechnet<- round(calcErfassteInfizierteBerechnet(tailCalcDf), digits = 0) 
  tailCalcDf$NeuInfizierteBerechnet   + tailCalcDf$ErfassteInfizierteBerechnet
```
   
#### Combine update line with already stored values

```r
 calcDf <- rbind(calcDf,updatecalcDf)
```



```r
  ta	<- input$ta # Infektiosität  [tage]
  ti	<- input$ti # Inkubationszeit [tage]
```  

### Berechnungen basierend auf Vorhergesagten Infektion

Die berechneten Infiziertenzahlen werden genutzt um verschiedene weitere Vorhersagen zu berechnen


#### Berechnen von `AktuellInfizierteBerechnet`

```
calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = GesamtAktuellInfizierteBerechnet/faktor_n_inf)
``` 


#### Berechnen von `IntensivBerechnet`

```r
  # In Intensiv
  # Diese Formel sollte noch einmal ueberprueft werden
  beginn_intensiv <- dt_inf_kh + dt_kh_int
  ende_intensiv   <- dt_inf_kh + dt_kh_int + t_intensiv
  calcDf <- calcDf %>% mutate(IntensivBerechnet = round(kh_normal * kh_intensiv * 
  (rollapply(NeuInfizierteBerechnet, ende_intensiv, sum,align = "right", partial = TRUE )- 
  rollapply(NeuInfizierteBerechnet, beginn_intensiv, sum,align = "right", partial = TRUE ))), digits=0)
```  


Der Code entspricht:
 
$$IntensivBerechnet = kh\_normal * kh\_intensiv\sum_{dt\_inf\_kh + dt\_kh\_int}^{dt\_inf\_kh + dt\_kh\_int + t_intensiv}{NeuInfizierteBerechnet} $$



#### Berechnen von `KhBerechnet`

```r
  # In KH
  beginn_kh <- dt_inf_kh
  ende_kh   <- dt_inf_kh + t_kh
  calcDf <- calcDf %>% mutate(KhBerechnet = kh_normal * (rollapply(NeuInfizierteBerechnet, ende_kh, sum,align = "right", partial = TRUE )- 
  rollapply(NeuInfizierteBerechnet, beginn_kh, sum,align = "right", partial = TRUE )))
  calcDf <- calcDf %>% mutate(KhBerechnet       = round(KhBerechnet-IntensivBerechnet),digits=0)
```

Der Code entspricht:
 
$$KhBerechnet = \sum_{beginn\_kh}^{ende\_kh}{NeuInfizierteBerechnet} + RestanteilStartwert-NeuInfizierteBerechnet - IntensivBerechnet$$


# Calculated variables explained

## TaeglichReproduktionsRateRt
Reproduction rate without any counter measures of day T0

$$Rt_i = Rt_{i-1} -\frac{ErfassteInfizierteBerechnet*(Rt_{i-1}-1)}{Y\_inf\_limit}$$ 

## ReduzierteRt
Is TaeglichReproduktionsRateRt corrected by the Reduzierende Massnahmen

```r
rt_i <- rt_i-WirksamkeitReduktion * (rt_i-1) * red_rt1
```

## GesamtInfizierteBerechnet

Summe Gesamt infizierten of day T0-1
calcDf$GesamtInfizierteBerechnet+calcDf$NeuGesamtInfizierteBerechnet

$$GesamtInfizierteBerechnet_i = GesamtInfizierteBerechnet_{i-1} + NeuGesamtInfizierteBerechnet_{i-1}$$

## GesamtAktuellInfizierteBerechnet
Sum of all people **who can infect others** which they can do ti days after being infected for ta days

$$ GesamtAktuellInfizierteBerechnet_i  = \sum_{i=-ti}^{i=-(ti+ta)}{NeuGesamtInfizierteBerechnet}$$

## NeuGesamtInfizierteBerechnet
All people who are newly infected at day T0

$$NeuGesamtInfizierteBerechnet_i = (ReduzierteRt_i-1)* GesamtAktuellInfizierteBerechnet_i$$


## NeuInfizierteBerechnet
Newly positivly tested people

$$NeuInfizierteBerechnet_i = \frac{NeuGesamtInfizierteBerechnet_i}{faktor\_n\_inf} =
\frac{NeuGesamtInfizierteBerechnet_i}{Dunkelziffer}$$



## ErfassteInfizierteBerechnet

All people ever tested postive 

$$ErfassteInfizierteBerechnet_i = ErfassteInfizierteBerechnet_{i-1} + NeuInfizierteBerechnet_{i-1}$$

## AktuellInfizierteBerechnet


$$AktuellInfizierteBerechnet_i = \frac{GesamtAktuellInfizierteBerechnet_i}{faktor\_n\_inf}$$

## RKI variables explained
The RKI data are gathered with a JSON api request

```r
  historyData <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
  
  historyDf <- historyData[["features"]][["properties"]]
``` 
  

## Variables which are self explaing

"IdBundesland"    "Bundesland"      "Landkreis" "IdLandkreis" 
## Altersgruppe
The age of people coded as 
 "A15-A34"   "A35-A59"   "A60-A79"   "A80+"      "A00-A04"   "A05-A14"   "unbekannt"
 
 ## Geschlecht
 Gender coded as  "M"         "W"         "unbekannt"
 

## AnzahlFall

Cases reported on entity on given day **Meldedatum**

## AnzahlTodesfall

Deaths reported on entity on given day **Meldedatum**

## Meldedatum
Date of reported data coded like "2020-03-21T00:00:00.000Z"

## Datenstand

Date when data were generated on server e.g. 07.04.2020, 00:00 Uhr	
## NeuerFall TBD

The values seen are 0  1 -1

## NeuerTodesfall
TBD

# Variables derived from RKI values
Those variables are derived form the RKI values

## sumAnzahlFallxxx
Data is grouped and then for each entity xxx and Meldedatum the sum of Anzahlfall is calculated.
Entity  xxx can be:

- Bund
- Bundesland
- Landkreis


$$sumAnzahlFallxxx= \sum AnzahlFall$$

## sumTotexxx
Data is grouped and then for each entity xxx and Meldedatum the sum of AnzahlTodesfall is calculated.
Entity  xxx can be:

- Bund
- Bundesland
- Landkreis

$$sumTotexxx= \sum AnzahlTodesfall$$

## Einwohnerxxx

Data is read in from excel file "bundesland_landkreis_200326_2.xlsx"
Entity  xxx can be:

- Bundesland
- Landkreis

For the whole country the sum of EinwohnerBundesland is build

$$EinwohnerBund =  \sum EinwohnerBundesland$$
 




