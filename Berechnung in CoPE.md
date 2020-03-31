# Berechnung in CoPE Stand 31.03.2020
Das Dokument beruht auf der Software [link zum Github repo commit](https://github.com/uwesterr/CoronaPredict/commit/2adb04beb1648197c441eefcfc1ed6303cda2736)


## Ermittlung von no und RT

Die beiden Werte für den Start der Vorhersageberechnung ist die Reproduktionsrate zum Zeitpunkt `startDate` und die Anzahl der Infizierten zum gleichen Zeitpunkt gemäß der Regression

$$SumAnzahl=10^{MeldeDate}+ 10^{n0\_erfasst}$$

- n_0: Anzahl erfasster Infizierter am Beginn  startDate [n0_erfasst]
- Rt: Reproduktionsrate pro Tag [R0]


```r
  # Define function to calculate regression
  expoModel <- function(df) {
    
    #TG: Regression zwischen Startdatum bzw. erstem Meldedatum und erster Massnahme. Annahme: bis dahin exponentieller Verlauf
    #    Wenn Start-Date > erste Massnahme ist: Ausnahme --> 10 Tage nach Start
    endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))
    startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) 
    
    if (endDate > startDate) {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= endDate)
    } else {
      df <- df %>% filter(MeldeDate >= startDate)
      df <- df %>% filter(MeldeDate <= startDate+10)
    }
    
    #TG durch oben ersetzt: df <- df %>% filter(MeldeDate >= FirstMelde)
    
    lm(log10(SumAnzahl) ~ MeldeDate, data = df)
  }
  
  
  predictLm <- function( model, data){
    #browser()
    #TG ersetzt durch unten: startDate <- data$FirstMelde %>% unique()
    #TG ersetzt durch unten: endDate <- as.Date('2020-03-16')  
    
    #TG: Wie in expoModel, wieso 2x ? habe ich noch nicht richtig verstanden
    startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) %>% unique()                      
    endDate <- as.Date(strptime(input$reduzierung_datum1, format="%Y-%m-%d"))  %>% unique() 
    
    if (endDate <= startDate) {
      endDate <- startDate+10
    } 
    
    data <- data.frame(MeldeDate = seq(startDate, endDate,by =1))
    add_predictions(data, model)
    
  }
  nesteddfModel <- nesteddf %>% 
    mutate(model = map(data, expoModel),
           predictionsRegressionPeriode  = map2(model,data, predictLm),
           predictions  = map2(data, model, add_predictions), # https://r4ds.had.co.nz/many-models.html
           tidiedFit = map(model,tidy)) 
    
``` 

Die Regression berechnet 

- n: Anzahl erfasster Infizierter am Beginn  startDate [n0_erfasst]
- Rt: Reproduktionsrate pro Tag [R0]

Die Werte sind in `tidiedFit` abgelegt.

Diese Werte werden an die Funktion `Rechenkern <- function(r0_no_erfasstDf, input)` übergeben

## Berechnung der Vorhersagewerte
In der Funktion `Rechenkern <- function(r0_no_erfasstDf, input)` werden die Vorhersagewerte für den Zeitraum 

```r 
  startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d"))
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
  calcDf <- tibble(Tag                     = startDate,
                   TaeglichReproduktionsRateRt       = Rt,
                   AktuellInfizierteBerechnet        = n0_erfasst,
                   RestanteilStartwert               = NA,
                   NeuInfizierteBerechnet            = NA,
                   ErfassteInfizierteBerechnet       = AktuellInfizierteBerechnet,
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

```r 
  startDate <- as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")) # Datum Beginn
  endDate <- as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")) # Datum Ende
 for (i in seq(startDate, endDate,by = 1)) {
    tailCalcDf <- tail(calcDf,lengthOfTail)
    date <- tailCalcDf$Tag +1
    updatecalcDf <- tibble(
      Tag                               = tailCalcDf$Tag+1,
      TaeglichReproduktionsRateRt       = calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit), # Rt-(tailCalcDf$ErfassteInfizierteBerechnet*(Rt-1))/Y_inf_limit
      AktuellInfizierteBerechnet        = n0_erfasst,
      RestanteilStartwert               = calcRestanteilStartwert(tailCalcDf, n0_erfasst, ta, startDate, date), #max(0,n0_erfasst*(ta - as.numeric(date - startDate) )/ta)
      NeuInfizierteBerechnet            = NA,
      ErfassteInfizierteBerechnet       = NA,
      GesamtInfizierteBerechnet         = NA,
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

```r
updatecalcDf$GesamtInfizierteBerechnet <-  round(calcGesamtInfizierteBerechnet(tailCalcDf), digits = 0)
calcDf$GesamtInfizierteBerechnet+calcDf$NeuGesamtInfizierteBerechnet
```

#### Update `NeuGesamtInfizierteBerechnet

```r
updatecalcDf$NeuGesamtInfizierteBerechnet<- round(calcNeuGesamtInfizierteBerechnet(updatecalcDf), digits = 0)
 max(0.1,(calcDf$ReduzierteRt-1)*calcDf$GesamtInfizierteBerechnet)
```

#### Update `NeuInfizierteBerechnet

updatecalcDf$NeuInfizierteBerechnet <- round(max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf), digits = 0)

#### Update ErfassteInfizierteBerechnet

```r
 updatecalcDf$ErfassteInfizierteBerechnet<- round(calcErfassteInfizierteBerechnet(tailCalcDf), digits = 0) 
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
  ende_inf <- ti+ta
  calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = ifelse(ID==1,n0_erfasst,
                                                                  rollapply(NeuInfizierteBerechnet, ende_inf, sum,align = "right", fill = NA, partial =TRUE) + RestanteilStartwert-NeuInfizierteBerechnet))
``` 

Der Code entspricht:
 
$$AktuellInfizierteBerechnet = \sum_{i}^{i-ende\_inf}{NeuInfizierteBerechnet} + RestanteilStartwert-NeuInfizierteBerechnet$$


#### Berechnen von `IntensivBerechnet`

```r
  # In Intensiv
  # Diese Formel sollte noch einmal ueberprueft werden
  beginn_intensiv <- dt_inf_kh + dt_kh_int
  ende_intensiv   <- dt_inf_kh + dt_kh_int + t_intensiv
  calcDf <- calcDf %>% mutate(IntensivBerechnet = round(kh_normal * kh_intensiv * 
  (rollapply(NeuInfizierteBerechnet, ende_intensiv, sum,align = "right", partial = TRUE )- 
  rollapply(NeuInfizierteBerechnet, beginn_intensiv, sum,align = "left", partial = TRUE ))), digits=0)
```  


Der Code entspricht:
 
$$IntensivBerechnet = \sum_{dt\_inf\_kh + dt\_kh\_int}^{dt\_inf\_kh + dt\_kh\_int + t_intensiv}{NeuInfizierteBerechnet} + RestanteilStartwert-NeuInfizierteBerechnet$$



#### Berechnen von `KhBerechnet`

```r
  # In KH
  beginn_kh <- dt_inf_kh
  ende_kh   <- dt_inf_kh + t_kh
  calcDf <- calcDf %>% mutate(KhBerechnet = kh_normal * (rollapply(NeuInfizierteBerechnet, ende_kh, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, beginn_kh, sum,align = "left", partial = TRUE )))
  calcDf <- calcDf %>% mutate(KhBerechnet       = round(KhBerechnet-IntensivBerechnet),digits=0)
```

Der Code entspricht:
 
$$KhBerechnet = \sum_{beginn\_kh}^{ende\_kh}{NeuInfizierteBerechnet} + RestanteilStartwert-NeuInfizierteBerechnet - IntensivBerechnet$$



