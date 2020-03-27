
# important ---------------------------------------------------------------

#based on covid19germany

# An R package to load, visualise and analyse daily updated data on the COVID-19 outbreak in Germany. This package exists to simplify data analysis and was developed in the context of the #WirvsVirus hackathon. https://github.com/nevrome/covid19germany 


# r environment -----------------------------------------------------------
if (!("shiny" %in% rownames(installed.packages()))) install.packages("shiny")
if (!("tidyverse" %in% rownames(installed.packages()))) install.packages("tidyverse")
if (!("lubridate" %in% rownames(installed.packages()))) install.packages("lubridate")
if (!("zoo" %in% rownames(installed.packages()))) install.packages("zoo")
if (!("jsonlite" %in% rownames(installed.packages()))) install.packages("jsonlite")
if (!("plotly" %in% rownames(installed.packages()))) install.packages("plotly")
if (!("readxl" %in% rownames(installed.packages()))) install.packages("readxl")
if (!("scales" %in% rownames(installed.packages()))) install.packages("scales")
if (!("tidyr" %in% rownames(installed.packages()))) install.packages("tidyr")
if (!("modelr" %in% rownames(installed.packages()))) install.packages("modelr")
if (!("DT" %in% rownames(installed.packages()))) install.packages("DT")

library(DT)
library(modelr)
library(tidyr)

library(jsonlite)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(plotly)
library(readxl)
library(scales)

source(file = "helper.R")
outpput <-  createDfBundLandKreis()
historyDfBund <- outpput[[1]]
historyDfBundesLand <- outpput[[2]]
historyDfLandkreis <- outpput[[3]]
# import data rki -------------------------------------------------------------

historyData <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")

historyDf <- historyData[["features"]][["properties"]]
historyDf$MeldeDate <- as.Date(historyDf$Meldedatum)
## read population file from thonmas
bundesLandPopulation <- read_excel("bundesland_landkreis.xlsx", "bundesland", col_names = c("Bundesland", "EinwohnerBundesland"))
landKreisPopulation <- read_excel("bundesland_landkreis.xlsx", "landkreis", col_names = c("Landkreis", "EinwohnerLandkreis"))


historyDf <- left_join(historyDf,bundesLandPopulation)
historyDf <- left_join(historyDf,landKreisPopulation)


EinwohnerDeutschland <- bundesLandPopulation %>% summarise(sum = sum(EinwohnerBundesland))


# Betroffene

Ygesamt	<- 83e6 # Gesamtmenge
n0_erfasst <- 	120 # Anzahl erfasster Infizierter am Beginn 
beginn_date	<- ymd(2020-03-01) # Datum Beginn



#Krankenhausaufenthalt
kh_normal	<- 4.50/100 # Anteil an aktuellen Infizierten [percent]
t_kh	<- 14 # Dauer [tage]
dt_inf_kh	<- 8 # Versatz nach Infektion [tage]
kh_intensiv	<- 25.00/100 #  Anteil Intensivstation [percent]
t_intensiv	<- 10 # Dauer Intensivstation [tage]
dt_kh_int	<- 1 # Versatz Krankenhaus - Intensivstation [tage]

# Expertenparameter für Infektionsverlauf


ges_inf_rate <- 	70/100 # Gesättige Infektionsrate [percent]
faktor_n_inf <- 	15 # Faktor der nicht erfassten Infizierten
ta	<- 10 # Dauer Ansteckbarkeit  [tage]
r0	<- 13 # Neuansteckung durch einen Infizierten
tod_rate <- 	2.00/100 # Sterblichkeit
td_tod <- 	8 # Dauer Infektion bis Tod  [tage]
reduzierung_datum	<- as.Date("2020-03-23") # Datum Reduktionsmassnahme
reduzierung_rt <- 	30/100 # Reduktion der Repr.rate/Tag

# Ausgabe


Y_inf_limit <- Ygesamt*ges_inf_rate/faktor_n_inf
Rt <- r0^(1/ta)



# functions

calcWirksamkeitReduktion <- function(calcDf, reduzierung_datum, ta) {
  if (calcDf$Tag < reduzierung_datum){
    
    WirksamkeitReduktion <- 0
    
  }  else {
    calcDf <- calcDf %>% tail(1)
    WirksamkeitReduktion <-min(1,(as.numeric(calcDf$Tag - reduzierung_datum)+1)/ta)
    
  }
  WirksamkeitReduktion
}


calcReduzierteRt <-  function(df){
  
  df <- df %>% tail(1)
  ReduzierteRt <- df$TaeglichReproduktionsRateRt-df$WirksamkeitReduktion * (df$TaeglichReproduktionsRateRt-1) * reduzierung_rt
  ReduzierteRt
}

# max(0,n0_erfasst*(ta - as.numeric(calcDf$Tag - startDate)+2 )/ta)



calcNeuGesamtInfizierteBerechnet <- function(calcDf){
  
  max(0.1,(calcDf$ReduzierteRt-1)*calcDf$GesamtInfizierteBerechnet)
}


# Initialize the dataframe
startDate <- as.Date('2020-03-01')
endDate <- as.Date('2020-05-31')
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
                 WirksamkeitReduktion              = 0,
                 ReduzierteRt                      = 0,
                 MaxKhBerechnet                    = 0,
                 MaxIntBerechnet                   = 0,
                 
)

initCalcDf <- function(calcDf, reduzierung_datum, ta, n0_erfasst, startDate, faktor_n_inf) {
  calcDf$WirksamkeitReduktion<- calcWirksamkeitReduktion(calcDf, reduzierung_datum, ta)  
  calcDf$ReduzierteRt<- calcReduzierteRt(calcDf)
  calcDf$NeuGesamtInfizierteBerechnet<- calcNeuGesamtInfizierteBerechnet(calcDf)
  calcDf$NeuInfizierteBerechnet <- max(.1,calcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf)
  return(calcDf)
}

initCalcDf <- initCalcDf(calcDf, reduzierung_datum, ta, n0_erfasst, startDate, faktor_n_inf)



lengthOfTail <- 1
calcDf <- initCalcDf


calcTaeglichReproduktionsRateRt <- function(Rt, calcDf, Y_inf_limit) {
  Rt-(tailCalcDf$ErfassteInfizierteBerechnet*(Rt-1))/Y_inf_limit
}

calcRestanteilStartwert <- function(tailCalcDf, n0_erfasst, ta, startDate, date) {
  
  max(0,n0_erfasst*(ta - as.numeric(date - startDate) )/ta)
}


calcGesamtInfizierteBerechnet <- function(calcDf){
  
  calcDf$GesamtInfizierteBerechnet+calcDf$NeuGesamtInfizierteBerechnet
}

calcErfassteInfizierteBerechnet <- function(tailCalcDf){
  tailCalcDf$NeuInfizierteBerechnet   + tailCalcDf$ErfassteInfizierteBerechnet
}

startDate <- as.Date('2020-03-01')
endDate <- as.Date('2020-05-31')


for (i in seq(startDate, endDate,by = 1)) {
  tailCalcDf <- tail(calcDf,lengthOfTail)
  date <- tailCalcDf$Tag +1
  updatecalcDf <- tibble(
    Tag                               = tailCalcDf$Tag+1,
    TaeglichReproduktionsRateRt       = calcTaeglichReproduktionsRateRt(Rt, tailCalcDf, Y_inf_limit),
    AktuellInfizierteBerechnet        = n0_erfasst,
    RestanteilStartwert               = calcRestanteilStartwert(tailCalcDf, n0_erfasst, ta, startDate, date),
    NeuInfizierteBerechnet            = NA,
    ErfassteInfizierteBerechnet       = NA,
    GesamtInfizierteBerechnet         = NA,
    NeuGesamtInfizierteBerechnet      = NA,
    KhBerechnet                       = NA,
    IntensivBerechnet                 = 0,
    NeueToteBerechnet                 = 0,
    ToteBerechnet                     = 0,
    ReduktionAbDatum                  = 0,
    WirksamkeitReduktion              = 0,
    ReduzierteRt                      = 0,
    MaxKhBerechnet                    = 0,
    MaxIntBerechnet                   = 0,
    
  )
  
  
  updatecalcDf$WirksamkeitReduktion<- calcWirksamkeitReduktion(updatecalcDf, reduzierung_datum, ta)  
  updatecalcDf$ReduzierteRt<- calcReduzierteRt(updatecalcDf)
  updatecalcDf$GesamtInfizierteBerechnet <-  calcGesamtInfizierteBerechnet(tailCalcDf)
  updatecalcDf$NeuGesamtInfizierteBerechnet<- calcNeuGesamtInfizierteBerechnet(updatecalcDf)
  updatecalcDf$NeuInfizierteBerechnet <- max(.1,updatecalcDf$NeuGesamtInfizierteBerechnet/faktor_n_inf)
  updatecalcDf$ErfassteInfizierteBerechnet<- calcErfassteInfizierteBerechnet(tailCalcDf)
  
  #test <- add_row(test,  Tag = test$Tag[i], TaeglichReproduktionsRateRt = test$TaeglichReproduktionsRateRt[i], 
  #                     GesamtInfizierteBerechnet = test$GesamtInfizierteBerechnet[i], ReduzierteRt = test$ReduzierteRt[i])
  
  calcDf <- rbind(calcDf,updatecalcDf)
  
}
calcDf$ID <- seq.int(nrow(calcDf))



calcDf <- calcDf %>% mutate(AktuellInfizierteBerechnet = ifelse(ID==1,n0_erfasst,
                                                                rollapply(NeuInfizierteBerechnet, 10, sum,align = "right", fill = NA, partial =TRUE) + RestanteilStartwert-NeuInfizierteBerechnet))

calcDf <- calcDf %>% mutate(KhBerechnet =   (rollapply(NeuInfizierteBerechnet, 22, sum,align = "right", partial = TRUE )- rollapply(NeuInfizierteBerechnet, 8, sum,align = "right", partial = TRUE )) *kh_normal)

ui <- 
  # widgets website https://shiny.rstudio.com/gallery/widget-gallery.html
  navbarPage("Covid19 Meldungen in Deutschland!",  position = c("fixed-bottom"),
             tabPanel("Einstellung und Ausgabe",
                      sidebarLayout( position ="left",
                                     
                                     
                                     sidebarPanel(
                                       
                                       wellPanel(
                                         radioButtons("regionSelected", label = h3("Region"),
                                                      choices = list("Deutschland" = 1, "Bundesländer" = 2, "Landkreise" = 3), 
                                                      selected = 2),
                                         selectInput("filterRegion", "Region to select", choices = "Baden-Württemberg", selected = NULL, multiple = FALSE,
                                                        selectize = TRUE, width = NULL, size = NULL)
                                       ),
                                      
                                         h4("Krankenhausaufenthalt"),   
                                         column(6,
                                                wellPanel(
                                         numericInput("kh_normal", label = "Anteil an aktuellen Infizierten [%]", value = 4.5),
                                         numericInput("t_kh", label = "Dauer", value = 14),
                                         numericInput("dt_inf_kh", label = "Versatz nach Infektion", value = 8))),
                                         column(6,
                                                wellPanel(
                                         numericInput("kh_intensiv", label = "Anteil Intensivstation [%]", value = 25),
                                         numericInput("t_intensiv", label = "Dauer Intensivstation", value = 10),
                                         numericInput("dt_kh_int", label = "Versatz Krankenhaus - Intensivstation", value = 1)
                                       )) ,  
                                         h3("Expertenparameter für den Infektionsverlauf"),   
                                         column(6,
                                                wellPanel(
                                         numericInput("ges_inf_rate", label = "Gesättige Infektionsrate [%]", value = 70),
                                         numericInput("faktor_n_inf", label = "Faktor der nicht erfassten Infizierten", value = 15),
                                         numericInput("ta", label = "Dauer Ansteckbarkeit", value = 10),
                                         numericInput("r0", label = "Neuansteckung durch einen Infizierten", value = 13))),
                                         column(6,
                                                wellPanel(
                                         numericInput("tod_rate", label = "Sterblichkeit [%]", value = 2),
                                         numericInput("td_tod", label = "Dauer Infektion bis Tod", value = 8),
                                         dateInput("reduzierung_datum", label = "Datum Reduktionsmassnahme [yyyy-mm-dd]", value = "2020-03-16"),
                                         numericInput("reduzierung_rt", label = "Reduktion der Repr.rate/Tag [%]", value = 30)
                                       )) , 
                                       
                
                                         h3("Einstellen der Darstellung") ,
                                       column(6,   
                                       wellPanel(
                                       dateRangeInput(inputId = "dateInput",
                                                        label = "Date",
                                                        start = as.Date('2020-03-01'),
                                                        end = as.Date('2020-05-01'),
                                                        min = as.Date('2020-02-01'),
                                                        max = as.Date('2020-07-01'),
                                                        format = "yyyy-mm-dd",
                                                        startview = "month",
                                                        weekstart = 1
                                         ))),
                                       column(6,
                                              wellPanel(
                                         radioButtons(inputId = "logyInput",
                                                      label = "Darstellung y-Achse",
                                                      choices = c("linear", "logarithmisch"),
                                                      selected =  "logarithmisch")
                                       )),
                                             
                                       
                                       
                                       # select axis transformation
                                       
                                       
                                       
                                       
                                       # wirvsvirus logo
                                       tags$img(src = "logo-admos.png",
                                                width = "200px", height = "100px"),
                                       tags$img(src = "Folie9.png",
                                                width = "100px", height = "100px"),
                                       
                                       #tags$p(class="header", checked=NA,
                                       #
                                       
                                       # adding the new div tag to the sidebar
                                       tags$div(class="header", checked=NA,
                                                list(
                                                  tags$p(
                                                    tags$a(href="https://github.com/uwesterr/CoronaPredict", "GitHub")),
                                                  
                                                  
                                                  
                                                  HTML(paste("Data accessed on",
                                                             Sys.time(),
                                                             tags$a(href="https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson",
                                                                    "from RKI"))))
                                       )
                                     ), # end sidebar panel
                                     mainPanel(
                                       h2("Rechenmodel Verlauf Covid19 Infektionen und deren Auswirkung"),
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId ="Kumuliert"), plotOutput(outputId ="Verlauf"))
                                       ),
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId ="Krankenhaus"), plotOutput(outputId ="Reproduktionsrate"))
                                       )       
                                       
                                     ) # end main panel
                      )
             ),
             tabPanel("Ergebnis",
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        
                      )
             ),
             
             tabPanel("Literatur",
                      tags$head(
                        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                      ),
                      
                      # Show a plot of the generated distribution
                      fluidRow(
                        h1("Eingabe der für die Berechnung notwendigen Daten"),              
                        column(4,
                               wellPanel(
                                 h3("Eingabe der für die Berechnung notwendigen Daten"),   
                                 
                                 hr(),
                                 numericInput("num", label = "Numeric input", value = 1)
                               )       
                        ),
                        column(4,
                               wellPanel(
                                 sliderInput("asdf", " of observations:",  
                                             min = 1, max = 1000, value = 500)
                               )       
                        )
                      ),
                      
                      fluidRow(
                        h1("Eingabe zur Steuerung der Anzeige"), 
                        
                        column(4,
                               wellPanel(
                                 sliderInput("asdf1", " of observations:",  
                                             min = 1, max = 1000, value = 500),
                                 p("The checkbox group controls the select input"),
                                 checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                                                    c("Item A", "Item B", "Item C")),
                                 selectInput("inSelect", "Select input",
                                             c("Item A", "Item B", "Item C")),
                                 
                               )       
                        )
                      )
                      
                      
             )
  )


server <- function(input, output, session) {
  dataPred <- reactive({
    
    calcDf %>% filter(Tag >=as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")),
                      Tag <=as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")))
   
  })
  rv <- reactiveValues()
  rv$run2 <- 0
  observe({
    updateSelectInput(
      session,
      "filterRegion",label = paste("Select input label"),
      
      if (input$regionSelected ==1) {
        
        choices = "Deutschland" 
        
      } else if (input$regionSelected ==2) {
        
        choices = historyDf %>% select(Bundesland) %>% unique() %>% .[[1]]
        
      } else if(input$regionSelected ==3) {
        
        choices = historyDf %>% select(Landkreis) %>% unique() %>% .[[1]]
      }
      
    )
  })
  
  rkiAndPredictData <- reactive({
      
      if (input$regionSelected ==1) {
      
        df <- historyDf %>% group_by(MeldeDate) %>% summarise_if(is.numeric, sum, na.rm = TRUE) 
      
    } else if (input$regionSelected ==2) {
      
      r0_no_erfasstDf <- createBundesLandR0_no_erfasstDf(historyDfBundesLand, input)
      
    } else if(input$regionSelected ==3) {
      
      df  <- historyDf %>% filter(Landkreis == input$filterRegion) %>% group_by(MeldeDate) %>% summarise_if(is.numeric, sum, na.rm = TRUE) 
    }
   df <-  Rechenkern(r0_no_erfasstDf ,input)
  })  
 
  color1 = 'blue'
  color2 = 'green'
  
  output$Kumuliert <- renderPlot({
    

    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = ErfassteInfizierteBerechnet, color = "Erfasste Infizierte berechnet")) + geom_line() + geom_point(data = rkiAndPredictData(), aes(x = Tag, y = sumAnzahlFall, color = "Erfasste Infizierte")) +
      scale_x_date(labels = date_format("%m-%Y")) + labs(title = "Kumulierte Infizierte", x = "Datum [mm-dd]", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Erfasste Infizierte berechnet' = color1,
                                                           'Erfasste Infizierte' = color2)) +
      labs(color = 'Daten')
    

    if(logy){
      p <- p +  scale_y_log10(label = label_number_si())
      
    } else {
      p
      
    }
    p
    
  })
  
  output$Verlauf <- renderPlot({
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = AktuellInfizierteBerechnet, color ="Aktuell Infizierte berechnet")) + geom_line() + geom_line(aes(y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) +
      scale_x_date(labels = date_format("%m-%Y")) + labs(title = "Verlauf Infizierte", x = "Datum [mm-dd]", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Aktuell Infizierte berechnet' = color1,
                                                           'Neu Infizierte berechnet' = color2)) +
        labs(color = 'Daten')
      
      
      
      if(logy){
        p <- p +  scale_y_log10(label = label_number_si())
        
      } else {
        p
        
      }
      p
      
  })  
  
  output$Krankenhaus <- renderPlot({
   # browser()
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    #browser()
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = KhBerechnet)) + geom_line(aes(color ="KH berechnet")) + geom_line(aes(y = IntensivBerechnet, color ="Intensiv berechnet")) +
      scale_x_date(labels = date_format("%m-%Y")) + labs(title = "Plätze in Krankenhaus / Intensivstation", x = "Datum [mm-dd]", y = "Anzahl",
                                                        caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                          'KH berechnet' = color1,
                                                          'Intensiv berechnet' = color2)) +
        labs(color = 'Daten')
    
      if(logy){
        p <- p +  scale_y_log10(label = label_number_si())
        
      } else {
        p
        
      }
      p
      
    
  }) 
  
  output$Reproduktionsrate <- renderPlot({
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = TaeglichReproduktionsRateRt)) + geom_line(aes(color = "Tägliche Reproduktionsrate")) + geom_line(aes(y = ReduzierteRt, color = "Reduzierte Reproduktionsrate")) +
      scale_x_date(labels = date_format("%m-%Y")) + labs(title = "Reproduktionsrate", x = "Datum [mm-dd]", y = "Wert",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Tägliche Reproduktionsrate' = color1,
                                                           'Reduzierte Reproduktionsrate' = color2)) +
      labs(color = 'Daten')
    
   p
    
  })  
  
  
}


shinyApp(ui = ui, server = server)

