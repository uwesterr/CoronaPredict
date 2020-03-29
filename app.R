
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
if (!("rlang" %in% rownames(installed.packages()))) install.packages("rlang")

library(rlang)
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
                                         selectInput("BundeslandSelected", "Bundesland auswählen", choices = historyDfBundesLand$Bundesland %>% unique(), selected = NULL, multiple = FALSE,
                                                     selectize = TRUE, width = NULL, size = NULL),
                                         selectInput("LandkreiseSelected", "Landkeis auswählen:", choices = historyDfLandkreis$Landkreis %>% unique(), selected = "LK Esslingen")
                                       ),
                                       
                                       
                                       h3("Expertenparameter Infektionsverlauf"),   
                                       column(6,
                                              wellPanel(
                                                numericInput("ges_inf_rate", label = "Durchseuchung [%]", value = 70),
                                                numericInput("ti", label = "Inkubationszeit [d]", value = 2),
                                                numericInput("tod_rate", label = "Sterblichkeit [%]", value = 2))),
                                       column(6,
                                              wellPanel(
                                                numericInput("faktor_n_inf", label = "Dunkelziffer Infizierte", value = 15),
                                                numericInput("ta", label = "Infektiosität [d]", value = 6),
                                                numericInput("td_tod", label = "Dauer Infektion bis Tod", value = 8))),
                                       
                                       h3("Reduzierende Massnahmen"), 
                                       column(6,
                                              wellPanel(
                                                numericInput("reduzierung_rt1", label = "1. Reduzierung Rt [%]", value = 50),
                                                numericInput("reduzierung_rt2", label = "2. Reduzierung Rt [%]", value = 50), 
                                                numericInput("reduzierung_rt3", label = "3. Reduzierung Rt [%]", value = 50))), 
                                       column(6,
                                              wellPanel(
                                                dateInput("reduzierung_datum1", label = "Datum", value = "2020-03-16", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de")),
                                                dateInput("reduzierung_datum2", label = "Datum", value = "2020-03-23", start=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de")),
                                                dateInput("reduzierung_datum3", label = "Datum", value = "2020-04-01", start=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de")))),
                                       
                                       h3("Krankenhausaufenthalt"),   
                                       column(6,
                                              wellPanel(
                                                numericInput("kh_normal", label = "Anteil an aktuellen Infizierten [%]", value = 4.5),
                                                numericInput("t_kh", label = "Dauer", value = 14),
                                                numericInput("dt_inf_kh", label = "Versatz nach Infektion", value = 8))),
                                       column(6,
                                              wellPanel(
                                                numericInput("kh_intensiv", label = "Anteil Intensivstation [%]", value = 25),
                                                numericInput("t_intensiv", label = "Dauer Intensivstation", value = 10),
                                                numericInput("dt_kh_int", label = "Versatz Krankenhaus - Intensivstation", value = 1))) ,                                        
                                       
                                       h3("Einstellen der Darstellung") ,
                                       column(6,   
                                              wellPanel(
                                                dateRangeInput(inputId = "dateInput",
                                                               label = "Datum",
                                                               start = as.Date('2020-03-01'),
                                                               end = as.Date('2020-06-01'),
                                                               min = as.Date('2020-03-01'),
                                                               max = as.Date('2020-12-31'),
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
                                         
                                         wellPanel(
                                           splitLayout(
                                             style = "border: 1px solid silver;",
                                             cellWidths =  c("50%", "50%"),
                                             cellHeight = "120%",
                                             cellArgs = list(style = "padding: 6px"), 
                                             plotlyOutput(outputId ="Kumuliert"), plotlyOutput(outputId ="Verlauf"))
                                         )
                                       ),
                                       fluidRow(
                                         wellPanel(
                                           splitLayout(
                                             style = "border: 1px solid silver;",
                                             cellWidths =  c("50%", "50%"),
                                             cellHeight = "120%",
                                             plotlyOutput(outputId ="Krankenhaus"), plotlyOutput(outputId ="Reproduktionsrate"))
                                         ),
                                         
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
                      
                      
                      # Show a plot of the generated distribution
                      
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        
                      )
                      
                      
             )
  )


server <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$run2 <- 0
  observe({
    updateSelectInput(
      session,
      "filterRegion",label = paste("Select input label"),
      
      if (input$regionSelected ==1) {
        
        choices = "Deutschland" 
        
      } else if (input$regionSelected ==2) {
        
        choices = historyDfBundesLand %>% select(Bundesland) %>% unique() %>% .[[1]]
        
      } else if(input$regionSelected ==3) {
        
        choices = historyDfLandkreis %>% select(Landkreis) %>% unique() %>% .[[1]]
      }
      
    )
  })
  
  rkiAndPredictData <- reactive({
    
    if (input$regionSelected ==1) {
      #      browser()
      r0_no_erfasstDf  <- createLandkreisR0_no_erfasstDf(historyDfBund, input,session)
      
      
    } else if (input$regionSelected ==2) {
      
      r0_no_erfasstDf <- createLandkreisR0_no_erfasstDf(historyDfBundesLand, input)
      
    } else if(input$regionSelected ==3) {
      
      r0_no_erfasstDf  <- createLandkreisR0_no_erfasstDf(historyDfLandkreis, input,session)
    }
    
    df <-  Rechenkern(r0_no_erfasstDf ,input)
    
    # restrict data to given date range
    df <- df %>% filter(Tag >=as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")),
                        Tag <=as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")))
    # browser()
    
  })  
  
  color1 = 'blue'
  color2 = 'green'
  # more options at https://ggplot2.tidyverse.org/reference/theme.html
  themeCust <-  theme(
    plot.title = element_text(color="blue", size=24, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=16, face="bold"),
    axis.title.y = element_text(color="blue", size=16, face="bold"),
    axis.text.x = element_text(color="blue", size=16, face="bold"),
    axis.text.y = element_text(color="blue", size=16, face="bold"),
    plot.background = element_rect(fill = "lightgray"),
    axis.text = element_text(colour = "blue"),
    legend.text = element_text(color="blue", size=16, face="bold"),
    legend.position = "bottom"
  )
  
  output$Kumuliert <- renderPlotly({
    
    
    #  browser()
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = ErfassteInfizierteBerechnet, color = "Erfasste Infizierte berechnet")) + geom_line() + geom_point(data = rkiAndPredictData(), aes(x = Tag, y = SumAnzahl, color = "Erfasste Infizierte")) +
      scale_x_date(labels = date_format("%d.%m")) + labs(title = paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Kumulierte Infizierte", sep =""),
                                                         x = "Datum", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Erfasste Infizierte berechnet' = color1,
                                                           'Erfasste Infizierte' = color2)) +
      
      labs(color = 'Daten') + scale_y_continuous(labels = scales::comma)
    
    
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_si())
      
    } else {
      p
      
    }
    p <- ggplotly(p)
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))
    #p <- p %>% layout(legend = list(orientation = 'h'))
    p
    
  })
  
  output$Verlauf <- renderPlotly({
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = AktuellInfizierteBerechnet, color ="Aktuell Infizierte berechnet")) + geom_line() + geom_line(aes(y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) +
      scale_x_date(labels = date_format("%d.%m")) + labs(title = paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Verlauf Infizierte", sep =""),
                                                         x = "Datum", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Aktuell Infizierte berechnet' = color1,
                                                           'Neu Infizierte berechnet' = color2)) +
      
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    
    
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_si())
      
    } else {
      p
      
    }
    p <- ggplotly(p)
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))
    p
    
  })  
  
  output$Krankenhaus <- renderPlotly({
    # browser()
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = KhBerechnet, color ="KH berechnet")) + geom_line() + geom_line(aes(y= IntensivBerechnet, color = "Intensiv berechnet")) +
      scale_x_date(labels = date_format("%d.%m")) + labs(title = paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Plätze in Krankenhaus / Intensivstation", sep ="")  ,
                                                         x = "Datum", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'KH berechnet' = color1,
                                                           'Intensiv berechnet' = color2)) +
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_si())
      
    } else {
      p
      
    }
    p <- ggplotly(p)
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))  
    p
    
    
  }) 
  
  output$Reproduktionsrate <- renderPlotly({
    paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Reproduktionsrate", sep ="")  
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    p <- ggplot(rkiAndPredictData(), aes(x=Tag, y = TaeglichReproduktionsRateRt)) + geom_line(aes(color = "Tägliche Reproduktionsrate")) + geom_line(aes(y = ReduzierteRt, color = "Reduzierte Reproduktionsrate")) +
      scale_x_date(labels = date_format("%d.%m")) + labs(title =  paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Reproduktionsrate", sep ="")  , x = "Datum", y = "Wert",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Tägliche Reproduktionsrate' = color1,
                                                           'Reduzierte Reproduktionsrate' = color2)) +
      
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    p <- ggplotly(p)
    p <- p %>% layout(legend = list(x = 0.01, y = 0.01, font = list(size = 8))) 
    
    p
    
  })  
  
  
}


shinyApp(ui = ui, server = server)
