
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
if (!("shinyjs" %in% rownames(installed.packages()))) install.packages("shinyjs")


library(shinyjs)
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

ui <- function(request) {
  # widgets website https://shiny.rstudio.com/gallery/widget-gallery.html
  navbarPage("Covid19 Meldungen in Deutschland!",  position = c("fixed-bottom"),
             tabPanel("Einstellung und Ausgabe",
                      
                      sidebarLayout( position ="left",
                                  
                                     
                                     
                                     sidebarPanel(
                                       wellPanel(
                                       h3("Link Erstellung mit allen Einstellungen"),
                                       bookmarkButton(label = "Generiere Link mit Einstellungen"),helpText("Mit dem Link kann die Applikation jederzeit wieder mit den jetzt eingestellen 
                                                                                                           Werten aufgerufen werden")),
                                       h3("Auswahl Analyse Deutschland/Bundesland oder einen Landkreis"),
                                       wellPanel(
                                         fluidRow(
                                           
                                           column(6,
                                                  
                                                  selectInput("BundeslandSelected", "Deutschland/Bundesland", choices = c("---","Deutschland",historyDfBundesLand$Bundesland %>% unique() %>% str_sort), selected = "Deutschland", multiple = FALSE,
                                                              selectize = TRUE, width = NULL, size = NULL)),
                                           column(6,
                                                  selectInput("LandkreiseSelected", "Landkeis", choices = c("---",historyDfLandkreis$Landkreis %>% unique() %>% str_sort), selected = "LK Esslingen")
                                           ))),
                                       
                                       
                                       h3("Expertenparameter Infektionsverlauf"),  
                                       fluidRow(
                                         column(6,
                                                wellPanel(
                                                  numericInput("ges_inf_rate", label = "Durchseuchung [%]", value = 70),
                                                  numericInput("ti", label = "Inkubationszeit [d]", value = 2),
                                                  numericInput("tod_rate", label = "Sterblichkeit [%]", value = 2))),
                                         column(6,
                                                wellPanel(
                                                  numericInput("faktor_n_inf", label = "Dunkelziffer Infizierte", value = 15),
                                                  numericInput("ta", label = "Infektiosität [d]", value = 6),
                                                  numericInput("td_tod", label = "Dauer Infektion bis Tod", value = 8)))),
                                       
                                       h3("Reduzierende Massnahmen"), 
                                       
                                       
                                       wellPanel(
                                         fluidRow(
                                           column(6,
                                                  dateInput("reduzierung_datum1", label = "Datum", value = "2020-03-16", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                           column(6,
                                                  sliderInput("reduzierung_rt1",label="1. Reduzierung Rt [%]", min = 0, max = 100, post  = " %", value = 50)))),
                                       wellPanel(
                                       fluidRow(
                                         column(6,
                                                dateInput("reduzierung_datum2", label = "Datum", value = "2020-03-23", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                         column(6,
                                                sliderInput("reduzierung_rt2",label="1. Reduzierung Rt [%]", min = 0, max = 100, post  = " %", value = 50)))),
                                       wellPanel(
                                         fluidRow(
                                       column(6,
                                              dateInput("reduzierung_datum3", label = "Datum", value = "2020-04-01", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                       column(6,
                                              sliderInput("reduzierung_rt3",label="1. Reduzierung Rt [%]", min = 0, max = 100, post  = " %", value = 50)))),

   
             
             h3("Krankenhausaufenthalt"), 
             fluidRow(
               column(6,
                      wellPanel(
                        numericInput("kh_normal", label = "Anteil an aktuellen Infizierten [%]", value = 4.5),
                        numericInput("t_kh", label = "Dauer", value = 14),
                        numericInput("dt_inf_kh", label = "Versatz nach Infektion", value = 8))),
               column(6,
                      wellPanel(
                        numericInput("kh_intensiv", label = "Anteil Intensivstation [%]", value = 25),
                        numericInput("t_intensiv", label = "Dauer Intensivstation", value = 10),
                        numericInput("dt_kh_int", label = "Versatz Krankenhaus - Intensivstation", value = 1)))) ,                                        
             
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
             column(6,
                    wellPanel(
                      tags$a(
                        href="https://admos.de/de/", 
                        tags$img(src = "logo-admos.png",
                                 width = "200px", height = "100px"),
                      ))),
             column(6,
                    wellPanel(
                      tags$a(
                        href="https://www.st2c.de", 
                        tags$img(src = "Folie9.png",
                                 width = "100px", height = "100px"),
                      ))),                                      
             
             
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
  
  h2("HoPE: Rechenmodel Verlauf Covid19 Infektionen und deren Auswirkung, version 0.11"),
  
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
tabPanel("Anleitung",
         
         # Show a plot of the generated distribution
         mainPanel(
       #    includeHTML("CoPE_200330.html") 
         )
),
tabPanel("Impressum",
         
         # Show a plot of the generated distribution
         mainPanel(
            includeHTML("AdmosImpressum.html")
          # includeHTML("AdmosImpressum.html")
          # includeMarkdown("AdmosImpressum.md")
          # includeText("AdmosImpressum.txt")
         )
),

tabPanel("Datenschutz",
         
         mainPanel(
          includeHTML("AdmosDatenschutz.html"),
          
         #  includeText("AdmosDatenschutz.txt")
         )
         
         
)
)

}
server <- function(input, output, session) {
  vals <- reactiveValues(Flag = "Landkreis")
  r0_no_erfasstDf <- reactiveVal(0) 

  observeEvent(input$BundeslandSelected,  ignoreInit = FALSE,{
    if(input$BundeslandSelected =="---"){
    } else{

      vals$Flag  <- "Bundesland"
    # browser()
    regionSelected = 2
    r0_no_erfasstDf  <- createLandkreisR0_no_erfasstDf(historyDfBundesLand, historyDfBund, regionSelected, vals, input,session)
    r0_no_erfasstDf(r0_no_erfasstDf)
    # set menu of Landkreis to "---"
    updateSelectInput(session, "LandkreiseSelected",  selected = "---")
    }

  })
  
  observeEvent(input$LandkreiseSelected, ignoreInit = TRUE,{
   if(input$LandkreiseSelected =="---"){
   } else{

    #browser()
      vals$Flag  <- "Landkreis"
    regionSelected = 3
    r0_no_erfasstDf  <- createLandkreisR0_no_erfasstDf(historyDfLandkreis, historyDfBund, regionSelected, vals, input,session)
    r0_no_erfasstDf(r0_no_erfasstDf)
   # browser()
    updateSelectInput(session, "BundeslandSelected",  selected = "---")
   }
  })
  
  rkiAndPredictData <- reactive({
    df <-  Rechenkern(r0_no_erfasstDf() ,input)
    df <- df %>% filter(Tag >=as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")),
                        Tag <=as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")))
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
    
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "SumAnzahl"] <- "ErfassteInfizierte"
    tmp$ErfassteInfizierte <- as.integer(tmp$ErfassteInfizierte)
    tmp$ErfassteInfizierteBerechnet <- as.integer(tmp$ErfassteInfizierteBerechnet)
    p <- ggplot(tmp, aes(color = "Erfasste Infizierte berechnet")) + geom_line(aes(x=Tag, y = ErfassteInfizierteBerechnet)) + geom_point(data = tmp, aes(x = Tag, y = ErfassteInfizierte, color = "Erfasste Infizierte")) +
      
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
    p <- ggplotly(p, tooltip = c("ErfassteInfizierteBerechnet", "ErfassteInfizierte", "Tag"))
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))
    #p <- p %>% layout(legend = list(orientation = 'h'))
    p
    
  })
  
  output$Verlauf <- renderPlotly({
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    
    tmp <- rkiAndPredictData()
    tmp$AktuellInfizierteBerechnet <- as.integer(tmp$AktuellInfizierteBerechnet)
    tmp$NeuInfizierteBerechnet <- as.integer(tmp$NeuInfizierteBerechnet)
    p <- ggplot(tmp, aes(color ="Aktuell Infizierte berechnet")) + geom_line(aes(x=Tag, y = AktuellInfizierteBerechnet)) + geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) +
      
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
    
    
    p <- ggplotly(p, tooltip = c("AktuellInfizierteBerechnet", "NeuInfizierteBerechnet", "Tag"))
    
    
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))
    p
    
  })  
  
  output$Krankenhaus <- renderPlotly({
    # browser()
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "KhBerechnet"] <- "Krankenhaus_berechnet"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet"] <- "Intensiv_berechnet"
    tmp$Intensiv_berechnet <- as.integer(tmp$Intensiv_berechnet)
    tmp$Krankenhaus_berechnet <- as.integer(tmp$Krankenhaus_berechnet)
    p <- ggplot(tmp, aes( color ="KH berechnet")) + geom_line(aes(x=Tag, y = Krankenhaus_berechnet)) + geom_line(aes(x=Tag,y= Intensiv_berechnet, color = "Intensiv berechnet")) +
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
    
    
    p <- ggplotly(p, tooltip = c("Krankenhaus_berechnet", "Intensiv_berechnet", "Tag"))
    
    
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))  
    p
    
    
  }) 
  
  output$Reproduktionsrate <- renderPlotly({
    paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Reproduktionsrate", sep ="")  
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "TaeglichReproduktionsRateRt"] <- "Taegliche_Reproduktionsrate"
    colnames(tmp)[colnames(tmp) == "ReduzierteRt"] <- "Reduzierte_Reproduktionsrate"
    tmp$Taegliche_Reproduktionsrate <- round(tmp$Taegliche_Reproduktionsrate, digits = 3)
    tmp$Reduzierte_Reproduktionsrate <- round(tmp$Reduzierte_Reproduktionsrate, digits = 3)
    
    p <- ggplot(tmp, aes(color = "Tägliche Reproduktionsrate")) + geom_line(aes(x=Tag, y = Taegliche_Reproduktionsrate)) + geom_line(aes(x=Tag,y = Reduzierte_Reproduktionsrate, color = "Reduzierte Reproduktionsrate")) +
      
      scale_x_date(labels = date_format("%d.%m")) + labs(title =  paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Reproduktionsrate", sep ="")  , x = "Datum", y = "Wert",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Tägliche Reproduktionsrate' = color1,
                                                           'Reduzierte Reproduktionsrate' = color2)) +
      
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    p <- ggplotly(p, tooltip = c("Taegliche_Reproduktionsrate","Reduzierte_Reproduktionsrate", "Tag"))
    p <- p %>% layout(legend = list(x = 0.01, y = 0.01, font = list(size = 8))) 
    
    p
    
  })  
  
 
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$currentSum <- vals$Flag
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    vals$Flag <- state$values$currentSum
  })
  
  
  
  
  
   
}


shinyApp(ui, server, enableBookmarking = "url")
