

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
if (!("shinyalert" %in% rownames(installed.packages()))) install.packages("shinyalert")
if (!("shinyWidgets" %in% rownames(installed.packages()))) install.packages("shinyWidgets")
if (!("staTools" %in% rownames(installed.packages()))) install.packages("staTools")
if (!("GA" %in% rownames(installed.packages()))) install.packages("GA")
if (!("tictoc" %in% rownames(installed.packages()))) install.packages("tictoc")
if (!("httr" %in% rownames(installed.packages()))) install.packages("httr")
if (!("leaflet" %in% rownames(installed.packages()))) install.packages("leaflet")
if (!("rgeolocate" %in% rownames(installed.packages()))) install.packages("rgeolocate")
#if (!("rgdal" %in% rownames(installed.packages()))) install.packages("rgdal")
if (!("RColorBrewer" %in% rownames(installed.packages()))) install.packages("RColorBrewer")

library(leaflet)
library(rgeolocate)
#library(rgdal)
library(RColorBrewer)

# library(httr)
# library(tictoc)
# library(GA)
# library(jsonlite)
# library(staTools)
# library(writexl)
# library(rlang)
# library(DT)
# library(modelr)
library(zoo)
library(shinyWidgets)
library(shinyalert)
library(tidyr)
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)
library(scales)

source(file = "src/Rechenkern.R")
source(file = "src/helperForCovid19.R")
inputFile <- list.files("data/InputFileForAppFolder/", full.names = TRUE)
if(!is_empty(inputFile)){
  
  load(inputFile)  
  RkiDataWithSumsNested <-  RkiDataICU_BeatmetOptiTotal
  
} else{
  
  showModal("Fehler, Daten fehlen ")
  
}

# source(file = "src/ui.R")
ui <- function(request) {
  
  
  # widgets website https://shiny.rstudio.com/gallery/widget-gallery.html
  navbarPage("Covid19 Meldungen in Deutschland!",  position = c("fixed-bottom"),
             
             tabPanel("Einstellung und Ausgabe",
                      setBackgroundColor("#ECF0F5"
                                         # color = c("#F7FBFF", "#2171B5"),
                                         # gradient = "radial",
                                         # direction = c("top", "left")
                      ),
                      
                      sidebarLayout( position ="left",
                                     
                                     
                                     sidebarPanel(
                                       wellPanel(
                                         
                                         h3("Auswahl Region"),
                                         
                                         wellPanel(
                                           fluidRow(
                                             
                                             column(6,
                                                    
                                                    selectInput("BundeslandSelected", "Deutschland/Bundesland", choices = c("---","Deutschland", RkiDataWithSumsNested %>% filter(groupedBy == "Bundesland") %>% 
                                                                                                                              select(whichRegion) %>% unlist %>% unique() %>% str_sort), selected = "Baden-Württemberg", multiple = FALSE,
                                                                selectize = TRUE, width = NULL, size = NULL)),
                                             column(6,
                                                    selectInput("LandkreiseSelected", "Landkeis", choices = c("---", RkiDataWithSumsNested %>% filter(groupedBy == "Landkreis") %>% 
                                                                                                                select(whichRegion) %>% unlist %>% unique() %>% str_sort), selected = "LK Esslingen")
                                             ))),
                                         
                                         
                                         
                                         h3("Reduzierende Massnahmen"), 
                                         
                                         wellPanel(
                                           fluidRow(
                                             column(4,
                                                    dateInput("reduzierung_datum1", label = "Datum", value = "2020-03-16", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de")),
                                                    helpText("Schulschließung")), 
                                             
                                             column(8,
                                                    sliderInput("reduzierung_rt1", label = "Reduzierung Rt [%]", min = 00, max = 100, post  = " %", value = 25)))),
                                         wellPanel(
                                           fluidRow(
                                             column(4,
                                                    dateInput("reduzierung_datum2", label = "Datum", value = "2020-03-23", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de")),
                                                    helpText("Kontaktsperre")),
                                             column(8,
                                                    sliderInput("reduzierung_rt2",label="Reduzierung Rt [%]", min = -100, max = 100, post  = " %", value = 30)))),
                                         wellPanel(
                                           fluidRow(
                                             column(4,
                                                    dateInput("reduzierung_datum3", label = "Datum", value = "2020-04-01", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                             column(8,
                                                    sliderInput("reduzierung_rt3",label="Reduzierung Rt [%]", min = -100, max = 100, post  = " %", value = 40)))),
                                         wellPanel(
                                           fluidRow(
                                             column(4,
                                                    dateInput("reduzierung_datum4", label = "Datum", value = "2020-04-20", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                             column(8,
                                                    sliderInput("reduzierung_rt4",label="Reduzierung Rt [%]", min = -100, max = 100, post  = " %", value = 0)))),
                                         wellPanel(
                                           fluidRow(
                                             column(4,
                                                    dateInput("reduzierung_datum5", label = "Datum", value = "2020-05-15", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                             column(8,
                                                    sliderInput("reduzierung_rt5",label="Reduzierung Rt [%]", min = -100, max = 100, post  = " %", value = 0)))),
                                         
                                         
                                         h3("Expertenparameter Infektionsverlauf"),  
                                         fluidRow(
                                           column(6,
                                                  wellPanel(
                                                    numericInput("ges_inf_rate", label = "Durchseuchung [%]", value = 70, min=1, max=100, step=1),
                                                    numericInput("ti", label = "Inkubationszeit [d]", value = 2, min=1, max=20, step=1),
                                                    numericInput("tod_rate", label = "Sterblichkeit [%]", value = 4, min=0, max=100, step = 0.1))),
                                           column(6,
                                                  wellPanel(
                                                    numericInput("faktor_n_inf", label = "Dunkelziffer Infizierte", value = 15, min=1, max=100, step=1),
                                                    numericInput("ta", label = "Infektiosität [d]", value = 6, min=1, max=20, step=1),
                                                    numericInput("td_tod", label = "Dauer Infektion bis Tod [d]", value = 4, min=1, max=20, step=1)))),
                                         
                                         
                                         h3("Einstellen der Darstellung") ,
                                         column(7,   
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
                                         column(5,
                                                wellPanel(
                                                  radioButtons(inputId = "logyInput",
                                                               label = "Y-Achse",
                                                               choices = c("linear", "logarithmisch"),
                                                               selected =  "logarithmisch")
                                                )),
                                         h3("Speichern Einstellungen"),
                                         bookmarkButton(label = "Generiere Link mit Einstellungen"),
                                         helpText("Mit dem Link kann die Applikation jederzeit wieder mit den jetzt eingestellten Werten aufgerufen werden.", 
                                                  "Sie können den Link in den Browserfavoriten durch die Tastenkombination CTRL+D zur späteren Wiederverwendung speichern.")),
                                       column(8,
                                              #wellPanel(
                                              tags$a(
                                                href="https://admos.de/de/", 
                                                tags$img(src = "logo-admos.png",
                                                         width = "220px", height = "71px"),
                                                #)
                                              )),
                                       column(4,
                                              #wellPanel(
                                              tags$a(
                                                href="https://www.st2c.de", 
                                                tags$img(src = "logoSt2c.png",
                                                         width = "100px", height = "100px"),
                                                #)
                                              )),                                      
                                       
                                       
                                       #tags$p(class="header", checked=NA,
                                       #
                                       
                                       # adding the new div tag to the sidebar
                                       tags$div(
                                         HTML(paste("Source code available on",
                                                    tags$a(href="https://github.com/uwesterr/CoronaPredict", "GitHub"))),
                                         br(),
                                         HTML(paste("Data accessed on",
                                                    Sys.time(),
                                                    tags$a(href="https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson",
                                                           "from RKI")))
                                       ),
                                       
                                       #dummy to show lowest elements
                                       column(6,
                                              br(),br(),br(),br(),br()
                                       ),
                                       
                                     ), # end sidebar panel
                                     mainPanel(
                                       
                                       h2("CoPE: Rechenmodel Verlauf Covid19 Infektionen und deren Auswirkung, version 0.24", color = "blue"),
                                       tags$head(tags$style('h2 {color:blue;}')),
                                       tags$head(tags$style('h3 {color:blue;}')),
                                       
                                       fluidRow(
                                         column(6,
                                                leafletOutput("mymap")),
                                         column(6,
                                                addSpinner(plotlyOutput(outputId ="MeldungenProWoche"), spin = "circle", color = "#E41A1C"))),   
                                       
                                       
                                       
                                       wellPanel(
                                         h3("Krankenhausaufenthalt"), 
                                         
                                         fluidRow(
                                           
                                           column(4,
                                                  wellPanel(
                                                    numericInput("kh_normal", label = "Aktuell Infizierte, stationär aufgenommen [%]", value = 4.5, step = 0.1),
                                                    numericInput("kh_intensiv", label = "davon Intensivstation [%]", value = 25))),
                                           column(4,  
                                                  wellPanel(
                                                    numericInput("t_kh", label = "Dauer stationäre Behandlung [d]", value = 14),
                                                    numericInput("t_intensiv", label = "Dauer Intensivstation [d]", value = 10))),
                                           
                                           column(4,
                                                  wellPanel(
                                                    numericInput("dt_inf_kh", label = "Versatz nach Infektion [d]", value = 8),
                                                    numericInput("dt_kh_int", label = "Versatz Krankenhaus - Intensivstation [d]", value = 1))))) , 
                                       fluidRow(
                                         wellPanel(
                                           #           splitLayout(
                                           #             style = "border: 1px solid silver;",
                                           #             cellWidths =  c("50%", "50%"),
                                           #             cellHeight = "120%",
                                           addSpinner(plotlyOutput(outputId ="Krankenhaus"), spin = "circle", color = "#E41A1C")),
                                         #             addSpinner(plotlyOutput(outputId ="MeldungenProWoche"), spin = "circle", color = "#E41A1C"))                                             
                                         # plotlyOutput(outputId ="Krankenhaus"), plotlyOutput(outputId ="Reproduktionsrate"))
                                         #        ),
                                         
                                       ),
                                       fluidRow(
                                         wellPanel(
                                           splitLayout(
                                             style = "border: 1px solid silver;",
                                             cellWidths =  c("50%", "50%"),
                                             cellHeight = "120%",
                                             cellArgs = list(style = "padding: 6px"), 
                                             addSpinner(plotlyOutput(outputId ="Kumuliert"), spin = "circle", color = "#E41A1C"),
                                             addSpinner(plotlyOutput(outputId ="Verlauf"), spin = "circle", color = "#E41A1C"))
                                           #addSpinner(plotOutput("plot1"), spin = "circle", color = "#E41A1C"),                                           
                                         )
                                       )
                                       
                                     ) # end main panel
                      )
             ),
             tabPanel("Anleitung",
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        includeMarkdown("Readme.md")
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
                        includeHTML("AdmosDatenschutz.html")
                        #  includeText("AdmosDatenschutz.txt")
                      )
                      
             )
  )
  
}

server <- function(input, output, session) {
  
  # Avoid caching of app.R by updating timestamp of app.R
  onStop(function() {
    # File name
    p <- paste0(getwd(), "/app.R")
    # Update file 'date creation'
    Sys.setFileTime(p, now())
  }) 
  
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
    p <- input$mymap_shape_click
    updateSelectInput(session, "LandkreiseSelected",  selected = input$mymap_shape_click[["id"]])
    # browser()
    
    print(p)
  })
  output$mymap <- renderLeaflet({
    load("data/meldeMap.RData")
   return(MeldeMap)
  })
  
  
  vals <- reactiveValues(Flag = "Bundesland")
  r0_no_erfasstDf <- reactiveVal(0) 
  
  
  
  observeEvent(input$optimizeReduzierendeBtn, {
    showModal(modalDialog(title = "Optimierung läuft"))
    
    if(vals$Flag  == "Bundesland"){
      regionSelected = 2
    } else {
      regionSelected = 3
    }
    dfRoNo <- r0_no_erfasstDf()[[1]]
    n0_erfasst_nom_min_max <- r0_no_erfasstDf()[[2]]
    R0_conf_nom_min_max <- r0_no_erfasstDf()[[3]]
    startDate <- r0_no_erfasstDf()[[4]]
    rechenDf_nom <- cbind(dfRoNo,n0_erfasst=n0_erfasst_nom_min_max$n0_erfasst_nom, R0 =R0_conf_nom_min_max$R0_nom)
    GA <- ga(type = "real-valued", 
             fitness =  function(x) calcPredictionsForOptimization(x[1], x[2], x[3], R0_conf_nom_min_max,  n0_erfasst_nom_min_max, startDate, rechenDf_nom, input),
             seed = 2020,
             lower = c(0, 0,-100 ), upper = c(100, 100, 100), 
             popSize = 10, maxiter = 30, run = 5)
    # browser()
    updateSliderInput(session, "reduzierung_rt1", value = GA@solution[[1]])
    updateSliderInput(session, "reduzierung_rt2", value = GA@solution[[2]])
    updateSliderInput(session, "reduzierung_rt3", value = GA@solution[[3]])
    removeModal()
  })
  
  observeEvent(input$BundeslandSelected,  ignoreInit = FALSE,{
    if(input$BundeslandSelected =="---"){
    }else {
      isolate(updateSelectInput(session, "LandkreiseSelected",  selected = "---"))
      vals$Flag  <- "Bundesland"
      regionSelected = 2
      RkiDataWithSumsNested  <- RkiDataWithSumsNested %>% filter(whichRegion == input$BundeslandSelected)
      OptResultDf <- RkiDataWithSumsNested[[1,"optimizedInput"]]
      inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
      
      for (inputVarName in OptResultDf[[1]] %>% names) {
        if (inputVarName %in% (input %>% names)) {
          if (inputVarName %in% (str_subset(input %>% names, "reduzierung"))) {
            isolate(updateSliderInput(session, inputVarName, value = OptResultDf[[1]][[inputVarName]]))
          } else {
            isolate(updateNumericInput(session, inputVarName, value = round(OptResultDf[[1]][[inputVarName]],1)))
          }
        }
      }
      
      r0_no_erfasstDf(RkiDataWithSumsNested)
      # set menu of Landkreis to "---"
      
    }
  })
  
  observeEvent(input$LandkreiseSelected, ignoreInit = TRUE,{
    
    if(input$LandkreiseSelected =="---"){
    }else {
      
      isolate(updateSelectInput(session, "BundeslandSelected",  selected = "---"))
      vals$Flag  <- "Landkreis"
      regionSelected = 3
      RkiDataWithSumsNested  <- RkiDataWithSumsNested %>% filter(whichRegion == input$LandkreiseSelected)
      OptResultDf <- RkiDataWithSumsNested[[1,"optimizedInput"]]
      inputForOptimization <- input # to make setting reduzierung_rtx easy and fast
      for (inputVarName in OptResultDf[[1]] %>% names) {
        if (inputVarName %in% (input %>% names)) {
          if (inputVarName %in% (str_subset(input %>% names, "reduzierung"))) {
            isolate(updateSliderInput(session, inputVarName, value = OptResultDf[[1]][[inputVarName]]))
          } else {
            isolate(updateNumericInput(session, inputVarName, value = round(OptResultDf[[1]][[inputVarName]],1)))
          }
        }
      }
      r0_no_erfasstDf(RkiDataWithSumsNested)
      
    }
  })
  
  rkiAndPredictData <- reactive({
    #  browser()
    if (r0_no_erfasstDf()$NotEnoughDataFlag) {
      showModal(modalDialog(title = "Zu wenige Fallzahlen für eine gute Schätzung des Verlaufs", 
                            "Glücklicherweise sind in diesem Kreis bisher nur wenige an COVID 19 erkrankt. 
                            Hierdurch ist aber auch keine valide Zukunftsschätzung möglich.",  footer = modalButton("Ok")))
      
    }
    RkiDataWithR0N0 <- r0_no_erfasstDf() %>% unnest(data)
    
    df_nom <-  Rechenkern(RkiDataWithR0N0, input)
    tmp <- df_nom %>% filter(!is.na(SumAnzahl))
    letzter_Tag <- max(tmp$Tag)
    konfidenz_je_tag <- mean(c(0.023, 0.029/2)) # Mittelwert aus zwei separaten Untersuchungen zu log. Standardabweichungen
    KonfidenzVektor <- function(v, Tag, konfidenz, konfidenz_je_tag, letzter_Tag, time_lag){
      tmp <- log10(v)
      i0 <- which(Tag == letzter_Tag)
      for (k in (i0+1):(i0+time_lag)){
        tmp[k] <- tmp[k] + konfidenz
      }
      j <- 1
      for (i in (i0+1+time_lag):length(v)){
        tmp[i] <- tmp[i] + j*konfidenz_je_tag + konfidenz 
        j <- j +1
      }
      10^tmp
    }
    
    df_nom$ErfassteInfizierteBerechnet_min <- KonfidenzVektor(df_nom$ErfassteInfizierteBerechnet, df_nom$Tag, 0, -konfidenz_je_tag, letzter_Tag, 0)
    df_nom$ErfassteInfizierteBerechnet_max <- KonfidenzVektor(df_nom$ErfassteInfizierteBerechnet, df_nom$Tag,0, +konfidenz_je_tag, letzter_Tag, 0)
    
    konfidenz <- log10(1.1)
    df_nom$ToteBerechnet_min <- KonfidenzVektor(df_nom$ToteBerechnet, df_nom$Tag, -konfidenz, -konfidenz_je_tag, letzter_Tag, input$td_tod)
    df_nom$ToteBerechnet_max <- KonfidenzVektor(df_nom$ToteBerechnet, df_nom$Tag,konfidenz, +konfidenz_je_tag, letzter_Tag, input$td_tod)
    
    df_nom$KhBerechnet_min <- KonfidenzVektor(df_nom$KhBerechnet, df_nom$Tag, -konfidenz, -konfidenz_je_tag, letzter_Tag, input$dt_inf_kh)
    df_nom$KhBerechnet_max <- KonfidenzVektor(df_nom$KhBerechnet, df_nom$Tag, +konfidenz, +konfidenz_je_tag, letzter_Tag, input$dt_inf_kh)
    
    df_nom$IntensivBerechnet_min <- KonfidenzVektor(df_nom$IntensivBerechnet, df_nom$Tag, -konfidenz, -konfidenz_je_tag, letzter_Tag, input$dt_kh_int+input$dt_inf_kh)
    df_nom$IntensivBerechnet_max <- KonfidenzVektor(df_nom$IntensivBerechnet, df_nom$Tag, +konfidenz, +konfidenz_je_tag, letzter_Tag, input$dt_kh_int+input$dt_inf_kh)
    
    
    df <- df_nom %>% filter(Tag >=as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")),
                            Tag <=as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")))
  }) 
  color1 = 'blue'
  color2 = 'green'
  color3 = '#6ab84d'
  color4 = 'black'
  color5 = 'gray'
  color6 = 'lightblue'
  perdictionHorizon <- Sys.Date()+21 # how far in the future confidance will be displayed
  alphaForConfidence <- 0.1 
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
  #############  output$Kumuliert ################
  output$Kumuliert <- renderPlotly({
    
    
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "SumAnzahl"] <- "Erfasste_Infizierte"
    colnames(tmp)[colnames(tmp) == "sumTote"] <- "Erfasste_Todesfaelle"
    colnames(tmp)[colnames(tmp) == "ToteBerechnet"] <- "Berechnete_Todesfaelle"
    colnames(tmp)[colnames(tmp) == "ErfassteInfizierteBerechnet"] <- "Berechnete_Infizierte"
    # min
    colnames(tmp)[colnames(tmp) == "SumAnzahl_min"] <- "Erfasste_Infizierte_min"
    colnames(tmp)[colnames(tmp) == "sumTote_min"] <- "Erfasste_Todesfaelle_min"
    colnames(tmp)[colnames(tmp) == "ToteBerechnet_min"] <- "Berechnete_Todesfaelle_min"
    colnames(tmp)[colnames(tmp) == "ErfassteInfizierteBerechnet_min"] <- "Berechnete_Infizierte_min"
    # max
    colnames(tmp)[colnames(tmp) == "SumAnzahl_max"] <- "Erfasste_Infizierte_max"
    colnames(tmp)[colnames(tmp) == "sumTote_max"] <- "Erfasste_Todesfaelle_max"
    colnames(tmp)[colnames(tmp) == "ToteBerechnet_max"] <- "Berechnete_Todesfaelle_max"
    colnames(tmp)[colnames(tmp) == "ErfassteInfizierteBerechnet_max"] <- "Berechnete_Infizierte_max"
    tmp$Erfasste_Infizierte <- as.integer(tmp$Erfasste_Infizierte)
    tmp$Berechnete_Infizierte <- as.integer(tmp$Berechnete_Infizierte)
    
    
    
    p <- ggplot(tmp, aes(color = "Erfasste Infizierte berechnet")) +
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon), 
                  aes( x= Tag, ymin = Berechnete_Infizierte_min, ymax = Berechnete_Infizierte_max), alpha =alphaForConfidence, outline.type = "full", fill = color1) + 
      geom_line(aes(x=Tag, y = Berechnete_Infizierte)) +   
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon), 
                  aes( x= Tag, ymin = Berechnete_Todesfaelle_min, ymax = Berechnete_Todesfaelle_max), alpha =alphaForConfidence, outline.type = "full", fill = color4) + 
      
      geom_point(data = tmp, aes(x = Tag, y = Erfasste_Infizierte, color = "Erfasste Infizierte")) +
      geom_line(data = tmp, aes(x = Tag, y = Berechnete_Todesfaelle, color = "Todesfälle berechnet")) + 
      geom_point(data = tmp, aes(x = Tag, y = Erfasste_Todesfaelle, color = "Todesfälle erfasst")) +
      scale_x_date(labels = date_format("%d.%m")) + labs(title = paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Kumulierte Infizierte / Todesfälle, CI 95%", sep =""),
                                                         x = "Datum", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Erfasste Infizierte berechnet' = color1,
                                                           'Erfasste Infizierte' = color2,
                                                           'Todesfälle berechnet' = color4,
                                                           'Todesfälle erfasst' = color5)) +
      labs(color = 'Daten')
    
    if(logy){
      p <- p + scale_y_log10(labels = label_number_auto(), limits = c(1, NA))
    } else {
      p <- p + scale_y_continuous(labels = scales::comma)
    }
    p <- ggplotly(p, tooltip = c("Erfasste_Infizierte", "Berechnete_Infizierte", "Tag", "Erfasste_Todesfaelle", "Berechnete_Todesfaelle"))
    p <- p %>% layout(legend = list(x = 0.69, y = 0.01, font = list(size = 8)))
    p
    
  })
  
  #########    output$Verlauf ############ 
  
  output$Verlauf <- renderPlotly({
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "AnzahlTodesfall"] <- "NeueToteErfasst"
    tmp$AktuellInfizierteBerechnet <- as.integer(tmp$AktuellInfizierteBerechnet)
    tmp$NeuInfizierteBerechnet <- as.integer(tmp$NeuInfizierteBerechnet)
    p <- ggplot(tmp, aes(color ="Aktuell Infizierte berechnet")) + geom_line(aes(x=Tag, y = AktuellInfizierteBerechnet)) +  geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) + 
      geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) + 
      
      geom_point(aes(x=Tag,y= AnzahlFall, color = "Neu Infizierte erfasst")) +
      geom_line(aes(x=Tag,y= NeueToteBerechnet, color = "Neue Todesfälle berechnet")) + geom_point(aes(x=Tag,y=NeueToteErfasst, color = "Neue Todesfälle erfasst")) +
      geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) + geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) +
      
      scale_x_date(labels = date_format("%d.%m")) + labs(title = paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Verlauf Infizierte / Todesfälle, CI 95%", sep =""),
                                                         x = "Datum", y = "Anzahl",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Aktuell Infizierte berechnet' = color1,
                                                           'Neu Infizierte berechnet' = color2,
                                                           'Neu Infizierte erfasst' = color3,
                                                           'Neue Todesfälle berechnet' = color4,
                                                           'Neue Todesfälle erfasst' = color5)) +
      labs(color = 'Daten')
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_auto())
    } else {
      p <- p +  scale_y_continuous(labels = scales::comma)
    }
    
    
    p <- ggplotly(p, tooltip = c("AktuellInfizierteBerechnet", "NeuInfizierteBerechnet", "Tag", "NeueToteErfasst", "NeueToteBerechnet","AnzahlFall"))
    
    
    p <- p %>% layout(legend = list(x = 0.69, y = 0.01, font = list(size = 8)))
    p
    
  }) 
  
  #########     output$Krankenhaus ############  
  output$Krankenhaus <- renderPlotly({
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "KhBerechnet"] <- "Krankenhaus_berechnet"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet"] <- "Intensiv_berechnet"
    colnames(tmp)[colnames(tmp) == "Stationaer"] <- "Krankenhaus_erfasst"
    colnames(tmp)[colnames(tmp) == "ICU_Beatmet"] <- "Intensiv_erfasst"
    
    #min
    colnames(tmp)[colnames(tmp) == "KhBerechnet_min"] <- "Krankenhaus_berechnet_min"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet_min"] <- "Intensiv_berechnet_min"
    #max
    colnames(tmp)[colnames(tmp) == "KhBerechnet_max"] <- "Krankenhaus_berechnet_max"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet_max"] <- "Intensiv_berechnet_max"
    
    tmp$Intensiv_berechnet <- as.integer(tmp$Intensiv_berechnet)
    tmp$Krankenhaus_berechnet <- as.integer(tmp$Krankenhaus_berechnet)
    p <- ggplot(tmp, aes( color ="KH berechnet")) + 
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = Krankenhaus_berechnet_min, ymax = Krankenhaus_berechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color1) + 
      geom_line(aes(x=Tag, y = Krankenhaus_berechnet))  + geom_point(aes(Tag, Krankenhaus_erfasst, color = "KH erfasst")) +
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = Intensiv_berechnet_min, ymax = Intensiv_berechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color2) + 
      geom_line(aes(x=Tag,y= Intensiv_berechnet, color = "Intensiv berechnet")) + geom_point(aes(Tag, Intensiv_erfasst, color = "Intensiv erfasst")) +
      
      scale_x_date(labels = date_format("%d.%m")) + 
      labs(title = paste0(rkiAndPredictData() %>%  filter(!is.na(whichRegion)) %>% select(whichRegion) %>% 
                            unique(), ": Plätze in Krankenhaus / Intensivstation, CI 95%", sep ="")  ,
           x = "Datum", y = "Anzahl",
           caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +  
      scale_color_manual(values = c(
        'KH berechnet' = color1,
        'Intensiv berechnet' = color2,
        'KH erfasst' = color6,
        'Intensiv erfasst' = color3)) +
      labs(color = 'Daten')
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_auto())
    } else {
      p <- p + scale_y_continuous(labels = scales::comma)
    }
    
    p <- ggplotly(p, tooltip = c("Krankenhaus_berechnet", "Intensiv_berechnet", "Tag", "Krankenhaus_erfasst", "Intensiv_erfasst" ))
    p <- p %>% layout(legend = list(x = 0.01, y = 0.99, font = list(size = 8)))  
    p
    
    
  }) 
  
  #########     output$MeldungenProWoche ############  
  output$MeldungenProWoche <- renderPlotly({
    paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Reproduktionsrate", sep ="")  
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    # browser()
    colnames(tmp)[colnames(tmp) == "MeldeWithinWeekPer100kInhabitantsBerechnet"] <- "Berechnet"
    colnames(tmp)[colnames(tmp) == "MeldeWithinWeekPer100kInhabitants"] <- "Gemeldet"
    
    tmp$Berechnet <- round(tmp$Berechnet, digits = 0)
    tmp$Gemeldet <- round(tmp$Gemeldet, digits = 0)
    # browser()
    #   tmp$Reproduktionszahl_ohne_Massnahmen <- round(input$ta*(tmp$Reproduktionszahl_ohne_Massnahmen-1), digits = 3)
    #   tmp$Aktuelle_Reproduktionszahl <- round(input$ta*(tmp$Aktuelle_Reproduktionszahl-1), digits = 3)
    
    p <- ggplot(tmp, aes(color = "Berechnet")) + geom_line(aes(x=Tag, y = Berechnet), linetype = 2) + geom_point(aes(x = Tag, y = Gemeldet, color = "Gemeldet")) +
      # geom_line(aes(x=Tag,y = Aktuelle_Reproduktionszahl, color = "R aktuell"))  +
      scale_x_date(labels = date_format("%d.%m")) + labs(title =  paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Meldungen der letzen Woche pro 100.000 Einwohner", sep ="")  , 
                                                         x = "Datum", y = "Meldungen der letzen Woche / 100.000 Einwohner",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Berechnet' = color1,
                                                           'Gemeldet' = color2)) + geom_hline( yintercept = 50) +
      
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    p <- ggplotly(p, tooltip = c("Berechnet","Gemeldet", "Tag"))
    p <- p %>% layout(legend = list(x = 0.01, y = 0.01, font = list(size = 8))) 
    
    p
    
  })  
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$currentSum <- vals$Flag
  })
  
  # copy url into browser url adress field
  onBookmarked(function(url) {
    showBookmarkUrlModal(url) # Default function
    updateQueryString(url) # Update Adresse im Browser
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    vals$Flag <- state$values$currentSum
    
    if(input$BundeslandSelected =="---"){
    }else {
      vals$Flag  <- "Bundesland"
      regionSelected = 2
      load("data/RkiDataICU_BeatmetOptiTotal.RData")
      RkiDataWithSumsNested <-  RkiDataICU_BeatmetOptiTotal  %>% filter(whichRegion == regionSelected)
      browser()
      r0_no_erfasstDf(RkiDataWithSumsNested)
      # set menu of Landkreis to "---"
      isolate(updateSelectInput(session, "LandkreiseSelected",  selected = "---"))
    }
    
    if(input$LandkreiseSelected =="---"){
      
    }else {
      vals$Flag  <- "Landkreis"
      regionSelected = 3
      load("data/RkiDataICU_BeatmetOptiTotal.RData")
      RkiDataWithSumsNested <-  RkiDataICU_BeatmetOptiTotal  %>% filter(whichRegion == regionSelected)
      r0_no_erfasstDf(RkiDataWithSumsNested)
      isolate(updateSelectInput(session, "BundeslandSelected",  selected = "---"))
    }
    
  })
  
}

shinyApp(ui, server, enableBookmarking = "server")

