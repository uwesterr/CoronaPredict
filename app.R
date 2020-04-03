

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


library(shinyWidgets)
library(shinyalert)
library(writexl)
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
                      setBackgroundColor("#ECF0F5"
                       # color = c("#F7FBFF", "#2171B5"),
                       # gradient = "radial",
                       # direction = c("top", "left")
                      ),
                      
                      sidebarLayout( position ="left",
                                     
                                     
                                     sidebarPanel(
                                       wellPanel(
                                         h3("Speichern Einstellungen"),
                                         bookmarkButton(label = "Generiere Link mit Einstellungen"),helpText("Mit dem Link kann die Applikation jederzeit wieder mit den jetzt eingestellten 
                                                                                                           Werten aufgerufen werden.", "Sie können den Link in den Browserfavoriten durch die Tastenkombination CTRL+D zur späteren Wiederverwendung speichern.")),
                                       h3("Auswahl Region"),
                                       
                                       wellPanel(
                                         fluidRow(
                                           
                                           column(6,
                                                  
                                                  selectInput("BundeslandSelected", "Deutschland/Bundesland", choices = c("---","Deutschland",historyDfBundesLand$Bundesland %>% unique() %>% str_sort), selected = "Deutschland", multiple = FALSE,
                                                              selectize = TRUE, width = NULL, size = NULL)),
                                           column(6,
                                                  selectInput("LandkreiseSelected", "Landkeis", choices = c("---",historyDfLandkreis$Landkreis %>% unique() %>% str_sort), selected = "LK Esslingen")
                                           ))),
                                       
                                       
                                       
                                       h3("Reduzierende Massnahmen"), 
                                       wellPanel(
                                         fluidRow(
                                           column(6,
                                                  dateInput("reduzierung_datum1", label = "1. Massnahme Datum, Rt [%]", value = "2020-03-16", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                           column(6,
                                                  sliderInput("reduzierung_rt1", label = "Reduzierung Rt [%]", min = 00, max = 100, post  = " %", value = 50)))),
                                       wellPanel(
                                         fluidRow(
                                           column(6,
                                                  dateInput("reduzierung_datum2", label = "2. Massnahme Datum", value = "2020-03-23", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                           column(6,
                                                  sliderInput("reduzierung_rt2",label="Reduzierung Rt [%]", min = -100, max = 100, post  = " %", value = 50)))),
                                       wellPanel(
                                         fluidRow(
                                           column(6,
                                                  dateInput("reduzierung_datum3", label = "3. Massnahme Datum", value = "2020-04-01", min=as.Date('2020-03-01'), max=as.Date('2020-12-31', language="de"))),
                                           column(6,
                                                  sliderInput("reduzierung_rt3",label="Reduzierung Rt [%]", min = -100, max = 100, post  = " %", value = 0)))),
                                       
                                       
                                       
                                       h3("Expertenparameter Infektionsverlauf"),  
                                       fluidRow(
                                         column(6,
                                                wellPanel(
                                                  numericInput("ges_inf_rate", label = "Durchseuchung [%]", value = 70),
                                                  numericInput("ti", label = "Inkubationszeit [d]", value = 2),
                                                  numericInput("tod_rate", label = "Sterblichkeit [%]", value = 2., step = 0.1))),
                                         column(6,
                                                wellPanel(
                                                  numericInput("faktor_n_inf", label = "Dunkelziffer Infizierte", value = 15),
                                                  numericInput("ta", label = "Infektiosität [d]", value = 6),
                                                  numericInput("td_tod", label = "Dauer Infektion bis Tod", value = 8)))),
                                       
#                                      h3("Krankenhausaufenthalt"), 
#                                      fluidRow(
#                                        column(6,
#                                               wellPanel(
#                                                 numericInput("kh_normal", label = "Anteil an aktuellen Infizierten [%]", value = 4.5, step = 0.1),
#                                                 numericInput("t_kh", label = "Dauer", value = 14),
#                                                 numericInput("dt_inf_kh", label = "Versatz nach Infektion", value = 8))),
#                                        column(6,
#                                               wellPanel(
#                                                 numericInput("kh_intensiv", label = "Anteil Intensivstation [%]", value = 25),
#                                                 numericInput("t_intensiv", label = "Dauer Intensivstation", value = 10),
#                                                 numericInput("dt_kh_int", label = "Versatz Krankenhaus - Intensivstation", value = 1)))) ,                                        
#                                      
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
                                       
                                       h2("CoPE: Rechenmodel Verlauf Covid19 Infektionen und deren Auswirkung, version 0.12", color = "blue"),
                                       tags$head(tags$style('h2 {color:blue;}')),
                                       tags$head(tags$style('h3 {color:blue;}')),
                                       
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
                                       
                                       wellPanel(
                                       h3("Krankenhausaufenthalt"), 
                                       
                                       fluidRow(
                                        
                                         column(4,
                                                wellPanel(
                                                  numericInput("kh_normal", label = "Anteil an aktuellen Infizierten [%]", value = 4.5, step = 0.1),
                                                  numericInput("kh_intensiv", label = "Anteil Intensivstation [%]", value = 25))),
                                         column(4,  
                                                wellPanel(
                                                  numericInput("t_kh", label = "Dauer", value = 14),
                                                  numericInput("t_intensiv", label = "Dauer Intensivstation", value = 10))),
                                                  
                                         column(4,
                                                wellPanel(
                                                  numericInput("dt_inf_kh", label = "Versatz nach Infektion", value = 8),
                                                  numericInput("dt_kh_int", label = "Versatz Krankenhaus - Intensivstation", value = 1))))) , 
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
                        includeMarkdown("Readme.md")
                      )
             ),
             
             tabPanel("Datenimport",
                      
                      # Show a plot of the generated distribution
                      sidebarPanel(
                        # daten einlesen
                        fileInput("importData",
                                  label="Upload der Bettenmeldedaten",         accept = c(
                                    "xls",
                                    "xlsx"),
                                  multiple = FALSE),
                        # daten runterladen
                        downloadButton("downloadData", "Runterladen von Bettenmeldungen Vorlage"),
                        mainPanel(
                          dataTableOutput("uploadedBettenmeldedaten"))
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
  
  ######  down and upload of data
  
  # Downloadable csv of selected dataset ----
  # https://shiny.rstudio.com/reference/shiny/1.0.3/downloadHandler.html
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(rkiAndPredictData(), file)
    }
  )
  
  ####### upload data ----
  # https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
  output$uploadedBettenmeldedaten <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$importData
    
    if (is.null(inFile))
      return(NULL)
    
    a <-  read_excel(inFile$datapath)
    a$Tag <- a$Tag %>% as.Date( format="%Y-%m-%d")
    # browser()
    a
  })
  
  
  
  
  vals <- reactiveValues(Flag = "Bundesland")
  r0_no_erfasstDf <- reactiveVal(0) 
  
  observeEvent(input$BundeslandSelected,  ignoreInit = FALSE,{
    if(input$BundeslandSelected =="---"){
    }else {
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
    }else {
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
    #browser()
    dfRoNo <- r0_no_erfasstDf()[[1]]
    n0_erfasst_nom_min_max <- r0_no_erfasstDf()[[2]]
    R0_conf_nom_min_max <- r0_no_erfasstDf()[[3]]
    startDate <- r0_no_erfasstDf()[[4]]
    #browser()
    rechenDf_nom <- cbind(dfRoNo,n0_erfasst=n0_erfasst_nom_min_max$n0_erfasst_nom, R0 =R0_conf_nom_min_max$R0_nom)
    df_nom <-  Rechenkern(rechenDf_nom,input, startDate)
    
    tmp <- df_nom %>% filter(!is.na(SumAnzahl))
    letzter_Tag <- max(tmp$Tag)
    konfidenz_je_tag <- mean(c(0.023, 0.029)) # Mittelwert aus zwei separaten Untersuchungen zu log. Standardabweichungen
    #browser()
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
    
  
    #rechenDf_min <- cbind(dfRoNo,n0_erfasst=n0_erfasst_nom_min_max$n0_erfasst_min, R0 =R0_conf_nom_min_max$R0_min)
    #df_min <-  Rechenkern(rechenDf_min,input)
    
    #rechenDf_max <- cbind(dfRoNo,n0_erfasst=n0_erfasst_nom_min_max$n0_erfasst_max, R0 =R0_conf_nom_min_max$R0_max)
    #df_max <-  Rechenkern(rechenDf_max,input)
    
    #df <- left_join(df_nom, df_min, by = "Tag", suffix = c("", "_min"))
    #df <- left_join(df, df_max, by = "Tag", suffix = c("", "_max"))
    #  browser()
    df <- df_nom %>% filter(Tag >=as.Date(strptime(input$dateInput[1], format="%Y-%m-%d")),
                        Tag <=as.Date(strptime(input$dateInput[2], format="%Y-%m-%d")))
  }) 
  #browser()
  color1 = 'blue'
  color2 = 'green'
  color3 = '#6ab84d'
  color4 = 'black'
  color5 = 'gray'
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
  
  output$Kumuliert <- renderPlotly({
    
    
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    
    
    
    tmp <- rkiAndPredictData()
    # browser()
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
    tmp$ErfassteInfizierte <- as.integer(tmp$Erfasste_Infizierte)
    tmp$ErfassteInfizierteBerechnet <- as.integer(tmp$Berechnete_Infizierte)
  
    
    # browser()
    
    p <- ggplot(tmp, aes(color = "Erfasste Infizierte berechnet")) + geom_line(aes(x=Tag, y = Berechnete_Infizierte)) +   
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon), 
                  aes( x= Tag, ymin = Berechnete_Infizierte_min, ymax = Berechnete_Infizierte_max), alpha =alphaForConfidence, outline.type = "full", fill = color1) + 
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
      
      labs(color = 'Daten') + scale_y_continuous(labels = scales::comma)
    
    
    
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_si())
      
    } else {
      p
      
    }
    p <- ggplotly(p, tooltip = c("Berechnete_Infizierte", "Erfasste_Infizierte", "Tag", "Erfasste_Todesfaelle", "Berechnete_Todesfaelle"))
    p <- p %>% layout(legend = list(x = 0.69, y = 0.01, font = list(size = 8)))
    #p <- p %>% layout(legend = list(orientation = 'h'))
    p
    
  })
  #browser()
  output$Verlauf <- renderPlotly({
    
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    #  browser()
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "AnzahlTodesfall"] <- "NeueToteErfasst"
    tmp$AktuellInfizierteBerechnet <- as.integer(tmp$AktuellInfizierteBerechnet)
    tmp$NeuInfizierteBerechnet <- as.integer(tmp$NeuInfizierteBerechnet)
    p <- ggplot(tmp, aes(color ="Aktuell Infizierte berechnet")) + geom_line(aes(x=Tag, y = AktuellInfizierteBerechnet)) +  geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) + 
      geom_line(aes(x=Tag,y= NeuInfizierteBerechnet, color = "Neu Infizierte berechnet")) + 
      #geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = NeuInfizierteBerechnet_min, ymax = NeuInfizierteBerechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color2) + 
      #geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = NeueToteBerechnet_min, ymax = NeueToteBerechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color4) + 
      #geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = AktuellInfizierteBerechnet_min, ymax = AktuellInfizierteBerechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color1) + 
      
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
      
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    
    
    
    if(logy){
      p <- p +  scale_y_log10(label = label_number_si())
      
    } else {
      p
      
    }
    
    
    p <- ggplotly(p, tooltip = c("AktuellInfizierteBerechnet", "NeuInfizierteBerechnet", "Tag", "NeueToteErfasst", "NeueToteBerechnet","AnzahlFall"))
    
    
    p <- p %>% layout(legend = list(x = 0.69, y = 0.01, font = list(size = 8)))
    p
    
  }) 
  
  output$Krankenhaus <- renderPlotly({
    # browser()
    logy <- ifelse(input$logyInput == "logarithmisch" , TRUE, FALSE)
    
    tmp <- rkiAndPredictData()
    colnames(tmp)[colnames(tmp) == "KhBerechnet"] <- "Krankenhaus_berechnet"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet"] <- "Intensiv_berechnet"
    
    #min
    colnames(tmp)[colnames(tmp) == "KhBerechnet_min"] <- "Krankenhaus_berechnet_min"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet_min"] <- "Intensiv_berechnet_min"
    #max
    colnames(tmp)[colnames(tmp) == "KhBerechnet_max"] <- "Krankenhaus_berechnet_max"
    colnames(tmp)[colnames(tmp) == "IntensivBerechnet_max"] <- "Intensiv_berechnet_max"
    
    tmp$Intensiv_berechnet <- as.integer(tmp$Intensiv_berechnet)
    tmp$Krankenhaus_berechnet <- as.integer(tmp$Krankenhaus_berechnet)
    p <- ggplot(tmp, aes( color ="KH berechnet")) + geom_line(aes(x=Tag, y = Krankenhaus_berechnet)) + 
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = Krankenhaus_berechnet_min, ymax = Krankenhaus_berechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color1) + 
      geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = Intensiv_berechnet_min, ymax = Intensiv_berechnet_max), alpha =alphaForConfidence, outline.type = "full",  fill = color2) + 
      
      geom_line(aes(x=Tag,y= Intensiv_berechnet, color = "Intensiv berechnet")) +
      scale_x_date(labels = date_format("%d.%m")) + labs(title = paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Plätze in Krankenhaus / Intensivstation, CI 95%", sep ="")  ,
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
    
    #min
    #colnames(tmp)[colnames(tmp) == "TaeglichReproduktionsRateRt_min"] <- "Taegliche_Reproduktionsrate_min"
    #colnames(tmp)[colnames(tmp) == "ReduzierteRt_min"] <- "Reduzierte_Reproduktionsrate_min"
    
    #max
    #colnames(tmp)[colnames(tmp) == "TaeglichReproduktionsRateRt_max"] <- "Taegliche_Reproduktionsrate_max"
    #colnames(tmp)[colnames(tmp) == "ReduzierteRt_max"] <- "Reduzierte_Reproduktionsrate_max"
    
    tmp$Taegliche_Reproduktionsrate <- round(tmp$Taegliche_Reproduktionsrate, digits = 3)
    tmp$Reduzierte_Reproduktionsrate <- round(tmp$Reduzierte_Reproduktionsrate, digits = 3)
    
    p <- ggplot(tmp, aes(color = "Rt ohne Maßnahmen")) + geom_line(aes(x=Tag, y = Taegliche_Reproduktionsrate), linetype = 2) +
      #geom_ribbon(data =tmp%>% filter(Tag <= perdictionHorizon),  aes( x= Tag, ymin = Reduzierte_Reproduktionsrate_min, ymax = Reduzierte_Reproduktionsrate_max), alpha =alphaForConfidence, outline.type = "full",  fill = color2) + 
geom_line(aes(x=Tag,y = Reduzierte_Reproduktionsrate, color = "Rt aktuell"))  +
      scale_x_date(labels = date_format("%d.%m")) + labs(title =  paste0(rkiAndPredictData() %>% filter(!is.na(whichRegion)) %>% select(whichRegion) %>% unique(), ": Tägliche Reproduktionsrate Rt, CI 95%", sep ="")  , x = "Datum", y = "Wert",
                                                         caption = "Daten von https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")  +   scale_color_manual(values = c(
                                                           'Rt ohne Maßnahmen' = color1,
                                                           'Rt aktuell' = color2)) +
      
      labs(color = 'Daten')+ scale_y_continuous(labels = scales::comma)
    p <- ggplotly(p, tooltip = c("Taegliche_Reproduktionsrate","Reduzierte_Reproduktionsrate", "Tag"))
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
      # browser()
      regionSelected = 2
      r0_no_erfasstDf  <- createLandkreisR0_no_erfasstDf(historyDfBundesLand, historyDfBund, regionSelected, vals, input,session)
      r0_no_erfasstDf(r0_no_erfasstDf)
      # set menu of Landkreis to "---"
      updateSelectInput(session, "LandkreiseSelected",  selected = "---")
    }
    
    if(input$LandkreiseSelected =="---"){
    }else {
      #browser()
      vals$Flag  <- "Landkreis"
      regionSelected = 3
      r0_no_erfasstDf  <- createLandkreisR0_no_erfasstDf(historyDfLandkreis, historyDfBund, regionSelected, vals, input,session)
      r0_no_erfasstDf(r0_no_erfasstDf)
      # browser()
      updateSelectInput(session, "BundeslandSelected",  selected = "---")
    }
    
  })
  
}

shinyApp(ui, server, enableBookmarking = "server")


