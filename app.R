#setwd("C:/Users/Walid/Documents/ShinyApp/Psyllids")
#*******************************************************************************
# Packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(ggplot2)
library(readr)
library(plotly)
library(leaflet)
library(rlang)
library(shinythemes)
library(dplyr)
library(plotly)
library(sass)
#*******************************************************************************
# Datasets
# Data Meteo Fr
Data_ALLMeteo_Montpellier <- read_csv2("dataMeteo/Montpellier/Data_ALLMeteo_Montpellier.csv")
Data_ALLMeteo_Montpellier$temperature <- as.numeric(Data_ALLMeteo_Montpellier$temperature)
Data_ALLMeteo_Montpellier$humidity <- as.numeric(Data_ALLMeteo_Montpellier$humidity)
Data_ALLMeteo_Montpellier$ID <- as.numeric(Data_ALLMeteo_Montpellier$ID)

Data_ALLMeteo_Montpellier_mean <- read_csv2("dataMeteo/Montpellier/Data_ALLMeteo_Montpellier_mean.csv")
Data_ALLMeteo_Montpellier_mean$temperature <- as.numeric(Data_ALLMeteo_Montpellier_mean$temperature)
Data_ALLMeteo_Montpellier_mean$humidity <- as.numeric(Data_ALLMeteo_Montpellier_mean$humidity)
Data_ALLMeteo_Montpellier_mean$ID <- as.numeric(Data_ALLMeteo_Montpellier_mean$ID)

Data_ALLMeteo_Montpellier_mean_plot <- read_csv2("dataMeteo/Montpellier/Data_ALLMeteo_Montpellier_mean_plot.csv")
Data_ALLMeteo_Montpellier_mean_plot$temperature <- as.numeric(Data_ALLMeteo_Montpellier_mean_plot$temperature)
Data_ALLMeteo_Montpellier_mean_plot$humidity <- as.numeric(Data_ALLMeteo_Montpellier_mean_plot$humidity)
Data_ALLMeteo_Montpellier_mean_plot$ID <- as.numeric(Data_ALLMeteo_Montpellier_mean_plot$ID)

Data_Meteo_Montpellier_2000 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2000,c(-1)]
Data_Meteo_Montpellier_2001 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2001,c(-1)]
Data_Meteo_Montpellier_2002 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2002,c(-1)]
Data_Meteo_Montpellier_2003 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2003,c(-1)]
Data_Meteo_Montpellier_2004 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2004,c(-1)]
Data_Meteo_Montpellier_2005 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2005,c(-1)]
Data_Meteo_Montpellier_2006 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2006,c(-1)]
Data_Meteo_Montpellier_2007 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2007,c(-1)]
Data_Meteo_Montpellier_2008 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2008,c(-1)]
Data_Meteo_Montpellier_2009 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2009,c(-1)]
Data_Meteo_Montpellier_2010 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2010,c(-1)]
Data_Meteo_Montpellier_2011 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2011,c(-1)]
Data_Meteo_Montpellier_2012 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2012,c(-1)]
Data_Meteo_Montpellier_2013 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2013,c(-1)]
Data_Meteo_Montpellier_2014 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2014,c(-1)]
Data_Meteo_Montpellier_2015 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2015,c(-1)]
Data_Meteo_Montpellier_2016 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2016,c(-1)]
Data_Meteo_Montpellier_2017 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2017,c(-1)]
Data_Meteo_Montpellier_2018 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2018,c(-1)]
Data_Meteo_Montpellier_2019 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2019,c(-1)]
Data_Meteo_Montpellier_2020 <- Data_ALLMeteo_Montpellier[Data_ALLMeteo_Montpellier$ID==2020,c(-1)]

Data_Meteo_Montpellier_mean_2000 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2000,c(-1)]
Data_Meteo_Montpellier_mean_2001 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2001,c(-1)]
Data_Meteo_Montpellier_mean_2002 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2002,c(-1)]
Data_Meteo_Montpellier_mean_2003 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2003,c(-1)]
Data_Meteo_Montpellier_mean_2004 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2004,c(-1)]
Data_Meteo_Montpellier_mean_2005 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2005,c(-1)]
Data_Meteo_Montpellier_mean_2006 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2006,c(-1)]
Data_Meteo_Montpellier_mean_2007 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2007,c(-1)]
Data_Meteo_Montpellier_mean_2008 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2008,c(-1)]
Data_Meteo_Montpellier_mean_2009 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2009,c(-1)]
Data_Meteo_Montpellier_mean_2010 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2010,c(-1)]
Data_Meteo_Montpellier_mean_2011 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2011,c(-1)]
Data_Meteo_Montpellier_mean_2012 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2012,c(-1)]
Data_Meteo_Montpellier_mean_2013 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2013,c(-1)]
Data_Meteo_Montpellier_mean_2014 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2014,c(-1)]
Data_Meteo_Montpellier_mean_2015 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2015,c(-1)]
Data_Meteo_Montpellier_mean_2016 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2016,c(-1)]
Data_Meteo_Montpellier_mean_2017 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2017,c(-1)]
Data_Meteo_Montpellier_mean_2018 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2018,c(-1)]
Data_Meteo_Montpellier_mean_2019 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2019,c(-1)]
Data_Meteo_Montpellier_mean_2020 <- Data_ALLMeteo_Montpellier_mean[Data_ALLMeteo_Montpellier_mean$ID==2020,c(-1)]

Data_Meteo_Montpellier_mean_2000_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2000,c(-1)]
Data_Meteo_Montpellier_mean_2001_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2001,c(-1)]
Data_Meteo_Montpellier_mean_2002_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2002,c(-1)]
Data_Meteo_Montpellier_mean_2003_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2003,c(-1)]
Data_Meteo_Montpellier_mean_2004_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2004,c(-1)]
Data_Meteo_Montpellier_mean_2005_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2005,c(-1)]
Data_Meteo_Montpellier_mean_2006_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2006,c(-1)]
Data_Meteo_Montpellier_mean_2007_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2007,c(-1)]
Data_Meteo_Montpellier_mean_2008_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2008,c(-1)]
Data_Meteo_Montpellier_mean_2009_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2009,c(-1)]
Data_Meteo_Montpellier_mean_2010_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2010,c(-1)]
Data_Meteo_Montpellier_mean_2011_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2011,c(-1)]
Data_Meteo_Montpellier_mean_2012_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2012,c(-1)]
Data_Meteo_Montpellier_mean_2013_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2013,c(-1)]
Data_Meteo_Montpellier_mean_2014_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2014,c(-1)]
Data_Meteo_Montpellier_mean_2015_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2015,c(-1)]
Data_Meteo_Montpellier_mean_2016_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2016,c(-1)]
Data_Meteo_Montpellier_mean_2017_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2017,c(-1)]
Data_Meteo_Montpellier_mean_2018_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2018,c(-1)]
Data_Meteo_Montpellier_mean_2019_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2019,c(-1)]
Data_Meteo_Montpellier_mean_2020_plot <- Data_ALLMeteo_Montpellier_mean_plot[Data_ALLMeteo_Montpellier_mean_plot$ID==2020,c(-1)]
#*******************************************************************************
# Data 2005
dataLab <- as.data.frame(read_csv("dataPlant/dataLab.csv"))[,-1]
#*******************************************************************************
# Data field

dataField <- as.data.frame(read_csv("dataField/dataField.csv"))[,-1]
dataFieldIM   <- dataField[,-3]
dataFieldREIM <- dataField[,-2]
dataFieldIM[,10]   <- paste('IM',dataFieldIM$year)
dataFieldREIM[,10] <- paste("REIM",dataFieldIM$year)
colnames(dataFieldIM)   <- c("day","numPsy","year","city","location","department","lon","lat","temperature","ID")
colnames(dataFieldREIM) <- c("day","numPsy","year","city","location","department","lon","lat","temperature","ID")

dataField_list <-split(dataField, dataField$department)
dataField_plot <- rbind(dataFieldIM,dataFieldREIM)
dataField_plot <- split(dataField_plot, dataField$department)
#*******************************************************************************
# Data Map

dataSpecies <- as.data.frame(read_csv("dataMap/dataSpecies.csv"))[,-1]

#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 50px}"),
                            tags$style(".main-header .logo {height: 50px}")
    ),
                    title = ""#,
                #    tags$li(class = "dropdown",
                #            tags$a(href="https://umr-phim.cirad.fr/", target="_blank", 
                #                   tags$img(height = "100px", alt="PHIM Logo", src="PHIMlogo.png")
                #            )),
                #    tags$li(class = "dropdown",
                #            tags$a(href="https://www.inrae.fr/", target="_blank", 
                #                   tags$img(height = "100px", alt="INRAE Logo", src="INRAElogo.png")
                #            )),
                #    tags$li(class = "dropdown",
                #            tags$a(href="https://www.cirad.fr/", target="_blank", 
                #                   tags$img(height = "100px", alt="CIRAD Logo", src="CIRADlogo.png")
                            ),
#///////////////////////////////////////////////////////////////////////////////
    dashboardSidebar(
        tags$style(".left-side, .main-sidebar {padding-top: 50px}"),

        sidebarMenu(
            HTML(paste0(
                "<br>",
                "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='PsyllidsLogo.png' width = '186'></a>",
                "<br>"
            )),
            menuItem("Acceuil", tabName = "Home", icon = icon("home")),
            menuItem("Visualisation", tabName = "Visualization", icon = icon("chart-area")),
            menuItem("Gallery", tabName = "Gallery", icon = icon("camera-retro")),
            menuItem("Maps", tabName = "Maps", icon = icon("map-marked-alt")),
            menuItem("DataMeteoMontpellier", tabName = "DataMeteoMontpellier", icon = icon("cloud-sun-rain")),
            menuItem("Donnes laboratoire (2005)", tabName = "data2005", icon = icon("file")),
            menuItem("Code R", tabName = "CodeR", icon = icon("code")),
            menuItem("About us", tabName = "AboutUs", icon = icon("question-circle")),
            menuItem("Sources", tabName = "Sources", icon = icon("book"))
        )
    ),
    dashboardBody(
        tabItems(
                tabItem(tabName = "Home", includeMarkdown("Accueil.md")),
#///////////////////////////////////////////////////////////////////////////////
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gallery
tabItem(tabName = "Gallery",
        includeHTML("Gallery.Rhtml"),
        includeCSS("Galleryy.css")
        ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualization
tabItem(tabName = "Visualization",
        fluidRow(
            tabBox(
                width = NULL,
                height = NULL,
                tabPanel(""),
                tabPanel("Herault: Plot",          
                         plotlyOutput("plotyHerault", width = "1000px", height = "500px")      
                         ),
                tabPanel("Pyrennes Orientales: Plot",          
                         plotlyOutput("plotyPyrOr", width = "1000px", height = "500px")      
                ),
                tabPanel("Tarn et Garonne: Plot",          
                         plotlyOutput("plotyTarnGar", width = "1000px", height = "500px")      
                ),
                tabPanel("Drome: Plot",          
                         plotlyOutput("plotyDrome", width = "1000px", height = "500px")      
                ),
                tabPanel("Suisse: Plot",          
                         plotlyOutput("plotySuisse", width = "1000px", height = "500px")      
                ),
                tabPanel("Donnees Terrain",          
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(
                                     inputId = "data_Field" ,
                                     label = "Donnees terrain",
                                     choices = c("Herault", "Pyrennes Orientales", "Tarn et Garonne", "Drome", "Suisse")
                                 )),
                             mainPanel(DT::dataTableOutput('dataTerrain'),)
                         )
                )
                )
            )
        ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data 2005
tabItem(tabName = "data2005",
        fluidRow(
            tabBox(
                width = NULL,
                height = NULL,
                tabPanel(""),
                tabPanel("Donnees labratoire 2005",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(
                                     inputId = "data_lab" ,
                                     label = "Donnees laboratoire (2005)",
                                     choices = c("1A","1B","2.3","2.4","3.1","3.2","3.3",
                                                 "4.1","4.2A","4.2B","4.3A","4.3B","5.1","5.2","5.3","5.4","6.1",
                                                 "6.2","6.3","7.1","7.2A","7.2B","8.1","8.2","9.1","9.2","10.1",
                                                 "10.2A","10.2B","11.2","12.1","12.2","13.3")
                                     )),
                             mainPanel(DT::dataTableOutput('dataLabo'),)
                             )
                         )
                )
            )
        ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MAP
tabItem(
    tabPanel(""),
    tabName = "Maps",
    fluidRow(
        tabBox(   
            width = NULL, 
            height = NULL,
            tabPanel("Bassins",    
                     leafletOutput("mapBassins", width = "1500", height = "1000")),    
            tabPanel("Especes A et B",
                     leafletOutput("mapSpecies", width = "1500", height = "1000"))            
            )       
        )        
    ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Meteo
tabItem(tabName = "DataMeteoMontpellier",       
        fluidRow(         
            tabBox(   
                width = NULL,  
                height = NULL, 
                tabPanel(""),
                tabPanel("Plot Meteo Montpellier (temperature)",
                         plotlyOutput("plotyplotMeteoTemp", width = "1000px", height = "500px")),       
                tabPanel("Plot Meteo Montpellier (humidity)",
                         plotlyOutput("plotyplotMeteoHumy", width = "1000px", height = "500px")),
                tabPanel("Data meteo Montpellier",  
                         sidebarLayout(   
                             sidebarPanel(    
                                 selectInput(
                                     inputId = "data_meteoMontpellier" ,
                                     label = "Donnees Meteo Montpellier",
                                     choices = as.character(c(2000:2020))             
                                     )),
                             mainPanel(DT::dataTableOutput('MeteoMontpellierData'),)               
                             )               
                         )               
                )               
            )                
        )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    )))
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
server <- function(input, output, session){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualisation
    output$plotyHerault <- renderPlotly({
        fig <- plot_ly(
            dataField_plot$Herault,
            x = ~ day,
            y = ~ numPsy,
            mode = "lines+markers",
            color = ~ factor(ID),
            hoverinfo = "text",
            text =  ~ paste(
                '</br>',
                ID,
                '</br> location: ',
                location,
                '</br> nombre de psylle: ',
                numPsy,
                '</br> temperature: ',
                temperature,
                '</br> jour: ',
                day
            )
        )
        fig <-
            fig %>% layout(
                title = 'Psyllids observed from 2000 to 2021',
                xaxis = list(title = 'Day',
                             zeroline = TRUE),
                yaxis = list(title = 'Number of Psyllids',
                             zeroline = TRUE)
            )
        fig
    })
    
    output$plotyDrome <- renderPlotly({
        fig <- plot_ly(
            dataField_plot$Drome,
            x = ~ day,
            y = ~ numPsy,
            mode = "lines+markers",
            color = ~ factor(ID),
            hoverinfo = "text",
            text =  ~ paste(
                '</br>',
                ID,
                '</br> location: ',
                location,
                '</br> nombre de psylle: ',
                numPsy,
                '</br> temperature: ',
                temperature,
                '</br> jour: ',
                day
            )
        )
        fig <-
            fig %>% layout(
                title = 'Psyllids observed from 2000 to 2021',
                xaxis = list(title = 'Day',
                             zeroline = TRUE),
                yaxis = list(title = 'Number of Psyllids',
                             zeroline = TRUE)
            )
        fig
    })
    
    output$plotyPyrOr <- renderPlotly({
        fig <- plot_ly(
            dataField_plot$`Pyrenees Orientales`,
            x = ~ day,
            y = ~ numPsy,
            mode = "lines+markers",
            color = ~ factor(ID),
            hoverinfo = "text",
            text =  ~ paste(
                '</br>',
                ID,
                '</br> location: ',
                location,
                '</br> nombre de psylle: ',
                numPsy,
                '</br> temperature: ',
                temperature,
                '</br> jour: ',
                day
            )
        )
        fig <-
            fig %>% layout(
                title = 'Psyllids observed from 2000 to 2021',
                xaxis = list(title = 'Day',
                             zeroline = TRUE),
                yaxis = list(title = 'Number of Psyllids',
                             zeroline = TRUE)
            )
        fig
    })
    
    output$plotySuisse <- renderPlotly({
        fig <- plot_ly(
            dataField_plot$Switzerland,
            x = ~ day,
            y = ~ numPsy,
            mode = "lines+markers",
            color = ~ factor(ID),
            hoverinfo = "text",
            text =  ~ paste(
                '</br>',
                ID,
                '</br> location: ',
                location,
                '</br> nombre de psylle: ',
                numPsy,
                '</br> temperature: ',
                temperature,
                '</br> jour: ',
                day
            )
        )
        fig <-
            fig %>% layout(
                title = 'Psyllids observed from 2000 to 2021',
                xaxis = list(title = 'Day',
                             zeroline = TRUE),
                yaxis = list(title = 'Number of Psyllids',
                             zeroline = TRUE)
            )
        fig
    })
    
    output$plotyTarnGar <- renderPlotly({
        fig <- plot_ly(
            dataField_plot$`Tarn et Garonne`,
            x = ~ day,
            y = ~ numPsy,
            mode = "lines+markers",
            color = ~ factor(ID),
            hoverinfo = "text",
            text =  ~ paste(
                '</br>',
                ID,
                '</br> location: ',
                location,
                '</br> nombre de psylle: ',
                numPsy,
                '</br> temperature: ',
                temperature,
                '</br> jour: ',
                day
            )
        )
        fig <-
            fig %>% layout(
                title = 'Psyllids observed from 2000 to 2021',
                xaxis = list(title = 'Day',
                             zeroline = TRUE),
                yaxis = list(title = 'Number of Psyllids',
                             zeroline = TRUE)
            )
        fig
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data field
datasetInput_field <- reactive({
    switch(
        input$data_Field,
        "Herault" = dataField_list$Herault,
        "Pyrennes Orientales" = dataField_list$`Pyrenees Orientales`,
        "Tarn et Garonne" = dataField_list$`Tarn et Garonne`,
        "Drome" = dataField_list$Drome,
        "Suisse" = dataField_list$Switzerland
    )
})
output$dataTerrain <- DT::renderDataTable(DT::datatable({
    datasetInput_field()
}))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map
    output$mapBassins <- renderLeaflet({
        map_bassins <- leaflet() %>%
            addTiles() %>%
            setView(lng = 2.209667,
                    lat = 46.232193,
                    zoom = 6) %>%
            addMarkers(lng = 1.35,
                       lat = 44.0167,
                       popup = "Tarn et Garonne") %>%
            addMarkers(lng = 2.34833,
                       lat = 42.6681,
                       popup = "Pyrenees-Orientales") %>%
            addMarkers(lng = 6.0667,
                       lat = 43.15,
                       popup = "Plaine de Valence") %>%
            addMarkers(lng = 4.9,
                       lat = 44.9333,
                       popup = "La Crau")
        map_bassins %>% addProviderTiles(providers$CartoDB.DarkMatter)
    })
    
    output$mapSpecies <- renderLeaflet({
        M <- leaflet(data = dataSpecies) %>% 
            addTiles() %>% 
            setView(lng = 2.209667,lat = 46.232193,zoom = 6) %>% 
            addCircleMarkers(lng = dataSpecies[dataSpecies$type=="A",4],  lat = dataSpecies[dataSpecies$type=="A",3], color = "yellow", radius = 10, stroke = F) %>%
            addCircleMarkers(lng = dataSpecies[dataSpecies$type=="B",4],  lat = dataSpecies[dataSpecies$type=="B",3], color = "red", radius = 10, stroke = F) %>%
            addCircleMarkers(lng = dataSpecies[dataSpecies$type=="AB",4], lat = dataSpecies[dataSpecies$type=="AB",3], color = "orange", radius = 10, stroke = F)
        M %>% addProviderTiles(providers$CartoDB.DarkMatter)
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2005
datasetInput_lab <- reactive({
    switch(
        input$data_lab,
        "1A"    = dataLab[dataLab$tree=="1A",],
        "1B"    = dataLab[dataLab$tree=="1B",],
        "2.3"   = dataLab[dataLab$tree=="2.3",],
        "2.4"   = dataLab[dataLab$tree=="2.4",],
        "3.1"   = dataLab[dataLab$tree=="3.1",],
        "3.2"   = dataLab[dataLab$tree=="3.2",],
        "3.3"   = dataLab[dataLab$tree=="3.3",],
        "4.1"   = dataLab[dataLab$tree=="4.1",],
        "4.2A"  = dataLab[dataLab$tree=="4.2A",],
        "4.2B"  = dataLab[dataLab$tree=="4.2B",],
        "4.3A"  = dataLab[dataLab$tree=="4.3A",],
        "4.3B"  = dataLab[dataLab$tree=="4.3B",],
        "5.1"   = dataLab[dataLab$tree=="5.1",],
        "5.2"   = dataLab[dataLab$tree=="5.2",],
        "5.3"   = dataLab[dataLab$tree=="5.3",],
        "5.4"   = dataLab[dataLab$tree=="5.4",],
        "6.1"   = dataLab[dataLab$tree=="6.1",],
        "6.2"   = dataLab[dataLab$tree=="6.2",],
        "6.3"   = dataLab[dataLab$tree=="6.3",],
        "7.1"   = dataLab[dataLab$tree=="7.1",],
        "7.2A"  = dataLab[dataLab$tree=="7.2A",],
        "7.2B"  = dataLab[dataLab$tree=="7.2B",],
        "8.1"   = dataLab[dataLab$tree=="8.1",],
        "8.2"   = dataLab[dataLab$tree=="8.2",],
        "9.1"   = dataLab[dataLab$tree=="9.1",],
        "9.2"   = dataLab[dataLab$tree=="9.2",],
        "10.1"  = dataLab[dataLab$tree=="10.1",],
        "10.2A" = dataLab[dataLab$tree=="10.2A",],
        "10.2B" = dataLab[dataLab$tree=="10.2B",],
        "11.2"  = dataLab[dataLab$tree=="11.2",],
        "12.1"  = dataLab[dataLab$tree=="12.1",],
        "12.2"  = dataLab[dataLab$tree=="12.2",],
        "13.3"  = dataLab[dataLab$tree=="13.3",]
    )
})
output$dataLabo <- DT::renderDataTable(DT::datatable({
    datasetInput_lab()
}))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Meteo Montpellier
datasetInput_meteo <- reactive({
    switch(
        input$data_meteoMontpellier,
        "2000" = Data_Meteo_Montpellier_2000,
        "2001" = Data_Meteo_Montpellier_2001,
        "2002" = Data_Meteo_Montpellier_2002,
        "2003" = Data_Meteo_Montpellier_2003,
        "2004" = Data_Meteo_Montpellier_2004,
        "2005" = Data_Meteo_Montpellier_2005,
        "2006" = Data_Meteo_Montpellier_2006,
        "2007" = Data_Meteo_Montpellier_2007,
        "2008" = Data_Meteo_Montpellier_2008,
        "2009" = Data_Meteo_Montpellier_2009,
        "2010" = Data_Meteo_Montpellier_2010,
        "2011" = Data_Meteo_Montpellier_2011,
        "2012" = Data_Meteo_Montpellier_2012,
        "2013" = Data_Meteo_Montpellier_2013,
        "2014" = Data_Meteo_Montpellier_2014,
        "2015" = Data_Meteo_Montpellier_2015,
        "2016" = Data_Meteo_Montpellier_2016,
        "2017" = Data_Meteo_Montpellier_2017,
        "2018" = Data_Meteo_Montpellier_2018,
        "2019" = Data_Meteo_Montpellier_2019,
        "2020" = Data_Meteo_Montpellier_2020
    )
})
output$MeteoMontpellierData <- DT::renderDataTable(DT::datatable({
    datasetInput_meteo()
}))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
output$plotyplotMeteoTemp <- renderPlotly({
    fig <- plot_ly(
        #filteredData(),
        Data_ALLMeteo_Montpellier_mean_plot,
        x = ~ date,
        y = ~ temperature,
        #split = id,
        mode = "lines",
        type = "scatter",
        color = ~ factor(ID)
    )
    fig <-
        fig %>% layout(
            title = 'Meteo Data from 2000 to 2021 (Montpellier)',
            xaxis = list(title = 'Time',
                         zeroline = TRUE),
            yaxis = list(title = 'Temperatures',
                         zeroline = TRUE)
        )
    fig
})

output$plotyplotMeteoHumy <- renderPlotly({
    fig <- plot_ly(
        #filteredData(),
        Data_ALLMeteo_Montpellier_mean_plot,
        x = ~ date,
        y = ~ humidity,
        #split = id,
        mode = "lines",
        type = "scatter",
        color = ~ factor(ID)
    )
    fig <-
        fig %>% layout(
            title = 'Meteo Data from 2000 to 2021 (Montpellier)',
            xaxis = list(title = 'Time',
                         zeroline = TRUE),
            yaxis = list(title = 'Humidity',
                         zeroline = TRUE)
        )
    fig
})
# Home page


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyApp(ui, server)