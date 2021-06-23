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
library(markdown)
#*******************************************************************************
# Datasets
# Data Meteo
meteoMontpellier <- as.data.frame(read_csv2("dataMeteo/meteoMontpellier.csv"))[,-1]
#*******************************************************************************
# Data lab
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
                    title = "Psyl'risk"
                            ),
#///////////////////////////////////////////////////////////////////////////////
    dashboardSidebar(
        tags$style(".left-side, .main-sidebar {padding-top: 50px}"),
        sidebarMenu(
            HTML(paste0(
                "<br>",
                "<a href='https://walidkandouci.shinyapps.io/PsylleShinyApp/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo.png' width = '186'></a>",
                "<br>"
            )),
            menuItem("Acceuil", tabName = "Home", icon = icon("home")),
            menuItem("Visualisation", tabName = "Visualization", icon = icon("chart-area")),
            menuItem("Gallery", tabName = "Gallery", icon = icon("camera-retro")),
            menuItem("Maps", tabName = "Maps", icon = icon("map-marked-alt")),
            menuItem("Donnes meteo", tabName = "DataMeteoMontpellier", icon = icon("cloud-sun-rain")),
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
            tabPanel(""),
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
                tabPanel("Donnes meteo Montpellier",  
                         sidebarLayout(   
                             sidebarPanel(    
                                 selectInput(
                                     inputId = "data_meteoMontpellier" ,
                                     label = "Donnees Meteo Montpellier",
                                     choices = as.character(c(2000:2020))             
                                 )),
                             mainPanel(DT::dataTableOutput('MeteoMontpellierData'),)               
                         )               
                ),
                tabPanel("Plot Meteo Montpellier 2005 (temperature)",
                         plotlyOutput("plotyplotMeteoTemp", width = "1000px", height = "500px")),       
                tabPanel("Plot Meteo Montpellier 2005 (humidite)",
                         plotlyOutput("plotyplotMeteoHumy", width = "1000px", height = "500px"))
                               
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
        "2000" = meteoMontpellier[meteoMontpellier$ID==2000,-4],
        "2001" = meteoMontpellier[meteoMontpellier$ID==2001,-4],
        "2002" = meteoMontpellier[meteoMontpellier$ID==2002,-4],
        "2003" = meteoMontpellier[meteoMontpellier$ID==2003,-4],
        "2004" = meteoMontpellier[meteoMontpellier$ID==2004,-4],
        "2005" = meteoMontpellier[meteoMontpellier$ID==2005,-4],
        "2006" = meteoMontpellier[meteoMontpellier$ID==2006,-4],
        "2007" = meteoMontpellier[meteoMontpellier$ID==2007,-4],
        "2008" = meteoMontpellier[meteoMontpellier$ID==2008,-4],
        "2009" = meteoMontpellier[meteoMontpellier$ID==2009,-4],
        "2010" = meteoMontpellier[meteoMontpellier$ID==2010,-4],
        "2011" = meteoMontpellier[meteoMontpellier$ID==2011,-4],
        "2012" = meteoMontpellier[meteoMontpellier$ID==2012,-4],
        "2013" = meteoMontpellier[meteoMontpellier$ID==2013,-4],
        "2014" = meteoMontpellier[meteoMontpellier$ID==2014,-4],
        "2015" = meteoMontpellier[meteoMontpellier$ID==2015,-4],
        "2016" = meteoMontpellier[meteoMontpellier$ID==2016,-4],
        "2017" = meteoMontpellier[meteoMontpellier$ID==2017,-4],
        "2018" = meteoMontpellier[meteoMontpellier$ID==2018,-4],
        "2019" = meteoMontpellier[meteoMontpellier$ID==2019,-4],
        "2020" = meteoMontpellier[meteoMontpellier$ID==2020,-4]
    )
})
output$MeteoMontpellierData <- DT::renderDataTable(DT::datatable({
    datasetInput_meteo()
}))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
output$plotyplotMeteoTemp <- renderPlotly({
    fig <- plot_ly(
        #filteredData(),
        meteoMontpellier,
        x = ~ date,
        y = ~ temperature,
        color = ~ factor(ID),
        mode = "lines",
        type = "scatter"
    )
    fig <-
        fig %>% layout(
            title = 'Temperatures Montpellier (2000-2020)',
            xaxis = list(title = 'Temps',
                         zeroline = TRUE),
            yaxis = list(title = 'Temperatures',
                         zeroline = TRUE)
        )
    fig
})

output$plotyplotMeteoHumy <- renderPlotly({
    fig <- plot_ly(
        #filteredData(),
        meteoMontpellier,
        x = ~ date,
        y = ~ humidity,
        color = ~ factor(ID),
        mode = "lines",
        type = "scatter"
    )
    fig <-
        fig %>% layout(
            title = 'Humidite Montpellier (2000-2020)',
            xaxis = list(title = 'Temps',
                         zeroline = TRUE),
            yaxis = list(title = 'Temperatures',
                         zeroline = TRUE)
        )
    fig
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyApp(ui, server)