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
Data_ALLMeteo_Montpellier <- read_csv2("Data_ALLMeteo_Montpellier.csv")
Data_ALLMeteo_Montpellier$temperature <- as.numeric(Data_ALLMeteo_Montpellier$temperature)
Data_ALLMeteo_Montpellier$humidity <- as.numeric(Data_ALLMeteo_Montpellier$humidity)
Data_ALLMeteo_Montpellier$ID <- as.numeric(Data_ALLMeteo_Montpellier$ID)

Data_ALLMeteo_Montpellier_mean <- read_csv2("Data_ALLMeteo_Montpellier_mean.csv")
Data_ALLMeteo_Montpellier_mean$temperature <- as.numeric(Data_ALLMeteo_Montpellier_mean$temperature)
Data_ALLMeteo_Montpellier_mean$humidity <- as.numeric(Data_ALLMeteo_Montpellier_mean$humidity)
Data_ALLMeteo_Montpellier_mean$ID <- as.numeric(Data_ALLMeteo_Montpellier_mean$ID)

Data_ALLMeteo_Montpellier_mean_plot <- read_csv2("Data_ALLMeteo_Montpellier_mean_plot.csv")
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
Data_semaineA1   <-
    as.data.frame(read_excel("Data_2005_Comptage_1A.xlsx", na = "NA"))
Data_semaineAB   <-
    as.data.frame(read_excel("Data_2005_Comptage_1B.xlsx", na = "NA"))
Data_semaine23   <-
    as.data.frame(read_excel("Data_2005_Comptage_2,3.xlsx", na = "NA"))
Data_semaine24   <-
    as.data.frame(read_excel("Data_2005_Comptage_2,4.xlsx", na = "NA"))
Data_semaine31   <-
    as.data.frame(read_excel("Data_2005_Comptage_3,1.xlsx", na = "NA"))
Data_semaine32   <-
    as.data.frame(read_excel("Data_2005_Comptage_3,2.xlsx", na = "NA"))
Data_semaine33   <-
    as.data.frame(read_excel("Data_2005_Comptage_3,3.xlsx", na = "NA"))
Data_semaine41   <-
    as.data.frame(read_excel("Data_2005_Comptage_4,1.xlsx", na = "NA"))
Data_semaine42A  <-
    as.data.frame(read_excel("Data_2005_Comptage_4,2A.xlsx", na = "NA"))
Data_semaine42B  <-
    as.data.frame(read_excel("Data_2005_Comptage_4,2B.xlsx", na = "NA"))
Data_semaine43A  <-
    as.data.frame(read_excel("Data_2005_Comptage_4,3A.xlsx", na = "NA"))
Data_semaine43B  <-
    as.data.frame(read_excel("Data_2005_Comptage_4,3B.xlsx", na = "NA"))
Data_semaine51   <-
    as.data.frame(read_excel("Data_2005_Comptage_5,1.xlsx", na = "NA"))
Data_semaine52   <-
    as.data.frame(read_excel("Data_2005_Comptage_5,2.xlsx", na = "NA"))
Data_semaine53   <-
    as.data.frame(read_excel("Data_2005_Comptage_5,3.xlsx", na = "NA"))
Data_semaine61   <-
    as.data.frame(read_excel("Data_2005_Comptage_6,1.xlsx", na = "NA"))
Data_semaine62   <-
    as.data.frame(read_excel("Data_2005_Comptage_6,2.xlsx", na = "NA"))
Data_semaine63   <-
    as.data.frame(read_excel("Data_2005_Comptage_6,3.xlsx", na = "NA"))
Data_semaine71   <-
    as.data.frame(read_excel("Data_2005_Comptage_7,1.xlsx", na = "NA"))
Data_semaine72A  <-
    as.data.frame(read_excel("Data_2005_Comptage_7,2A.xlsx", na = "NA"))
Data_semaine72B  <-
    as.data.frame(read_excel("Data_2005_Comptage_7,2B.xlsx", na = "NA"))
Data_semaine81   <-
    as.data.frame(read_excel("Data_2005_Comptage_8,1.xlsx", na = "NA"))
Data_semaine82   <-
    as.data.frame(read_excel("Data_2005_Comptage_8,2.xlsx", na = "NA"))
Data_semaine91   <-
    as.data.frame(read_excel("Data_2005_Comptage_9,1.xlsx", na = "NA"))
Data_semaine92   <-
    as.data.frame(read_excel("Data_2005_Comptage_9,2.xlsx", na = "NA"))
Data_semaine101  <-
    as.data.frame(read_excel("Data_2005_Comptage_10,1.xlsx", na = "NA"))
Data_semaine102B <-
    as.data.frame(read_excel("Data_2005_Comptage_10,2B.xlsx", na = "NA"))
Data_semaine112  <-
    as.data.frame(read_excel("Data_2005_Comptage_11,2.xlsx", na = "NA"))
Data_semaine121  <-
    as.data.frame(read_excel("Data_2005_Comptage_12,2.xlsx", na = "NA"))
Data_semaine122  <-
    as.data.frame(read_excel("Data_2005_Comptage_12,2.xlsx", na = "NA"))
Data_semaine131  <-
    as.data.frame(read_excel("Data_2005_Comptage_13,3.xlsx", na = "NA"))
#*******************************************************************************
# Data field
Data_terrain <- as.data.frame(read_csv("Data_terrain.csv"))
Data_terrain_toplotly <- read.csv("Data_terrrain_toplotly.csv")
Data_terrain_toplotly <- Data_terrain_toplotly[, -1]
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
            menuItem("Home page", tabName = "Home", icon = icon("home")),
            menuItem("Visualization", tabName = "Visualization", icon = icon("chart-area")),
            menuItem("Gallery", tabName = "Gallery", icon = icon("camera-retro")),
            menuItem("Map: Collect data", tabName = "Map_bassins", icon = icon("map-marked-alt")),
            menuItem("Datasets", tabName = "Datasets", icon = icon("database"),
                     menuSubItem("DataMeteoMontpellier", tabName = "DataMeteoMontpellier", icon = icon("cloud-sun-rain")),
                     menuSubItem("Data2005", tabName = "Data2005", icon = icon("file"))
                     ),
            menuItem("Code R", tabName = "CodeR", icon = icon("code")),
            menuItem("About us", tabName = "AboutUs", icon = icon("question-circle")),
            menuItem("Sources", tabName = "Sources", icon = icon("book"))
        )
    ),
    dashboardBody(
        tabItems(
                tabItem(tabName = "Home", tags$video(src="VIDEO.MP4", type = "video/mp4",
                                                     height = "500px", width = "500px",#alt= 'Something went wrong',
                                                     controls = "controls")
                        ),
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
                                tabPanel(
                                    "Plot donnees Terrain",          
                                    plotlyOutput("plotyplot", width = "1000px", height = "500px")      
                                    ),          
                                tabPanel("Donnees Terrain",          
                                         sidebarLayout(
                                             sidebarPanel(
                                                 checkboxGroupInput(
                                                     inputId =  "show_vars",
                                                     label =  "Selectioner:",
                                                     choices =  names(Data_terrain),
                                                     selected = names(Data_terrain),
                                                     inline = TRUE     
                                                     )    
                                                 ),
                                             mainPanel(div(style = 'overflow-x: scroll',DT::dataTableOutput('Dataterrain'))),
                                             )
                                         )
                                )
                            )
                        ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data 2005
                tabItem(tabName ="Data2005",
                        fluidRow(
                            #tabBox(
                                #width = NULL,
                                #height = NULL,
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput(
                                            inputId = "data_labo2005" ,
                                            label = "Donnees Laboratoire 2005",
                                            choices = c(
                                                "semaine 1A","semaine 1B","semaine 2.3",
                                                "semaine 2.4","semaine 3.1","semaine 3.2",
                                                "semaine 3.3","semaine 4.1","semaine 4.2A",
                                                "semaine 4.2B","semaine 4.3A","semaine 5.1",
                                                "semaine 5.2","semaine 5.3","semaine 5.4",
                                                "semaine 6.1","semaine 6.2","semaine 6.3",
                                                "semaine 7.1","semaine 7.2A","semaine 7.2B",
                                                "semaine 8.1","semaine 8.2","semaine 9.1",
                                                "semaine 9.2","semaine 10.1","semaine 10.2A",
                                                "semaine 10.2B","semaine 11.2","semaine 12.1",
                                                "semaine 12.2","semaine 13.1"
                                                )
                            )
                        ),
                        mainPanel(DT::dataTableOutput('DataLabo2005'))
                    #)
                ))
                ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MAP
                tabItem(tabName = "Map_bassins",
                        sidebarLayout(
                            sidebarPanel(includeHTML("Bassins.Rhtml")),
                            mainPanel(leafletOutput(
                                "map", width = "1000", height = "1000"
                            ))
                        )
                ),
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Meteo
                tabItem(tabName = "DataMeteoMontpellier",
                        fluidRow(
                            tabBox(
                                width = NULL,
                                height = NULL,
                                tabPanel(
                                    "Plot Meteo Montpellier (temperature)",          
                                    plotlyOutput("plotyplotMeteoTemp", width = "1000px", height = "500px")
                                    ),
                                tabPanel(
                                    "Plot Meteo Montpellier (humidity)",          
                                    plotlyOutput("plotyplotMeteoHumy", width = "1000px", height = "500px")
                                ),
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
    colors_plotly <- c(
        "red",
        #1
        "firebrick",
        #2
        "firebrick3",
        #3
        "tomato",
        #4
        "tomato4",
        #5
        "green",
        #6
        "green4",
        #7
        "greenyellow",
        #8
        "mediumseagreen",
        #9
        "olivedrab1",
        #10
        "blue",
        #11
        "royalblue",
        #12
        "skyblue",
        #13
        "darkturquoise",
        #14
        "mediumblue",
        #15
        "deeppink",
        #16
        "darkorchid",
        #17
        "darkmagenta",
        #18
        "blueviolet",
        #19
        "black",
        #20
        "grey",
        #21
        "red",
        #1
        "firebrick",
        #2
        "firebrick3",
        #3
        "tomato",
        #4
        "tomato4",
        #5
        "green",
        #6
        "green4",
        #7
        "greenyellow",
        #8
        "mediumseagreen",
        #9
        "olivedrab1",
        #10
        "blue",
        #11
        "royalblue",
        #12
        "skyblue",
        #13
        "darkturquoise",
        #14
        "mediumblue",
        #15
        "deeppink",
        #16
        "darkorchid",
        #17
        "darkmagenta",
        #18
        "blueviolet",
        #19
        "black" #20
    )
    colcol <- rainbow(21)
    colors_plotly <- c(colcol[1:21], colcol[1:20])
    
    output$plotyplot <- renderPlotly({
        fig <- plot_ly(
            #filteredData(),
            Data_terrain_toplotly,
            x = ~ jours,
            y = ~ numPsy,
            #split = ~id,
            mode = "lines+markers",
            color = ~ factor(id),
            colors = colors_plotly,
            hoverinfo = "text",
            text =  ~ paste(
                '</br>',
                id,
                '</br> Number of Psyllids: ',
                numPsy,
                '</br> Day: ',
                jours
            )
        )
        fig <-
            fig %>% layout(
                title = 'Psyllids observed from 2000 to 2021 (Montpellier)',
                xaxis = list(title = 'Day',
                             zeroline = TRUE),
                yaxis = list(title = 'Number of Psyllids',
                             zeroline = TRUE)
            )
        fig
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map
    output$map <- renderLeaflet({
        leaflet() %>%
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
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data field
output$Dataterrain <- DT::renderDataTable({
    DT::datatable(Data_terrain[, input$show_vars, drop = FALSE])
})
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