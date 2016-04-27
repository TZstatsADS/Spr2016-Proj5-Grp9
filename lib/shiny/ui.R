library(shiny)
library(shinydashboard)
library(plotly)
library(RColorBrewer)

library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(dygraphs)
library(xts)
library(leaflet)

################
load("year.RData")
################
shinyUI( fluidPage(
  navbarPage(
    "*************",
    tabPanel("Analysis",icon=icon("line-chart"),
             sidebarLayout(position="left",
                           sidebarPanel(width = 2,
                                        conditionalPanel(condition = "input.conditionedPanels == 1",
                                                         selectInput("inputPack", "Input Package:", levels(packs$pack),multiple=T,selected = "Amelia"),
                                                         selectInput("inputPack2", "Input Another Package:", levels(packs$pack),multiple=T,selected = "Amelia")
                                                         ),
                                        conditionalPanel(condition = "input.conditionedPanels == 2",
                                                         selectInput("inputYear", "Year:",
                                                                     choices = list(2012,2013,2014,2015))
                                                         )
                                        
                           ),
                           
                           mainPanel(width = 10,tabsetPanel(id = "conditionedPanels", type="pill",
                                                            tabPanel("Overview",br(),
                                                                     fluidRow(
                                                                       column(6,plotlyOutput("plotrank30", width="500px",height="500px")),
                                                                       column(6,plotlyOutput("plotcomb", width="500px",height="500px"))
                                                                     ),value = 0),
                                                            
                                                            tabPanel("Package",br(),
                                                                     fluidRow(
                                                                       column(1,valueBoxOutput("ranking")),
                                                                       column(9,plotlyOutput("barpack"))),
                                                                     fluidRow(
                                                                       column(1,valueBoxOutput("ranking2")),
                                                                       column(9,plotlyOutput("barpack2"))
                                                                     ),value = 1
                                                            ),
                                                            
                                                            tabPanel("Country",br(),
                                                                     fluidRow(
                                                                       column(6, plotlyOutput("scattercoun",width="500px",height="500px")),
                                                                       column(6, plotlyOutput("piecoun",width="500px",height="500px"))
                                                                     ),value = 2),
                                                            tabPanel("OS",br(),
                                                                       plotlyOutput("baros", width="900px",height="500px"))
                                     
                                                            
                           ))
             ))
  )))