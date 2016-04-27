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
load("1.RData")
load("2.RData")
################
shinyUI( fluidPage(
  navbarPage(
    "R Package Mining",
    tabPanel("Analysis",icon=icon("line-chart"),
             sidebarLayout(position="left",
                           sidebarPanel(width = 2,
                                        conditionalPanel(condition = "input.conditionedPanels == 0",
                                                         p(strong("*"),"The first three popular packages from 2012 to 2015 were:",br(),code("Rcpp"), code("ggplot2"),code("Plyr")),
                                                         p(strong("*"),"The first three popular combination of packages from 2012 to 2015 were:",br(),code("digest & stringr"), code("colorspace & ggplot"),code("reshape2 & ggplot2"))
                                                         ),
                                        conditionalPanel(condition = "input.conditionedPanels == 1",
                                                         selectInput("inputPack", "Input Package:", levels(packs$pack),multiple=T,selected = "Amelia"),
                                                         selectInput("inputPack2", "Input Another Package:", levels(packs$pack),multiple=T,selected = "Amelia")
                                                         ),
                                        conditionalPanel(condition = "input.conditionedPanels == 2",
                                                         selectInput("inputYear", "Select Year:",
                                                                     choices = list(2012,2013,2014,2015)),
                                                         p(strong("*"),code("U.S."),"had the most download times from 2012 to 2015."),
                                                         p(strong("*"),code("China"),"had signficant increased download times from 2012 to 2015.")
                                                         ),
                                        conditionalPanel(condition = "input.conditionedPanels == 4",
                                                         p(strong("*"),code("mingw32"),code("linux-gnu"),"were the most popular OS for users to download packages from 2012 to 2015.")
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
                                                                       plotlyOutput("baros", width="900px",height="500px"
                                                                                    ),value = 4)
                                     
                                                            
                           ))
             )),
    tabPanel("Similarity",icon=icon("spinner"),
             sidebarLayout(position="left",
             sidebarPanel(width = 2,
                          conditionalPanel(condition = "input.conditionedPanels == 0",
                                           p(strong("Cluster Methods:")),
                                           p(strong("*"),"Calculate the word frequency of each packages' description."),
                                           p(strong("*"),"Create the", code("latent semantic space")),
                                           p(strong("*"),"Remove stop words and select 1000 most popular packages"),
                                           p(strong("*"),"Compute the distance between every two words using",code("TFIDF")),
                                           p(strong("*"),"Reduce Dimension using",code("CMD")),
                                           p(strong("*"),"Cluster the packages using",code("K-means"))
                                            ),
                          conditionalPanel(condition = "input.conditionedPanels == 1"
                          )
                          ),
             mainPanel(width = 10,tabsetPanel(id = "conditionedPanels", type="pill",
                                              tabPanel("Scatterplot",br(),
                                                       plotlyOutput("cluster", width="900px",height="500px"
                                                       ),value = 0),
                                              tabPanel("Heatmap",br(),
                                                       plotlyOutput("heatmap", width="900px",height="500px"
                                                       ),value = 1)
             )
             )))
             
  )))