
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
library('jsonlite')
library('httpuv')
library('httr')
library('RCurl')
library('psych')
library('stringr')
library('fmsb')
library('recommenderlab')

################
load("year.RData")
load("1.RData")
load("2.RData")
################
shinyUI( fluidPage(
  includeCSS(path = "AdminLTE.css"),
  includeCSS("shinydashboard.css"),
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
                                                         p(strong("*"),"If we use install.packages",code("ggplot2"),"to download package, it automatically install its dependencies. Also, we need to install its import packages when we library",code("ggplot2"),"."),
                                                         p(strong("*"),"In the package we find three more packages of the Rcpp family:",br(),code("Rcpp"), br(),code("RcppArmadillo"), br(),code("RcppEigen"),br(), "but only a limited number of other packages as C/C++ level dependencies are still somewhat rare in the R universe.")
                                        ),
                                        conditionalPanel(condition = "input.conditionedPanels == 2",
                                                         selectInput("inputPack", "Input Package:", levels(packs$pack),multiple=T,selected = "Amelia"),
                                                         selectInput("inputPack2", "Input Another Package:", levels(packs$pack),multiple=T,selected = "Amelia")
                                        ),
                                        conditionalPanel(condition = "input.conditionedPanels == 3",
                                                         selectInput("inputYear", "Select Year:",
                                                                     choices = list(2012,2013,2014,2015)),
                                                         p(strong("*"),code("U.S."),"had the most download times from 2012 to 2015."),
                                                         p(strong("*"),code("China"),"had a strong grwoth of download times from 2012 to 2015.")
                                        ),
                                        conditionalPanel(condition = "input.conditionedPanels == 4",
                                                         p(strong("*"),code("mingw32"),code("linux-gnu"),"were the most popular OS for users to download packages from 2012 to 2015.")
                                        )
                                        
                           ),
                           
                           mainPanel(width = 10,tabsetPanel(id = "conditionedPanels", type="pill",
                                                            tabPanel("Overview",br(),
                                                                     fluidRow(
                                                                       column(6,plotlyOutput("plotrank30")),
                                                                       column(6,plotlyOutput("plotcomb"))
                                                                     ),value = 0),
                                                            tabPanel("Combination",br(),
                                                                     fluidRow(
                                                                       h5(strong("Relations for Top 20 Packages")),
                                                                       column(6,imageOutput("image1", width = "100%", height = 500)),
                                                                       column(6,imageOutput("image2", width = "100%", height = 500))
                                                                     ),value = 1
                                                            ),
                                                            tabPanel("Package",br(),
                                                                     fluidRow(
                                                                       column(1,valueBoxOutput("ranking")),
                                                                       column(5,plotlyOutput("barpack")),
                                                                       column(1,valueBoxOutput("ranking2")),
                                                                       column(5,plotlyOutput("barpack2")))
                                                                     ,value = 2
                                                            ),
                                                            
                                                            tabPanel("Country",br(),
                                                                     fluidRow(
                                                                       column(6, plotlyOutput("scattercoun",width="500px",height="500px")),
                                                                       column(6, plotlyOutput("piecoun",width="500px",height="500px"))
                                                                     ),value = 3),
                                                            tabPanel("OS",br(),
                                                                     plotlyOutput("baros", width="900px",height="500px"
                                                                     ),value = 4)
                                                            
                                                            
                                                            
                           ))
             )),
    tabPanel("Cluster",icon=icon("spinner"),
             sidebarLayout(position="left",
                           sidebarPanel(width = 2,
                                        p(strong("Cluster Methods:")),
                                        p(strong("*"),"Calculate the word frequency of each packages' description."),
                                        p(strong("*"),"Create the", code("latent Semantic Space")),
                                        p(strong("*"),"Remove stop words and select 1000 most popular packages"),
                                        p(strong("*"),"Compute the distance between every two words using",code("TFIDF")),
                                        p(strong("*"),"Reduce Dimension using",code("CMD")),
                                        p(strong("*"),"Cluster the packages using",code("K-Means"))
                                        
                           ),
                           mainPanel(width = 10,tabsetPanel(type="pill",
                                                            tabPanel("Scatterplot",br(),
                                                                     plotlyOutput("cluster", width="900px",height="500px"
                                                                     )),
                                                            tabPanel("Heatmap",br(),
                                                                     plotlyOutput("heatmap", width="900px",height="500px"
                                                                     ))
                           )
                           ))),
    #########################Github#############################
    tabPanel("Conplementary libraries",icon=icon("space-shuttle"),
             fluidRow(
               
               
               
               h5("Explore more:",
                  a(href = "http://weihan.net16.net/", icon("hand-scissors-o"))),
               img(src="dep.jpg",width=1200),
               h5("We scraped the data from the log info from Rstudio webstie which ranges from 2012 to 2016. To explore more about the dependencies 
                  between the libraries,please click top left hand")
               
               
               )),
    
    
    
    #################### end of github####################
    
    #########################Github#############################
    tabPanel("Github Analysis",icon=icon("github-alt"),
             fluidRow(
               column(width = 4,
                      box(width = NULL, 
                          textInput("githubid", label = h3("Plase Enter your Github id"), 
                                    value = "tz33cu")),
                      box(width=NULL,
                          uiOutput("githubinfo")),
                      
                      box(width=NULL,
                          uiOutput("librecom"))
               ),
               column(width = 8,
                      box(width = NULL,
                          valueBoxOutput("repnumBox"),
                          valueBoxOutput("rscriptBox"),
                          valueBoxOutput("rlibraryBox"),
                          plotOutput("radar", height = 800))
               )
             ))
    
    

  )))