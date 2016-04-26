#Rcpp_linkingto <- read.csv("~/Desktop/2016spring 4249/project5/Rcpp_linkingto.csv")
library(plotly)
library(dplyr)
plot_ly(Rcpp_linkingto, x = package, y = count,color=factor(year),type = "bar",text=paste(Rcpp_linkingto$package),colors =c("#0072B2","#E69F00")) %>% 
  layout(title = "CRAN Packages by Reserved Linking To",barmode="dodge",xaxis = list(title = "",tickangle = -15,tickfont = list(size=10)))
