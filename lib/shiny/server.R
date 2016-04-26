library(shiny)
library(shinydashboard)
library(plotly)
library(RColorBrewer)

################
load("year.RData")
################

shinyServer(function(input, output,session) {
  
  package<-reactive({input$inputPack})
  year<-reactive({input$inputYear})
  
# pack rank
  output$rank <- renderValueBox({
    valueBox(packs$rank[which(pack == package())], "Rank of Download Times")
    }) 
  
  
# Bar Plot of Download Times Pack
  output$barpack <- renderPlotly({
  times = c(pack[which(pack == package()),][1,2],pack[which(pack == package()),][1,3],pack[which(pack == package()),][1,4],pack[which(pack == package()),][1,5])
  f <- list(family = "Courier New, monospace", size = 18, color = "#7f7f7f")
  xlab <- list(title = "year", titlefont = f)
  ylab <- list(title = "times", titlefont = f)
  plot_ly(
    x = c("2012","2013","2014","2015"),
    y = times,
    type = "bar")%>%
    layout(xaxis = xlab, yaxis = ylab,title = paste0("Bar Plot of Download Times for ",package()))
  })

#country pieplot
  output$barpack <- renderPlotly({
    per = percent[which(percent$year == year()),]
    plot_ly(per, labels = country, values = percent, type = "pie",group = list(colors = brewer.pal(7, "Paired"))) %>% 
    layout(title = paste0("Pie Chart of Top 7 Countries in ",year))
  })
  
#scatterplot rank 20
  output$plotrank20 <- renderPlotly({
  plot_ly(data = packs[1:20,], x = pack, y = count,color = count,mode = "markers")
  })
  
#group bar chart for os 
  output$baros <- renderPlotly({
  plot_ly(data = os,x = year, y = value, type = "bar", color = os) %>% 
    layout(title = paste0("Bar Chart for Number of Users' OS"))
  })
  
  
  
  
  }
)