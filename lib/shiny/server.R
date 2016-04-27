library(shiny)
library(shinydashboard)
library(plotly)
library(RColorBrewer)

################
load("year.RData")
load("1.RData")
load("2.RData")
################

shinyServer(function(input, output,session) {
  
  package<-reactive({input$inputPack})
  years<-reactive({input$inputYear})
  per<-reactive(percent[which(percent$year == input$inputYear),])
  package2<-reactive({input$inputPack2})
  
# pack rank
  output$ranking <- renderValueBox({
    valueBox(packs$rank[which(pack == package())], " Rank",color = "blue")
    }) 
  
# Bar Plot of Download Times Pack
  output$barpack <- renderPlotly({
  times = c(pack[which(pack == package()),][1,2],pack[which(pack == package()),][1,3],pack[which(pack == package()),][1,4],pack[which(pack == package()),][1,5])
  f <- list(family = "Courier New, monospace", size = 18, color = "#7f7f7f")
  xlab <- list(title = "year",tick0 = 2011, dtick = 1)
  ylab <- list(title = "times")
  plot_ly(x = c("2012","2013","2014","2015"),y = times,opacity = 0.6, type = "bar")%>%
    layout(xaxis = xlab, yaxis = ylab,title = paste0("Bar Plot of Download Times for ",package()))
  })
  
# pack rank2
  output$ranking2 <- renderValueBox({
    valueBox(packs$rank[which(pack == package2())], " Rank",color = "blue")
  }) 
  
# Bar Plot of Download Times Pack2
  output$barpack2 <- renderPlotly({
    times = c(pack[which(pack == package2()),][1,2],pack[which(pack == package2()),][1,3],pack[which(pack == package2()),][1,4],pack[which(pack == package2()),][1,5])
    f <- list(family = "Courier New, monospace", size = 18, color = "#7f7f7f")
    xlab <- list(title = "year",tick0 = 2011, dtick = 1)
    ylab <- list(title = "times")
    plot_ly(x = c("2012","2013","2014","2015"),y = times,opacity = 0.6, type = "bar", color = "orange")%>%
      layout(xaxis = xlab, yaxis = ylab,title = paste0("Bar Plot of Download Times for ",package2()))
  })

#country pieplot
  output$piecoun <- renderPlotly({
    plot_ly(per(), labels = country, values = percent, type = "pie",marker = list(colors = brewer.pal(7, "Paired"))) %>% 
    layout(title = paste0("Pie Chart of Top 7 Countries in ",years()))
  })

#country scatterplot
  output$scattercoun <- renderPlotly({
    plot_ly(data = percent, x = year, y = percent, color = country, marker = list(color = brewer.pal(9, "Set1"))) %>% 
      layout(title = paste0("Scatter Plot of Download Times in Each Countries"))
  })
  
  
    
#scatterplot rank 30
  output$plotrank30 <- renderPlotly({
    f2 <- list(family = "Old Standard TT, serif", size = 9, color = "black")
    xlab2 <- list(title = "", tickfont = f2,tickangle = 45)
    plot_ly(data = packs[1:30,], x = pack, y = count,size = count,color = count,mode = "markers")%>%
      layout(xaxis = xlab2, title = paste0("Scatter Plot of First 30 Packages "))
  })
  
#scatterplot for combine packs
  output$plotcomb <- renderPlotly({
    f2 <- list(family = "Old Standard TT, serif", size = 5,color = "black")
    xlab2 <- list(title = "Pack Comb", tickfont = f2,tickangle = 45,showticklabels = FALSE)
    plot_ly(data = combine, x = comb, y = count,size = count,color = count,mode = "markers")%>%
      layout(xaxis = xlab2, title = paste0("Scatter Plot of Combination of the first 20 Packages "))
  })
    
#group bar chart for os 
  output$baros <- renderPlotly({
  plot_ly(data = os,x = year, y = value, type = "bar", color = os,marker = list(colors = brewer.pal(5, "Set1"))) %>% 
    layout(title = paste0("Bar Chart for Number of Users' OS"))
  })
  
#scatterplot of clusters
  output$cluster <- renderPlotly({
  plot_ly(data=points_p, x=x,y=y, mode = "markers",text=paste(points_p$rname),color=km$cluster) %>%
    layout(title = paste0("K-means Cluster of Packages"))
  })

#Heatmap of clusters
  output$heatmap <- renderPlotly({
    plot_ly(z = corMatrix,x=colnames(corMatrix),y=colnames(corMatrix), type = "heatmap") %>%
      layout(xaxis = list(title=""),yaxis=list(title=""),title = paste0("Heatmap of Packages' Similarity"))
  })
    
  

  }
)