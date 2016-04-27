
library(shiny)
library(shinydashboard)
library(plotly)
library(RColorBrewer)
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
gtoken <- config(token = github_token)
pack_class<-read.csv('rclass.csv')
package_name<-as.character(pack_class[,1])
class<-table(pack_class[,2])  
cocur_matrix=read.table('githubco.txt')
package_name_for_recom<-unique(as.character(pack_class[,1]))

shinyServer(function(input, output,session) {
  
  package<-reactive({input$inputPack})
  years<-reactive({input$inputYear})
  per<-reactive(percent[which(percent$year == input$inputYear),])
  package2<-reactive({input$inputPack2})
  
  # pack rank
  output$ranking <- renderText({
    paste(as.character(packs$rank[which(pack == package())]), "Rank")
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
  output$ranking2 <- renderText({
    paste(as.character(packs$rank[which(pack == package2())]), "Rank")
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
  
  #image1
  output$image1 <- renderImage({
    list(src = "a.jpg",alt = "Image failed to render",width=650,height=550)
  }, deleteFile = FALSE)
  
  
  #image2
  output$image2 <- renderImage({ 
    list(src = "b.jpg", alt = "Image failed to render",width=650,height=550)
  }, deleteFile = FALSE)
  
  
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
  
  #############################Github###############################################    
  info<-reactive({
    content(GET(paste0("https://api.github.com/users/",input$githubid),gtoken))})
  info_c<-reactive({
    content(GET( paste0("https://api.github.com/search/code?page=1&per_page=1000&q=in:file+language:R+user:",input$githubid),gtoken))
  })
  
  
  r_packageuse<-reactive({
    r_packageuse<-rep(0,length(package_name))
    if (info_c()[[1]]==0){
      
    }else{
      for (p in 1:min(100,info_c()[[1]])){
        r_url=info_c()$items[[p]]$html_url
        r_url<-gsub('https://github.com/','https://raw.githubusercontent.com/',r_url)
        r_url<-gsub('/blob','',r_url)
        rcode=content(GET(r_url))
        a<-str_count(rcode,package_name)
        r_packageuse=r_packageuse+a
      }
    }
    r_packageuse
  })
  
  
  r_packageuse_for_recom<-reactive({
    r_packageuse_for_recom<-rep(0,length(package_name_for_recom))
    if (info_c()[[1]]==0){
      
    }else{
      for (p in 1:min(100,info_c()[[1]])){
        r_url=info_c()$items[[p]]$html_url
        r_url<-gsub('https://github.com/','https://raw.githubusercontent.com/',r_url)
        r_url<-gsub('/blob','',r_url)
        rcode=content(GET(r_url))
        b<-str_count(rcode,package_name_for_recom)
        r_packageuse_for_recom = r_packageuse_for_recom+b
      }
      
    }
    r_packageuse_for_recom
  })
  
  resultlib<-reactive({
    result<-as.matrix(cocur_matrix)%*%as.matrix(r_packageuse_for_recom())
    rownames(result)<-package_name_for_recom
    result1<-result[order(-result[,1]),]
    
    
    index<-which(r_packageuse_for_recom()!=0)
    libname<-package_name[index]
    
    index<-which(names(result1) %in% libname)
    result1<-result1[-index]
    names(result1[1:5])  
  })
  
  
  
  
  
  
  
  radar_date<-reactive({
    class_use<-rep(0,length(class))
    for(i in 1:length(package_name)){
      for (j in 1:length(class)){
        if(pack_class[i,2]==names(class)[j]){
          class_use[j]=class_use[j]+r_packageuse()[i]
        }
      }
    }
    
    radar_date<-data.frame(rbind(rep(max(class_use),33), rep(0,33), class_use))
    colnames(radar_date)<-names(class)
    radar_date
  })
  
  
  output$repnumBox <- renderValueBox({
    valueBox(
      info()$public_repos, "Repositories", icon =icon("code-fork"),
      color = "orange")
  })
  
  output$rscriptBox <- renderValueBox({
    valueBox(
      info_c()[[1]], "R Scripts", icon = icon("code"),
      color = "blue")
  })
  
  output$rlibraryBox <- renderValueBox({
    valueBox(
      sum(r_packageuse()!=0), "R libraries", icon = icon("hand-peace-o"),
      color = "green")
  })
  
  output$githubinfo<-renderUI({
    str0<-"User Infomation:"
    str1<-paste("User ID: ", input$githubid)
    str2<-paste("Following: ",info()$following)
    str3<-paste("Follower: ",info()$followers)
    str4<-paste("Join Date: ",info()$created_at)
    str5<-paste("Last time update: ",info()$updated_at)
    HTML(paste(h4(str0),h5(str1),h5(str2),
               h5(str3),h5(str4),h5(str5), 
               sep = '<br>'))
  })
  
  output$librecom<-renderUI({
    str0<-"Our Recommendations for you:"
    str1<-resultlib()[1]
    str2<-resultlib()[2]
    str3<-resultlib()[3]
    str4<-resultlib()[4]
    str5<-resultlib()[5]
    HTML(paste(h5(str0),h5(str1),h5(str2),
               h5(str3),h5(str4),h5(str5), 
               sep = '<br>'))})
  output$radar<-renderPlot({
    radarchart(radar_date(),cglcol='cyan',cglwd=1,pfcol='royalblue3',seg=3,cglty=1,pty=32,plwd=5,pdensity=100,vlcex=0.8,calcex=100)
  })
  
}

)