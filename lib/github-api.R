library('jsonlite')
library('httpuv')
library('httr')
library('RCurl')
library('psych')
library('stringr')
library('fmsb')

####################
# R-PACKAGE CLASSIFICATION
####################
setwd("C:/Users/lwh/Desktop/finalproject-group-9/data")
pack_class<-read.csv('rclass.csv')
package_name<-as.character(pack_class[,1])
class<-table(pack_class[,2])           #33
####################
# GET TOKEN
####################
oauth_endpoints("github")
myapp <- oauth_app("github",
                   key = "19c8034e916ab8dd7f3c",
                   secret = "a54ce3798c0ed7c188d87ddb8a2b61d1951b7935")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)
####################
# API FORMAT
####################
req <- GET("https://api.github.com/search/code?q=library(+in:file+user:tz33cu+language:R",gtoken) #code search
stop_for_status(req)
content(req)$total_count
content(req)$item[[1]]$html_url



request_code=GET("https://api.github.com/search/code?q=library(+in:file+user:tz33cu+language:R") #code search
request_user=GET('https://api.github.com/users/tz33cu')                      #user search
# https://api.github.com/user/18000000  user for id number && users for name

code<-content(request_code)
code_<-jsonlite::fromJSON(toJSON(code))

user<-content(request_user)
user_<-jsonlite::fromJSON(toJSON(user))

code_$total_count    # packaged used times

user_$followers
user_$following



####################
# GET USER INFO
####################
sample_userid=sample(1:18000000,10000)

num_R=c()

for(i in 1:length(sample_userid)){
  url=paste0('https://api.github.com/user/',sample_userid[1])
  request_user=GET(url)                                                      #user search
  user_name<-content(request_user)$login
  code_url=paste0("https://api.github.com/search/code?q=in:file+language:R+user:",user_name)
  info=content(GET(code_url))
  num_R[i]<-content(GET(code_url))$total_count                               # number of R scripts
}

# rcode_1<-gsub('\\(',"lllll",rcode)
# rcode_1<-gsub("\\'","ppppp",rcode_1)
# wcount_1 <- str_count(rcode_1, paste0("librarylllllpppp" ,"jsonlite"))   #library('jsonlite')
# wcount_2 <- str_count(rcode_1, paste0("librarylllll" ,"jsonlite"))   #library(jsonlite)
# wcount_3 <- str_count(rcode_1, paste0("requirelllllpppp" ,"jsonlite"))   #require('jsonlite')
# wcount_4 <- str_count(rcode_1, paste0("requirelllll" ,"jsonlite"))   #require(jsonlite)
r_url=info$items[2][[1]]$html_url
GET(r_url)

####################
# CODE, ORGANIZATION FORMAT
####################
# https://raw.githubusercontent.com/tz33cu/PartitionRetention/3adfbf64fd0bebd3ee9206a55be97beeec16a2da/PNAS2015/lib/Iscore.R
# 
# https://api.github.com/orgs/rOpenSci/public_members   #rOpenSci(R user)
rOpenSci_member<-GET('https://api.github.com/orgs/rOpenSci/public_members',gtoken)
n=length(content(rOpenSci_member))        # some are unavailable


user_table=matrix(0,ncol=length(package_name),nrow=1)
b<-rep(0,length(package_name))
for(i in 1:n){
  user_name=content(rOpenSci_member)[i][[1]]$login
  code_url=paste0("https://api.github.com/search/code?page=1&per_page=1000&q=in:file+language:R+user:",user_name)
  info=content(GET(code_url,gtoken))
  num_Rscripts=info[[1]]            #includes .rd file
  b<-rep(0,length(package_name))
  if (num_Rscripts==0){
    user_table=rbind(user_table,t(as.matrix(b)))
  }
  else{
    for (p in 1:min(100,num_Rscripts)){
    r_url=info$items[[p]]$html_url
    r_url<-gsub('https://github.com/','https://raw.githubusercontent.com/',r_url)
    r_url<-gsub('/blob','',r_url)
    rcode=content(GET(r_url))
    a<-str_count(rcode,package_name)
    b=b+a
    }
    user_table=rbind(user_table,t(as.matrix(b)))
    print(i)
    Sys.sleep(1)
  }
}

user_classuse<-matrix(0,ncol=1+n,nrow=length(class))
# class(class[1])
# #1:110 bayessian
# package_name[1:110]

colnames(user_table)<-package_name

aa=user_table[2,]                                    #33-class result
c<-rep(0,length(class))
for(i in 1:length(package_name)){
  for (j in 1:length(class)){
    if(pack_class[i,2]==names(class)[j]){
      c[j]=c[j]+aa[i]
    }
  }
}


# test<-user_table[2,]
# sum(test[1:2774])
# sum(test)

# for(i in 1:(n+1)){
#   for( j in 1:length(package_name)){
#     user_table[i,j]
#     for( p in 1:length(class)){
#      if([i,j]=)
#     }
#     
#     }
# }

# usage of every user

# user_name=content(rOpenSci_member)[1][[1]]$login
# code_url=paste0("https://api.github.com/search/code?page=1&per_page=1000&q=in:file+language:R+user:",user_name)
# info=content(GET(code_url))
# info$total_count
# r_url=info$items[[1]]$html_url
# rcode=content(GET(r_url))
# length(info$items)
# 
# https://raw.githubusercontent.com/DASpringate/Configuration-files/290e8a8d79a41bcd2171b2861a58189c265df4da/.vim/r-plugin/build_omniList.R
# url='https://github.com/Saintat1/R-package-usage/blob/d3e395f1a9f7e424d75ea047652321e084b08852//github-api.R'
# url<-gsub('https://github.com/','https://raw.githubusercontent.com/',url)
# url<-gsub('/blob','',url)
# rcode=content(GET(url))

####################
# SINGLE PERSON SEARCH
####################

#input: user_ID
#output: num_following,num_follower,num_repo,join_date,lastuse_date,num_Rscripts,radar chart

user_ID<-'tz33cu'
user_url=paste0("https://api.github.com/users/",user_ID)
info=content(GET(user_url,gtoken))
num_following<-info$following
num_follower<-info$followers
num_repo<-info$public_repos
join_date<-info$created_at
lastuse_date<-info$updated_at

code_url=paste0("https://api.github.com/search/code?page=1&per_page=1000&q=in:file+language:R+user:",user_ID)
info=content(GET(code_url,gtoken))
num_Rscripts=info[[1]]            #includes .rd file
r_packageuse<-rep(0,length(package_name))

if (num_Rscripts==0){
  print('No R Scripts Found')
}else{
  for (p in 1:min(100,num_Rscripts)){
    r_url=info$items[[p]]$html_url
    r_url<-gsub('https://github.com/','https://raw.githubusercontent.com/',r_url)
    r_url<-gsub('/blob','',r_url)
    rcode=content(GET(r_url))
    a<-str_count(rcode,package_name)
    r_packageuse=r_packageuse+a
  }
}

                                    #33-class result
class_use<-rep(0,length(class))
for(i in 1:length(package_name)){
  for (j in 1:length(class)){
    if(pack_class[i,2]==names(class)[j]){
      class_use[j]=class_use[j]+r_packageuse[i]
    }
  }
}
radar_date<-data.frame(rbind(rep(max(class_use),33), rep(0,33), class_use))
colnames(radar_date)<-names(class)
radarchart(radar_date,cglcol='grey',pfcol='navy',pcol='blue',seg=3,cglty=1)



####################
# RADAR CHART
####################

#visualization
#machine learning
#
#
#
# op <- par(mfrow=c(1,1))
# spider(y=1,x=2:9,data=Thurstone,connect=FALSE) #a radar plot
# spider(y=1,x=2:9,data=Thurstone) #same plot as a spider plot
# radar(y=1:3,x=1:3,data=Thurstone,overlay=TRUE)
# #make a somewhat oversized plot
# radar(y=26:28,x=1:25,data=cor(bfi,use="pairwise"),fill=TRUE,scale=2) 
# par(op)
radar_date<-data.frame(rbind(rep(max(c),33), rep(0,33), c))
colnames(radar_date)<-names(class)
radarchart(radar_date,cglcol='grey',pfcol='navy',pcol='blue',seg=3,cglty=1)
