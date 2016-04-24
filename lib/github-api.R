library('jsonlite')
library('httpuv')
library('httr')
library('RCurl')
library('psych')

oauth_endpoints("github")

####################
# GET TOKEN
####################

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
content(req)
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

for(i in 1:30){
  url=paste0('https://api.github.com/user/',sample_userid[1])
  request_user=GET(url)                                                      #user search
  user_name<-content(request_user)$login
  code_url=paste0("https://api.github.com/search/code?q=in:file+language:R+user:",user_name)
  info=content(GET(code_url))
  num_R[i]<-content(GET(code_url))$total_count                               # number of R scripts
}

r_url=info$items[2][[1]]$html_url
GET(r_url)

####################
# GET code
####################
https://raw.githubusercontent.com/tz33cu/PartitionRetention/3adfbf64fd0bebd3ee9206a55be97beeec16a2da/PNAS2015/lib/Iscore.R



####################
# RADAR CHART
####################
op <- par(mfrow=c(1,1))
spider(y=1,x=2:9,data=Thurstone,connect=FALSE) #a radar plot
spider(y=1,x=2:9,data=Thurstone) #same plot as a spider plot
spider(y=1:3,x=4:9,data=Thurstone,overlay=TRUE)
#make a somewhat oversized plot
spider(y=26:28,x=1:25,data=cor(bfi,use="pairwise"),fill=TRUE,scale=2) 
par(op)
