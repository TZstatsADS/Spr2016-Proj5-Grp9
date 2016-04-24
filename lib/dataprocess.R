#this is an empty file

library(dplyr)
setwd("C:/ADS/project5/project5/files")
tmp=read.csv(file="1.csv",head=TRUE,sep=",")

#change data type

#tmp1=tmp%>%
#  mutate_each_(funs(as.character),date)


tmp$date=as.character(tmp$date)
tmp$ip_id=as.character(tmp$ip_id)
tmp$package=as.character(tmp$package)

DateAndID=paste(tmp$date,tmp$ip_id)

newTable=cbind(DateAndID,tmp)

head(newTable,n=3)

GroupedTable=newTable%>%
  group_by(DateAndID)%>%
  summarise(pack=paste(package, collapse = ' '))