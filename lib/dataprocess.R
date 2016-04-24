#this is an empty file

library(dplyr)

filelist=dir("../data/",pattern=".csv")
for(i in 1:length(filelist))
{
 tmp=read.csv(file=paste("../data/",filelist[i],sep=""),header = TRUE ,sep = ",")
 tmp$date=as.character(tmp$date)
 tmp$ip_id=as.character(tmp$ip_id)
 tmp$package=as.character(tmp$package)
 
 DateAndID=paste(tmp$date,tmp$ip_id)
 
 newTable=cbind(DateAndID,tmp)
 
 GroupedTable=newTable%>%
   group_by(DateAndID)%>%
   summarise(pack=paste(package, collapse = ' '))
 
 write.table(GroupedTable,file="../data/dataoutput/DateUserPack.csv",sep=",",col.names = FALSE,row.names = FALSE,append = TRUE)
}

#change data type
#tmp1=tmp%>%
#  mutate_each_(funs(as.character),date)


#DateAndID=paste(tmp$date,tmp$ip_id)

#newTable=cbind(DateAndID,tmp)

#head(newTable,n=3)



#write.table(GroupedTable,file="../data/dataoutput/DateUserPack.csv",sep=",",col.names = FALSE,append = TRUE)
