require(ffbase)
options(java.parameters = "-Xmx4000m")

#genus_gbif <- read.csv2.ffdf(x=NULL,file="D:/Colombia.csv",sep = "|",VERBOSE = TRUE,na.strings="\\N",first.rows = 50000,next.rows = 50000,quote="",header=T,colClasses=rep("factor",180))        
genus_gbif <- read.csv2.ffdf(x=NULL,file="D:/Colombia2.csv",sep = "|",VERBOSE = TRUE,na.strings="",first.rows = 50000,next.rows = 50000,quote="",header=T,colClasses=rep("factor",180))        

#write.xlsx (genus_gbif,"D:/Colombia.xls",row.names=F,showNA = F)

#s<-as.data.frame(genus_gbif[1:100,])
write.table(genus_gbif,"D:/Colombia2.csv",row.names=FALSE,sep="|",dec=".",na="", quote = F)

# write.xlsx2 (genus_gbif[1:5000,],"D:/Colombia_1.xls",row.names=F,showNA = F)
# write.xlsx2 (genus_gbif[50001:100000,],"D:/Colombia_2.xls",row.names=F,showNA = F)
# write.xlsx2 (genus_gbif[100001:129395,],"D:/Colombia_3.xls",row.names=F,showNA = F)