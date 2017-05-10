#\data
#
right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}
#
left = function (string,char){
  substr(string,1,char)
}
#
input_dir<-"C:/Users/csosa/SharePoint/CWR of North America (Springe - Boo/"
crop<-"Industrial_indust_Dierig"


##QUERY##
idir_1<-paste0(input_dir,crop,"/","data")

##GBIF##
idir_2<-paste0(input_dir,crop,"/","data/GBIF")
##OU##
out_path<-paste0(input_dir,crop,"/","data/FINAL")


##QUERY##
rr<-list.files(idir_1,pattern=".csv",full.names = T)
##GBIF##
rrr<-list.files(idir_2,pattern=".csv",full.names = T)
rrr_names<-list.files(idir_2,pattern=".csv",full.names = F)
rrr_names<-gsub('[[:digit:]]+', '', rrr_names);rrr_names<-gsub('--', '', rrr_names);rrr_names<-gsub('.csv', '', rrr_names)

for(i in 1:length(rrr_names)){
  
  rrr_names[[i]]<-left(as.character(rrr_names[[i]]),(nchar(as.character(rrr_names[[i]]))-1))
};rm(i)


##QUERY##
rr2<-lapply(1:length(rr),function(i){
  x<-read.csv(rr[[i]],header = T,sep="|")
  return(x)
})
  
##GBIF##
rrr2<-lapply(1:length(rrr),function(i){
  x<-read.csv(rrr[[i]],header = T,sep="|")
  return(x)
})

for(i in 1:length(rrr2)){
  rrr2[[i]]$taxon_final<-rep(rrr_names[[i]],nrow(rrr2[[i]]))
};rm(i)

##DIFERENT COLUMN NUMBERS QUERY##

rr4<-matrix(ncol=2,nrow=length(rr2))
rr4[,1]<-1:nrow(rr4);rr4[,2]<-unlist(lapply(1:length(rr2),function(j){
  c<-ncol(rr2[[j]])
  return(c)
}))

rr4_cat<-unlist(unique(rr4[,2]))


for(i in 1:length(rr4_cat)){
vals_rr2<-rr4[,1][which(rr4[,2]==rr4_cat[[i]])]
list<-rr2[vals_rr2]
rr3<-do.call(rbind,list)

write.table(rr3,paste0(out_path,"/",crop,"_",i,".csv"),row.names=F,quote=F,sep="|",na = "")

};rm(i)

rm(rr4_cat,list,rr3,vals_rr2,rr4)

##DIFERENT COLUMN NUMBERS GBIF##
rr4<-matrix(ncol=2,nrow=length(rrr2))
rr4[,1]<-1:nrow(rr4);rr4[,2]<-unlist(lapply(1:length(rrr2),function(j){
  c<-ncol(rrr2[[j]])
  return(c)
}))

rr4_cat<-unlist(unique(rr4[,2]))


for(i in 1:length(rr4_cat)){
  vals_rrr2<-rr4[,1][which(rr4[,2]==rr4_cat[[i]])]
  list<-rrr2[vals_rrr2]
  rrr3<-do.call(rbind,list)  
  write.table(rrr3,paste0(out_path,"/",crop,"_GBIF_",i,".csv"),row.names=F,quote=F,sep="|",na = "")
  
};rm(i)





  # rr3<-do.call(rbind,rr2)
  # rrr3<-do.call(rbind,rrr2)
  

  
 
  

  
  
  
#write.table(rrr3,"C:/Users/csosa/SharePoint/CWR of North America (Springe - Boo/Industrial_indust_Dierig/data/final/Industrial_indust_Dierig_GBIF.csv",row.names=F,quote=F,sep="|",na = "")

# write.csv(rr3,"C:/Users/csosa/SharePoint/CWR of North America (Springe - Boo/Fruits_tempsmall_Hummer/data/final/Fruits_tempsmall_Hummer_GBIF.csv",row.names=F,quote=F)
  