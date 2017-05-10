###########################
## SAV TO CSV SAL PROJECT##
###########################
##CCSA 2015              ##
###########################

library(foreign)
path<-"//dapadfs/Workspace_cluster_9/Ecosystem_Services/SAL-project/Data/DataBase/PE/Modulos_CENAGRO"

paths_enc<-list.dirs(path,full.names = T,recursive = F)
paths_enc_name<-list.dirs(path,full.names = F,recursive = F)




files<-lapply(1:length(paths_enc),function(i){
  cat("loading files from ", paths_enc_name[[i]],"\n")
  
  a<-list.files(paths_enc[[i]],pattern = ".sav$",full.names = T)
  a_name<-list.files(paths_enc[[i]],pattern = ".sav$",full.names = F)
  cat("reading from ", a_name,"\n")
  files<-read.spss(a, to.data.frame=TRUE)
  return(files)
  cat("done!","\n")
})


lapply(1:length(paths_enc),function(i){
  cat("writing file for", paths_enc_name[[i]],"\n")
  write.table(files[[i]],paste0("Z:/",paths_enc_name[[i]],".csv"),sep="|",row.names = F,quote = F)
  
  cat("done!","\n")
})
cat("done!","\n")

rm(list = ls(all=T))
cat("Cleaned all workspace!","\n")

