path<-"C:/Users/csosa/SharePoint/CWR of North America (Springe - Boo/Fruits_grape_Heinitz/data"
files<-list.files(path,pattern = ".csv$",full.names = T)

files<-lapply(1:length(files),function(i){
  
  x<-read.csv(files[[i]],header = T,sep="|",na.strings = NA)
return(x)
  })

ss<-do.call(rbind,files)
 
write.table(ss,paste0(path,"/","Vitis.csv"),sep = "|",na = "")

