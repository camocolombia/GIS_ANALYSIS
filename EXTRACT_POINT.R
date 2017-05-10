require(raster)

dir_ras<-list.files("D:/SS",pattern = ".tif$",full.names = T)
dir_ras_N<-list.files("D:/SS",pattern = ".tif$",full.names = F)


lat=4.221186
lon=-72.553422

coordM<-cbind(lon,lat)

sInfo<-lapply(1:length(dir_ras),function(i){
  
  ss<-extract(raster(dir_ras[[i]]),coordM)
  gc()
  return(ss)

})

sInfo<-as.matrix(unlist(sInfo))
row.names(sInfo)<-dir_ras_N
colnames(sInfo)<-c("Value")

write.csv(sInfo,"D:/info.csv")
