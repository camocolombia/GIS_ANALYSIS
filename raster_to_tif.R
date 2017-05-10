require(raster)

#path<-"D:/CWR/HELIANTHUS_SOIL"
path<-"D:/CWR/2016_05_11/PECAN/gap_spp/"
s<-list.files(path,pattern=".asc*",full.names = T)
s<-lapply(s,raster)
lapply(1:length(s),function(i){
  
  s[[i]][which(s[[i]][]<=0)]<-NA  
  proj4string(s[[i]])<-CRS("+proj=longlat +datum=WGS84") 
  writeRaster(s[[i]],paste0(path,"/",names(s[[i]]),".tif"));gc()
})