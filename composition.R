require(raster);require(ff);require(ffbase)

###outcome dir###

impact_dir<-"E:/C_C/output/Bats_current_future"

##call out rasters and names##

current<-list.files(impact_dir,pattern = "_currrent_th.tif$",full.names = T)
current_name<-list.files(impact_dir,pattern = "_currrent_th.tif$",full.names = F)
current_name<-sub("_currrent_th.tif","",current_name)
current<-lapply(current,raster)

### Establishing ff vector###

temp.dt <-(current[[1]][])
temp.dt <- ff(1,dim=c(ncell(temp.dt),length(current)),vmode="double")

###Extracting raster values to ff vector###

lapply(1:length(current),function(i){
t <- getValues(current[[i]]) 
temp.dt[,i] <- t[]
return(cat(current_name[[i]]," Done\n"))})

temp.dt2<- as.ffdf(temp.dt)
names(temp.dt2) <- current_name
temp.dt2<- as.data.frame(temp.dt2)

###converting to character and preparing to extract spp data###

for(i in 1:ncol(temp.dt2)){
  temp.dt2[,i] <- as.character(temp.dt2[,i])
}; rm(i)

for(j in 1:ncol(temp.dt2)){
  
  cat("processing col=",j,"\n")
  temp.dt2[,j][which(temp.dt2[,j]=="0")] <- NA
  temp.dt2[,j][which(temp.dt2[,j]=="1")] <- current_name[j]
};rm(j)

###Concatenating spp data###

final <- ff(1,dim=c(ncell(temp.dt),2),vmode="double")
final[,1] <- 1:nrow(temp.dt2)

values<-unlist(lapply(1:nrow(temp.dt2), function(i){
  cat("concatenating spp data","- row:",i," of",nrow(temp.dt2),"\n")
  vector <- as.vector(as.matrix(temp.dt2[i,]))
  vector <- as.vector(na.omit(vector))
  z <- paste(vector, collapse = "-")
  return(z)
}))

###final fixes###
 
final<- as.ffdf(final)
names(final) <- c("ID","COMPOSITION")
final<- as.data.frame(final)
final[,2] <- values
val<- as.data.frame(matrix(ncol=2,nrow=length(unique(values))))
val[,1]<-(unique(values))
val<-val[-1,]
val[,2]<-1:nrow(val)
names(val)<-c("COMPOSITION","ID")
#write.table(final,"E:/C_C/output/current.csv",row.names=F,sep="|",na="",quote=F)

###Finding out unique spp composition###

final2<-final

for(i in 1:nrow(val)){
  cat("changing data for unique combination: ",i," of ",nrow(val),"\n")
  final2[,2][which(final2[,2]==val[i,1])] <- val[i,2]
};rm(i)

final2_3<-final2
final2_3[,2]<-as.numeric(final2_3[,2])
final3<-current[[1]]
count2<-1:ncell(final3)
final3[count2]<-final2_3[,2]

###saving data###

writeRaster(final3,"E:/C_C/output/current_com.tif")
write.csv(val,"E:/C_C/output/composition_ID.csv",quote = F,row.names = F)

###Cleaning console###

rm(list=ls(all=T));gc()
