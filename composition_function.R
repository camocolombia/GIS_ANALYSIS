composition<-function(out_dir,input_dir,pattern_r,filename){
require(raster);require(ff);require(ffbase)

##call out rasters and names##
  
  cat("#################################","\n") 
  cat("##COMPOSITION FUNCTION RUNNING!##","\n") 
  cat("#################################","\n")   
  cat("#######PLEASE BE PATIENT!########","\n") 
  cat("#################################","\n") 
  
  cat("                          ","\n")
  cat("###Loading raster files###","\n")  
  cat("                          ","\n")

current<-list.files(input_dir,pattern =pattern_r ,full.names = T)
current_name<-list.files(input_dir,pattern = pattern_r,full.names = F)
current_name<-sub(pattern_r,"",current_name)
current<-lapply(current,raster)

### Establishing ff vector###

temp.dt <-(current[[1]][])
temp.dt <- ff(1,dim=c(ncell(temp.dt),length(current)),vmode="double")

###Extracting raster values to ff vector###

cat("                              ","\n") 
cat("###Extracting raster values###","\n") 
cat("                              ","\n")

lapply(1:length(current),function(i){
t <- getValues(current[[i]]) 
temp.dt[,i] <- t[]
return(cat(current_name[[i]]," done\n"))})


gc()

temp.dt2<- as.ffdf(temp.dt)
names(temp.dt2) <- current_name
temp.dt2<- as.data.frame(temp.dt2)

###converting to character and preparing to extract spp data###

cat("                                              ","\n")
cat("###Preparing data files to perform analysis###","\n") 
cat("                                              ","\n") 

for(i in 1:ncol(temp.dt2)){
  temp.dt2[,i] <- as.character(temp.dt2[,i])
}; rm(i)

for(j in 1:ncol(temp.dt2)){
  
  cat("processing",current_name[[j]],"\n")
  temp.dt2[,j][which(temp.dt2[,j]=="0")] <- NA
  temp.dt2[,j][which(temp.dt2[,j]=="1")] <- current_name[j]
};rm(j)

###Concatenating spp data###

final <- ff(1,dim=c(ncell(temp.dt),2),vmode="double")
final[,1] <- 1:nrow(temp.dt2)

values<-unlist(lapply(1:nrow(temp.dt2), function(i){
  #cat("concatenating spp data","- row:",i," of",nrow(temp.dt2),"\n")
  cat("concatenating spp data",round((i/nrow(temp.dt2)*100),2)," %","\n")
  
   vector <- as.vector(as.matrix(temp.dt2[i,]))
  vector <- as.vector(na.omit(vector))
  z <- paste(vector, collapse = "-")
  return(z)
}))

gc()

###final fixes###
cat("                                        ","\n")
cat("###Finding unique composition species###","\n") 
cat("                                        ","\n")

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
gc()
###Finding out unique spp composition###

cat("                                ","\n") 
cat("###Codifying composition data###","\n") 
cat("                                ","\n") 

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
gc()

cat("                    ","\n") 
cat("###Saving results###","\n")
cat("                    ","\n") 

final4<-ratify(final3)
rat <- levels(final4)[[1]]
rat$legend<-val$COMPOSITION
levels(final4) <- rat

#writeRaster(final4,paste0(out_dir,"/",filename,"_RAT.tif"), datatype="INT1U", RAT=TRUE, overwrite=TRUE)
writeRaster(final4,paste0(out_dir,"/",filename,"_RAT.tif"), RAT=TRUE)
writeRaster(final3,paste0(out_dir,"/",filename,".tif"))
write.csv(val,paste0(out_dir,"/",filename,"_IDs.csv"),quote = F,row.names = F)

gc()
cat("#########","\n") 
cat("##DONE!##","\n") 
cat("#########","\n") 
###Cleaning console###
#rm(list=ls(all=T))

}


#input_dir<-"E:/C_C/output/Bats_current_future"
input_dir<-"E:/Dropbox/Dropbox/PARAGUAY/NEW/Resultado_Paraguay_05_11_2015"
pattern_r<-"_currrent_th.tif$"
pattern_rF<-"_future__th.tif$"

#out_dir<-"E:/C_C/output"
out_dir<-"E:/Dropbox/Dropbox/PARAGUAY/NEW/COMPOSITION"
filenameF<-"composition_FUT"
filename<-"composition_CUR"



#cut<-composition(out_dir=out_dir,input_dir=input_dir,pattern_r=pattern_r,filename=filename)
fut<-composition(out_dir=out_dir,input_dir=input_dir,pattern_r=pattern_rF,filename=filenameF)


