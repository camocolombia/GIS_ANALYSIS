require(FactoMineR);
require(raster)
require(dismo)
require(SDMTools)
require(sp)
require(usdm)


bios<-list.files("D:/ccsosa/Mikey_request/ALL/asc",pattern = ".asc$",full.names = T)

bios<-lapply(bios,raster)
bios<-stack(bios)


occs<-list.files("D:/ccsosa/Mikey_request/points",pattern=".csv$",full.names=T)
occs_name<-list.files("D:/ccsosa/Mikey_request/points",pattern=".csv$",full.names=F)


lapply(1:length(occs),function(i){
  
  cat("loading points from ", occs_name[[i]],"\n")
  
points<-read.csv(occs[[i]],header=T,sep=",")  
  
p2=points

p2[,1]<-as.numeric(as.character(p2[,1]))
p2[,2]<-as.numeric(as.character(p2[,2]))

p2<-SpatialPoints(p2)

cat("Extracting climatic values  for ", occs_name[[i]],"\n")

bio_point<-extract(x = bios,y = p2)

cat(" climatics values  for ", occs_name[[i]]," done!","\n")

cat(" Doing PCA for ", occs_name[[i]],"\n")

ss<-PCA(bio_point)
ssdim<-dimdesc(ss, axes=c(1,2))
axe1<-row.names(ssdim$Dim.1$quanti)[which(ssdim$Dim.1$quanti[,1]>=0.7 | ssdim$Dim.1$quanti[,1]<=-0.7)]
axe2<-row.names(ssdim$Dim.2$quanti)[which(ssdim$Dim.2$quanti[,1]>=0.7 | ssdim$Dim.2$quanti[,1]<=-0.7)]
axe<-c(axe1,axe2)
axe<-c(axe,"scec")
axe<-unique(axe) 
  

bio2<-bio_point[,axe]
cat(" Doing VIF for ", occs_name[[i]],"\n")
vif2<-vifstep(bio2)
result<-rbind(vif2@results[1],"scec")
result<-unique(result)

cat(" Saving vars to run Maxent  for ", occs_name[[i]],"\n")
write.csv(result,paste0("D:/ccsosa/Mikey_request/var_choice","/",occs_name[[i]],"_choice.csv"),row.names=F,quote=F)

cat("Done!","\n")
  
})