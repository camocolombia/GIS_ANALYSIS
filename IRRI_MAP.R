require(RgoogleMaps);  require(ggplot2);require(ggmap);


path<-"D:/CWR_OCC_VALIDATION"
ACC<-as.data.frame(read.csv(paste0(path,"/","RAW_GEO_IRR_IMPROVED_ID.csv"),sep="|",header=T,na.strings = ""))
gc()
#ACC$final_lon,ACC$final_lat
ACC2<-as.data.frame(cbind(as.numeric(as.character(ACC$final_lon)),as.numeric(as.character(ACC$final_lat))))
colnames(ACC2)<-c("final_lon","final_lat")
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders


# for(i in 1:ncol(ACC)){
#   ACC[,1]<-as.numeric(ACC[,1])
#   ACC[,2]<-as.numeric(ACC[,2])
#   
# };rm(i)
xmin<-min(ACC2$final_lon,na.rm=T)
xmax<-max(ACC2$final_lon,na.rm=T)


ymin<-min(ACC2$final_lat,na.rm=T)
ymax<-max(ACC2$final_lat,na.rm=T)
# 
# MaxZoom2<-MaxZoom(c(ymin,ymax),c(xmin,xmax))
# 
# if(MaxZoom2<3){
#   MaxZoom2=3
# }
# 
# center <- rev(sapply(ACC2, mean,na.rm=T))
# center<-rev(center)
# 
# ss<-get_map(location=center, zoom = MaxZoom2, maptype='road', source='google',crop=TRUE,color='bw')
# 
# map1 <- ggmap(ss, extent='panel')#, base_layer=ggplot(data=temp_dt2,aes(x=long, y=lat)))
# # map1<-map1+geom_point(color="black",size=0.5)
# map1<-map1+geom_point(aes(x=final_lon, y=final_lat),size=2,stroke = 1,data=ACC2, show.legend = TRUE,shape=17)#Alt + 9733	
# map1<-map1+labs(x ="Longitude", y="Latitude")
# map1<-map1+ggtitle("IRRI_GEO_IMPROVED")
# map1<-map1+ theme(axis.text=element_text(size=10),axis.title=element_text(size=10),legend.position="bottom",legend.title=element_blank())+
#   #scale_fill_manual(name="Taxon occurrences",values="black") # no title) 
#   scale_colour_manual(name="Taxon occurrences",values="black")



mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=final_lon, y=final_lat) ,color="blue", size=3,data = ACC2) 
mp <- mp + ggtitle("IRRI_GEO_IMPROVED")
mp<-mp+labs(x ="Longitude", y="Latitude")

ggsave(paste0(path,"/","IRRI_GEO_IMPROVED","_",Sys.Date(),".pdf"),units="in",width=18,height=8,scale=2,dpi=600)
########################################
########################################
########################################
########################################
ACC<-as.data.frame(read.csv(paste0(path,"/","USDA_GEO_ID.csv"),sep="|",header=T,na.strings = ""))
gc()
#ACC$final_lon,ACC$final_lat
ACC2<-as.data.frame(cbind(as.numeric(as.character(ACC$longitude)),as.numeric(as.character(ACC$latitude))))
colnames(ACC2)<-c("final_lon","final_lat")
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders


# for(i in 1:ncol(ACC)){
#   ACC[,1]<-as.numeric(ACC[,1])
#   ACC[,2]<-as.numeric(ACC[,2])
#   
# };rm(i)
xmin<-min(ACC2$final_lon,na.rm=T)
xmax<-max(ACC2$final_lon,na.rm=T)


ymin<-min(ACC2$final_lat,na.rm=T)
ymax<-max(ACC2$final_lat,na.rm=T)
# 
# MaxZoom2<-MaxZoom(c(ymin,ymax),c(xmin,xmax))
# 
# if(MaxZoom2<3){
#   MaxZoom2=3
# }
# 
# center <- rev(sapply(ACC2, mean,na.rm=T))
# center<-rev(center)
# 
# ss<-get_map(location=center, zoom = MaxZoom2, maptype='road', source='google',crop=TRUE,color='bw')
# 
# map1 <- ggmap(ss, extent='panel')#, base_layer=ggplot(data=temp_dt2,aes(x=long, y=lat)))
# # map1<-map1+geom_point(color="black",size=0.5)
# map1<-map1+geom_point(aes(x=final_lon, y=final_lat),size=2,stroke = 1,data=ACC2, show.legend = TRUE,shape=17)#Alt + 9733	
# map1<-map1+labs(x ="Longitude", y="Latitude")
# map1<-map1+ggtitle("IRRI_GEO_IMPROVED")
# map1<-map1+ theme(axis.text=element_text(size=10),axis.title=element_text(size=10),legend.position="bottom",legend.title=element_blank())+
#   #scale_fill_manual(name="Taxon occurrences",values="black") # no title) 
#   scale_colour_manual(name="Taxon occurrences",values="black")



mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=final_lon, y=final_lat) ,color="blue", size=3,data = ACC2) 
mp <- mp + ggtitle("IRRI_DATASET")
mp<-mp+labs(x ="Longitude", y="Latitude")

ggsave(paste0(path,"/","IRRI_DATASET","_",Sys.Date(),".pdf"),units="in",width=18,height=8,scale=2,dpi=600)
