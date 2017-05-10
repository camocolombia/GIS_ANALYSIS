require(plotKML);require(raster);library(rgdal)

#Sys.setenv(R_ZIPCMD="C:/Program Files/7-Zip/7zFM.exe")

#ras<-raster("D:/KML/Helianthus_tuberosus_worldclim2_5_EMN_PA.asc")
#gr<-raster("D:/KML/gap-richness_HPS.asc")
#gr<-raster("D:/KML/gap-richness_CP.asc")

gr<-raster("E:/Helianthus_new_maps/gap-richness_HPS.asc",RAT=T)
#ras[which(ras[]==0)]<-NA
gr[which(gr[]==0)]<-NA
#gr<-setMinMax(gr)


#proj4string(ras) <- CRS("+proj=longlat +datum=WGS84")

proj4string(gr) <-CRS("+proj=longlat +datum=WGS84") 
# mask<-shapefile("D:/KML/mask.shp")
# 
 gr2<-rasterToPolygons(gr,na.rm=T)

#get("ref_CRS", envir = plotKML.opts)
#plotKML(ras)
#num<-unique(gr[])
maxColorValue <- 40

setwd("E:")
#palette <- colorRampPalette(c("blue","green","red"))(maxColorValue)
palette <- colorRampPalette(c("#EDB01D","#77ED1D","#1DED75","#1DB3ED","#391DED","#EC1DED","#ED1D3C"))(maxColorValue)


#kml(ras, file.name = "ras2.kml", colour_scale="green",plot.legend =F, kmz = TRUE)#"C:/Program Files/7-Zip/7zFM.exe"
kml(gr,file.name = "gap_r.kml",raster_name="gap_richness.png",colour_scale=palette,plot.legend =T, kmz = F,labels="# of taxa",var.name="gap_richness",altitude=1)
#kml_compress("gap_r.kmz",files=c("gap_richness.png","gap_richness_legend.png","gap_r.kml"),rm=F)
# files<-c(paste0("E:/Dropbox/Dropbox/WIEW","/",".png"),
# paste0("E:/Dropbox/Dropbox/WIEW","/","_legend.png"),paste0("E:/Dropbox/Dropbox/WIEW","/","gap_r.kml"))
#kml_compress(file.name = "gap_r.kml",files=files,rm=F)
#kml_layer.Raster(gr,plot.legend = T,raster_name="gap_richness.png",colour_scale=palette)
zip("gap_richness.kmz",files=c("gap_richness.png","gap_richness_legend.png","gap_r.kml"))
file.remove(c("gap_richness.png","gap_richness_legend.png","gap_r.kml"))









##############


KML_CONVERT_MODELS<-function(crop,src.dir,gap.dir,out_dir){
  
  require(plotKML);require(raster);library(rgdal)
  
  source(paste0(src.dir,"/","000.zipRead.R"))
  
  
  #crop details
  crop_dir <- paste(gap.dir,"/gap_",crop,sep="")
  sp_list<-read.csv(paste0(crop_dir,"/","priorities","/","priorities.csv"),header=T)
  taxa_list<-sp_list$TAXON
  model_dir<-paste0(crop_dir,"/","maxent_modeling/models")
  buffer_dir<-paste0(crop_dir,"/","samples_calculations")
  
  out_CrDir<-paste0(out_dir,"/","gap_",crop);if (!file.exists(out_CrDir)) {dir.create(out_CrDir)}
  out_CrModel_Dir<-  out_CrDir<-paste0(out_CrDir,"/","models") ;if (!file.exists(out_CrModel_Dir)) {dir.create(out_CrModel_Dir)}


  
  for(i in 1:length(taxa_list)){
    setwd(out_CrModel_Dir)
    sp_info<-sp_list[which(sp_list$TAXON==taxa_list[[i]]),]

        if(sp_info$IS_VALID==1){
          if(file.exists(paste0(model_dir,"/",taxa_list[[i]],"/","projections","/",taxa_list[[i]],"_worldclim2_5_EMN_PA.asc.gz")))
             {
          sp_model<-zipRead(paste0(model_dir,"/",taxa_list[[i]],"/","projections"),paste0(taxa_list[[i]],"_worldclim2_5_EMN_PA.asc.gz"))
            }
          }else{
            if(file.exists(paste0(buffer_dir,"/",taxa_list[[i]]),"samples-buffer-na.asc.gz")){
              
              sp_model<-zipRead(paste0(buffer_dir,"/",taxa_list[[i]]),"samples-buffer-na.asc.gz")
          
        }
      } 
     
  sp_model[which(sp_model[]==0)]<-NA
  proj4string(sp_model) <-CRS("+proj=longlat +datum=WGS84") 
    
  
  #if (!file.exists(out_CrModel_Dir)) {dir.create(out_CrModel_Dir)}
  
  
  kml(sp_model,file.name=paste0(taxa_list[[i]],".kml"),raster_name=paste0(taxa_list[[i]],".png"),colour_scale="#80ff00",plot.legend =F, kmz = F,var.name="Distribution map")
  #kml_compress("gap_r.kmz",files=c("gap_richness.png","gap_richness_legend.png","gap_r.kml"),rm=F)
  # files<-c(paste0("E:/Dropbox/Dropbox/WIEW","/",".png"),
  # paste0("E:/Dropbox/Dropbox/WIEW","/","_legend.png"),paste0("E:/Dropbox/Dropbox/WIEW","/","gap_r.kml"))
  #kml_compress(file.name = "gap_r.kml",files=files,rm=F)
  #kml_layer.Raster(gr,plot.legend = T,raster_name="gap_richness.png",colour_scale=palette)
  zip(paste0(taxa_list[[i]],".kmz"),files=c(paste0(taxa_list[[i]],".png"),paste0(taxa_list[[i]],".kml")))
  file.remove(c(paste0(taxa_list[[i]],".png"),paste0(taxa_list[[i]],".kml")))

  }
}


crop<-"bean"
out_dir<-"/mnt/workspace_cluster_6/KML"
src.dir <- paste("/curie_data/ncastaneda/gap-analysis/gap_",crop,"/_scripts",sep="") # !!! change accordingly !!!
gap.dir <-"/curie_data/ncastaneda/gap-analysis" # !!! change accordingly !!!

KML_CONVERT_MODELS(crop=crop,src.dir=src.dir,gap.dir=gap.dir,out_dir=out_dir)
