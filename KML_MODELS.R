###################################
####        RASTER TO KMZ       ###
####           CCSA 2015        ###
###################################


cat("loading KML functions","\n")

KML_CONVERT_MODELS<-function(crop,src.dir,gap.dir,out_dir){
  cat("@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("CONVERTING MODELS TO KMZ","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@","\n")
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

    cat("Processing ",as.character(taxa_list[[i]]),"\n")
    
        if(sp_info$IS_VALID==1){
          if(file.exists(paste0(model_dir,"/",taxa_list[[i]],"/","projections","/",taxa_list[[i]],"_worldclim2_5_EMN_PA.asc.gz")))
             {
          sp_model<-zipRead(paste0(model_dir,"/",taxa_list[[i]],"/","projections"),paste0(taxa_list[[i]],"_worldclim2_5_EMN_PA.asc.gz"))
            }
          }else{
            if(file.exists(paste0(buffer_dir,"/",taxa_list[[i]],"/","samples-buffer-na.asc.gz"))){
              
              sp_model<-zipRead(paste0(buffer_dir,"/",taxa_list[[i]]),"samples-buffer-na.asc.gz")
          
            }else{
              cat("There is not buffer sp_model available for ",as.character(taxa_list[[i]]),"\n")
              sp_model<-0
          }
      } 
     
    if(!is.numeric(sp_model)){
  sp_model[which(sp_model[]==0)]<-NA
  proj4string(sp_model) <-CRS("+proj=longlat +datum=WGS84") 
    
  
  #if (!file.exists(out_CrModel_Dir)) {dir.create(out_CrModel_Dir)}
  
  
  kml(sp_model,file.name=paste0(taxa_list[[i]],".kml"),raster_name=paste0(taxa_list[[i]],".png"),colour_scale="#80ff00",plot.legend =F, kmz = F,layer.name="Distribution map",png.width=(ncol(sp_model)-1000),png.height = (nrow(sp_model)-1000))
  zip(paste0(taxa_list[[i]],".kmz"),files=c(paste0(taxa_list[[i]],".png"),paste0(taxa_list[[i]],".kml")))
  file.remove(c(paste0(taxa_list[[i]],".png"),paste0(taxa_list[[i]],".kml")))
    }else{
      cat("There is not buffer sp_model available for ",as.character(taxa_list[[i]]), "...SKIPPING...","\n")
  
    }
    
  };rm(i)
  cat("     ","\n");  cat("MODELS DONE!","\n");  cat("     ","\n")  
}



####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################

KML_CONVERT_GAP_SPP<-function(crop,src.dir,gap.dir,out_dir){
  
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("CONVERTING Priotiries map  TO KMZ","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  
  
  require(plotKML);require(raster);library(rgdal)
  
  source(paste0(src.dir,"/","000.zipRead.R"))
  
  
  #crop details
  crop_dir <- paste(gap.dir,"/gap_",crop,sep="")
  sp_list<-read.csv(paste0(crop_dir,"/","priorities","/","priorities.csv"),header=T)
  
    gap_spp_dir<-paste0(crop_dir,"/","gap_spp")
  
  out_CrDir<-paste0(out_dir,"/","gap_",crop);if (!file.exists(out_CrDir)) {dir.create(out_CrDir)}
  out_Cr_P<-paste0(out_CrDir,"/","gap_spp");if (!file.exists(out_Cr_P)) {dir.create(out_Cr_P)}
  #out_CrModel_Dir<-  out_CrDir<-paste0(out_CrDir,"/","models") ;if (!file.exists(out_CrModel_Dir)) {dir.create(out_CrModel_Dir)}
  
  p_cat<-c("HPS","MPS","LPS","NFCR")
  
  for(p in p_cat){  
    
   
    cat("CONVERTING Priotiries map  TO KMZ for ",p,"\n")
    
    gap_spp_dir_P<-paste0(gap_spp_dir,"/",p)
    
    sp_info<-sp_list[which(sp_list$FPCAT==p),]
   if(length(sp_info$TAXON)==0){
     
     cat("Not available data for ", p,"\n")
   }else{
     out_gap_spp<-paste0(out_Cr_P,"/",p);if (!file.exists(out_gap_spp)) {dir.create(out_gap_spp)}
     
     taxa_list<-sp_info$TAXON
for(i in 1:length(taxa_list)){
  
    taxa_info<-sp_info[which(sp_info$TAXON==taxa_list[[i]]),]
    setwd(out_gap_spp)
    
    if(file.exists(paste0(gap_spp_dir_P,"/",taxa_list[[i]],".asc.gz"))){
      
      gap_model<-zipRead(paste0(gap_spp_dir_P,"/"),paste0(taxa_list[[i]],".asc.gz"))
      
    }
    
    
    
    gap_model[which(gap_model[]==0)]<-NA
    proj4string(gap_model) <-CRS("+proj=longlat +datum=WGS84") 
    
    
    #if (!file.exists(out_CrModel_Dir)) {dir.create(out_CrModel_Dir)}
    
    
    kml(gap_model,file.name=paste0(taxa_list[[i]],".kml"),raster_name=paste0(taxa_list[[i]],".png"),colour_scale="#ff8000",plot.legend =F, kmz = F,layer.name="Collecting priorities map",png.width=(ncol(gap_model)-1000),png.height = (nrow(gap_model)-1000))
    zip(paste0(taxa_list[[i]],".kmz"),files=c(paste0(taxa_list[[i]],".png"),paste0(taxa_list[[i]],".kml")))
    file.remove(c(paste0(taxa_list[[i]],".png"),paste0(taxa_list[[i]],".kml")))
    
   }
  }
 };rm(p)
  cat("     ","\n");  cat("GAP SPP DONE!","\n");  cat("     ","\n")  
  }

