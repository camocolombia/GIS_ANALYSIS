###################################
####        RASTER TO KMZ       ###
####           CCSA 2015        ###
###################################


cat("loading KML summary functions","\n")
KML_SUMMARIES<-function(crop,src.dir,gap.dir,out_dir){

  require(plotKML);require(raster);library(rgdal)
  
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("CONVERTING SP RICHNESS TO KMZ","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  
 
  
  source(paste0(src.dir,"/","000.zipRead.R"))
  
  maxColorValue <- 40
  palette <- colorRampPalette(c("#EDB01D","#77ED1D","#1DED75","#1DB3ED","#391DED","#EC1DED","#ED1D3C"))(maxColorValue)
  
  
  #crop details
  crop_dir <- paste0(gap.dir,"/gap_",crop)
  sp_rich_dir<-paste0(crop_dir,"/","species_richness")
  out_CrDir<-paste0(out_dir,"/","gap_",crop);if (!file.exists(out_CrDir)) {dir.create(out_CrDir)}
  out_sprich_Dir<-paste0(out_CrDir,"/","species_richness") ;if (!file.exists(out_sprich_Dir)) {dir.create(out_sprich_Dir)}
  gp_rich_dir<-paste0(crop_dir,"/","gap_richness")
  out_gprich_Dir<-paste0(out_CrDir,"/","gap_richness") ;if (!file.exists(out_gprich_Dir)) {dir.create(out_gprich_Dir)}
  
  if(file.exists(paste0(sp_rich_dir,"/","species-richness.asc.gz"))){
  sprich<-zipRead(sp_rich_dir,"species-richness.asc.gz")
  }
  
  sprich[which(sprich[]==0)]<-NA
  proj4string(sprich) <-CRS("+proj=longlat +datum=WGS84") 
  
   
  setwd(out_sprich_Dir)
  
  kml(sprich,file.name="species_richness.kml",raster_name="species_richness.png",colour_scale=palette,plot.legend =T, kmz = F,layer.name="Taxon richness map",png.width=7640,png.height = 1600)
  zip("species_richness.kmz",files=c("species_richness.png","species_richness.kml","species_richness_legend.png"))
  file.remove(c("species_richness.png","species_richness.kml","species_richness_legend.png"))
  
  cat("     ","\n");  cat(" SP_RICH DONE!","\n");  cat("     ","\n")
  
  
  rm(sprich,out_sprich_Dir,sp_rich_dir)
  
  
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("CONVERTING GAP RICHNESS TO KMZ","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  
  p<-c("HPS","MPS","LPS","NFCR")
  
  for(pcat in p){
    
    out_gprich_Dir_P<-paste0(out_gprich_Dir,"/",pcat) ;if (!file.exists(out_gprich_Dir_P)) {dir.create(out_gprich_Dir_P)}
    setwd(out_gprich_Dir_P)
    
    if(file.exists(paste0(gp_rich_dir,"/",pcat,"/","gap-richness.asc.gz"))){
      
      gaprich<-zipRead(paste0(gp_rich_dir,"/",pcat),"gap-richness.asc.gz")
      
    }
    
    
    
    gaprich[which(gaprich[]==0)]<-NA
    proj4string(gaprich) <-CRS("+proj=longlat +datum=WGS84") 
   
    kml(gaprich,file.name="gap_richness.kml",raster_name="gap_richness.png",colour_scale=palette,plot.legend =T, kmz = F,layer.name="Collecting hotspots map",png.width=7640,png.height = 1600)
    zip("gap_richness.kmz",files=c("gap_richness.png","gap_richness",".kml","gap_richness_legend.png"))
    file.remove(c("gap_richness.png","gap_richness",".kml","gap_richness_legend.png"))
  }
  
  rm(gp_rich_dir,out_gprich_Dir,gaprich)
  
  cat("     ","\n");  cat("GAP RICHNESS DONE!","\n");  cat("     ","\n")  
}


####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################

# crop<-"cajanusPaper"
# out_dir<-"/mnt/workspace_cluster_6/KML"
# src.dir <- paste("/curie_data/ncastaneda/gap-analysis/gap_",crop,"/_scripts",sep="") # !!! change accordingly !!!
# gap.dir <-"/curie_data/ncastaneda/gap-analysis" # !!! change accordingly !!!
# KML_CONVERT_MODELS(crop=crop,src.dir=src.dir,gap.dir=gap.dir,out_dir=out_dir)
# KML_CONVERT_GAP_SPP(crop=crop,src.dir=src.dir,gap.dir=gap.dir,out_dir=out_dir)
# KML_SUMMARIES(crop=crop,src.dir=src.dir,gap.dir=gap.dir,out_dir=out_dir)