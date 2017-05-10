cut_raster<-function(input_dir,gpname,spname,shp_dir,shpname,src_dir,out_dir,countries_list){
  
require(raster)
source(paste0(src_dir,"/","000.zipWrite.R"))

gap_rich<-raster(paste0(input_dir,"/",gpname))
sp_rich<-raster(paste0(input_dir,"/",spname))
shp_world<-shapefile(paste0(shp_dir,"/",shpname))


lapply(1:length(countries_list),function(i){
  
a<-shp_world[which(shp_world@data$NAME==countries_list[[i]]),]

cat("Fitting gap_richness map to ",countries_list[[i]],"\n") 
g<-crop(gap_rich,a);g<-mask(g,a)
cat("Fitting spp_richness map to ",countries_list[[i]],"\n")
s<-crop( sp_rich,a);s<-mask(s,a)


x<-zipWrite(g,out_dir,paste0(countries_list[[i]],"_gap_rich",".asc.gz"))
y<-zipWrite(s,out_dir,paste0(countries_list[[i]],"_spp_rich",".asc.gz"))

cat(countries_list[[i]]," DONE!","\n")  
gc()
})

}

input_dir<-"Z:/CWR/global"                 ##raster path
gpname<-"gap-taxa-rich_2015-08-14_all.asc" ##filename
spname<-"spp-taxa-rich_2015-08-14_all2.asc"##filename
shp_dir<-"Z:/CWR/ADMIN"                    ##ADMIN shapefile
shpname<-"TM_WORLD_BORDERS-0.3.shp"        ##shapefile name
src_dir<-"Z:/CWR/SCRIPTS"                  ##source name
out_dir<-"Z:/CWR/output"
countries_list<-c(
  
  "Armenia",
  "Guatemala",
  "Peru",
  "Costa Rica",
  "Ecuador",
  "Chile")


x<-cut_raster(input_dir,gpname,spname,shp_dir,shpname,src_dir,out_dir,countries_list)
