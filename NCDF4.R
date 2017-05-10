require(raster);library(ncdf4)


out_dir<-"D:/NAREA/STA"
fname <-nc_open("D:/NAREA/historical_global_1-degree_forest_biomass.nc4")

varnames<-c(
'ForestStockChange',
'Forest_carbonstock',
'Total_Biomass',
'R',
'AGB_ha',
'AboveGroundBiomass',
'ConBCEF',
'BrBCEF',
'ConiferShare',
'stockperarea',
'stock',
'area',
'Unc_GS',
'Unc_Area')



lapply(1:length(varnames),function(i){
  
  varname2<-varnames[[i]]
  
  cat("Processing ",as.character(varname2),"\n")
TAS <- brick("D:/NAREA/historical_global_1-degree_forest_biomass.nc4", varname=varname2)
names(TAS)<-c(
  "1950",
  "1955",
  "1960",
  "1965",
  "1970",
  "1975",
  "1980",
  "1985",
  "1990",
  "1995",
  "2000",
  "2005",
  "2010"
)




TAS2<-stack(TAS)
TAS2_NAMES<-names(TAS2)
TAS2_NAMES<-sub("X","",TAS2_NAMES)

lapply(1:nlayers(TAS2),function(j){
  cat("Processing ",as.character(varname2)," for ",as.character(TAS2_NAMES[[j]]),"\n")
  
  writeRaster(TAS2[[j]],paste0(out_dir,"/",varname2,"_",TAS2_NAMES[[j]],".tif"))
  
  
  })

})

# 
#require(animation)
# title_n<-c(
#     "1950",
#     "1955",
#     "1960",
#     "1965",
#     "1970",
#     "1975",
#     "1980",
#     "1985",
#     "1990",
#     "1995",
#     "2000",
#     "2005",
#     "2010"
#   )
# 
# saveGIF({
#   
#   ani.options(inteval=0.2,nmax=50)
#     
# 
# for(i in 1:nlayers(TAS2)){
#   img<-TAS2[[i]]
#   
#   col5 <- colorRampPalette(c('blue', 'gray96', 'red'))  #create color ramp starting from blue to red
#   color_levels=10
#   max_abolute_value=max(abs(c(cellStats(img, min), cellStats(img, max)))) #what is the maximum absolute value of raster?
#   color_sequence=seq(-max_abolute_value,max_abolute_value,length.out=color_levels+1)
#   
#   plot(TAS2[[i]], col=col5(n=color_levels), breaks=color_sequence, axes=FALSE)
#   title(title_n[[i]])
#   
#   ani.pause()
# }
# },movie.name="TEST.gif",
# ani.width=1800,ani.height=1000)
# 
# 
# 

