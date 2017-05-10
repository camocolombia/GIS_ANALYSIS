require(ggplot2);require(reshape2);require(raster):require(FactoMineR):require(pca3d);require(car);require(rgl);require(usdm);require(Hmisc)


out_dir<-"D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31"
#######
# raster_dir<-list.files("D:/CWR_OCC_VALIDATION/NORTHAMERICA_RASTER_DATA/BIOCLIM",pattern=".asc$",full.names=T)
# raster_dir_name<-list.files("D:/CWR_OCC_VALIDATION/NORTHAMERICA_RASTER_DATA/BIOCLIM",pattern=".asc$",full.names=F)
# raster_dir<-lapply(raster_dir,raster)
# raster_dir<-stack(raster_dir)
# #######
# csv_file<-read.csv("D:/CWR_OCC_VALIDATION/LatLongDormcomparison_2016_10_26_1.csv",sep="|",header=T)
# csv_file<-csv_file[complete.cases(csv_file),]
# csv_coord<-as.data.frame(cbind(csv_file$Long,csv_file$Lat))
# colnames(csv_coord)<-c("Long","Lat")
# coordinates(csv_coord)<-~Long+Lat
# values<-extract(raster_dir,cbind(csv_file$Long,csv_file$Lat))
# #######
# csv_file<-cbind(csv_file,values)
# #write.table(csv_file,"D:/CWR_OCC_VALIDATION/comparison_2016_10_26_1_VALUES.csv",sep="|",row.names = F)

# 

# 
# csv_file<-cbind(csv_file,values)
#write.table(csv_file,"D:/CWR_OCC_VALIDATION/LatLongDorm_comparison_2016_08_31_VALUES.csv",sep="|",row.names = F)
#taxa<-unique(csv_file$Taxon)
csv_file<-read.csv("D:/CWR_OCC_VALIDATION/comparison_2016_10_26_1_VALUES.csv",sep="|",header=T)
#
csv_file[,c("bio_1","bio_2","bio_5","bio_6","bio_7","bio_8","bio_9","bio_10","bio_11")] <- csv_file[,c("bio_1","bio_2","bio_5","bio_6","bio_7","bio_8","bio_9","bio_10","bio_11")]/10
csv_file[,c("bio_3","bio_4","bio_15")] <- csv_file[,c("bio_3","bio_4","bio_15")]/100
csv_file[,c("bio_24")] <- csv_file[,c("bio_24")]/10





colnames(csv_file)<-c("Inventory.Number",
                      "Inventory.Suffix",
                      "Accession",
                      "Taxon name (Trinomial Nomenclature)",
                      "Species name",
                      "Latitude",
                      "Longitude",
                      "percent dormancy",
                      "altitude",
                      "mean annual temperature", #10
                      "mean diurnal range",
                      "Isothermality",
                      "Temperature seasonality",
                      "Max temp warmest month",
                      "Min temp coldest month",
                      "Temperature annual range",
                      "Mean temperature wettest quarter",
                      "Mean temp of driest quarter",
                      "Mean temp warmest quarter",
                      "Mean temp coldest quarter", #20
                      "Annual precipitation",
                      "Precipitation in wettest month",
                      "Precipitation in driest month",
                      "Precipitation Seasonality",
                      "Precipitation in wettest quarter",
                      "Precipitation in driest quarter",
                      "Precipitation in warmest quarter",
                      "Precipitation in coldest quarter",
                      "Bulk density",
                      "Cation Exchange Capacity", #30
                      "Percent Clay",
                      "Organic Carbon",
                      "pH in H2O",
                      "Percent Silt",
                      "Percent Sand"
)

csv_file$Dormancy_cat<-NA

for(i in 1:nrow(csv_file)){
  if(csv_file$`percent dormancy`[[i]]>=0 & csv_file$`percent dormancy`[[i]]<=9){
    csv_file$Dormancy_cat[[i]]<-"Very low" 
  }else if(csv_file$`percent dormancy`[[i]]>=10 & csv_file$`percent dormancy`[[i]]<=49){
    csv_file$Dormancy_cat[[i]]<-"Low"
  }else if(csv_file$`percent dormancy`[[i]]>=50 & csv_file$`percent dormancy`[[i]]<=89){
    csv_file$Dormancy_cat[[i]]<-"Medium"
  }else if(csv_file$`percent dormancy`[[i]]>=90 & csv_file$`percent dormancy`[[i]]<=100){
    csv_file$Dormancy_cat[[i]]<-"High"
  }  
};rm(i)


csv_file<-csv_file[order(csv_file$Dormancy_cat== "Very low",csv_file$Dormancy_cat== "Low",csv_file$Dormancy_cat== "Medium",csv_file$Dormancy_cat== "High",decreasing = T),]
csv_file$color<-NA
csv_file$color[csv_file$Dormancy_cat=="Very low"]<-"#799244"#"#0035ff" #azul
csv_file$color[csv_file$Dormancy_cat=="Low"]<-"#c3d69b"#"#00ff19" #verde
csv_file$color[csv_file$Dormancy_cat=="Medium"]<-"#f2dcdb"#"#faff00" #amarillo
csv_file$color[csv_file$Dormancy_cat=="High"]<-"#d99694"#"#e60b00" #rojo

  taxa<-unique(csv_file$`Species name`)

NAME<-unlist(lapply(1:length(taxa),function(i){
  taxon3<-as.character(taxa[[i]])
  ssN<-substitute(expr=italic(taxon3),env=list(taxon3=taxon3))
  return(ssN)
}))


###################################################################################################
###################################################################################################
###################################################################################################

taxa_File<-subset(csv_file,csv_file$`Species name`==taxa[[1]])
taxa_File<-taxa_File[,-c(1:5)]
taxa_File<-taxa_File[,-c(2)]

#
res_pca <- PCA(taxa_File,graph = F,quali.sup = c(30,31),scale.unit=T)
res_pca_1<-PCA(taxa_File,graph = F,ncp = 12,quali.sup = c(30,31),scale.unit=T)
res_pca_2<-PCA(taxa_File[,c(1:29)],graph = F,ncp = 12,scale.unit=T)


###################################################################################################

res_pca_prcomp<-prcomp(taxa_File[,-c(30,31)])

#################################EQUALING PCA METHODS TO USE PCA3D FUNCTION###################################
res_pca_prcomp_test<-res_pca_prcomp
#sqrt(res_pca$eig$eigenvalue)
res_pca_prcomp_test$sdev<-sqrt(res_pca$eig$eigenvalue)#Standard Deviation  Vs Standard Error

rot<-t(apply(res_pca$var$coord, 1, function(x) {x/sqrt(res_pca$eig[,1])}))

res_pca_prcomp_test$rotation<-rot[order(row.names(res_pca_prcomp$rotation)),]
colnames(res_pca_prcomp_test$rotation)<-paste0("PC",1:ncol(res_pca_prcomp_test$rotation))
res_pca_prcomp_test$center<-res_pca_prcomp_test$center #NO CAMBIA
res_pca_prcomp_test$scale<-res_pca_prcomp_test$scale   #NO CAMBIA
or_ISO<-matrix(row.names(res_pca_prcomp$x))
ind<-as.data.frame(res_pca_1$ind$coord)
ind$nam<-row.names(ind)
#ind[order(or_ISO,ind$ISO3),]
ind2<-merge(data.frame(nam=row.names(res_pca_prcomp$x)),ind,by.x = "nam",by.y = "nam",all.x = T,all.y = F,sort = F)
row.names(ind2)<-ind2$nam
ind2<-ind2[,-1]
res_pca_prcomp_test$x<- as.matrix(ind2) 

colnames(res_pca_prcomp_test$x)<-paste0("PC",1:ncol(res_pca_prcomp_test$x))
####################################################################################################################################

# summary(res_pca_prcomp_test)
# summary(res_pca)

pca3d(res_pca_prcomp_test,
      title=NAME[[1]],
      group = taxa_File[,30], 
      col=taxa_File[,31],
      labels.col = "white",
      #labels.col="antiquewhite3",#
      show.plane=F,
      show.ellipses=F,
      ellipse.ci = 0.95,
      show.shapes = T,
     # show.shadows = F,
      show.labels = F,
      #fancy=T,
      axes.color= "black",
      #axes.color= "white",
      #show.centroids = F,
      biplot=T,
      #show.group.labels = F, 
      radius = 4,
      #  bg = "white",
     # bg = "snow1",
     bg="antiquewhite3",
      biplot.vars=12,
      axe.titles=c(paste("PC1 ",round(res_pca$eig$`percentage of variance`[1],2)),
                   paste("PC2 ",round(res_pca$eig$`percentage of variance`[2],2)),
                   paste("PC3 ",round(res_pca$eig$`percentage of variance`[3],2))
                   )
     )

#legend3d("topright", legend = paste(c('Very low', 'Low', 'Medium', 'High')," dormancy")
legend3d("topright", legend = paste(unique(taxa_File$Dormancy_cat)," dormancy")
         , pch = 16, col = unique(taxa_File$color), cex=2, inset=c(0.01))


tax_name<-taxa[[1]]
tax_name<-sub(" ","_",tax_name)
setwd("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA")


######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
#########################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################

for(i in 1:length(taxa)){


taxa_File<-subset(csv_file,csv_file$`Species name`==taxa[[i]])
taxa_File<-taxa_File[,-c(1:5)]
taxa_File<-taxa_File[,-c(2)]

#
res_pca <- PCA(taxa_File,graph = F,quali.sup = c(30,31),scale.unit=T)
res_pca_1<-PCA(taxa_File,graph = F,ncp = 12,quali.sup = c(30,31),scale.unit=T)
res_pca_2<-PCA(taxa_File[,c(1:29)],graph = F,ncp = 12,scale.unit=T)


###################################################################################################

res_pca_prcomp<-prcomp(taxa_File[,-c(30,31)])

#################################EQUALING PCA METHODS TO USE PCA3D FUNCTION###################################
res_pca_prcomp_test<-res_pca_prcomp
#sqrt(res_pca$eig$eigenvalue)
res_pca_prcomp_test$sdev<-sqrt(res_pca$eig$eigenvalue)#Standard Deviation  Vs Standard Error

rot<-t(apply(res_pca$var$coord, 1, function(x) {x/sqrt(res_pca$eig[,1])}))

res_pca_prcomp_test$rotation<-rot[order(row.names(res_pca_prcomp$rotation)),]
colnames(res_pca_prcomp_test$rotation)<-paste0("PC",1:ncol(res_pca_prcomp_test$rotation))
res_pca_prcomp_test$center<-res_pca_prcomp_test$center #NO CAMBIA
res_pca_prcomp_test$scale<-res_pca_prcomp_test$scale   #NO CAMBIA
or_ISO<-matrix(row.names(res_pca_prcomp$x))
ind<-as.data.frame(res_pca_1$ind$coord)
ind$nam<-row.names(ind)
#ind[order(or_ISO,ind$ISO3),]
ind2<-merge(data.frame(nam=row.names(res_pca_prcomp$x)),ind,by.x = "nam",by.y = "nam",all.x = T,all.y = F,sort = F)
row.names(ind2)<-ind2$nam
ind2<-ind2[,-1]
res_pca_prcomp_test$x<- as.matrix(ind2) 

colnames(res_pca_prcomp_test$x)<-paste0("PC",1:ncol(res_pca_prcomp_test$x))
####################################################################################################################################

# summary(res_pca_prcomp_test)
# summary(res_pca)

pca3d(res_pca_prcomp_test,
      title=NAME[[i]],
      group = taxa_File[,30], 
      col=taxa_File[,31],
      labels.col = "white",
      #labels.col="antiquewhite3",#
      show.plane=F,
      show.ellipses=F,
      ellipse.ci = 0.95,
      show.shapes = T,
      # show.shadows = F,
      show.labels = F,
      #fancy=T,
      axes.color= "black",
      #axes.color= "white",
      #show.centroids = F,
      biplot=T,
      #show.group.labels = F, 
      radius = 4,
      #  bg = "white",
      # bg = "snow1",
      bg="antiquewhite3",
      biplot.vars=12,
      axe.titles=c(paste("PC1 ",round(res_pca$eig$`percentage of variance`[1],2)),
                   paste("PC2 ",round(res_pca$eig$`percentage of variance`[2],2)),
                   paste("PC3 ",round(res_pca$eig$`percentage of variance`[3],2))
      )
)

#legend3d("topright", legend = paste(c('Very low', 'Low', 'Medium', 'High')," dormancy")
legend3d("topright", legend = paste(unique(taxa_File$Dormancy_cat)," dormancy")
         , pch = 16, col = unique(taxa_File$color), cex=2, inset=c(0.01))


tax_name<-taxa[[i]]
tax_name<-sub(" ","_",tax_name)
setwd("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA")

snapshotPCA3d(paste0(taxa[[i]],"_PCA.png"))
#ftp://ftp.imagemagick.org/pub/ImageMagick/binaries/ImageMagick-6.9.5-2-Q16-x64-dll.exe 

###RUN MANUALLY
makeMoviePCA( dir="D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA", clean=T,type = "gif",movie=tax_name)#convert="C:/Program Files/ImageMagick-6.9.5-Q16/convert.exe")
#,legend=unique(res_hcpc$data.clust$clust))


di<-dimdesc(res_pca_2, axes=c(1,2,3))
write.csv(di$Dim.1$quanti,paste0("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA","/",tax_name,"_PC1.csv"))
write.csv(di$Dim.2$quanti,paste0("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA","/",tax_name,"_PC2.csv"))
write.csv(di$Dim.3$quanti,paste0("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA","/",tax_name,"_PC3.csv"))

VS<-vifstep(taxa_File[,c(1:29)],th=10)

vif_res = sort(as.character(VS@results$Variables))


rc<-rcorr(as.matrix(taxa_File[,c(1:29)]),type = "pearson")
write.csv(rc$r,paste0("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA","/",tax_name,"_correlation_pearson.csv"))
write.csv(rc$P,paste0("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA","/",tax_name,"_correlation_pearson_PVALUE.csv"))

}

######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
#########################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
######################################################################################################################################################################################################
