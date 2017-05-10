require(raster);require(dismo);require(corrplot);require(reshape2);require(ggplot2);require(FactoMineR)

out_dir<-"D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31"

#######
# raster_dir<-list.files("D:/CWR_OCC_VALIDATION/NORTHAMERICA_RASTER_DATA/BIOCLIM",pattern=".asc$",full.names=T)
# raster_dir_name<-list.files("D:/CWR_OCC_VALIDATION/NORTHAMERICA_RASTER_DATA/BIOCLIM",pattern=".asc$",full.names=F)
# raster_dir<-lapply(raster_dir,raster)
# raster_dir<-stack(raster_dir)
# #######
# csv_file<-read.csv("D:/CWR_OCC_VALIDATION/LatLongDorm_comparison_2016_08_31.csv",sep="|",header=T)
# csv_file<-csv_file[complete.cases(csv_file),]
# csv_coord<-as.data.frame(cbind(csv_file$Long,csv_file$Lat))
# colnames(csv_coord)<-c("Long","Lat")
# coordinates(csv_coord)<-~Long+Lat
# values<-extract(raster_dir,cbind(csv_file$Long,csv_file$Lat))
# 
# 
# csv_file<-cbind(csv_file,values)
#write.table(csv_file,"D:/CWR_OCC_VALIDATION/LatLongDorm_comparison_2016_08_31_VALUES.csv",sep="|",row.names = F)
#taxa<-unique(csv_file$Taxon)
#csv_file<-read.csv("D:/CWR_OCC_VALIDATION/LatLongDorm_comparison_2016_08_31_VALUES.csv",sep="|",header=T)
csv_file<-read.csv("D:/CWR_OCC_VALIDATION/comparison_2016_10_26_1_VALUES.CSV",sep="|",header=T)

#
#taxa<-unique(csv_file$Taxon.comb)
taxa<-unique(csv_file$Taxon_comb)

colnames(csv_file)<-c("Inventory.Number",
                      "Inventory.Suffix",
                      "Accession",
                      "Taxon name (Trinomial Nomenclature)",
                      "Species name",
                      "Latitude",
                      "Longitude",
                      "percent dormancy",
                      "altitude",
                      "mean annual temperature",
                      "mean diurnal range",
                      "Isothermality",
                      "Temperature seasonality",
                      "Max temp warmest month",
                      "Min temp coldest month",
                      "Temperature annual range",
                      "Mean temperature wettest quarter",
                      "Mean temp of driest quarter",
                      "Mean temp warmest quarter",
                      "Mean temp coldest quarter",
                      "Annual precipitation",
                      "Precipitation in wettest month",
                      "Precipitation in driest month",
                      "Precipitation Seasonality",
                      "Precipitation in wettest quarter",
                      "Precipitation in driest quarter",
                      "Precipitation in warmest quarter",
                      "Precipitation in coldest quarter",
                      "Bulk density",
                      "Cation Exchange Capacity",
                      "Percent Clay",
                      "Organic Carbon",
                      "pH in H2O ",
                      "Percent Silt",
                      "Percent Sand"
)

# 
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
  
cat<-unique(csv_file$Dormancy_cat)
csv_file2<-csv_file
for(z in 1:length(cat)){
  csv_file<-csv_file2
  csv_file<-csv_file[which(csv_file$Dormancy_cat==cat[[z]]),]
  
  if(nrow(csv_file)==0){
    
    cat("ommiting...", cat[[z]],"\n")
  }else{
    cat("processing ", cat[[z]],"\n")
    
  
pe<-list()
pe_p<-list()
sp<-list()
sp_p<-list()
  for(i in 1:length(taxa)){
    
    taxon<-taxa[[i]]
    cat("processing ", cat[[z]]," and ",taxa[[i]] ,"\n")
    
    taxon_subset<-csv_file[which(csv_file$`Species name`==taxa[[i]]),]
    if(nrow(taxon_subset)>=0 & nrow(taxon_subset)<5){
      cat("ommiting...", cat[[z]]," and ",taxa[[i]],"\n")
      
      
    }else{
    taxon_subset<-taxon_subset[,6:35]  
    
    st<-matrix(ncol=1,nrow=ncol(taxon_subset));row.names(st)<-colnames(taxon_subset);colnames(st)<-taxa[[i]]
    n_records<-nrow(taxon_subset);st<-rbind(st,n_records)
    st2<-matrix(ncol=1,nrow=ncol(taxon_subset));row.names(st2)<-colnames(taxon_subset);colnames(st2)<-taxa[[i]]
    n_records<-nrow(taxon_subset);st2<-rbind(st2,n_records)
    sst<-matrix(ncol=1,nrow=ncol(taxon_subset));row.names(sst)<-colnames(taxon_subset);colnames(sst)<-taxa[[i]]
    n_records<-nrow(taxon_subset);sst<-rbind(sst,n_records)
    sst2<-matrix(ncol=1,nrow=ncol(taxon_subset));row.names(sst2)<-colnames(taxon_subset);colnames(sst2)<-taxa[[i]]
    n_records<-nrow(taxon_subset);sst2<-rbind(sst2,n_records)
    
    
    for(j in 1:ncol(taxon_subset)){
      cat("processing ", cat[[z]]," and ",taxa[[i]]," and ","column: ",j ,"\n")
      pe2<-cor.test(taxon_subset$`percent dormancy`,taxon_subset[,j],method = "pearson")
      sp2<-cor.test(taxon_subset$`percent dormancy`,taxon_subset[,j],method = "spearman")
      
      st[j,]<-pe2$estimate
      st2[j,]<-pe2$p.value
      
      sst[j,]<-sp2$estimate
      sst2[j,]<-sp2$p.value
      
      
    };rm(j)
    
    pe[[i]] <- st
    pe_p[[i]] <- st2
    sp[[i]] <- sst
    sp_p[[i]] <- sst2
   
    #rm(pe2,sp2,st,st2,sst,sst2)
  }
 ###################### 
  
###################
    };rm(i)
  
  pearson<-as.data.frame(do.call(cbind,pe));pearson<-pearson[-3,]
  if(is.null(ncol(pearson))){
    pearson<-as.data.frame(pearson)
    colnames(pearson)<-taxon
    row.names(pearson)<-c("Latitude",
                          "Longitude",
                          "altitude",
                          "mean annual temperature",
                          "mean diurnal range",
                          "Isothermality",
                          "Temperature seasonality",
                          "Max temp warmest month",
                          "Min temp coldest month",
                          "Temperature annual range",
                          "Mean temperature wettest quarter",
                          "Mean temp of driest quarter",
                          "Mean temp warmest quarter",
                          "Mean temp coldest quarter",
                          "Annual precipitation",
                          "Precipitation in wettest month",
                          "Precipitation in driest month",
                          "Precipitation Seasonality",
                          "Precipitation in wettest quarter",
                          "Precipitation in driest quarter",
                          "Precipitation in warmest quarter",
                          "Precipitation in coldest quarter",
                          "Bulk density",
                          "Cation Exchange Capacity",
                          "Percent Clay",
                          "Organic Carbon",
                          "pH in H2O",
                          "Percent Silt",
                          "Percent Sand",
                          "n_records"
    )
  }
 pearson_pvalues<-as.data.frame(do.call(cbind,pe_p));pearson_pvalues<-pearson_pvalues[-3,]
 if(is.null(ncol(pearson_pvalues))){
   pearson_pvalues<-as.data.frame(pearson_pvalues) 
   colnames(pearson_pvalues)<-taxon
   row.names(pearson_pvalues)<-c("Latitude",
                                   "Longitude",
                                   "altitude",
                                   "mean annual temperature",
                                   "mean diurnal range",
                                   "Isothermality",
                                   "Temperature seasonality",
                                   "Max temp warmest month",
                                   "Min temp coldest month",
                                   "Temperature annual range",
                                   "Mean temperature wettest quarter",
                                   "Mean temp of driest quarter",
                                   "Mean temp warmest quarter",
                                   "Mean temp coldest quarter",
                                   "Annual precipitation",
                                   "Precipitation in wettest month",
                                   "Precipitation in driest month",
                                   "Precipitation Seasonality",
                                   "Precipitation in wettest quarter",
                                   "Precipitation in driest quarter",
                                   "Precipitation in warmest quarter",
                                   "Precipitation in coldest quarter",
                                   "Bulk density",
                                   "Cation Exchange Capacity",
                                   "Percent Clay",
                                   "Organic Carbon",
                                   "pH in H2O",
                                   "Percent Silt",
                                   "Percent Sand",
                                   "n_records"
   ) }
  spearman<-as.data.frame(do.call(cbind,sp));spearman<-spearman[-3,]
  if(is.null(ncol(spearman))){
    spearman<-as.data.frame(spearman)
    colnames(spearman)<-taxon
    row.names(spearman)<-c("Latitude",
                           "Longitude",
                           "altitude",
                           "mean annual temperature",
                           "mean diurnal range",
                           "Isothermality",
                           "Temperature seasonality",
                           "Max temp warmest month",
                           "Min temp coldest month",
                           "Temperature annual range",
                           "Mean temperature wettest quarter",
                           "Mean temp of driest quarter",
                           "Mean temp warmest quarter",
                           "Mean temp coldest quarter",
                           "Annual precipitation",
                           "Precipitation in wettest month",
                           "Precipitation in driest month",
                           "Precipitation Seasonality",
                           "Precipitation in wettest quarter",
                           "Precipitation in driest quarter",
                           "Precipitation in warmest quarter",
                           "Precipitation in coldest quarter",
                           "Bulk density",
                           "Cation Exchange Capacity",
                           "Percent Clay",
                           "Organic Carbon",
                           "pH in H2O",
                           "Percent Silt",
                           "Percent Sand",
                           "n_records"
    )
  }
   spearman_pvalues<-as.data.frame(do.call(cbind,sp_p));spearman_pvalues<-spearman_pvalues[-3,]
   if(is.null(ncol(spearman_pvalues))){
     spearman_pvalues<-as.data.frame(spearman_pvalues)
     colnames(spearman_pvalues)<-taxon
     row.names(spearman_pvalues)<-c("Latitude",
                                    "Longitude",
                                    "altitude",
                                    "mean annual temperature",
                                    "mean diurnal range",
                                    "Isothermality",
                                    "Temperature seasonality",
                                    "Max temp warmest month",
                                    "Min temp coldest month",
                                    "Temperature annual range",
                                    "Mean temperature wettest quarter",
                                    "Mean temp of driest quarter",
                                    "Mean temp warmest quarter",
                                    "Mean temp coldest quarter",
                                    "Annual precipitation",
                                    "Precipitation in wettest month",
                                    "Precipitation in driest month",
                                    "Precipitation Seasonality",
                                    "Precipitation in wettest quarter",
                                    "Precipitation in driest quarter",
                                    "Precipitation in warmest quarter",
                                    "Precipitation in coldest quarter",
                                    "Bulk density",
                                    "Cation Exchange Capacity",
                                    "Percent Clay",
                                    "Organic Carbon",
                                    "pH in H2O",
                                    "Percent Silt",
                                    "Percent Sand",
                                    "n_records"
     )
   }
   
  write.table(pearson,paste0(out_dir,"/","PEARSON_",cat[[z]],"_",Sys.Date(),".csv"),sep="|",row.names = T)
  write.table(pearson_pvalues,paste0(out_dir,"/","PEARSON_PVALUES_",cat[[z]],"_",Sys.Date(),".csv"),sep="|",row.names = T)
  
  write.table(spearman,paste0(out_dir,"/","SPEARMAN_",cat[[z]],"_",Sys.Date(),".csv"),sep="|",row.names = T)
  write.table(spearman_pvalues,paste0(out_dir,"/","SPEARMAN_PVALUES_",cat[[z]],"_",Sys.Date(),".csv"),sep="|",row.names = T)
    }
};rm(z)

# ss<-PCA(csv_file[,-c(1:5)])
# ss2<-HCPC(ss,nb.clust = -1)
# ss2$data.clust$clust
# csv_file2<-csv_file
# csv_file2$HCPC<-ss2$data.clust$clust
# 
# write.table(csv_file2,paste0(out_dir,"/","HCPC",Sys.Date(),".csv"),sep=",",row.names = F)

#   rnam<-row.names(cormat)
#   
#   get_upper_tri <- function(cormat){
#     cormat[lower.tri(cormat)]<- NA
#     return(cormat)
#   }
# 
#   # upper_tri <- get_upper_tri(cormat)
#   # upper_tri
#   # 
#   # melted_cormat <- melt(upper_tri,na.rm=T)
#   # 
#   # reorder_cormat <- function(cormat){
#   #   # Use correlation between variables as distance
#   #   dd <- as.dist((1-cormat)/2)
#   #   hc <- hclust(dd)
#   #   cormat <-cormat[hc$order, hc$order]
#   # }
# 
#   cormat2<-cbind(
#     cormat[, "mean annual temperature"],
#     cormat[, "mean diurnal range"],
#     cormat[, "Isothermality"],
#     cormat[, "Temperature seasonality"],
#     cormat[, "Max temp warmest month"],
#     cormat[, "Min temp coldest month"],
#     cormat[, "Temperature annual range"],
#     cormat[, "Mean temperature wettest quarter"],
#     cormat[, "Mean temp of driest quarter"],
#     cormat[, "Mean temp warmest quarter"],
#     cormat[, "Mean temp coldest quarter"],
#     cormat[, "Annual precipitation"],
#     cormat[, "Precipitation in wettest month"],
#     cormat[, "Precipitation in driest month"],
#     cormat[, "Precipitation Seasonality"],
#     cormat[, "Precipitation in wettest quarter"],
#     cormat[, "Precipitation in driest quarter"],
#     cormat[, "Precipitation in warmest quarter"],
#     cormat[, "Precipitation in coldest quarter"],
#     cormat[, "Bulk density"],
#     cormat[, "Cation Exchange Capacity"],
#     cormat[, "Percent Clay"],
#     cormat[, "Organic Carbon"],
#     cormat[, "pH in H2O "],
#     cormat[, "Percent Silt"],
#     cormat[, "Percent Sand"],
#     cormat[, "Latitude"],
#     cormat[, "Longitude"],
#     cormat[, "altitude"],
#     cormat[, "percent dormancy"]
# )
#   cormat<-as.data.frame(cormat2);rm(cormat2)
# colnames(cormat)<-c("mean annual temperature",
#                     "mean diurnal range",
#                     "Isothermality",
#                     "Temperature seasonality",
#                     "Max temp warmest month",
#                     "Min temp coldest month",
#                     "Temperature annual range",
#                     "Mean temperature wettest quarter",
#                     "Mean temp of driest quarter",
#                     "Mean temp warmest quarter",
#                     "Mean temp coldest quarter",
#                     "Annual precipitation",
#                     "Precipitation in wettest month",
#                     "Precipitation in driest month",
#                     "Precipitation Seasonality",
#                     "Precipitation in wettest quarter",
#                     "Precipitation in driest quarter",
#                     "Precipitation in warmest quarter",
#                     "Precipitation in coldest quarter",
#                     "Bulk density",
#                     "Cation Exchange Capacity",
#                     "Percent Clay",
#                     "Organic Carbon",
#                     "pH in H2O ",
#                     "Percent Silt",
#                     "Percent Sand",
#                     "Latitude",
#                     "Longitude",
#                     "altitude",
#                     "percent dormancy")
# 
# row.names(cormat)<-rnam
#   # Reorder the correlation matrix
#   # cormat <- reorder_cormat(cormat)
#   
# cormat[which(cormat$`percent dormancy`>=0.5 & cormat$`percent dormancy`<= -0.5)]
#   upper_tri <- get_upper_tri(cormat)
#   #upper_tri<-upper_tri[order(upper_tri[,30]),]
#   
#   # Melt the correlation matrix
#   melted_cormat <- melt(upper_tri, na.rm = TRUE)
#   # Create a ggheatmap
#   
#   
#  ######################################## 
#   
#   cat("Testing ranking such as:  subsp. ,var., f. for ", as.character(taxon3),"/n")
#   
#   taxon<-as.character(taxa[[1]])
#   split_text <- unlist(strsplit(taxon, split=' '))
#   cond <- c('subsp.', 'var.', 'f.')
#   
#   selected_text <- split_text[split_text %in% cond]
#   
#   
#   cat("Savig file for ", as.character(taxon),"/n")
#   
#   
#   if(length(selected_text)==0){
#     
#     NAME<-substitute(expr=italic(taxon),env=list(taxon=taxon))
#   }else{
#     a<-split_text[[1]]
#     b<-split_text[[2]]
#     c<-split_text[[4]]
#     
#     NAME<-substitute(expr=paste(italic(a)," ",italic(b)," ",selected_text," ",italic(c)),
#                      env=list(a=a,b=b,selected_text=selected_text,c=c))
#   }
#   
#   
#   
#   
#   
#   
#   
#   ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
#     geom_tile(color = "white")+
#     scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                          midpoint = 0, limit = c(-1,1), space = "Lab", 
#                          name="Spearman\n Correlation") +
#     theme_minimal()+ # minimal theme
#     theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                      size = 12, hjust = 1))+
#     coord_fixed()
#   # Print the heatmap
#  # print(ggheatmap)
#   
#   
#   
#   
#   ggheatmap<-ggheatmap + 
#     geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
#     ggtitle(NAME)+
#     theme(
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.border = element_blank(),
#       panel.background = element_blank(),
#     axis.ticks = element_blank())
#   
#   
#   
#   ggsave(paste0(out_dir,"/","PDF","/",taxon,"_",Sys.Date(),".pdf"),units="in",width=8,height=8,scale=2,dpi=600)
#   
# #+
#   
#   
#     # theme(
#     #   axis.title.x = element_blank(),
#     #   axis.title.y = element_blank(),
#     #   panel.grid.major = element_blank(),
#     #   panel.border = element_blank(),
#     #   panel.background = element_blank(),
#       #axis.ticks = element_blank())+
#       #legend.justification = c(1, 0),
#       # legend.position = c(0.6, 0.7),
#       # legend.direction = "horizontal")+
#   #  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,title.position = "top", title.hjust = 0.5)
#    #        )
# #corrplot(cor(taxon_subset,taxon_subset,method="spearman"))
# 
# 
# # for(i in 1:length(taxa))
# #   {
# #   taxon_subset<-csv_file[which(csv_file$Taxon==taxa[[1]]),]
# #   taxon_subset<-taxon_subset[,5:34]
# #   if(nrow(taxon_subset)>=10){
# #     
# #     p_values_m<-matrix(nrow=ncol(taxon_subset),ncol=ncol(taxon_subset))
# #     colnames(p_values_m)<-colnames(taxon_subset)
# #     estimates_m<-matrix(nrow=ncol(taxon_subset),ncol=ncol(taxon_subset))
# #     colnames(estimates_m)<-colnames(taxon_subset)
# #     statistics_m<-matrix(nrow=ncol(taxon_subset),ncol=ncol(taxon_subset))
# #     colnames(statistics_m)<-colnames(taxon_subset)
# #     df_m<-matrix(nrow=ncol(taxon_subset),ncol=ncol(taxon_subset))
# #     colnames(df_m)<-colnames(taxon_subset)
# #     
# #     for(j in 1:ncol(taxon_subset)){
# #       for(k in 1:ncol(taxon_subset)){
# #       x<-cor.test(taxon_subset[,j],taxon_subset[,k],method="spearman")     
# #       estimates_m[j,k]<-x$p.value
# #       p_values_m[j,k]<-x$estimate[[1]]
# #       statistics_m[j,k]<-x$statistic
# #       df_m[j,k]<-x$df
# #   };rm(j)
# #     
# #   };rm(k)
# #    
# # }
# 
#tapply(csv_file$`Species name`,csv_file$`Species name`,length)


