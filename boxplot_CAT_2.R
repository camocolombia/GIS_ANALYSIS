require(ggplot2);require(reshape2);require(raster)


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

labels<-c(
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "(%)",
  "(MASL)",
  "(°C)", #10
  "(°C)",
  "(°C)",
  "(CV)",
  "(°C)",
  "(°C)",
  "(°C)",
  "(°C)",
  "(°C)",
  "(°C)",
  "(°C)",#20
  "(°C)",
  "(mm)",
  "(mm)",
  "(CV)",
  "(mm)",
  "(mm)",
  "(mm)",
  "(mm)",
  "(Kg/m3)",
  "(cmol/Kg)", #30
  "(%)",
  "(g/Kg)",
  "(pH)",
  "(%)",
  "(%)"
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



csv_file<-csv_file[order(csv_file$Dormancy_cat== "Very low",csv_file$Dormancy_cat== "Low",csv_file$Dormancy_cat== "Medium",csv_file$Dormancy_cat== "High",decreasing = T),]
csv_file$color<-NA
csv_file$color[csv_file$Dormancy_cat=="Very low"]<-"#799244"#"#0035ff" #azul
csv_file$color[csv_file$Dormancy_cat=="Low"]<-"#c3d69b"#"#00ff19" #verde
csv_file$color[csv_file$Dormancy_cat=="Medium"]<-"#f2dcdb"#"#faff00" #amarillo
csv_file$color[csv_file$Dormancy_cat=="High"]<-"#d99694"#"#e60b00" #rojo
# 
# csv_file$color[csv_file$`Species name`=="Ambrosia dumosa"]<-"#0035ff" #azul
# csv_file$color[csv_file$`Species name`=="Atriplex canescens"]<-"#00ff19" #verde
# csv_file$color[csv_file$`Species name`=="Heterotheca villosa"]<-"#faff00" #amarillo
# csv_file$color[csv_file$`Species name`=="Heliomeris multiflora"]<-"#e60b00" #rojo


csv_file$`Species name`<-factor(csv_file$`Species name`,unique(csv_file$`Species name`))
taxa<-levels(csv_file$`Species name`)

NAME<-unlist(lapply(1:length(taxa),function(i){
  taxon3<-as.character(taxa[[i]])
  ssN<-substitute(expr=italic(taxon3),env=list(taxon3=taxon3))
  return(ssN)
}))


 

for( i in 6:35){
  
  tx<-csv_file[,c(5,36,37,i)]
  #txt<-melt(tx,"Dormancy_cat")
  
    
  taxa_sub<-tx
    
   # taxa_sub<-tx[which(tx$`Species name`==taxa[[j]]),]
    taxa_sub<-taxa_sub[order(taxa_sub$Dormancy_cat== "Very low",taxa_sub$Dormancy_cat== "Low",taxa_sub$Dormancy_cat== "Medium",taxa_sub$Dormancy_cat== "High",decreasing = T),]
    taxa_sub$Dormancy_cat<-factor(taxa_sub$Dormancy_cat,unique(taxa_sub$Dormancy_cat))
    #taxa_sub$`Species name`<-factor(taxa_sub$`Species name`,unique(taxa_sub$`Species name`))
    
    colnames(taxa_sub)<-c("Taxon","Dormancy_cat","color","value")

    #taxa_sub$color<-factor(taxa_sub$color,levels=unique(taxa_sub$color))
    
    black.bold.italic.16.text <- element_text(face = "italic", color = "black", size = 30)
    
    #colnames(csv_file)[i]
    prom<-ggplot(taxa_sub, aes(x =Taxon, y = value,fill=Dormancy_cat)) +
      geom_boxplot()+
      #  stat_boxplot(geom ='errorbar') +
      #scale_fill_manual(labels=levels(taxa_sub$Dormancy_cat),values = unique(taxa_sub$color))+
      #scale_colour_manual(labels =unique(taxa_sub$Dormancy_cat),values = unique(taxa_sub$color))+
      #c("#0035ff","#00ff19","#faff00","#e60b00")
      scale_fill_manual(name="Dormancy\n Category",labels = levels(taxa_sub$Dormancy_cat),values = c("#799244","#c3d69b","#f2dcdb","#d99694"))+
      xlab("") +
      ylab(paste(colnames(csv_file)[i],labels[[i]],sep=" "))+  
      #ggtitle(NAME) + 
      #scale_shape_discrete(name="",label=c("Very High","High","Low","Very low"))+
      #theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=42),axis.text.x  = element_text(size=42,colour="black"),axis.text.y  = element_text(size=42,colour="black"),legend.position="none")+ 
      theme(panel.background = element_rect(fill = "gray90"),text=element_text(size=36),axis.text.x  =black.bold.italic.16.text,axis.text.y  = element_text(size=30,colour="black"),legend.title=element_text(size=36,colour="black"))
    
    #ggsave(paste0(out_dir,"/","PDF","/",as.character(colnames(csv_file)[i]),"_",Sys.Date(),".pdf"),units="in",width=12.5,height=7.5,scale=2,dpi=600)
    ggsave(paste0(out_dir,"/","PDF","/",as.character(colnames(csv_file)[i]),"_",Sys.Date(),".pdf"),units="in",width=20,height=9,scale=2,dpi=600)
    
};rm(i)

