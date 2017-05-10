
##########################################'
require(rgdal);require(raster)

gap_dir<-"/curie_data/ncastaneda/gap-analysis/gap_helianthusSoils"
src_dir<-paste0(gap_dir,"/","_scripts")
source(paste0(src_dir,"/","000.zipWrite.R"))
source(paste0(src_dir,"/","000.zipRead.R"))


###DEFINING FOLDERS###

sample_dir<-paste0(gap_dir,"/","samples_calculations")
priorities_tab<-read.csv(paste0(gap_dir,"/","/priorities","/","priorities.csv"),header = T,sep = ",")


taxon<-priorities_tab$TAXON


#i=1

##########################
###MAKE A EMPTY MATRIX###
##########################
matrix_sp<-matrix(nrow=length(taxon),ncol=length(taxon))
second<-matrix(nrow=length(taxon),ncol=length(taxon))
name_sp<-matrix(nrow=length(taxon),ncol=length(taxon))
#matrix_sp<-as.data.frame(matrix_sp)
colnames(matrix_sp)<-taxon
rownames(matrix_sp)<-taxon

colnames(second)<-taxon
rownames(second)<-taxon

colnames(name_sp)<-taxon
rownames(name_sp)<-taxon


##############################################
###DEFINING A MAIN LIST TO PERFORM ANALYSIS###
##############################################




models_files<-lapply(1:length(taxon),function(i){


hbuffer_file<-paste0(sample_dir,"/",taxon[i],"/","hsamples-buffer.asc.gz")
gbuffer_file<-paste0(sample_dir,"/",taxon[i],"/","gsamples-buffer.asc.gz")

hbuffer_name<-paste0(sample_dir,"/",taxon[i],"/","hsamples-buffer.asc.gz")
gbuffer_name<-paste0(sample_dir,"/",taxon[i],"/","gsamples-buffer.asc.gz")

if(file.exists(hbuffer_file)){
  hbuffer_file  <-zipRead(paste0(sample_dir,"/",taxon[i]),"hsamples-buffer.asc.gz")
  
}else{
  
  cat("Herbaria buffer is not available for ", as.character(taxon[i]),"\n")
  }

if(file.exists(gbuffer_file)){
  
  gbuffer_file<-zipRead(paste0(sample_dir,"/",taxon[i]),"gsamples-buffer.asc.gz")
  
}else{
  
  cat("Germplasm buffer is not available for ", as.character(taxon[i]),"\n")
}


if(file.exists(hbuffer_name) & file.exists(gbuffer_name)){
  cat("Germplasma and Herbaria buffers are available for ", as.character(taxon[i]),"...summarizing...","\n")
  buffer_final<-sum(hbuffer_file,gbuffer_file)
  buffer_final[which(buffer_final[]>1)]<-1
}else if(!file.exists(hbuffer_name)){
  cat("Germplasm buffer available for ", as.character(taxon[i]),"......","\n")
  
  buffer_final<-gbuffer_file
}else if(!file.exists(gbuffer_name)){
  
  cat("Herbarium buffer available for ", as.character(taxon[i]),"......","\n")
  buffer_final<-hbuffer_file
}

return(buffer_final)
})






# lapply(1:length(models_files),function(temp){
for(temp in 1:length(models_files)){

  template<-models_files[[temp]]
  template_cells<-sum(template[]==1,na.rm =T)
  
  cat("processing ", as.character(taxon[[temp]]),"\n")
  meas<-matrix(nrow=length(taxon),ncol=1)
  sec<-matrix(nrow=length(taxon),ncol=1)
  names<-matrix(nrow=length(taxon),ncol=1)
  
# secundary<-lapply(1:length(models_files),function(i){
  for(i in 1:length(models_files)){
    x<-models_files[[i]]
    xcells<-sum(x[]==1,na.rm =T)
    y=x*template
    inter_cells<-sum(y[]==1,na.rm =T)
    
    cat("processing ", as.character(taxon[[temp]]),"vs ",as.character(taxon[[i]]),"\n")
    
    Measure<-2*(inter_cells/(xcells+template_cells))
    
    if(template_cells<xcells){
      
      n<-as.character(taxon[[temp]])
      cat("minor range is ",n,"\n")
      s<-2*(template_cells/(xcells+template_cells))
      s<-Measure/s
    }else{
      s<-2*(xcells/(xcells+template_cells))
      s<-Measure/s
      n<-as.character(taxon[[temp]])
      cat("minor range is ",n,"\n")}
    
    
    cat("overlap measure is ",Measure,"\n")
    cat("second overlap measure is ",s,"\n")
#     
#     return(Measure)
#     return(s)
#     return(n)
    
    meas[[i]]<-Measure
    sec[[i]]<-s
    names[[i]]<-n
    
    # return(Measure)
  }  
 # })
  
 
  
#   return(meas)
#   return(sec)
#   return(names)

matrix_sp[,temp]<-meas
second[,temp]<-sec
name_sp[,temp]<-names
}
  
 #})

#matrix_sp
write.csv(matrix_sp,paste0(gap_dir,"/","Geographic_overlap2.csv"))
write.csv(second,paste0(gap_dir,"/","minor_Geo_overlap2.csv"))
write.csv(name_sp,paste0(gap_dir,"/","minor_names_Geo_overlap2.csv"))

