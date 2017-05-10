# #######################################################################
# ##GGPLOT SCATTERPLOT##CCSA 2015
# #SAVE MANUALLY ACCORDLY YOUR REQUIREMENTS!!!
# #######################################################################
# 
MORAN_PLOT_OUT<-function(input_dir,out_dir,tag,xlab,ylab){
  require(ggplot2)
  cat("reading ",tag," csv file","\n")
  mat2<-read.csv(paste0(input_dir,"/",tag,".csv"),header=T,encoding ="UTF-8")
  cormat<-cbind(mat2[paste0(paste0("SD","_",tag))],mat2[paste0(paste0("LAG","_",tag))])
  cormat2<-cormat
  #cormat2<-as.data.frame(cormat2)
  colnames(cormat2)<-c(xlab,ylab)
  cormat2[,3]<-mat2$NOMBRE_ENT
  colnames(cormat2)<-c("x","y","Municipalities")
  l<-lm(cormat[,2]~ cormat[,1])
  
  cat("plotting ",tag,"\n")
  
  p<-ggplot(cormat2, aes(x=x, y=y, label=Municipalities),xlim=c(-2,4),ylim=c(-1,5))
  p<-p+ geom_rect(xmin=3,xmax=5,ymin=-2,ymax=4, alpha=1,fill="lightblue")+
    geom_abline(intersect=l$coefficients[1],slope=l$coefficients[2],colour="red")+
    geom_hline(yintercept=0,0,linetype="dotted")+
    geom_vline(xintercept =0, 0,linetype="dotted")+
    geom_text(size=2.8)+
    xlab(xlab) +
    ylab(ylab) + 
    ggtitle(paste0("Moran`s I: "," ",round(as.numeric(l$coefficients[2]),3)))+
    theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=38),axis.text.x  = element_text(size=38,colour="black"),axis.text.y  = element_text(size=38,colour="black"))
  
  cat("saving ",tag," Moran graphic","\n")
  
  ggsave(paste0(out_dir,"/",ylab,"_outliers",".tiff"),p,dpi=600,width =67.34,height=34.395,units = "cm",scale=1.2)
  cat("DONE!","\n")
  
}

xlab<-"Carbon"
ylabs<-c("Armed actions","Victims","Hectares of coca")
input_dir<-"D:/Dropbox/Dropbox/MORAN_CORRECTION_2015_10_26/CSV"
out_dir<-"D:/Dropbox/Dropbox/MORAN_CORRECTION_2015_10_26/graphics/CO2/"
tags<-c("CO2_A","CO2_AF","CO2_C")


x<-lapply(1:length(tags),function(i){
  
  x<-MORAN_PLOT_OUT(input_dir = input_dir,out_dir = out_dir,tag=tags[[i]],xlab = xlab,ylab=ylabs[[i]])
  
  
})


# xlab<-"Forest lost (%) 2000 - 2010"
# tags<-c("CL_A","CL_AF","CL_C")
# out_dir<-"D:/Dropbox/Dropbox/MORAN_CORRECTION_2015_10_26/graphics/CL/"
# input_dir<-"D:/Dropbox/Dropbox/MORAN_CORRECTION_2015_10_26/CSV"
# ylabs<-c("Armed actions","Victims","Hectares of coca")
# 
# 
# x<-lapply(1:length(tags),function(i){
#   
#   x<-MORAN_PLOT_OUT(input_dir = input_dir,out_dir = out_dir,tag=tags[[i]],xlab = xlab,ylab=ylabs[[i]])
#   
#   
# })