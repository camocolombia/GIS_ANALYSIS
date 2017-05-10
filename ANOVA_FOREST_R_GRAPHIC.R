MORAN_ANOVA<-function(data_table,field_D,labels,lab_sup,out_dir){
  require(ggplot2); require(agricolae);require(dendextend)
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")  
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("@@@@@@@","procesing",field_D,"@@@@@@@","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")

#  i=1
#   data_table=datos
#   field_D=vectors[[i]]
#   labels=labs[[i]]
#   out_dir=out_dir
  
data_table<-as.data.frame(read.csv(data_table,sep="|"))
data_table<-data_table[!duplicated(data_table$DANE), ]

#field_D<-vectors[[i]]
s<-data_table[which(data_table[field_D] !=0),]
s<-s[which(s[field_D] !=5),]

#vectors[[i]]
s<-s[order(s[field_D], na.last = NA),]

s$CAT[s[field_D]==1]<-"High - High"
s$CAT[s[field_D]==2]<-"Low - Low"
s$CAT[s[field_D]==3]<-"Low - High"
s$CAT[s[field_D]==4]<-"High - Low"

s$ACCI_CAT[s[field_D]==1]<-"High"
s$ACCI_CAT[s[field_D]==3]<-"High"
s$ACCI_CAT[s[field_D]==2]<-"Low"
s$ACCI_CAT[s[field_D]==4]<-"Low"


##############################################################################################
###########GRAPHICS         ##################################################################
##############################################################################################
ggplot(s, aes(x =CAT, y = COVER_LOST)) +
  geom_boxplot(aes(fill=CAT),outlier.size=NA)+
  stat_summary(fun.y=mean, geom="line", aes(group=1))+
  stat_summary(fun.y=mean, geom="point")+
  #"High - High","High - Low","Low - High","Low - Low"
  #"#4C7300","#ff0000","#005CE6","#aaff00"
  #scale_fill_manual(labels = c("High - High","Low - Low","Low - High","High - Low"),values = c("#4C7300","#aaff00","#005CE6","#ff0000"))+
  scale_fill_manual(labels = c("High - High","High - Low","Low - High","Low - Low"),values = c("#4C7300","#ff0000","#005CE6","#aaff00"))+
  #geom_point(size=13)+
  ylab("Forest lost rate (%) 2000-2010") + 
  xlab(labs[[i]]) +
  ylim(0,100)+
  theme(plot.title = element_text(size=30),panel.background = element_rect(fill = "gray95"),text=element_text(size=38),axis.text.x  = element_text(size=30,colour="black"),axis.text.y  = element_text(size=28,colour="black"),legend.position="none")
#ggsave(paste0(out_dir,"/","Gr_",vectors[[i]],"_",Sys.Date(),".tiff"),units="mm",width=238.125,height=153.19375,scale=2)
ggsave(paste0(out_dir,"/","Gr_",field_D,"_",Sys.Date(),".png"),units="mm",width=238.125,height=153.19375,scale=2)


##############################################################################################
##############################################################################################
ggplot(s, aes(x =ACCI_CAT, y = COVER_LOST)) +
  geom_boxplot(aes(fill=ACCI_CAT),outlier.size=NA)+
  stat_summary(fun.y=mean, geom="line", aes(group=1))+
  stat_summary(fun.y=mean, geom="point")+
  #"High - High","High - Low","Low - High","Low - Low"
  #"#4C7300","#ff0000","#005CE6","#aaff00"
  #scale_fill_manual(labels = c("High - High","Low - Low","Low - High","High - Low"),values = c("#4C7300","#aaff00","#005CE6","#ff0000"))+
  scale_fill_manual(labels = c("High","Low"),values = c("#ff0000","#4C7300"))+
  #geom_point(size=13)+
  ylab("Forest lost rate (%) 2000-2010") + 
  xlab(lab_sup[[i]]) +
  ylim(0,100)+
  theme(plot.title = element_text(size=38),panel.background = element_rect(fill = "gray95"),text=element_text(size=38),axis.text.x  = element_text(size=30,colour="black"),axis.text.y  = element_text(size=30,colour="black"),legend.position="none")

ggsave(paste0(out_dir,"/","SCAT_",field_D,"_",Sys.Date(),".png"),units="mm",width=238.125,height=153.19375,scale=2)
##############################################################################################

t2<-matrix(nrow=nrow(s),ncol=8)
t2<-as.data.frame(t2)

t2[,1]<-as.character(s$OBJECTID)
t2[,2]<-as.character(s$DANE)
t2[,3]<-as.character(s$DEPARTAMEN)
t2[,4]<-as.character(s$NOMBRE_ENT)
t2[,5]<-as.character(s$CAT)
t2[,6]<-s$ACCI_CAT
t2[,7]<-as.numeric(s$COVER_LOST)
t2[,8]<-sqrt(t2[,7])

colnames(t2)<-c("OBJECTID","DANE","DEPTO","NOMBRE_ENT","CAT_MORAN","SUPER_CAT","COVER_LOST","SQRT_LOST")
t2<-t2[complete.cases(t2),]


cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
# st<-shapiro.test(t2[,8])
# print(st)
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("Using Kruskal Wallis test","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
#K<-kruskal.test(t2$CAT_MORAN,t2$COVER_LOST,data=t2)
#pK<-posthoc.kruskal.nemenyi.test(x=t2$COVER_LOST,g=as.factor(t2$CAT_MORAN),data=t2,method="Chisq")

K<-kruskal(y=t2$COVER_LOST,trt=t2$CAT_MORAN,alpha = 0.05,p.adj="bonferroni",console = F,group = F)


cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@Perform summary table test@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")




md<-as.data.frame(matrix(nrow=2,ncol=7))
md[1,1]<-median(t2$COVER_LOST[t2$SUPER_CAT=="High"])
md[1,2]<-mean(t2$COVER_LOST[t2$SUPER_CAT=="High"])                                 
md[1,3]<-sd(t2$COVER_LOST[t2$SUPER_CAT=="High"])   
md[1,4]<-min(t2$COVER_LOST[t2$SUPER_CAT=="High"])
md[1,5]<-max(t2$COVER_LOST[t2$SUPER_CAT=="High"])
md[1,6]<-length(t2$COVER_LOST[t2$SUPER_CAT=="High"])
md[1,7]<-"High"

md[2,1]<-median(t2$COVER_LOST[t2$SUPER_CAT=="Low"])
md[2,2]<-mean(t2$COVER_LOST[t2$SUPER_CAT=="Low"])                                 
md[2,3]<-sd(t2$COVER_LOST[t2$SUPER_CAT=="Low"])   
md[2,4]<-min(t2$COVER_LOST[t2$SUPER_CAT=="Low"])
md[2,5]<-max(t2$COVER_LOST[t2$SUPER_CAT=="Low"])
md[2,6]<-length(t2$COVER_LOST[t2$SUPER_CAT=="Low"])
md[2,7]<-"Low"
colnames(md)<-c("median","mean","sd","min","max","length","group")


cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@Perform Wilcoxon test @@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")

wt<-wilcox.test(t2$COVER_LOST[t2$SUPER_CAT=="High"],t2$COVER_LOST[t2$SUPER_CAT=="Low"],conf.level = 0.95,conf.int=T,paired=F)
wt2<-as.data.frame(cbind(as.numeric(wt$statistic),wt$p.value,wt$method,wt$estimate,wt$conf.int[1],wt$conf.int[2]))
colnames(wt2)<-c("Statistic(W)","pvalue","method","diff","conf_int_1","conf_int_2")

cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@Perform clustering approach@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")

di<-as.matrix(dist(scale(K$means$t2.COVER_LOST),"euclidean"))
row.names(di)<-row.names(K$means)
colnames(di)<-row.names(K$means)
hdi<-hclust(as.dist(di),method ="ward.D")
dend <- as.dendrogram(hdi)

png(paste0(out_dir,"/","Cluster_",field_D,".png"),res=300,pointsize=10,width=3200,height=1300,units="px")
plot_horiz.dendrogram(dend,type="rectangle",side=F,center=T,text_pos=3,xlab="Mean Forest lost (%) 2000 - 2010 dendogram",nodePar = list(cex=0.07),dLeaf= 0.05)
dev.off()

cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@Saving results!            @@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")

write.csv(t2,paste0(out_dir,"/","DATA_",field_D,".csv"),row.names = F,quote=F)
write.csv(K$means,paste0(out_dir,"/","Kruskal_means",field_D,".csv"),row.names=T,quote=F)
write.csv(K$comparison,paste0(out_dir,"/","Kruskal_comparison",vectors[[i]],".csv"),row.names=T,quote=F)
write.csv(cbind(K$statistics,K$parameters),paste0(out_dir,"/","Kruskal_global_values",vectors[[i]],".csv"),row.names=T,quote=F)
write.csv(md,paste0(out_dir,"/","groups_summary_",field_D,".csv"),row.names=F,quote=F)
write.csv(wt2,paste0(out_dir,"/","Wilcox_",field_D,".csv"),row.names=F,quote=F)


# 
# pval<-as.data.frame(matrix(nrow = 6,ncol = 3))
# colnames(pval)<-c("categories","pvalue","Differences")
# 
# pval$categories<-row.names(K$comparison)
# pval$pvalue<-as.numeric(K$comparison$pvalue)
# pval$Differences<-as.numeric(K$comparison$Difference)

# 
# ggplot(pval,aes(x=categories,y=Differences,fill=categories))+
#   geom_bar(stat = "identity")+
#   coord_flip()+
#   geom_text(aes(label=pvalue), hjust=2, color="black", size=4)+
#   # ggtitle("Mean differences among comparison groups")+
#   xlab(paste0(labs[[i]],"s")) +
#   ylab("Mean differences between clusters")+
#   #ylim(0,1)+
#   theme(plot.title = element_text(size=38),panel.background = element_rect(fill = "gray95"),text=element_text(size=38),axis.text.x  = element_text(size=30,colour="black"),axis.text.y  = element_text(size=30,colour="black"),legend.position="none")
# ggsave(paste0(out_dir,"/","mean_differences_",vectors[[i]],"_",Sys.Date(),".png"),units="mm",width=238.125,height=153.19375,scale=2)





#tapply(t2$COVER_LOST,t2$CAT_MORAN,FUN = sum)

cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@          DONE!            @@@@@@@@@@@@@@@@@@@@@","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")

}

##############################################################################################
##############################################################################################


vectors<-c("CL_CO2_A","CL_CO2_AF","CL_CO2_C")
labs<-c("Carbon - Armed actions","Carbon - Victims","Carbon - Hectares of coca")
lab_sup<-c("Armed actions","Victims","Hectares of coca")
out_dir<-"D:/Dropbox/Dropbox/MORAN_CORRECTION_2015_10_26/results_anova2"
datos<-"D:/Dropbox/Dropbox/MORAN_CORRECTION_2015_10_26/BASE.csv"


##############################################################################################

for(i in 1:length(vectors)){
x<-MORAN_ANOVA(data_table=datos,field_D=vectors[[i]],labels=labs[[i]],lab_sup=lab_sup,out_dir=out_dir)
};rm(i)
