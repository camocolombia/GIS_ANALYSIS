library(ggplot2);library(reshape2);require(stringi);require(plyr):library(Hmisc)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}



corrs<-read.csv("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA/CORRELATION/PEARSON_CORRELATIONS_2016_11_07.csv",header=T,sep="|")
#corrs<-read.csv("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA/CORRELATION/PEARSON_CORRELATIONS_2016_11_07_2.csv",header=T,sep="|")
#cor_order<-as.factor(colnames(corrs))
corrs<-corrs[,-6]

corrs$id = as.numeric(rownames(corrs))

nba.m <- melt(corrs,c("id","Species"))
nba.m$variable<-stri_replace_all(nba.m$variable, " ", fixed=".")

for(i in 1:nrow(nba.m)){
  
  
  nba.m$variable[[i]]<-simpleCap(nba.m$variable[[i]])  
}


nba.m$variable[which(nba.m$variable=="PH In H2O")]<-"pH"

nba.m$variable<-factor(nba.m$variable,unique(nba.m$variable))




#nba.m <- ddply(nba.m, .(variable), transform,rescale = rescale(value))

#p <- ggplot(nba.m, aes(Species,variable,fill=value)) + 
p <- ggplot(nba.m, aes(variable,Species,fill=value)) + 
  
 geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1,1), space = "Lab", 
                      name=paste("Pearson Correlation \nfor Dormancy (%)\n","p value ","\u2264","0.05"))+ 
  theme_minimal()+ 
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, 
  # theme(axis.text.x = element_text(angle = 45, vjust = 1,
  # 
  #                                    size = 12, hjust = 1,face="italic"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,

                                     size = 16, hjust = 1),axis.text.y=element_text(angle = 0, vjust = 1,
                                                                                    
                                                                                    size = 20, hjust = 1,face="italic"),
        legend.direction = "horizontal")+#,legend.justification = c(1, 0))+
  coord_fixed()+
  labs(x = "", y = "") +
  #geom_text(aes(Species, variable, label = round(value,2)), color = "black", size = 4)
  geom_text(aes(variable,Species, label = round(value,2)), color = "black", size = 4)+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#p

ggsave(paste0("D:/CWR_OCC_VALIDATION/OUTPUT_2016_08_31/PCA/CORRELATION/","COR_HEAT_MAP","_",Sys.Date(),".png"),units="in",width=8.4,height=5.5,scale=2,dpi=600)
