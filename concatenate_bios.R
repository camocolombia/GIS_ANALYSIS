name<-"Ceroxylon_quindiuense"
ou<-"D:/Tesis_palma/step"

#inDir<-paste0(ou,"/",name)
inDir<-ou

# s<-list.dirs(inDir,recursive=F)
# snm<-list.dirs(inDir,recursive=F,full.names=F)



#out_dir_csv<-paste0(ou,"/",name)
out_dir_csv<-ou


oCsv_labels <- list.files(out_dir_csv, pattern=".csv$")
oCsv_labels <- gsub(pattern=".csv",replacement="", oCsv_labels)
#oCsv_labels <- gsub(pattern=paste0(name,"_",replacement="", oCsv_labels))

oCsv<-list.files(out_dir_csv,full.names=T,pattern=".csv$")
oCsv<-lapply(oCsv,read.csv)





names(oCsv) <- oCsv_labels

oCsv <- lapply(oCsv, function(x){ x <- x[x[,ncol(x)] < 0.05,]; return(x) })
oCsv <- lapply(oCsv, function(x){ x <- x[,1:2]; names(x) <- c("Variable", "Estimate"); return(x) })
for(i in 1:length(oCsv)){
  oCsv[[i]]$GCM_id <- paste(names(oCsv)[i])
}; rm(i)

oCsv_final = Reduce(function(...) merge(..., all=T), oCsv)
oCsv_final = oCsv_final[order(oCsv_final$GCM_id),]
rownames(oCsv_final) = 1:nrow(oCsv_final)



oCsv_final <- oCsv_final[oCsv_final$Variable!="(Intercept)",]
oCsv_current<- oCsv_final[oCsv_final$GCM_id=="step_current",]
oCsv_current$Variable <- as.character(oCsv_current$Variable)
oCsv_final <- oCsv_final[oCsv_final$GCM_id!="step_current",]
oCsv_final$Variable <- as.character(oCsv_final$Variable)

smed<- with(oCsv_final, reorder(oCsv_final$Variable, oCsv_final$Estimate, median))

boxplot(oCsv_final$Estimate ~ smed, data = oCsv_final,border=NULL,outline=F, pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
        xlab = "Variables", ylab = "Estimate values",
        main = "Stepwise regression", varwidth = TRUE,
        col = "lightgray")

abline(0,0)


cmed<- with(oCsv_current, reorder(oCsv_current$Variable, oCsv_current$Estimate, median))

# boxplot(oCsv_current$Estimate ~ cmed, data = oCsv_current,border=NULL,outline=F, pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
#         xlab = "Variables", ylab = "Estimate values",
#         main = "Stepwise regression", varwidth = TRUE,
#         col = "lightgray")
# 
# abline(0,0)

#boxplot(oCsv_final$Estimate ~ oCsv_final$Variable))






