require(raster)
require(rgdal)
require(dismo)
require(usdm)

#@@@@@@@@@@@@ VIF analysis @@@@@@@@@@@@@@#
zone<-read.csv("D:/Backup_JG/Bounderlands/Mask/Coffee_Points/Narino/Presence_c_arabica_swd.csv")
zone=as.data.frame(zone)
vif.res <- vif(x=zone[,4:22])
vif.step <- vifstep(x=zone[,4:22], th=10)
result<-c("bio_2","bio_3","bio_8","bio_9","bio_15","bio_19","alt_para_30","bio_1","bio_12")

#@@@@@@@@@@@@ Analysis Current @@@@@@@@@@@@@@#
mc<-"E:/Global_30s_bats_project_09_03_2015/output/MaxEnt_results/Artibeus fimbriatus/Artibeus_fimbriatus_currrent_th.tif"
#mc<-raster(mc)
#current=list.files('D:/Backup_JG/Bounderlands/Climate/CMIP3/Sucumbios/current', full.names=T, pattern='.asc$')
cur_dir<-"E:/Global_30s_bats_project_09_03_2015/Layers/current"
current<-list()
for(i in result){
  current[[i]] <-paste0(cur_dir,"/",i,".asc")
}
current<-as.character(current)
#current=current[1:19]
current<-c(mc,current)
data_c=lapply(current,FUN=raster)
data_c<-stack(data_c)
data_c1=getValues(data_c)
data_c1=as.data.frame(data_c1)
data_c1$Artibeus_fimbriatus_currrent_th=data_c1$Artibeus_fimbriatus_currrent_th/100
data_c2<-na.omit(data_c1)

a<-spline(data_c2$bio_2/10,data_c2$Artibeus_fimbriatus_currrent_th*100)
plot(data_c2$bio_2/10,data_c2$Artibeus_fimbriatus_currrent_th*100,col="red")
lines(a)

#@@@@@@@@@@@@ GLM @@@@@@@@@@@@@@#
model_1<-glm(Artibeus_fimbriatus_currrent_th~., data=data_c2)
summary(model_1)
model_2<-step(model_1, direction="both")
cu_stp<-summary(model_2)
cu_stp_1 <- cu_stp$coefficients
cu_stp_1 <- as.data.frame(cu_stp_1)
write.csv(cu_stp_1, "E:/Global_30s_bats_project_09_03_2015/Stepwise_current/Stepwise_current_artibeus_fimbriatus.csv", row.names=T)


#@@@@@@@@@@@@ Analysis Future @@@@@@@@@@@@@@#
mf=list.files("E:/Global_30s_bats_project_09_03_2015/Layers")
mf=mf[-10]
#mf=list()
#for(i in xxx){
       # mf[[i]]=(xxx[[i]])
#}
#mf=list.files("E:/Global_30s_bats_project_09_03_2015/Layers")
fut_dir<-"E:/Global_30s_bats_project_09_03_2015/output/MaxEnt_results/Artibeus fimbriatus"
climate<-"E:/Global_30s_bats_project_09_03_2015/Layers"
x<-list()
y<-list()
i="bcc_csm1_1"
for(i in mf){

MaxFut<-paste0(fut_dir,"/",as.character(i),"_cons_th.tif")
fut_cl<-paste0(climate,"/",i)
future<-list.files(fut_cl, full.names=T, pattern=".asc$")
names<-c("bio_2","bio_3","bio_8","bio_9","bio_15","bio_19","alt_para_30","bio_1","bio_12")
f=list()
for (s in names){
    f[[s]]=paste0(fut_cl, "/", s, ".asc")
}

future<-as.character(f)
fut<-c(MaxFut,future)
fut<-lapply(fut,raster)
names(fut[[1]])="future"
fut<-stack(fut)
cat("getting values for",as.character(i),"\n")
data_c1=getValues(fut)
data_c1=as.data.frame(data_c1)
data_c1[,1]=data_c1[,1]/100
data_c2<-na.omit(data_c1)
name=paste0("Artibeus_fimbriatus",as.character(i))
cat("modelling GLM for",as.character(i),"\n")

model_1<-glm(future~., data=data_c2)
model_2<-step(model_1, direction="both")
x[[i]]<-model_1
y[[i]]<-model_2
cu_stp<-summary(model_2)
cu_stp_1 <- cu_stp$coefficients

cu_stp_1 <- cu_stp$coefficients
cu_stp_1 <- as.data.frame(cu_stp_1)
write.csv(cu_stp_1, paste0("E:/Global_30s_bats_project_09_03_2015/Stepwise_current/Stepwise_fut_artibeus_fimbriatus_Stepwise_",as.character(i),".csv"),row.names=T)
}

#@@@@@@@@@@@@ Graphics @@@@@@@@@@@@@@#
narino <- read.table(file="clipboard",header=T)

boxplot(narino, outline=F, horizontal=F,boxwex=0.6, main="Stepwise Regression (Suitability ~ GCM's 2020)", 
        col="lightgray", xlab="Bioclim variables", ylab=" Estimate values")
abline(0,0, col="red",lty = 2, lwd=2)
boxplot(narino, outline=F, horizontal=F,boxwex=0.6, main="Stepwise Regression (Suitability ~ GCM's 2020)", 
        col="lightgray", xlab="Bioclim variables", ylab=" Estimate values", add=T)








