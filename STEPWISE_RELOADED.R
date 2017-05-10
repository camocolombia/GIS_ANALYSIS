

STEPWISE_STEP<-function(inDir,out_dir,year,sp,layers_b){
  
require(raster);require(rgdal);require(dismo);require(ff);require(ffbase)
  
  
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("RUNNING STEPWISE FOR CURRENT","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  #@@@@@@@@@@@@ Analysis Current @@@@@@@@@@@@@@#
mc<-paste0(inDir,"/",year,"/","Output/MaxEnt_results","/",sp,"/","current_cons.tif")
#mc<-raster(mc)
current<-paste0(inDir,"/",year,"/","Layers/Current","/",layers_b,".asc")
  #list.files(, full.names=T, pattern='.asc$')
#cur_dir<-"D:/Tesis_palma/2050/Layers/Current"


current<-c(mc,current)
data_c=lapply(current,FUN=raster)
data_c<-stack(data_c)

temp.dt<-data_c[[1]][]
#temp.dt <-(na.omit(data_c[[1]][]))
temp.dt <- ff(1,dim=c(ncell(temp.dt),nlayers(data_c)),vmode="double")

lapply(1:nlayers(data_c),function(i){
  t <- getValues(data_c[[i]]) 
  temp.dt[,i] <- na.omit(t[])
  return(cat(names(data_c[[i]])," done\n"))})
gc()
temp.dt<-as.ffdf(temp.dt)
 #temp.dt<- as.data.frame(temp.dt)
colnames(temp.dt)<-names(data_c)


# a<-spline(data_c2$bio_2/10,data_c2$current_cons*100)
# plot(data_c2$bio_2/10,data_c2$current_cons*100,col="red")
# lines(a)

#@@@@@@@@@@@@ GLM @@@@@@@@@@@@@@#
model_1<-glm(current_cons~., data=temp.dt)
gc();rm(data_c)
summary(model_1)
gc()
model_2<-step(model_1, direction="both")
cu_stp<-summary(model_2)
cu_stp_1 <- cu_stp$coefficients
cu_stp_1 <- as.data.frame(cu_stp_1)
write.csv(cu_stp_1, paste0(out_dir,"/","Stepwise_current.csv"), row.names=T)

rm(model_1,model_2,cu_stp_1,data_c,temp.dt)


#@@@@@@@@@@@@ Analysis Future @@@@@@@@@@@@@@#

cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("RUNNING STEPWISE FOR,",year," period","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")


mf=list.files(paste0(inDir,"/",year,"/","Layers"))
mf<-mf[which(mf!="Current")]
mf<-mf[which(mf!="Current.zip")]

fut_dir<-paste0(inDir,"/",year,"/","Output/MaxEnt_results","/",sp)
climate<-paste0(inDir,"/",year,"/","Layers")
x<-list()
y<-list()
#i="bcc_csm1_1"
for(i in mf){
MaxFut<-paste0(fut_dir,"/",as.character(i),"_cons_th.tif")
fut_cl<-paste0(climate,"/",i)
future<-list.files(fut_cl, full.names=T, pattern=".asc$")
names<-layers_b
f=list()

for (s in names){
    f[[s]]=paste0(fut_cl, "/", s, ".asc")
};rm(s)
gc()
future<-as.character(f)
fut<-c(MaxFut,future)
fut<-lapply(fut,raster)
names(fut[[1]])="future"
fut<-stack(fut)
######
temp.dt<-fut[[1]][]
#temp.dt <-(na.omit(data_c[[1]][]))
temp.dt <- ff(1,dim=c(ncell(temp.dt),nlayers(fut)),vmode="double")

lapply(1:nlayers(fut),function(i){
  t <- getValues(fut[[i]]) 
  temp.dt[,i] <- na.omit(t[])
  return(cat(names(fut[[i]])," done\n"))})
gc()
temp.dt<-as.ffdf(temp.dt)
#temp.dt<- as.data.frame(temp.dt)
colnames(temp.dt)<-names(fut)

#####
name=paste0(sp,"_",as.character(i))
cat("modelling GLM for",sp,"_",as.character(i),"\n")

model_1<-glm(future~., data=temp.dt)
gc()
model_2<-step(model_1, direction="both")
gc()
# x[[i]]<-model_1
# y[[i]]<-model_2
cu_stp<-summary(model_2)
cu_stp_1 <- cu_stp$coefficients
cu_stp_1 <- cu_stp$coefficients
cu_stp_1 <- as.data.frame(cu_stp_1)
write.csv(cu_stp_1, paste0(out_dir,"/",sp,"_","Stepwise_",as.character(i),".csv"),row.names=T)
gc()
};rm(i)
gc()
}
#@@@@@@@@@@@@ Graphics @@@@@@@@@@@@@@#
# narino <- read.table(file="clipboard",header=T)
# 
# boxplot(narino, outline=F, horizontal=F,boxwex=0.6, main="Stepwise Regression (Suitability ~ GCM's 2020)", 
#         col="lightgray", xlab="Bioclim variables", ylab=" Estimate values")
# abline(0,0, col="red",lty = 2, lwd=2)
# boxplot(narino, outline=F, horizontal=F,boxwex=0.6, main="Stepwise Regression (Suitability ~ GCM's 2020)", 
#         col="lightgray", xlab="Bioclim variables", ylab=" Estimate values", add=T)

year=2050
out_dir<-"D:/Tesis_palma/step"
sp<-"Ceroxylon_quindiuense"
inDir<-"D:/Tesis_palma"
#layers_b=c("bio_2","bio_3","bio_4","bio_8","bio_15","bio_18","bio_19","alt","bio_1","bio_12","bio_20","bio_21","bio_22","bio_23","bio_24","bio_25")

layers_b=c("bio_2","bio_15","bio_21")


x<-STEPWISE_STEP(inDir,out_dir,year,sp,layers_b)
  



