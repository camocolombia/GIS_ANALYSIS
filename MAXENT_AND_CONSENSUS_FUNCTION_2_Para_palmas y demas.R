#####################################################
##############MAXENT RUN AND PROJECTING##############
##written by Chrystian C. Sosa and J.D. Carvajal#####
#####################################################

MAXENT_STUFF<-function(name,ou,output_dir,occ_dir,bg_dir,bg,cl_dir,vars,n){
  require(raster);require(dismo);require(SDMTools); require(parallel);require(rJava)
   source("http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R")
   source(paste(src.dir,"/mclapply2.R",sep=""))
  
cat("#################################################","\n")  
cat("beginning to run Climate change projection script","\n")
cat("#################################################","\n")
cat("please be patient, this process is very slow","\n")  
cat("#################################################","\n")  
 

n<-1:n
  points<-read.csv(paste0(occ_dir,"/",name,".csv"),header=T)
  #points<-cbind(points$lon,points$lat)

  
  
  inDir<-paste0(ou,"/",name)
  # create a new output dir accordly a name parameter
  output2_dir<-paste0(output_dir,"/",name);if(!file.exists(output2_dir)){dir.create(output2_dir)}
  
  snm<-list.dirs(cl_dir,recursive=F,full.names=F)
  snm<-snm[-10]
  
  #snm<-list.dirs(inDir,recursive=F,full.names=F)
  
  cl_dir<-paste0(cl_dir)
  cl_dir<-list.dirs(cl_dir,recursive=F,full.names=T)
  curr<-cl_dir[10] 
  curr_di<-cl_dir[10] 
  cl_dir<-cl_dir[-10]              
  
  
  #curr<-list.files(curr,pattern=".asc$",full.names=T)# this line was silenced because we used some selected variables to make up the models
  #curr<-curr[c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11)]
  #curr<-curr[c(2,13,14,15,16,17,18,19,20,3,4,5,6,7,8,9,10,11,12,1)]
  curr<-paste0(curr,"/",vars,".asc")
  curr<-lapply(curr,raster)
  curr<-stack(curr)
  
  
  ##################################################
  
  points2<-extract(curr,points[,2:3])
  
  points3<-matrix(nrow=nrow(points),ncol=3)
  points3<-as.data.frame(points3)
  
  
  points3[,1]<-rep(name,nrow(points))
  points3[,2:3]<-points[,2:3]
  lx<-(length(vars)+4)-1
  points3[,4:lx]<-points2
  colnames(points3)<-c("sp","lon","lat",names(curr))
  
  ###############################################
  
  #############################################
  
  #bg
  
#   group <- kfold(bg, 5)
#   backg_train <- bg[group != 1, ]
#   backg_test <- bg[group == 1, ]


#write.csv(backg_train,paste0(occ_dir,"/",name,"_bg_train.csv"),row.names=F)
# write.csv(bg,paste0(occ_dir,"/",name,"_bg_train.csv"),row.names=F)

#write.csv(backg_test,paste0(occ_dir,"/",name,"_bg_test.csv"),row.names=F)
# write.csv(bg,paste0(occ_dir,"/",name,"_bg_test.csv"),row.names=F)

  ####################################################
  
  
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep="")
  
 # cat("running Maxent for",as.character(name),"\n")

cat("###############################","\n")
cat("#running multiple repeats#####","\n")
cat("###############################","\n")
  
  for(i in 1:length(n)){
    
	num<-paste0(occ_dir,"/",i);if(!file.exists(num)){dir.create(num)
	  }###Para crear las carpetas con las replicas.
	
	
   #cat("##binning and writing training and test points","\n")
  group <- kfold(points3, 5)
  
  pres_train <- points3[group != 1, ]
  pres_test <- points3[group == 1, ]
  
  write.csv(pres_train,paste0(occ_dir,"/",i,"/",name,"_pres_train.csv"),row.names=F)
  write.csv(pres_test,paste0(occ_dir,"/",i,"/",name,"_pres_test.csv"),row.names=F)
	
	
	
	
    cat("running Maxent for",as.character(name)," ",i,"\n")
    #x<-maxent(p=pres_train[,2:3],a=backg_train,removeDuplicates=T,x=curr,path=paste0(output2_dir,"/",i),args=c("-J", "-P","-r","-a","-z","pictures=true","fadebyclamping","nowarnings","randomtestpoints=0"))
    x<-maxent(p=pres_train[,2:3],a=bg,removeDuplicates=T,x=curr,path=paste0(output2_dir,"/",i),args=c("-J", "-P","-r","-a","-z","pictures=true","fadebyclamping","nowarnings","randomtestpoints=0"))
    
    #("-a","-r","-J","-P","replicates=10","fadebyclamping","writebackgroundpredictions","nowarnings","randomtestpoints=0","randomseed=false","replicatetype=false"))#"-X","5","replicatetype=NULL"             
    
    
    cat("projecting Maxent outputs for",as.character(name)," ",i,"\n")
    p=predict(object=x,x=curr,filename=paste0(name,"_","Current","_,",i),ext=curr[[1]],progress="text",overwrite=TRUE)        
    
    
  ####evaluation####
    
    cat("evaluating ENM performance for ",i,"\n")
    
    
    
    pre_con<-extract(p,pres_test[,2:3])
  
    #bg_con<-extract(p,backg_test)
    bg_con<-extract(p,bg)
  
    e_train<-evaluate(pre_con,bg_con)
    th_test<-threshold(e_train)
    
    mat_pres<-rep(1,nrow(points))
    mat_pres_1<-cbind(points[,2:3],mat_pres)
    mat_aus<-rep(0,nrow(bg)) 
    mat_aus_1<-cbind(bg,mat_aus)
    colnames(mat_aus_1)<-colnames(mat_pres_1)
    
    mat_1<-rbind(mat_pres_1,mat_aus_1)
    con_mat_1<-extract(p,mat_1[,1:2])
    
    confusion<-confusion.matrix(mat_1$mat_pres,con_mat_1,threshold =th_test$spec_sens)
    
    cat("calculating TSS statistic"," ",i,"\n")
    TSS=(sensitivity(confusion)+specificity(confusion))-1
    
    auc_train<-read.csv(paste0(output2_dir,"/",i,"/","maxentResults.csv"))
    
    #csv_eval<-cbind(name,nrow(pres_train),nrow(pres_test),nrow(backg_train),nrow(backg_test),e_train@auc,auc_train$Training.AUC,sensitivity(confusion),specificity(confusion),omission(confusion),Kappa(confusion),prop.correct(confusion),TSS,th_test$spec_sens)
  csv_eval<-cbind(name,nrow(pres_train),nrow(pres_test),nrow(bg),nrow(bg),e_train@auc,auc_train$Training.AUC,sensitivity(confusion),specificity(confusion),omission(confusion),Kappa(confusion),prop.correct(confusion),TSS,th_test$spec_sens)
  
  colnames(csv_eval)<-c("taxon","presences_training","presences testing","background_training","background_testing","AUC_testing","AUC_training","sensitivity_R","specificityR","omission","Kappa","prop_correct","TSS","threshold")
    lambdas<-x@lambdas
    write.csv(csv_eval,paste0(output2_dir,"/",i,"/","model_evaluation","_",i,".csv"),quote=F,row.names=F)
    write.csv(lambdas,paste0(output2_dir,"/",i,"/","lambdas","_",i,".lambdas"),quote=F,row.names=F)
    
    cons_thr<-p
    cons_thr[which(cons_thr[]>=th_test$spec_sens)]<-1
    
    cons_thr[which(cons_thr[]<th_test$spec_sens)]<-0
    
    #plot(cons_thr)
    cat("writing ENM rasters for baseline ",i,"\n")
    
    writeRaster(p,paste0(output2_dir,"/",i,"/","current",".tif"),format="GTiff")
    writeRaster(cons_thr,paste0(output2_dir,"/",i,"/","currrent_th",".tif"),format="GTiff")
  
cat("#####################################","\n")
cat("#####Project using GCMs layers#######","\n")
cat("#####################################","\n")
    for(j in 1:length(cl_dir)){
      fut<-list()
      
      fut<-cl_dir[-10]              
      fut<-list.dirs(cl_dir,recursive=F,full.names=T)
      
      #fut<-list.files(cl_dir[[j]],pattern=".asc$",full.names=T)#because we use some selected variables
      #fut<-fut[c(2,13,14,15,16,17,18,19,20,3,4,5,6,7,8,9,10,11,12,1)]
      fut<-paste0(cl_dir[[j]],"/",vars,".asc")
      fut<-lapply(fut,raster)
      fut<-stack(fut)
      
      st<-predict(object=x,x=fut,filename=paste0(name,"_",snm[[j]]),ext=curr[[1]],progress="text",overwrite=T)           
      
      con_fut_th_i<-st
      con_fut_th_i[which(con_fut_th_i[]>=th_test$no_omission)]<-1
      
      con_fut_th_i[which(con_fut_th_i[]<th_test$no_omission)]<-0
    
      writeRaster(st,paste0(output2_dir,"/",i,"/",snm[[j]],"_fut",".tif"),format="GTiff")
      writeRaster(con_fut_th_i,paste0(output2_dir,"/",i,"/",snm[[j]],"_fut_th",".tif"),format="GTiff")
      
    }
    
#delete just if is neccesary
#removeTmpFiles(h=24)
cat("                                                        ","\n")
cat("             done!                                      ","\n")
cat("                                                        ","\n")
  
  }
}
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################

consensus_function<-function(name,ou,output_dir,occ_dir,bg_dir,bg,cl_dir){
  require(raster);require(dismo);require(SDMTools)
  cat("#########################################","\n")
  cat("###consensus_function script beginning###","\n")
  cat("#########################################","\n")
  cat("--------### please be patient###---------","\n")
  
##############################################
cat("###################################","\n")
cat("###current consensus projection##","\n")
cat("###################################","\n")
# create a new output dir accordly a name parameter
output2_dir<-paste0(output_dir,"/",name);if(!file.exists(output2_dir)){dir.create(output2_dir)}


list_curr<-list()
list_out<-list.dirs(output2_dir,full.names = F,recursive =F )

for(m in 1:length(list_out)){
  
  list_curr[[m]]<-raster(paste0(output2_dir,"/",m,"/","current",".tif"))
  
}

list_curr<-stack(list_curr)
cur_mean<-mean(list_curr)
cur_sd<-calc(list_curr,sd)
cur_cv<-(cur_sd/cur_mean)*100


writeRaster(cur_mean,paste0(output2_dir,"/","current_cons.tif"),format="GTiff")
writeRaster(cur_sd,paste0(output2_dir,"/","current_cons_sd.tif"),format="GTiff")
writeRaster(cur_cv,paste0(output2_dir,"/","current_cons_cv.tif"),format="GTiff")

##########################


cat("###################################","\n")
cat("##future projections GCM Projection##","\n")
cat("###################################","\n")
output_dir<-paste0(ou,"/","MaxEnt_results");if(!file.exists(output_dir)){dir.create(output_dir)}

output2_dir<-paste0(output_dir,"/",name);if(!file.exists(output2_dir)){dir.create(output2_dir)}

list_out<-list.dirs(output2_dir,full.names = F,recursive =F )
snm<-list.dirs(cl_dir,recursive=F,full.names=F)
snm<-snm[-10]

####please checkout this step###
for(k in 1:length(snm)){
  list_snm<-list()
  
  #list_out<-list.dirs(output2_dir,full.names = F,recursive =F )
  
  for(m in 1:length(list_out)){
    list_snm[[m]]<-raster(paste0(output2_dir,"/",list_out[[m]],"/",snm[[k]],"_fut.tif"))
  }
  
  
  snm_mean<-stack(list_snm);snm_m<-mean(snm_mean)
  
  snm_sd<-calc(snm_mean,sd)
  snm_cv<-(snm_sd/snm_m)*100
  writeRaster(snm_m,paste0(output2_dir,"/",snm[[k]],"_cons.tif"),format="GTiff")
  writeRaster(snm_sd,paste0(output2_dir,"/",snm[[k]],"_cons_sd.tif"),format="GTiff")
  writeRaster(snm_cv,paste0(output2_dir,"/",snm[[k]],"_cons_cv.tif"),format="GTiff")
    }

cat("#######################################################","\n")
cat("#######evaluation over consensus rasters ##############","\n")
cat("#######################################################","\n")

cat("evaluating ENM performance for consensus raster","\n")
pres_train<-read.csv(paste0(occ_dir,"/",name,"_pres_train.csv"),header=T)
pres_test<-read.csv(paste0(occ_dir,"/",name,"_pres_test.csv"),header=T)
backg_test<-read.csv(paste0(occ_dir,"/",name,"_bg_test.csv"),header=T)
backg_train<-read.csv(paste0(occ_dir,"/",name,"_bg_train.csv"),header=T)

points<-read.csv(paste0(occ_dir,"/",name,".csv"),header=T)


pre_con<-extract(cur_mean,pres_test[,2:3])
#bg_con<-extract(cur_mean,backg_test)
bg_con<-extract(cur_mean,bg)

e_train<-evaluate(pre_con,bg_con)
th_test<-threshold(e_train)

mat_pres<-rep(1,nrow(points))
mat_pres_1<-cbind(points[,2:3],mat_pres)
mat_aus<-rep(0,nrow(bg)) 
mat_aus_1<-cbind(bg,mat_aus)
colnames(mat_aus_1)<-colnames(mat_pres_1)

mat_1<-rbind(mat_pres_1,mat_aus_1)
con_mat_1<-extract(cur_mean,mat_1[,1:2])

confusion<-confusion.matrix(mat_1$mat_pres,con_mat_1,threshold =th_test$spec_sens)

cat("calculating TSS statistic","for consensus current layer","\n")
TSS=(sensitivity(confusion)+specificity(confusion))-1


auc_list<-list.dirs(output2_dir,full.names = T,recursive = F)
auc_train_list<-list()

for(o in 1:length(auc_list)){
  auc_train_list[[o]]<-read.csv(paste0(output2_dir,"/",o,"/","maxentResults.csv"))
  auc_train_list[[o]]<-auc_train_list[[o]]$Training.AUC
}
auc_train_list<-do.call(rbind,auc_train_list)
auc_train_list<-mean(auc_train_list,na.rm = T)

csv_eval<-cbind(name,nrow(pres_train),nrow(pres_test),nrow(backg_train),nrow(backg_test),e_train@auc,auc_train_list,sensitivity(confusion),specificity(confusion),omission(confusion),Kappa(confusion),prop.correct(confusion),TSS,th_test$spec_sens)
csv_eval<-cbind(name,nrow(pres_train),nrow(pres_test),nrow(bg),nrow(bg),e_train@auc,auc_train_list,sensitivity(confusion),specificity(confusion),omission(confusion),Kappa(confusion),prop.correct(confusion),TSS,th_test$spec_sens)

colnames(csv_eval)<-c("taxon","presences_training","presences testing","background_training","background_testing","AUC_testing","AUC_training","sensitivity_R","specificityR","omission","Kappa","prop_correct","TSS","threshold")
write.csv(csv_eval,paste0(output2_dir,"/","model_evaluation",".csv"),quote=F,row.names=F)

cons_thr<-cur_mean
cons_thr[which(cons_thr[]>=th_test$spec_sens)]<-1

cons_thr[which(cons_thr[]<th_test$spec_sens)]<-0

#plot(cons_thr)
cat("writing thresholded ENM rasters for consensus current","\n")

writeRaster(cons_thr,paste0(output2_dir,"/","currrent_th",".tif"),format="GTiff")



cat("########################################################","\n")
cat("       ####Future consensus projections####            ","\n")
cat("########################################################","\n")

cat("reading and writing final future consensus","\n")
fut_consensus<-list()

fut_consensus<-lapply(paste0(output2_dir,"/",snm,"_cons.tif"),raster)
fut_consensus<-stack(fut_consensus)
cons_fut<-mean(fut_consensus)
cons_fut_thr<-cons_fut
cons_fut_thr[which(cons_fut_thr[]>=th_test$no_omission)]<-1

cons_fut_thr[which(cons_fut_thr[]<th_test$no_omission)]<-0

cons_fut_sd<-calc(fut_consensus,sd)
cons_fut_cv<-(cons_fut_sd/cons_fut)*100


cat("processing and writing future consensus raster projections","\n")
writeRaster(cons_fut,paste0(output2_dir,"/","future_cons",".tif"),format="GTiff")
writeRaster(cons_fut_thr,paste0(output2_dir,"/","future__th",".tif"),format="GTiff")
writeRaster(cons_fut_sd,paste0(output2_dir,"/","future__sd",".tif"),format="GTiff")
writeRaster(cons_fut_cv,paste0(output2_dir,"/","future__cv",".tif"),format="GTiff")



cat("########################################################","\n")
cat("       ####Future thresholded projections####            ","\n")
cat("########################################################","\n")

fut_consensus<-lapply(paste0(output2_dir,"/",snm,"_cons.tif"),raster)
fut_consensus<-stack(fut_consensus)



for(q in 1:nlayers(fut_consensus)){
fut_thr<-fut_consensus[[q]] 
names_th<-names(fut_thr)
fut_thr[which(fut_thr[]>=th_test$no_omission)]<-1
fut_thr[which(fut_thr[]<th_test$no_omission)]<-0
writeRaster(fut_thr,paste0(output2_dir,"/",names_th,"_th",".tif"),format="GTiff")
}
#delete just if is neccesary
#removeTmpFiles(h=24)
cat("                                                        ","\n")
cat("             done!                                      ","\n")
cat("                                                        ","\n")

 }



# for 1 sp
###sp name###
name<-"Ceroxylon_quindiuense"
###output_dir###
ou<-"D:/Tesis_palma/2050/Output"
###final output dir###
output_dir<-paste0(ou,"/","MaxEnt_results");if(!file.exists(output_dir)){dir.create(output_dir)}
###background_dir###
bg_dir<-"D:/Tesis_palma/2050/bg"
###background file (THIS IS NOT A SWD FORMAT) ###
bg<-read.csv(paste0(bg_dir,"/","Colombia","_bg",".csv"),header=T)
###occurrence data dir###
occ_dir<-"D:/Tesis_palma/2050/Ocurrence"
###Climate layers (GCM and CURRENT Rasters)###
cl_dir<-"D:/Tesis_palma/2080/Layers"
###variables to run the MaxEnt script
vars<-c("bio_2","bio_3","bio_4","bio_8","bio_15","bio_18","bio_19","alt","bio_1","bio_12","bio_20","bio_21","bio_22","bio_23","bio_24","bio_25")
###number of replicates to do###
n=20
####running the scripts###
# zz<-MAXENT_STUFF(name,ou,output_dir,occ_dir,bg_dir,bg,cl_dir,vars,n)
# zz2<-consensus_function(name,ou,output_dir,occ_dir,bg_dir,bg,cl_dir)


###for 2 or more taxa###

###output_dir###
ou<-"D:/Proyecto_lontra/output"
###final output dir###
output_dir<-paste0(ou,"/","MaxEnt_results");if(!file.exists(output_dir)){dir.create(output_dir)}
###background_dir###
bg_dir<-"D:/Proyecto_lontra/bg"
###background file (THIS IS NOT A SWD FORMAT) ###
bg<-read.csv(paste0(bg_dir,"/","Colombia","_bg",".csv"),header=T)
###occurrence data dir###
occ_dir<-"D:/Proyecto_lontra/ocurrence"
###Climate layers (GCM and CURRENT Rasters)###
cl_dir<-"D:/Proyecto_lontra/Layers"
###variables to run the MaxEnt script
vars<-c("bio_2","bio_3","bio_4","bio_8","bio_15","bio_18","bio_19","alt","bio_1","bio_12","bio_20","bio_21","bio_22","bio_23","bio_24","bio_25")
###number of replicates to do###
n=5


###this is a new directory that has the same file that the occurrence directory##3
#this new parameter is neccesary because in the occ file 
#the script write the background and points subsets to test the model##

names_dir<-"D:/Proyecto_lontra/ocurrence - copia"
###list files###
#names=c("Desmodus rotundus","Eumops patagonicus","Molossops planirostris","Molossus molossus","Myotis albescens")
#names<-list.files(names_dir,pattern=".csv$",full.names=F)
##delete the extension format in order to get the correct name###
#names<-sub(".csv","",names)


####run this loop to get results for a lot of taxa###


MAXENT2 <- mclapply2(1:length(spList), fun=function(i){
 MAXENT_STUFF(names[[name]],ou,output_dir,occ_dir,bg_dir,bg,cl_dir,vars,n)
  })
  
CONSENSUS2 <- mclapply2(1:length(spList), fun=function(i){
  consensus_function(names[[name]],ou,output_dir,occ_dir,bg_dir,bg,cl_dir)
})  
  

# for(name in 1:length(names)){
#   
#   zz<-MAXENT_STUFF(names[[name]],ou,output_dir,occ_dir,bg_dir,bg,cl_dir,vars,n)
#   zz2<-consensus_function(names[[name]],ou,output_dir,occ_dir,bg_dir,bg,cl_dir) 
#   }