#####################################################
##############MAXENT RUN AND PROJECTING##############
##written by Chrystian C. Sosa#######################
#####################################################


MAXENT_CURRENT<-function(name,ou,output_dir,occ_dir,bg_dir,bg,cl_dir,vars,n){
  require(raster);require(dismo);require(SDMTools)
  
  cat("#######################################################","\n")
  cat("####             MAXENT FUNCTION (CURRENT)     ########","\n")
  cat("#######################################################","\n")
  cat("####               PLEASE BE PATIENT           ########","\n")
  cat("#######################################################","\n")
  
  
  points<-read.csv(paste0(occ_dir,"/",name,".csv"),header=T)
  n<-1:n
  inDir<-paste0(ou,"/",name)
  
  # create a new output dir accordly a name parameter
  output2_dir<-paste0(output_dir,"/",name);if(!file.exists(output2_dir)){dir.create(output2_dir)}
  
  cl_dir<-paste0(cl_dir)
  
  curr<-paste0(cl_dir,"/",vars,".asc")
  curr<-lapply(curr,raster)
  curr<-stack(curr)
  
  cat("#######################################################","\n")
  cat("####   BUILDING  TEST AND TRAINNING DATASETS   ########","\n")
  cat("#######################################################","\n")
  
  ##################################################
  
  points2<-extract(curr,points[,1:2])
  
  points3<-matrix(nrow=nrow(points),ncol=3)
  points3<-as.data.frame(points3)
  
  
  points3[,1]<-rep(name,nrow(points))
  points3[,2:3]<-points[,1:2]
  lx<-(length(vars)+4)-1
  points3[,4:lx]<-points2
  colnames(points3)<-c("sp","lon","lat",names(curr))
  
  ###############################################
  group <- kfold(points3, 5)
  
  pres_train <- points3[group != 1, ]
  pres_test <- points3[group == 1, ]
  write.csv(pres_test,paste0(occ_dir,"/","points_workspace","/",name,"_pres_test.csv"),row.names=F)
  write.csv(pres_train,paste0(occ_dir,"/","points_workspace","/",name,"_pres_train.csv"),row.names=F)
  
  #############################################
  
  #bg
  
  group <- kfold(bg, 5)
  backg_train <- bg[group != 1, ]
  backg_test <- bg[group == 1, ]
  
  write.csv(backg_train,paste0(occ_dir,"/","points_workspace","/",name,"_backg_train.csv"),row.names=F)
  write.csv(backg_test, paste0(occ_dir,"/","points_workspace","/",name,"_backg_test.csv"),row.names=F)
  pres_test<-cbind(pres_test$lon,pres_test$lat)
  
  backg_test<-cbind(backg_test$lon,backg_test$lat)
  
  ####################################################
  library(rJava)
  
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep="")
  
  #pres_train<-cbind(pres_train$lon,pres_train$lat)
  #bg<-cbind(bg$lon,bg$lat)
  
  
  
  predictors<-curr
  
  n_dirs<-paste0(output2_dir,"/",n)
  for(j in n_dirs){
    if(file.exists(j)){dir.create(j)}
  }
  cat("#######################################################","\n")
  cat("####               RUNNING MAXENT              ########","\n")
  cat("#######################################################","\n")
  
  
  lapply(1:length(n),function(i){
    # for(i in 1:length(n)){
    
    cat("running Maxent for",as.character(name)," ",i,"\n")
    x<-maxent(p=pres_train[,2:3],a=backg_train,removeDuplicates=T,x=curr,path=paste0(output2_dir,"/",i),args=c("-J", "-P","-r","-a","-z","pictures=true","fadebyclamping","nowarnings","randomtestpoints=0"))
    #("-a","-r","-J","-P","replicates=10","fadebyclamping","writebackgroundpredictions","nowarnings","randomtestpoints=0","randomseed=false","replicatetype=false"))#"-X","5","replicatetype=NULL"             
    
    
    cat("projecting Maxent outputs for",as.character(name)," ",i,"\n")
    p=predict(object=x,x=curr,filename=paste0(name,"_","Current","_,",i),ext=curr[[1]],progress="text",overwrite=TRUE)        
    
    
    #consens<-mean(p)
    #cons_sd<-calc(p,sd)
    #cons_cv<-(cons_sd/consens)*100
    cat("#######################################################","\n")
    cat("####                   EVALUATING...           ########","\n")
    cat("#######################################################","\n")
    
    ####evaluation####
    
    cat("evaluating ENM performance for ",i,"\n")
    
    
    
    pre_con<-extract(p,pres_test)
    bg_con<-extract(p,backg_test)
    
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
    
    csv_eval<-cbind(name,nrow(pres_train),nrow(pres_test),nrow(backg_train),nrow(backg_test),e_train@auc,auc_train$Training.AUC,sensitivity(confusion),specificity(confusion),omission(confusion),Kappa(confusion),prop.correct(confusion),TSS,th_test$spec_sens)
    colnames(csv_eval)<-c("taxon","presences_training","presences testing","background_training","background_testing","AUC_testing","AUC_training","sensitivity_R","specificityR","omission","Kappa","prop_correct","TSS","threshold")
    lambdas<-x@lambdas
    write.csv(csv_eval,paste0(output2_dir,"/",i,"/","model_evaluation","_",i,".csv"),quote=F,row.names=F)
    write.csv(lambdas,paste0(output2_dir,"/",i,"/","lambdas","_",i,".lambdas"),quote=F,row.names=F)
    
    cons_thr<-p
    cons_thr[which(cons_thr[]>=th_test$spec_sens)]<-1
    
    cons_thr[which(cons_thr[]<th_test$spec_sens)]<-0
    
    #plot(cons_thr)
    cat("writing ENM rasters for baseline ",i,"\n")
    
    writeRaster(p,paste0(output2_dir,"/",i,"/","current","_",i,".tif"),format="GTiff")
    writeRaster(cons_thr,paste0(output2_dir,"/",i,"/","currrent_th","_",i,".tif"),format="GTiff")
  })
  
  predicts<-lapply(1:length(n),function(k){
    # for(k in 1:length(n)){
    t<-paste0(output2_dir,"/",k,"/","current_",k,".tif")
    predicts<-raster(t)
    return(predicts)
  })
  predicts<-stack(predicts)
  
  consens_pr<-mean(predicts)
  cons_sd_pr<-calc(predicts,sd)
  cons_cv_pr<-(cons_sd_pr/consens_pr)*100
  
  #######
  
  
  cat("#######################################################","\n")
  cat("####               THRESHOLDING...!            ########","\n")
  cat("#######################################################","\n")
  
  pre_con_f<-extract(consens_pr,pres_test)
  bg_con_f<-extract(consens_pr,backg_test)
  
  e_train_co<-evaluate(p=pre_con_f,a=bg_con_f)
  th_test_co<-threshold(e_train_co)
  
  mat_pres_co<-rep(1,nrow(points))
  mat_pres_1<-cbind(points[,2:3],mat_pres_co)
  mat_aus<-rep(0,nrow(bg)) 
  mat_aus_1<-cbind(bg,mat_aus)
  colnames(mat_aus_1)<-colnames(mat_pres_1)
  
  mat_1<-rbind(mat_pres_1,mat_aus_1)
  con_mat_1<-extract(consens_pr,mat_1[,1:2])
  
  confusion<-confusion.matrix(mat_1$mat_pres,con_mat_1,threshold =th_test_co$spec_sens)
  
  cat("calculating TSS statistic","\n")
  TSS=(sensitivity(confusion)+specificity(confusion))-1
  
  aucs<-lapply(1:length(n),function(k){
    #for(k in 1:length(n)){
    s<-read.csv(paste0(output2_dir,"/",k,"/","maxentResults.csv"),header=T)
    
    aucs<-s$Training.AUC
    return(aucs)
  })
  
  aucs<-unlist(aucs)
  aucs<-mean(aucs)
  
  
  csv_eval<-cbind(name,nrow(pres_train),nrow(pres_test),nrow(backg_train),nrow(backg_test),e_train_co@auc,aucs,sensitivity(confusion),specificity(confusion),omission(confusion),Kappa(confusion),prop.correct(confusion),TSS,th_test_co$spec_sens)
  colnames(csv_eval)<-c("taxon","presences_training","presences testing","background_training","background_testing","AUC_testing","AUC_training","sensitivity","specificity","omission","Kappa","prop_correct","TSS","threshold")
  write.csv(csv_eval,paste0(output2_dir,"/","model_evaluation",".csv"),quote=F,row.names=F)
  
  cons_thr<-consens_pr
  cons_thr[which(cons_thr[]>=th_test_co$spec_sens)]<-1
  
  cons_thr[which(cons_thr[]<th_test_co$spec_sens)]<-0
  
  
  cat("#######################################################","\n")
  cat("####      SAVING PROJECTING FILES...           ########","\n")
  cat("#######################################################","\n")
  
  
  #plot(cons_thr)
  cat("writing ENM rasters for baseline" ,"\n")
  
  
  
  writeRaster(consens_pr,paste0(output2_dir,"/","current",".tif"),format="GTiff")
  writeRaster(cons_thr,paste0(output2_dir,"/","currrent_th",".tif"),format="GTiff")
  writeRaster(cons_cv_pr,paste0(output2_dir,"/","currrent_cv",".tif"),format="GTiff")
  writeRaster(cons_sd_pr,paste0(output2_dir,"/","currrent_sd",".tif"),format="GTiff")
  
  
  
  cat("#######################################################","\n")
  cat("####               DONE!                       ########","\n")
  cat("#######################################################","\n")
  
  
}




names<-c("annuus","argophyllus","petiolaris_fallax","petiolaris_petiolaris")
ou<-"D:/ccsosa/Mikey_request/OUTPUT"
output_dir<-paste0(ou,"/","test");if(!file.exists(output_dir)){dir.create(output_dir)}

bg_dir<-"D:/ccsosa/Mikey_request/bg"
bg<-read.csv(paste0(bg_dir,"/","background",".csv"),header=T)

occ_dir<-"D:/ccsosa/Mikey_request/points"
cl_dir<-"D:/ccsosa/Mikey_request/ALL/asc"


vars<-list.files("D:/ccsosa/Mikey_request/var_choice",pattern = ".csv$",full.names = T)
vars<-lapply(vars,read.csv)

vars2<-c("bio_2","bio_1","bio_5","bio_8","bio_12","bio_13","bio_15","bio_17","bio_18","alt")
n=10


# lapply(1:length(names),function(z){
#   
#   x<-MAXENT_CURRENT(name=names[[z]],ou=ou,output_dir=output_dir,occ_dir=occ_dir,bg_dir=bg_dir,bg=bg,cl_dir=cl_dir,vars=vars[[z]]$Variables,n=n) 
#   
# })

library(parallel)

detectCores(logical = TRUE)
source("http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R")
source("D:/ccsosa/Mikey_request/mclapply2.R")

test <- mclapply2(1:length(names), fun=function(z){MAXENT_CURRENT(name=names[[z]],ou=ou,output_dir=output_dir,occ_dir=occ_dir,bg_dir=bg_dir,bg=bg,cl_dir=cl_dir,vars=vars[[z]]$Variables,n=n)})



