
tar_dir<-"//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/GAP_SPP_ALL/sp"
csvs<-list.files(tar_dir,pattern=".csv$",full.names = T)
csvs<-lapply(csvs,read.csv)
for(i in 1:length(csvs)){
  
  csvs[[i]]$SPECIE<-as.factor(csvs[[i]]$SPECIE)
};rm(i)
csv_tot<-do.call(rbind,csvs)

#write.csv(csv_tot,"//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/GAP_SPP_ALL/csv_crops.csv")
