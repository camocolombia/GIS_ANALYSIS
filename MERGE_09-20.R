require(plyr);require(ff);require(ffbase)

IDs<-read.csv.ffdf(file="D:/CWR_OCC_VALIDATION/OccurrencesValidation/DATASET/TO_USDA_NPGS_GRIN_IRRI_FF.csv",
                   header=T,
                   sep="|",
                   first.rows=80000,
                   next.rows=70000)




GRIN_DB<-read.csv.ffdf(file="D:/CWR_OCC_VALIDATION/GRIN_2016-09-02_SQL_E.csv",
                   header=T,
                   sep="|",
                   first.rows=80000,
                   next.rows=70000)

#Mobj<-ff(nrow(IDs),ncol(GRIN_DB))

#temp.dt <-as.ffdf(as.data.frame(matrix(nrow = nrow(IDs),ncol=ncol(GRIN_DB))))


# temp_dt<-lapply(1:nrow(IDs),function(i){
#   idx <- ffwhich(x=GRIN_DB, GRIN_DB$old_id == IDs$old_id[i])
#   if(is.null(idx)){
#     x<-NA
#     
#     cat(as.character(IDs$old_id[i])," | ",as.character(i),"NULL","\n")
#     
#   }else{
#    x<-GRIN_DB[idx,] 
#     
#     cat(as.character(IDs$old_id[i])," | ",as.character(i),"done","\n")
#   }
#   return(x)
#   })
# 
# 
# 

temp<-merge(IDs,GRIN_DB,by=c("old_id","unique_number"),all.x = F)

write.table.ffdf(temp,"D:/CWR_OCC_VALIDATION/MERGE_09-20.csv",row.names=F,sep="|")
