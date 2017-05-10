require(tpl)
require(tpldata)


t<-read.table("clipboard",header=F,sep="\t")

tpl2<-tpl.get(t[,1])


write.table(tpl2,"D:/CWR/TPL_GBIF_9743.csv",sep="^",quote = F,row.names = F)
