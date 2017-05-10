library(RMySQL)
work_export_extension <- ".csv"
work_sep_field <- ">"
work_decimal <- "."
work_na <- ""
db_host <- "192.168.1.2" #US  CIAT PC
db_user <- "csosa"
db_password <- "12345678"
db_name <- "mcpd"

db_query<-"Select * from mcpd_table;"
out_dir <- "D:/CWR_OCC_VALIDATION" #DONDE SE VA A GUARDAR


db_cnn <- dbConnect(MySQL(),user = db_user,password = db_password,host = db_host,dbname=db_name)
print("Connected to database")

db_rs_query <- dbSendQuery(db_cnn,db_query )

print("Fetching")
db_rs_data <- fetch(db_rs_query, n=-1)
print("Data attached")

exp_filename<-paste0(out_dir,"/","GRIN_",Sys.Date(),".csv")

#db_rs_data<-gsub("|", " ", db_rs_data)

cat("writing file ",exp_filename," please be patient","\n")
write.table(db_rs_data,exp_filename,row.names=FALSE,sep=work_sep_field,dec=work_decimal,na=work_na, quote = F)

# Close connection database
dbDisconnect(db_cnn);gc()
rm(list=ls(all=T))
