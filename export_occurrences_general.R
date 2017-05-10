###################### Dependencies #######################



###OPEN WAMP SERVER BEFORE RUN!
library(RMySQL)
 SQL_TO_CSV<-function(out_dir,public_status,fields,sp_name,sp_type_query,special_sp_type_query,db_host,db_user,db_password,db_name){
###################### Configuration #######################

   work_folder<-out_dir
   work_filter_fields<-public_status
   work_filter_condition<-fields
   work_filter_type<-sp_type_query
   work_filter_special<-special_sp_type_query
   work_filter<-sp_name
   
   
# Parameters
work_cores <- 10 
work_export_extension <- ".csv"
work_sep_field <- "|"
work_decimal <- "."
work_na <- ""

###################### Internal Variables #######################

# Constants queries
work_filter_condition_public <- " data_public_access=1 and visibility=1 and "
db_query_fields_all <- "id,old_id,taxon_id,metadata_id,field_collected_data,data_public_access,filename,username,source,provider_institute_id,provider_name,institute_id,institute_name,is_expert,collection,source_url,botrecat,availability,image,final_image,unique_number,barcode,vno_1,vno_2,voucher_id,x1_family,x1_genus,x1_sp1,x1_author1,x1_rank1,x1_sp2,x1_author2,x1_rank2,x1_sp3,x1_author3,x1_detby,x1_detdate,x1_detdd,x1_detmm,x1_detyy,x1_detstat,x2_family,x2_genus,x2_sp1,x2_author1,x2_rank1,x2_sp2,x2_author2,x2_rank2,x2_sp3,x2_author3,x2_detby,x2_detdate,x2_detdd,x2_detmm,x2_detyy,x2_detstat,x3_family,x3_genus,x3_sp1,x3_author1,x3_rank1,x3_sp2,x3_author2,x3_rank2,x3_sp3,x3_author3,x3_detby,x3_detdate,x3_detdd,x3_detmm,x3_detyy,x3_detstat,is_hybrid,hybrid_memo,tnrs_overall_score,tnrs_source,tnrs_x1_family,tnrs_final_taxon,tnrs_author1,taxon_final,f_x1_genus,f_x1_sp1,f_x1_rank1,f_x1_sp2,f_x1_rank2,f_x1_sp3,annotated_specimen,type,type_memo,collector,addcoll,collnumber,prefix,number,suffix,colldate,colldd,collmm,collyy,final_country,country,iso,final_iso2,adm1,adm2,adm3,adm4,local_area,locality,coord,lat_deg,lat_min,lat_sec,ns,final_ns,latitude,long_deg,long_min,long_sec,ew,final_ew,longitude,llorig,lldatum,georef_history_id,latitude_georef,longitude_georef,distance_georef,georef_flag,alt,final_alt,alt_max,final_alt_max,cult_stat,final_cult_stat,origin_stat,final_origin_stat,soil,slope,aspect,habitat_txt,plant_desc,frequency,fl_code,fr_code,inflo_graminea,vernacular,language,uses,dups,notes,comments,timestamp,coord_check_old,citation,final_lat,final_lon,coord_source,taxstand_family,taxstand_genus,taxstand_sp1,taxstand_sp2,taxstand_author1,taxstand_final_taxon,temp_id,grin_final_taxon,x1_taxon,gbif_genus,gbif_species,collection_code,gbif_references,datasetKey,gbif_rank,origin_stat_inv,iso2,taxon_source,f_x1_family,quality_row,visibility"
db_query_fields_public <- "id,source,provider_institute_id,provider_name,institute_id,institute_name,collection,source_url,availability,unique_number,barcode,vno_1,vno_2,x1_family,x1_genus,x1_sp1,x1_author1,x1_rank1,x1_sp2,x1_author2,x1_rank2,x1_sp3,x1_author3,x1_detby,x1_detdate,x1_detdd,x1_detmm,x1_detyy,x1_detstat,x2_genus,x2_sp1,x2_author1,x2_rank1,x2_sp2,x2_rank2,x2_sp3,is_hybrid,hybrid_memo,grin_final_taxon,tnrs_final_taxon,taxstand_final_taxon,taxon_final,f_x1_genus,f_x1_sp1,f_x1_rank1,f_x1_sp2,f_x1_rank2,f_x1_sp3,annotated_specimen,type,type_memo,collector,addcoll,collnumber,prefix,number,suffix,colldate,colldd,collmm,collyy,final_country,final_iso2,adm1,adm2,adm3,adm4,local_area,locality,lat_deg,lat_min,lat_sec,ns,final_ns,latitude,long_deg,long_min,long_sec,ew,final_ew,longitude,latitude_georef,final_lat,final_lon,coord_source,alt,final_alt,final_cult_stat,final_origin_stat,habitat_txt,fl_code,fr_code,dups,notes,comments,citation"
db_table <- "final_occurrences"

# Variables Database Query
db_query <- NULL

# Database
db_cnn <- NULL
db_rs_query <- NULL
db_rs_data <- NULL

# Export
exp_dir <- NULL
exp_filename <- NULL

###################### Process #######################

if(work_filter_fields == "PUBLIC_ACCESS"){
  db_query <- paste0("Select ",db_query_fields_public, " from ",db_table," ")
  print("Filter public fields")
} else if(work_filter_fields == "FULL_ACCESS"){
  db_query <- paste0("Select ",db_query_fields_all, " from ",db_table, " ")
  print("Filter all fields")
}

if(work_filter_condition == "public and visibility"){    
  db_query <- paste0(db_query," where ",work_filter_condition_public)
  print("Filter public data")    
} else if(work_filter_condition == "all"){
  db_query <- paste0(db_query)
  print("Filter all data")    
} else if(work_filter_condition == "custom"){
  db_query <- paste0(db_query," where ",work_filter_special)
  print("Filter all data")    
} 

if(work_filter_type == "CONDITIONAL"){ 
  if(grepl("and $", db_query)){
    db_query <- substr(db_query, 1, nchar(db_query)-4)
  }
  db_query <- paste0(db_query,";")
  print("Filter nothing...CUSTOM_QUERY...")    
} else if(work_filter_type == "TAXON"){    
  db_query <- paste0(db_query," taxon_final ='",work_filter,"';")
  print("Filter taxon")    
} else if(work_filter_type == "GENUS"){
  db_query <- paste0(db_query," f_x1_genus ='",work_filter,"';")
  print("Filter genus")    
} 

print("Start process")

# Connection Database
db_cnn <- dbConnect(MySQL(),user = db_user,password = db_password,host = db_host,dbname=db_name)
print("Connected to database")

db_rs_query <- dbSendQuery(db_cnn,db_query )

print("Fetching")
db_rs_data <- fetch(db_rs_query, n=-1)
print("Data attached")

if(work_filter==""){
  exp_filename<- paste0(work_folder,"/","Query_",Sys.Date(),work_export_extension)
}else{
  exp_filename <- paste0(work_folder,"/",work_filter,work_export_extension)
 }
  
cat("writing file ",exp_filename," please be patient","\n")
write.table(db_rs_data,exp_filename,row.names=FALSE,sep=work_sep_field,dec=work_decimal,na=work_na, quote = F)

# Close connection database
dbDisconnect(db_cnn);gc()


####################

print("End process")
################
}


###Varibles Global
out_dir <- "D:/CWR_OCC_VALIDATION" #DONDE SE VA A GUARDAR

# Fields to export "PUBLIC_ACCESS"/"FULL_ACCESS"
public_status <- "FULL_ACCESS"

# Data to export  #"public and visibility","all","custom"
fields <- "custom" #"public and visibility"/"all"/"custom"

# Filter to export  #"CONDITIONAL"/"TAXON"/"GENUS"
sp_type_query <- "CONDITIONAL"

#taxa name
sp_name=""

# Data special filter
#special_sp_type_query<-"data_public_access=1 and visibility=1 and final_iso2 in ('US','MX','CA') and final_cult_stat not in ('cultivated','weedy') and f_x1_genus in ('Actaea','Amaranthus','Aphanisma','Capsicum','Chenopodium','Cucurbita','Daucus','Fragaria','Helianthus','Ipomoea','Lactuca','Manihot','Nicotiana','Pachyrhizus','Panax','Panicum','Parthenium','Phaseolus','Phlox','Physaria','Ribes','Rubus','Setaria','Solanum','Tripsacum','Vaccinium','Vitis','Xanthosoma','Zea')"
#special_sp_type_query<-"data_public_access=1 and final_iso2 in ('US','MX','CA') and final_cult_stat not in ('cultivated','weedy') and f_x1_genus in ('Actaea','Amaranthus','Aphanisma','Capsicum','Chenopodium','Cucurbita','Daucus','Fragaria','Helianthus','Ipomoea','Lactuca','Manihot','Nicotiana','Pachyrhizus','Panax','Panicum','Parthenium','Phaseolus','Phlox','Physaria','Ribes','Rubus','Setaria','Solanum','Tripsacum','Vaccinium','Vitis','Xanthosoma','Zea')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and final_cult_stat not in ('cultivated','weedy') and f_x1_genus in ('Actaea','Amaranthus','Aphanisma','Capsicum','Chenopodium','Cucurbita','Daucus','Fragaria','Helianthus','Ipomoea','Lactuca','Manihot','Nicotiana','Pachyrhizus','Panax','Panicum','Parthenium','Phaseolus','Phlox','Physaria','Ribes','Rubus','Setaria','Solanum','Tripsacum','Vaccinium','Vitis','Xanthosoma','Zea')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and f_x1_genus in ('Actaea','Amaranthus','Aphanisma','Capsicum','Chenopodium','Cucurbita','Daucus','Fragaria','Helianthus','Ipomoea','Lactuca','Manihot','Nicotiana','Pachyrhizus','Panax','Panicum','Parthenium','Phaseolus','Phlox','Physaria','Ribes','Rubus','Setaria','Solanum','Tripsacum','Vaccinium','Vitis','Xanthosoma','Zea')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Actaea','Amaranthus','Aphanisma','Capsicum','Chenopodium','Cucurbita','Daucus','Fragaria','Helianthus','Ipomoea','Lactuca','Manihot','Nicotiana','Pachyrhizus','Panax','Panicum','Parthenium','Phaseolus','Phlox','Physaria','Ribes','Rubus','Setaria','Solanum','Tripsacum','Vaccinium','Vitis','Xanthosoma','Zea')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Lactuca')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Nicotiana')"
#special_sp_type_query<-"data_public_access=1 and f_x1_genus in ('Beilschmiedia','Cucurbita','Digitaria','Echinochloa','Ilex','Phaseolus','Persea','Tripsacum','Vitis','Xanthosoma')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Gossypium','Humulus','Simmondsia','Theobroma','Persea','Carica','Psidium','Annona','Prunus','Diospyros','Malus','Asimina','Pouteria','Manilkara','Juglans','Carya','Castanea','Corylus','Pistacia')"
#special_sp_type_query<-"final_iso2 in ('CO') and data_public_access=1"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Parthenium')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Cucurbita')"
#special_sp_type_query<-"final_iso2 in ('US') and data_public_access=1"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Cucurbita')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Limnanthes','Hesperaloe','Opuntia','Agave')"
#special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Solanum')"
special_sp_type_query<-"final_iso2 in ('US','MX','CA') and data_public_access=1 and f_x1_genus in ('Zea')"

#
#
#special_sp_type_query<-paste0("taxon_final = ",sp_type_query)
#special_sp_type_query<-""
###########################
#db_host <- "192.168.1.2" #US  CIAT PC
db_host<-"localhost"
db_user <- "csosa"
db_password <- "12345678"
db_name <- "cwr_gapanalysis"


x<-SQL_TO_CSV(out_dir,public_status,fields,sp_name,sp_type_query,special_sp_type_query,db_host,db_user,db_password,db_name)
# 
 # Varibles Global
 # out_dir <- "D:/CWR_OCC_VALIDATION" #DONDE SE VA A GUARDAR
 # 
 # # Fields to export "PUBLIC_ACCESS"/"FULL_ACCESS"
 # public_status <- "FULL_ACCESS" #"FULL_ACCESS"
 # 
 # # Data to export  #"public and visibility","all","custom"
 # fields <- "custom" #"public and visibility"/"all"/"custom"
 # 
 # # Filter to export  #"CONDITIONAL"/"TAXON"/"GENUS"
 # sp_type_query <- "TAXON"
 # 
 # #taxa name
 # sp_name="Chenopodium_berlandieri"
 # 
 # x<-SQL_TO_CSV(out_dir,public_status,fields,sp_name,sp_type_query,special_sp_type_query,db_host,db_user,db_password,db_name)

