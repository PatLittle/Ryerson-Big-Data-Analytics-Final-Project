
#to get the latest data from Open.Canada.ca you can use wget on https://open.canada.ca/data/dataset/2916fad5-ebcc-4c86-b0f3-4f619b29f412/resource/4ebc050f-6c3c-4dfd-817e-875b2caf3ec6/download/downloads-012020-012021.xls for downloads
# and on https://open.canada.ca/static/od-do-canada.jsonl.gz | for the purposes of the research paper, files as they existed on 2 Feb 2021 will be used.
# This script would take about 20 minutes to run, so saving the .rds can save rerunning it excessively 

library(doParallel)
all_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = all_cores)

wb<-loadWorkbook("downloads-012020-012021.xlsx")
removeWorksheet(wb, 1)#remove the unwanted tabs
removeWorksheet(wb, 1)#do it again for the 2nd unwanted
saveWorkbook(wb,"downloads.xlsx", overwrite = T)

path<-"downloads.xlsx"
dls<-lapply(excel_sheets(path), read_excel, path = path)

dl_df<-map_dfr(dls,`[`, c("ID / Identificateur","Title English / Titre en anglais","Number of downloads / Nombre de téléchargements"))
dl_df<-na.omit(dl_df)
dl_df$`Title English / Titre en anglais`<-NULL
names(dl_df)<-c("ID","downloads")

query1<-readLines("od-do-canada.jsonl")
lines <- lapply(query1,unlist)

#setup our dataframe with the 1st record in the JSON lines
q1<-fromJSON(lines[[1]])
ID<-q1$id
org<-q1$organization$name
desc<-as.numeric(length(unlist(strsplit(q1$notes," "))))
collection<-q1$collection
freq<-q1$frequency
jurisdiction<-q1$jurisdiction
key1<-q1$keywords$en[1]
key2<-q1$keywords$en[2]
key3<-q1$keywords$en[3]
num_keys<-as.numeric(length(q1$keywords$en))
num_res<-as.numeric(q1$num_resources)
subj1<-q1$subject[1]
subj2<-q1$subject[2]
subj3<-q1$subject[3]
subj4<-q1$subject[4]
date_created<-q1$metadata_created
date_last_mod<-q1$metadata_modified
q1data<-data.frame(ID,org,desc,collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4,date_created,date_last_mod)
names(q1data)<-c("ID","org","desc","collection","freq","jurisdiction","key1","key2","key3","num_keys","num_res","subj1","subj2","subj3","subj4","date_created","date_last_mod")

for(i in 2:length(lines)){ #loop over this for each line of json - except the 1st line
  q1<-fromJSON(lines[[i]])
  ID<-q1$id
  org<-q1$organization$name
  desc<-as.numeric(length(unlist(strsplit(q1$notes," "))))
  collection<-q1$collection
  freq<-q1$frequency
  jurisdiction<-q1$jurisdiction
  key1<-q1$keywords$en[1]
  key2<-q1$keywords$en[2]
  key3<-q1$keywords$en[3]
  num_keys<-as.numeric(length(q1$keywords$en))
  num_res<-as.numeric(q1$num_resources)
  subj1<-q1$subject[1]
  subj2<-q1$subject[2]
  subj3<-q1$subject[3]
  subj4<-q1$subject[4]
  date_created<-q1$metadata_created
  date_last_mod<-q1$metadata_modified
  q1data<- q1data %>% add_row(ID,org,desc,collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4,date_created,date_last_mod)
  print(i)
}




#join the downloads to the metadata - save it as a file so we don't need to loop over the lines again
combined<-merge(x = q1data, y = dl_df, by = "ID", all.x = TRUE)
combined<-na.replace(combined,0)

#create a place to look up of the plain text factor values to reference for EDA or output analysis, before we convert to numeric
factors_combined_org<-levels(as.factor(combined$org))
factors_combined_collection<-levels(as.factor(combined$collection))
factors_combined_freq<-levels(as.factor(combined$freq))
factors_combined_jurisdiction<-levels(as.factor(combined$jurisdiction))
factors_combined_key1<-levels(as.factor(combined$key1))
factors_combined_key2<-levels(as.factor(combined$key2))
factors_combined_key3<-levels(as.factor(combined$key3))
factors_combined_subj1<-levels(as.factor(combined$subj1))
factors_combined_subj2<-levels(as.factor(combined$subj2))
factors_combined_subj3<-levels(as.factor(combined$subj3))
factors_combined_subj4<-levels(as.factor(combined$subj4))

#normalize the datetime stamps to R date format
combined$date_created<-as.Date(combined$date_created)
combined$date_last_mod<-as.Date(combined$date_last_mod)


saveRDS(combined,"combined_factor.rds")

#convert character to factor collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4
#combined$org<-as.numeric(as.factor(combined$org))
#combined$collection<-as.numeric(as.factor(combined$collection))
#combined$freq<-as.numeric(as.factor(combined$freq))
#combined$jurisdiction<-as.numeric(as.factor(combined$jurisdiction))
#combined$key1<-as.numeric(as.factor(combined$key1))
#combined$key2<-as.numeric(as.factor(combined$key2))
#combined$key3<-as.numeric(as.factor(combined$key3))
#combined$subj1<-as.numeric(as.factor(combined$subj1))
#combined$subj2<-as.numeric(as.factor(combined$subj2))
#combined$subj3<-as.numeric(as.factor(combined$subj3))
#combined$subj4<-as.numeric(as.factor(combined$subj4))


saveRDS(combined, "combined.rds")
combined_1h<-one_hot(as.data.table(combined)) 
#saveRDS(combined_1h, "combined_1h.rds") #save this 