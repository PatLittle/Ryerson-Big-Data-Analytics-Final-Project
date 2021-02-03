


library(jsonlite)
library(openxlsx)
library(readxl)
library(tidyverse)
library(fs)
library(readxl)


wb<-loadWorkbook("downloads-012020-012021.xlsx")
removeWorksheet(wb, 1)
removeWorksheet(wb, 1)
saveWorkbook(wb,"downloads.xlsx", overwrite = T)

path<-"downloads.xlsx"
dls<-lapply(excel_sheets(path), read_excel, path = path)

dl_df<-map_dfr(dls,`[`, c("ID / Identificateur","Title English / Titre en anglais","Number of downloads / Nombre de téléchargements"))
dl_df<-na.omit(df)
dl_df$`Title English / Titre en anglais`<-NULL
names(dl_df)<-c("ID","downloads")

query1<-readLines("od-do-canada.jsonl")
lines <- lapply(query1,unlist)

#loop over this for each line of json


for(i in 1:length(lines)){

q1<-fromJSON(lines[[i]])
ID<-q1$id
org<-q1$organization$name
collection<-q1$collection
freq<-q1$frequency
jurisdiction<-q1$jurisdiction
key1<-q1$keywords$en[1]
q1data<- q1data %>% add_row(ID,org,collection,freq,jurisdiction,key1)
print(i)
}

# q1data<-data.frame(ID,org,collection,freq,jurisdiction,key1)
names(q1data)<-c("ID","org","collection","freq","jurisdiction","key1")

#join the downloads to the metadata
combined<-merge(x = q1data, y = dl_df, by = "ID", all.x = TRUE)
