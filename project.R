install.packages("mltools")
install.packages("randomForest")
install.packages("randomForestExplainer")
library(data.table)
library(mltools)
library(rpart)
library(rpart.plot)
library(gtools)
library(jsonlite)
library(openxlsx)
library(readxl)
library(tidyverse)
library(fs)
library(readxl)
library(randomForest)
library(randomForestExplainer)
library()

readxl


removeWorksheet(wb, 1)
removeWorksheet(wb, 1)
saveWorkbook(wb,"downloads.xlsx", overwrite = T)

path<-"downloads.xlsx"
dls<-lapply(excel_sheets(path), read_excel, path = path)

dl_df<-map_dfr(dls,`[`, c("ID / Identificateur","Title English / Titre en anglais","Number of downloads / Nombre de téléchargements"))
dl_df<-na.omit(dl_df)
dl_df$`Title English / Titre en anglais`<-NULL
names(dl_df)<-c("ID","downloads")

sum_dls<-na.omit(dl_df)
summary(sum_dls)
summary(dl_df)

query1<-readLines("od-do-canada.jsonl")
lines <- lapply(query1,unlist)


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


saveRDS(combined, file="combined.rds")
combined<-readRDS("combined.rds")

str(combined)

#convert character to factor collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4
combined$org<-as.numeric(as.factor(combined$org))
combined$collection<-as.numeric(as.factor(combined$collection))
combined$freq<-as.numeric(as.factor(combined$freq))
combined$jurisdiction<-as.numeric(as.factor(combined$jurisdiction))
combined$key1<-as.numeric(as.factor(combined$key1))
combined$key2<-as.numeric(as.factor(combined$key2))
combined$key3<-as.numeric(as.factor(combined$key3))
combined$subj1<-as.numeric(as.factor(combined$subj1))
combined$subj2<-as.numeric(as.factor(combined$subj2))
combined$subj3<-as.numeric(as.factor(combined$subj3))
combined$subj4<-as.numeric(as.factor(combined$subj4))

head(combined)

subj2_weird<-(combined$subj2)
unique(subj2_weird)

combined_1h<-one_hot(as.data.table(combined))

write.table(combined_1h,"combined_1h.csv")

combined_1h<-readRDS("combined_1h.rds")

fiveequal<-cut(combined$downloads, breaks=5, dig.lab = 5)
table(fiveequal)
str(fiveequal)
min(combined$downloads)
max(combined$downloads)
mean(combined$downloads)
median(combined$downloads)
str(combined$downloads)
downlog<-log(combined$downloads)
str(downlog)
downs<-downlog<-round(downlog, 0)
downs<-replace(downs,downs<0,0)


downs<-as.factor(downs)

print(downs[1:11])
hist(as.numeric(downs))


ddownlog<-density(downlog)
plot(ddownlog)
#take a 100 line sample for prototyping 
d100<-combined[1:100,]
d100_1h<-one_hot(as.data.table(d100),dropUnusedLevels = T)
names(d100_1h)<-str_replace(names(d100_1h),"-",".")
names(d100_1h)<-str_replace(names(d100_1h)," ","_")


d1000<-combined[1:1000,]
d10000<-combined[1:10000,]
d80000<-combined[1:80000,]
d100_1h<-combined_1h[1:100,]
d100_1h<-d100_1h[,1:11802]
names(d100_1h)
d100_1h<-str_replace(d100_1h,"-",".")
d100_1h<-
rf_classifier <- randomForest(downloads ~ ., data=d100_1h, ntree=1000, mtry=6, importance=TRUE)

saveRDS(rf_classifier,"rf80k.rds")

min_d<-min_depth_distribution(testrf)
head(min_d)

plot_importance_ggpairs(testrf)

testrf<-randomForest(downloads ~ , data= combined, mtry=6)


glimpse(lines)
str(fromJSON(lines[[3333]]))

str(combined_1h)
