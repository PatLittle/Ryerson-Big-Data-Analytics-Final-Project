library(ckanr)
library(parsnip)
library(recipes)
library(magrittr)
library(tidymodels)



ckanr_setup(url="https://open.canada.ca/data")

api_data <- package_search(sort='metadata_created desc')

model<-readRDS(url("https://github.com/PatLittle/Ryerson-Big-Data-Analytics-Final-Project/raw/master/final-report/saved_model.Rds"))

ID<-api_data$results[[1]]$id
title<-api_data$results[[1]]$title
org<-api_data$results[[1]]$organization$name
desc<-as.numeric(length(unlist(strsplit(api_data$results[[1]]$notes," "))))
collection<-api_data$results[[1]]$collection
freq<-api_data$results[[1]]$frequency
jurisdiction<-api_data$results[[1]]$jurisdiction
key1<-as.character(api_data$results[[1]]$keywords$en[1])
key2<-as.character(api_data$results[[1]]$keywords$en[2])
key3<-as.character(api_data$results[[1]]$keywords$en[3])
num_keys<-as.numeric(length(api_data$results[[1]]$keywords$en))
num_res<-as.numeric(api_data$results[[1]]$num_resources)
subj1<-as.character(api_data$results[[1]]$subject[1])
subj2<-as.character(api_data$results[[1]]$subject[2])
subj3<-as.character(api_data$results[[1]]$subject[3])
subj4<-as.character(api_data$results[[1]]$subject[4])

new_dataset<-data.frame(ID,title,org,desc,collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,
                        subj1,subj2,subj3,subj4)

predict_data<-new_dataset[3:16]

pred_full<-predict(model,predict_data)
pred_val<-as.numeric(pred_full$.pred_class)



output<-data.frame(date(),org,title,ID,collection,freq)
names(output)<-c("date","org","title","ID","collection","freq")
output$prediction<-pred_val



write.table(output, file="popularity-predictions.csv",sep=',', row.names=F, col.names=T)
