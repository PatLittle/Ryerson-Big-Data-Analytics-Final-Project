library(readxl)
library(openxlsx)
library(tidyverse)
library(R.utils)
library(tidymodels)
library(jsonlite)
library(gtools)
library(ggpubr)
library(plyr)
library(dplyr)
library(tictoc)
library(tidymodels)
library(data.table)
library(R.oo)
library(tibble)

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


R.utils::gunzip("od-do-canada.jsonl.gz", remove=F)
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
}



combined<-merge(x = q1data, y = dl_df, by = "ID", all.x = TRUE)
combined<-na.replace(combined,0)


combined$date_created<-as.Date(combined$date_created)
combined$date_last_mod<-as.Date(combined$date_last_mod)

##################start removing combined_factor####################


date_of_downloads<-as.Date.character("2021-01-31","%Y-%m-%d")
combined$created_days<-as.numeric(difftime(date_of_downloads,combined$date_created, units="days"))
combined$modified_days<-as.numeric(difftime(date_of_downloads,combined$date_last_mod, units="days"))

j<-1
for(i in 1:length(combined$downloads)){
  if (combined$created_days[i]< 365){
    combined$adj_downloads[j]<-round(365*(combined$downloads[j]/combined$created_days[j]),digits=0)
  } else combined$adj_downloads[j]<-combined$downloads[j]
  j<-j+1
}

quant_list<-as.list(quantile(combined$adj_downloads, probs = seq(0, 1, by= 0.01)))

j<-1
for(i in 1:length(combined$adj_downloads)){
  if (combined$adj_downloads[j]< quant_list[96]){
    combined$bin_downloads[j]<-0
  } else {
    combined$bin_downloads[j]<-1
  }
  j<-j+1
}

combined$bin_downloads<-as.factor(combined$bin_downloads)




df<-combined

df$ID<-NULL
df$date_created<-NULL
df$date_last_mod<-NULL
df$downloads<-NULL
df$created_days<-NULL
df$modified_days<-NULL
df$adj_downloads<-NULL

df<-as.data.table(df,keep.rownames = F)
df<-subset(df, collection!="geogratis")
df$key1<-str_to_lower(df$key1)
df$key2<-str_to_lower(df$key2)
df$key3<-str_to_lower(df$key3)
df$key1<-str_replace_all(df$key1, regex("\\W+"),"")
df$key2<-str_replace_all(df$key2, regex("\\W+"),"")
df$key3<-str_replace_all(df$key3, regex("\\W+"),"")

df3<-df



set.seed(888)

pop_split4<- initial_split(df3,strata = bin_downloads)
pop_train4<-training(pop_split4)
pop_test4<-testing(pop_split4)

xgb_spec4 <- boost_tree(
  trees = 2000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec4

xgb_grid4 <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), pop_train4),
  learn_rate(),
  size = 50
)
xgb_grid4


xgb_recipe4 <- recipe(bin_downloads ~ ., data = pop_train) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_ordinalscore(all_nominal()) %>%
  step_novel(all_nominal())



tic()
xgb_recipe4 %>%
  prep() %>%
  bake(new_data = pop_train4) 
toc()


xgb_w4f <- workflow() %>%
  add_model(xgb_spec4) %>%
  add_recipe(xgb_recipe4)

xgb_wf4




pop_folds4<-vfold_cv(pop_train4,v=10,strata=bin_downloads)
pop_folds4


library(doParallel)
cores<-detectCores()
cl<- makeCluster(cores[1]-4)
registerDoParallel(cl)

tic()
set.seed(888)
xgb_res4 <- tune_grid(
  xgb_wf4,
  resamples = pop_folds4,
  grid = xgb_grid3,
  control = control_grid(save_pred = TRUE)
)
toc()

collect_metrics(xgb_res4)

best_auc <- select_best(xgb_res4, "roc_auc")
best_auc


final_xgb4 <- finalize_workflow(
  xgb_wf4,
  best_auc
)

final_xgb4

library(vip)

final_xgb4 %>%
  fit(data = pop_train3) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


final_res4 <- last_fit(final_xgb4, pop_split4)
collect_metrics(final_res2)

final_res4 %>%
  collect_predictions() %>%
  roc_curve(bin_downloads, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

final_res4 %>%
  collect_predictions() %>% 
  conf_mat(truth = bin_downloads, estimate = .pred_class)




saveRDS(final_res4$.workflow[[1]],"saved_model4.Rds")

saved_model4<-readRDS("saved_model4.Rds")