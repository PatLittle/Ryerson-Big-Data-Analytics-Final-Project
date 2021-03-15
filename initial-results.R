library(caret)
# data prep
library(dplyr)
# tidymodels
library(rsample)
library(recipes)
library(parsnip)
library(tune)
library(dials)
library(workflows)
library(yardstick)
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
library(tidymodels)


library(doParallel)
cores<-detectCores()
cl<- makeCluster(cores[1]-4)
registerDoParallel(cl)

df<-readRDS("combined_factor_adj.rds")

#removing all data we aren't going to feed into the XGBoost algo
df$ID<-NULL
df$date_created<-NULL
df$date_last_mod<-NULL
df$downloads<-NULL
df$created_days<-NULL
df$modified_days<-NULL
df$adj_downloads<-NULL

df<-as.data.table(df,keep.rownames = F)

#convert character to factor collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4
df$org<-as.numeric(as.factor(df$org))
df$collection<-as.numeric(as.factor(df$collection))
df$freq<-as.numeric(as.factor(df$freq))
df$jurisdiction<-as.numeric(as.factor(df$jurisdiction))
df$key1<-as.numeric(as.factor(df$key1))
df$key2<-as.numeric(as.factor(df$key2))
df$key3<-as.numeric(as.factor(df$key3))
df$subj1<-as.numeric(as.factor(df$subj1))
df$subj2<-as.numeric(as.factor(df$subj2))
df$subj3<-as.numeric(as.factor(df$subj3))
df$subj4<-as.numeric(as.factor(df$subj4))

set.seed(1)

cb_split<- initial_split(df,strata = bin_downloads)
cb_train<-training(cb_split)
cb_test<-testing(cb_split)

xgb_spec <- boost_tree(
  trees = 100, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), cb_train),
  learn_rate(),
  size = 20
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(bin_downloads ~ .) %>%
  add_model(xgb_spec)

xgb_wf



cb_folds<-vfold_cv(cb_train,v=5,strata=bin_downloads)
cb_folds

set.seed(123)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = cb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

collect_metrics(xgb_res)
show_best(xgb_res, "roc_auc")


xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc


final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb

library(vip)
install.packages("vip")
final_xgb %>%
  fit(data = cb_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


final_res <- last_fit(final_xgb, cb_split)
collect_metrics(final_res)


final_res %>%
  collect_predictions() %>%
  roc_curve(bin_downloads, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )