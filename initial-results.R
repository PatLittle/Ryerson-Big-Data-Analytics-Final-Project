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
all_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = all_cores)

combined<-readRDS("combined.rds")

combined

boxplot(combined$downloads)

str(combined$downloads)

hist(combined$downloads)

combined$ID<-NULL

cb_split<- initial_split(combined,strata=downloads)

cb_train<-training(cb_split)
cb_test<-testing(cb_split)

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), cb_train),
  learn_rate(),
  size = 30
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(downloads ~ .) %>%
  add_model(xgb_spec)

xgb_wf

cb_folds<-vfold_cv(cb_train,strata=downloads)
cb_folds

xgb_res <- tune_grid(
  xgb_wf,
  resamples = cb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

