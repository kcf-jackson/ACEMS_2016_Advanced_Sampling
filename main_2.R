rm(list = ls())
source("load.R")

# Problem 2 ===================================================================
# Setup
num_train <- 1000
num_dim <- 128

library(neuralnet)
train_data_2 <- create_train_data(num_train, num_dim)
test_data_2 <- create_test_data(num_dim)

name_FUN <- c("logit", "logit", "logit_expanded", "svm_linear",
              "svm_sigmoid", "rf", "knn")[c(1,3:7)]
list_FUN <- list(logit, logit, logit_expanded, caret_svm_linear, 
                 caret_svm_sigmoid, caret_rf, myKnn)[c(1,3:7)]

res <- map(
  .x = 1:50, 
  .f = ~run_simulation_2(train_data_2, test_data_2, list_FUN)
) %>% 
  do.call(rbind, .)

res %>% as.data.frame() %>% rename_df(name_FUN) %>% print()
