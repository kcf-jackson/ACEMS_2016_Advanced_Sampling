rm(list = ls())
source("load.R")

# Problem 1 ===================================================================
# Setup
num_train <- 1000
num_dim <- 100

train_data_1 <- create_train_data(num_train, num_dim)
test_data_1 <- create_test_data(num_dim)

# name_FUN <- c("features_logit", "logit_oracle", "logit", "logit_expanded", 
#               "svm_linear", "svm_sigmoid", "rf", "knn", "nn", "hdda")[c(2, 3, 5, 6)]
# list_FUN <- list(features_logit, logit_oracle, logit, logit_expanded, svm_linear, 
#                  svm_sigmoid, caret_rf, myKnn, myNN, high_dda)[c(2, 3, 5, 6)]

name_FUN <- c("logit", "logit_oracle", "svm_linear", "svm_sigmoid")
list_FUN <- list(logit, logit_oracle, svm_linear, svm_sigmoid)

res <- map(
  # .x = rep(50, 100),
  .x = 0:100,
  .f = ~run_simulation_1(.x, train_data_1, test_data_1, list_FUN)
) %>% 
  do.call(rbind, .)

res %>% as.data.frame() %>% rename_df(name_FUN) %>% print()
