rm(list = ls())
source("load.R")

# Problem 3 ===================================================================
# Setup
num_train <- 1000
num_dim <- 64

train_data_3 <- create_train_data(num_train, num_dim)
test_data_3 <- create_test_data(num_dim)

name_FUN <- c("logit", "logit", "logit_expanded", "svm_linear",
              "svm_sigmoid", "rf", "knn", "nn")[c(1,3:8)]
list_FUN <- list(logit, logit, logit_expanded, caret_svm_linear, 
                 caret_svm_sigmoid, caret_rf, myKnn, myNN)[c(1,3:8)]
res <- map(
  .x = 1:50, 
  .f = ~run_simulation_3(train_data_3, test_data_3, list_FUN)
) %>% 
  do.call(rbind, .)

res %>% as.data.frame() %>% rename_df(name_FUN) %>% print()