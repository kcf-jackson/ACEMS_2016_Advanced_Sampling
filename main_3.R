rm(list = ls())
source("load.R")

# Problem 3 ===================================================================
# Setup
num_train <- 1000
num_dim <- 64

train_data_3 <- create_train_data(num_train, num_dim)
# train_data_3 <- create_test_data(num_dim) %>% do.call(rbind, .)
test_data_3 <- create_test_data(num_dim)

name_FUN <- c("logit", "logit", "logit_expanded", "svm_linear",
              "svm_sigmoid", "rf", "knn", "nn", "hdda")[c(1,4,5,7)]
list_FUN <- list(logit, logit, logit_expanded, caret_svm_linear, 
                 caret_svm_sigmoid, caret_rf, myKnn, myNN, high_dda)[c(1,4,5,7)]
res <- map(
  .x = 1:10, 
  .f = ~run_simulation_3(train_data_3, test_data_3, list_FUN)
) %>% 
  do.call(rbind, .)

res %>% as.data.frame() %>% rename_df(name_FUN) %>% print()
res %>% as.data.frame() %>% rename_df(name_FUN) %>% apply(2, mean) %>% print()
res %>% as.data.frame() %>% rename_df(name_FUN) %>% apply(2, sd) %>% print()
