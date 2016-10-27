rm(list = ls())
source("load.R")

# Problem 3 ===================================================================
# Setup
num_train <- 1000
num_dim <- 64

train_data_3_rand <- create_train_data(num_train, num_dim)
train_data_3_bal  <- create_test_data(num_dim) %>% do.call(rbind, .)
test_data_3 <- create_test_data(num_dim)

# name_FUN <- c("logit", "logit", "logit_expanded", "svm_linear",
#               "svm_sigmoid", "rf", "knn", "nn", "hdda")[c(1,4,5,7)]
# list_FUN <- list(logit, logit, logit_expanded, caret_svm_linear, 
#                  caret_svm_sigmoid, caret_rf, myKnn, myNN, high_dda)[c(1,4,5,7)]

name_FUN <- c("logit", "logit_oracle", "svm_linear", "svm_sigmoid", "rf", "knn", 
              "nn", "hdda")[c(1,3,4,6)]
list_FUN <- list(logit, logit_oracle, svm_linear, svm_sigmoid, caret_rf, myKnn, 
                 myNN, high_dda)[c(1,3,4,6)]


res <- 1:10 %>% 
  map(function(i) {
    res <- rep(i, 10) %>% 
      map(~.x %>% create_response_for_oracle_3() %>% init_oracle_3(resp = .)) %>% 
      map(function(.x) {
        cbind(
          run_simulation_3(train_data_3_rand, test_data_3, list_FUN, oracle_3 = .x),
          run_simulation_3(train_data_3_bal, test_data_3, list_FUN, oracle_3 = .x),
          run_simulation_3b(test_data_3, list_FUN, oracle = .x)
        )
      }) %>% 
      map(t) %>% do.call(rbind, .) %>% data.frame() %>% 
      rename_df(name_FUN) 
    res
  })

res 
res %>% apply(2, mean) %>% print()
res %>% apply(2, sd) %>% print()


map(1:3, ~res[seq(.x, 30, 3), ]) %>% do.call(cbind, .) %>% apply(2, mean)

write.csv(res, file = "sampling_test.csv")
