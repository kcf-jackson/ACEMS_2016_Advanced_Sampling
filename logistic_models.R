# Logistic regression
logit <- function(train_data_1, y) {
  logit_model <- safely(glmnet::cv.glmnet)(
    x = train_data_1, y = as.factor(y), family = "binomial"
  )
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx, type = 'class') {
    m <- logit_model$result
    m %>% 
      predict(newx = newx, type = type, s = "lambda.1se") %>% 
      as.numchar()
  }
}


# Logistic regression
logit_oracle <- function(train_data_1, y) {
  logit_model <- safely(glmnet::glmnet)(
    x = train_data_1, y = as.factor(y), family = "binomial"
  )
  
  if (is.null(logit_model$result))
    return(NULL)
  
  num_dim <- ncol(train_data_1)
  lmr <- logit_model$result
  col_index <- ncol(lmr$beta)
  
  threshold <- (1 - mean(y)) %>% 
    qnorm(mean = num_dim * 0.5, sd = sqrt(num_dim * 0.25)) 
  z1 <- as.numeric(lmr$beta[,col_index] / lmr$a0[col_index] > 0)
  z2 <- as.numeric(lmr$beta[,col_index] / lmr$a0[col_index] <= 0)
  est_oracle_1 <- init_oracle_1(num_dim, z = z1, threshold = threshold)
  est_oracle_2 <- init_oracle_1(num_dim, z = z2, threshold = threshold)
  in_sample_fit <- evaluate_train(train_data_1, est_oracle_1)
  in_sample_fit_2 <- evaluate_train(train_data_1, est_oracle_2)
  
  if (sum(in_sample_fit == y) > sum(in_sample_fit_2 == y)) {
    est_oracle <- est_oracle_1
  } else {
    est_oracle <- est_oracle_2
  }
  
  # print(sum(evaluate_train(train_data_1, est_oracle) == y))
  # if (sum(in_sample_fit == y) <= nrow(train_data_1)) {
  #   z <- update_z(train_data_1, y, est_oracle)
  #   est_oracle <- init_oracle_1(num_dim, z = z, threshold = threshold)
  # }
  # print(sum(evaluate_train(train_data_1, est_oracle) == y))
  
  function(newx) {
    evaluate_train(newx, est_oracle)
  }
}
# update_z <- function(train_data_1, y, est_oracle) {
#   z <- est_oracle$z
#   threshold <- est_oracle$threshold
#   current_perf <- sum(evaluate_train(train_data_1, est_oracle) == y)
#   has_update <- TRUE
#   while (has_update) {
#     has_update <- FALSE
#     for (i in seq_along(z)) {
#       new_z <- z
#       new_z[i] <- abs(new_z[i] - 1)
#       new_oracle <- init_oracle_1(ncol(train_data_1), z = new_z, threshold = threshold)
#       new_perf <- sum(evaluate_train(train_data_1, new_oracle) == y)
#       if (new_perf > current_perf) {
#         z <- new_z
#         est_oracle <- new_oracle
#         current_perf <- new_perf
#         has_update <- TRUE
#         print("Updated!")
#       }
#     }
#   }
#   z
# }


## Logistic regression with perturbation
# logit_expanded <- function(train_data_1, oracle_1, num_exp = 1000) {
#   y <- evaluate_train(train_data_1, oracle_1) 
#   
#   art_data <- create_artifical_data(train_data_1, y, num_exp)
#   train_data_1 %<>% 
#     cbind(y = y) %>% 
#     rbind(art_data)
#   
#   X <- train_data_1 %>% df_remove_last_column()
#   y <- train_data_1 %>% df_get_last_column()
#   
#   logit_model <- safely(glmnet::glmnet)(x = X, y = as.factor(y), 
#                                         family = "binomial")
#   
#   if (is.null(logit_model$result))
#     return(NULL)
#   
#   function(newx) {
#     m <- logit_model$result
#     predict(m, newx = newx, type = 'class', s = tail(m$lambda, 1)) %>% 
#       as.character() %>% 
#       as.numeric()
#   }
# }
