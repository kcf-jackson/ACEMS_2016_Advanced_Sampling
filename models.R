# Finite mixture classification
mixture_gaussian <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  gauss_mix <- safely(mclust::MclustDA)(train_data_1, as.factor(y), G = 10,
                                        modelType = "MclustDA")
  m <- gauss_mix$result
  if (is.null(m)) return(NULL)
  
  function(newx) {
    m %>% 
      predict(newx = newx) %>% 
      `$`("classification") %>% 
      as.character() %>% 
      as.numeric()
  }
}


# Logistic regression
logit_oracle <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  logit_model <- safely(glmnet::glmnet)(x = train_data_1, y = as.factor(y), 
                                        family = "binomial")
  
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
  
  print(threshold)
  if (sum(in_sample_fit == y) <= nrow(train_data_1)) {
    threshold <- update_threshold(train_data_1, y, est_oracle)
  }
  print(sum(evaluate_train(train_data_1, est_oracle) == y))
  print(threshold)
  function(newx) {
    evaluate_train(newx, est_oracle)
  }
}
update_threshold <- function(train_data_1, y, est_oracle) {
  z <- est_oracle$z
  current_perf <- sum(evaluate_train(train_data_1, est_oracle) == y)
  new_oracle <- init_oracle_1(ncol(train_data_1), z = z,
                              threshold = est_oracle$threshold + 1)
  new_perf <- sum(evaluate_train(train_data_1, new_oracle) == y)
  count <- 0
  while ((new_perf >= current_perf) & (count <= 100)) {
    est_oracle <- new_oracle
    current_perf <- new_perf
    new_oracle <- init_oracle_1(ncol(train_data_1), z = z,
                                threshold = est_oracle$threshold + 1)
    new_perf <- sum(evaluate_train(train_data_1, new_oracle) == y)
    count <- count + 1
  }
  est_oracle$threshold
}



# Logistic regression
logit <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  logit_model <- safely(glmnet::glmnet)(x = train_data_1, y = as.factor(y), 
                                        family = "binomial")
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx) {
    m <- logit_model$result
    predict(m, newx = newx, type = 'class', s = tail(m$lambda, 1)) %>% 
      as.character() %>% 
      as.numeric()
  }
}


# Logistic regression with perturbation
logit_expanded <- function(train_data_1, oracle_1, num_exp = 1000) {
  y <- evaluate_train(train_data_1, oracle_1) 
  
  art_data <- create_artifical_data(train_data_1, y, num_exp)
  train_data_1 %<>% 
    cbind(y = y) %>% 
    rbind(art_data)
  
  X <- train_data_1 %>% df_remove_last_column()
  y <- train_data_1 %>% df_get_last_column()
  
  logit_model <- safely(glmnet::glmnet)(x = X, y = as.factor(y), 
                                        family = "binomial")
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx) {
    m <- logit_model$result
    predict(m, newx = newx, type = 'class', s = tail(m$lambda, 1)) %>% 
      as.character() %>% 
      as.numeric()
  }
}


# SVM
caret_svm_linear <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  logit_model <- safely(e1071::svm)(x = train_data_1, y = as.factor(y), 
                                    type = "C-classification",
                                    kernel = "linear")
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx) {
    m <- logit_model$result
    predict(m, newdata = newx) %>% 
      as.character() %>% 
      as.numeric()
  }
}


# SVM
caret_svm_sigmoid <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  logit_model <- safely(e1071::svm)(x = train_data_1, y = as.factor(y), 
                                    type = "C-classification",
                                    kernel = "sigmoid")
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx) {
    m <- logit_model$result
    predict(m, newdata = newx) %>% 
      as.character() %>% 
      as.numeric()
  }
}


# Random Forest
caret_rf <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  model <- safely(randomForest)(x = train_data_1, y = as.factor(y))
  
  if (is.null(model$result))
    return(NULL)
  
  function(newx) {
    predict(model$result, newdata = newx, type="response") %>% 
      as.character() %>% 
      as.numeric()
  }
}


# KNN
myKnn <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  return(function(test) {
      class::knn(train = train_data_1, test = test, cl = as.factor(y))
  })
}


# KNN
myNN <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)  
  nn_data <- data.frame(x = train_data_1, y = y)
  nn_form <- nn_data %>% names() %>% head(-1) %>% 
    paste(collapse = "+") %>% paste("y ~", .)
  nn_model <- neuralnet(nn_form, nn_data, hidden = c(25, 5), 
                        act.fct = 'logistic', linear.output = FALSE)
    
  return(function(test) {
    compute(nn_model, test)$net.result %>% `>=`(0.5) %>% as.numeric()
  })
}
