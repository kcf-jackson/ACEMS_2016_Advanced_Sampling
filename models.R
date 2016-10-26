# high dimensional discriminant analysis
high_dda <- function(train_data_1, y) {
  hdda_model <- safely(HDclassif::hdda)(train_data_1, y, model = "ABQkDk")

  m <- hdda_model$result
  if (is.null(m)) return(NULL)
  
  function(newx) {
    m %>% 
      predict(newx) %>% 
      `$`("class") %>% 
      as.character() %>% 
      as.numeric()
  }
}


# Finite mixture classification
mixture_gaussian <- function(train_data_1, y) {
  gauss_mix <- safely(mclust::Mclust)(train_data_1, as.factor(y), G = 10,
                                      modelType = "EDDA")
  m <- gauss_mix$result
  if (is.null(m)) return(gauss_mix$error)
  
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
  
  print(sum(evaluate_train(train_data_1, est_oracle) == y))
  if (sum(in_sample_fit == y) <= nrow(train_data_1)) {
    z <- update_z(train_data_1, y, est_oracle)
    est_oracle <- init_oracle_1(num_dim, z = z, threshold = threshold)
  }
  print(sum(evaluate_train(train_data_1, est_oracle) == y))
  
  function(newx) {
    evaluate_train(newx, est_oracle)
  }
}
update_z <- function(train_data_1, y, est_oracle) {
  z <- est_oracle$z
  threshold <- est_oracle$threshold
  current_perf <- sum(evaluate_train(train_data_1, est_oracle) == y)
  has_update <- TRUE
  while (has_update) {
    has_update <- FALSE
    for (i in seq_along(z)) {
      new_z <- z
      new_z[i] <- abs(new_z[i] - 1)
      new_oracle <- init_oracle_1(ncol(train_data_1), z = new_z, threshold = threshold)
      new_perf <- sum(evaluate_train(train_data_1, new_oracle) == y)
      if (new_perf > current_perf) {
        z <- new_z
        est_oracle <- new_oracle
        current_perf <- new_perf
        has_update <- TRUE
        print("Updated!")
      }
    }
  }
  z
}



# Logistic regression
logit <- function(train_data_1, y) {
  logit_model <- safely(glmnet::glmnet)(x = train_data_1, y = as.factor(y), 
                                        family = "binomial")
  
  if (is.null(logit_model$result))
    return(logit_model)
  
  function(newx, type = 'class') {
    m <- logit_model$result
    predict(m, newx = newx, type = type, s = tail(m$lambda, 1)) %>% 
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
caret_svm_linear <- function(train_data_1, y) {
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
caret_svm_sigmoid <- function(train_data_1, y) {
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
myKnn <- function(train_data_1, y) {
  my_data <- data.frame(x = train_data_1, y = y)
  return(function(test, k = 20, distance = 1, kernel = "optimal", 
                  threshold = 0.5) {
    kknn::kknn(
      formula = y~., train = my_data, data.frame(x = test), k = k, 
      distance = distance, kernel = kernel
    ) %>% fitted() %>% binarise(threshold)
  })
}


# NN
myNN <- function(train_data_1, y, hidden = c(64, 8)) {
  nn_data <- data.frame(x = train_data_1, y = y)
  nn_form <- nn_data %>% names() %>% head(-1) %>% 
    paste(collapse = "+") %>% paste("y ~", .)
  nn_model <- neuralnet(nn_form, nn_data, hidden = hidden, 
                        act.fct = 'logistic', linear.output = FALSE)
    
  return(function(test, threshold = 0.5) {
    compute(nn_model, test)$net.result %>% `>=`(threshold) %>% as.numeric()
  })
}


# NN
myNN_with_features <- function(train_data_1, oracle_1) {
  y <- evaluate_train(train_data_1, oracle_1)
  f_num <- 10
  train_data_1 %<>% df_create_features(f_num)
  nn_data <- data.frame(x = train_data_1, y = y)
  nn_form <- nn_data %>% names() %>% head(-1) %>% 
    paste(collapse = "+") %>% paste("y ~", .)
  nn_model <- neuralnet(nn_form, nn_data, hidden = c(8, 2), 
                        act.fct = 'logistic', linear.output = FALSE)
  
  return(function(test) {
    test %<>% df_create_features(f_num)
    compute(nn_model, test)$net.result %>% `>=`(0.5) %>% as.numeric()
  })
}
