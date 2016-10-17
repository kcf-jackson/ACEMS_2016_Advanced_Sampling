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
      knn(train = train_data_1, test = test, cl = as.factor(y))
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
