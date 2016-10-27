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
