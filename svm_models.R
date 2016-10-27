# SVM
svm_linear <- function(train_data_1, y) {
  logit_model <- safely(e1071::svm)(
    x = train_data_1, y = as.factor(y), 
    type = "C-classification", kernel = "linear"
  )
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx) {
    logit_model$result %>% 
      predict(newdata = newx) %>% 
      as.numchar()
  }
}


# SVM
svm_sigmoid <- function(train_data_1, y) {
  logit_model <- safely(e1071::svm)(
    x = train_data_1, y = as.factor(y), 
    type = "C-classification", kernel = "sigmoid"
  )
  
  if (is.null(logit_model$result))
    return(NULL)
  
  function(newx) {
    logit_model$result %>% 
      predict(newdata = newx) %>% 
      as.numchar()
  }
}
