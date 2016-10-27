run_simulation_1 <- function(threshold, train_data_1, test_data_1, list_FUN) {
  cat("Threshold:", threshold, "\n")
  num_dim <- ncol(train_data_1)
  oracle_1 <- init_oracle_1(num_dim, threshold = threshold)
  
  #safeguard
  y <- evaluate_train(train_data_1, oracle_1)  
  if (all(y == 1) | all(y == 0)) {
    pred_test_y <- array(list(rep(y[1])), length(test_data_1))
    true_test_y <- evaluate_test(test_data_1, oracle_1)
    return(error_test(true_test_y, pred_test_y))
  }
  
  #Build model and evaluate performance
  list_FUN %>% 
    map_dbl(~model_perf(train_data_1, test_data_1, .x, oracle_1))
}


run_simulation_2 <- function(train_data_2, test_data_2, list_FUN) {
  print("Running simulation...")
  oracle_2 <- init_oracle_2()

  #safeguard
  y <- oracle_2$FUN(as.matrix(train_data_2))
  if (all(y == 1) | all(y == 0)) 
    return(rep(y[1], length(list_FUN)))
  
  #Build model
  map_dbl(list_FUN, ~model_perf(train_data_2, test_data_2, .x, oracle_2))
}


run_simulation_3b <- function(test_data_3, list_FUN, .x) {
  cat("Running simulation", .x, "\n")
  oracle_3 <- init_oracle_3()
  cat("Number of 1-cluster: ", sum(oracle_3$resp), "\n")
  
  train_data_3 <- exploration_train_data(1000, 64, 100, oracle_3)$x
  #safeguard
  y <- evaluate_train(train_data_3, oracle_3)  
  if (all(y == 1) | all(y == 0)) return(numeric(length(list_FUN)))
  #Build model
  # map_dbl(list_FUN, ~model_perf(train_data_3, test_data_3, .x, oracle_3)) %>% 
  # c(model_avg(train_data_3, test_data_3, list_FUN, oracle_3), sum(y))
  map_dbl(list_FUN, ~model_perf(train_data_3, test_data_3, .x, oracle_3))
}


run_simulation_3 <- function(train_data_3, test_data_3, list_FUN, .x) {
  print("Running simulation...")
  resp <- numeric(10)
  flip_index <- sample(10, .x)
  resp[flip_index] %<>% flip_bit()
  
  oracle_3 <- init_oracle_3(resp = resp)
  print(.x)
  cat("Number of 1-cluster: ", sum(oracle_3$resp), "\n")
  #safeguard
  y <- evaluate_train(train_data_3, oracle_3)  
  if (all(y == 1) | all(y == 0)) return(numeric(length(list_FUN)))
  #Build model
  # map_dbl(list_FUN, ~model_perf(train_data_3, test_data_3, .x, oracle_3)) %>% 
  # c(model_avg(train_data_3, test_data_3, list_FUN, oracle_3), sum(y))
  map_dbl(list_FUN, ~model_perf(train_data_3, test_data_3, .x, oracle_3))
}
model_avg <- function(train_data_1, test_data_1, list_FUN, oracle_1) {
  true_test_y <- evaluate_test(test_data_1, oracle_1)
  train_y <- evaluate_train(train_data_1, oracle_1)  
  list_FUN %>% 
    map(~.x(train_data_1, train_y)) %>% 
    map(~pred_test(test_data_1, .x)) %>% 
    reduce(function(l0, l1) {map2(l0, l1, ~as.numeric(.x) + as.numeric(.y))}) %>% 
    list_divide_by_constant(length(list_FUN)) %>% 
    list_binarise() %>% 
    error_test(true_test_y)
}


model_perf <- function(train_data_1, test_data_1, my_FUN, oracle_1, verbose = FALSE) {
  y <- evaluate_train(train_data_1, oracle_1)
  my_fun_1 <- my_FUN(train_data_1, y)
  pred_test_y <- pred_test(test_data_1, my_fun_1)
  true_test_y <- evaluate_test(test_data_1, oracle_1)
  error_test(true_test_y, pred_test_y, verbose = verbose)
}
pred_test <- function(test_data_1, my_fun_1) {
  if (is.null(my_fun_1)) 
    return(numeric(length(test_data_1)))
  test_data_1 %>% map(my_fun_1)
}
error_test <- function(a, b, verbose = FALSE) {
  if (verbose) print(map2_dbl(a, b, ~sum(.x == .y)))
  map2_dbl(a, b, ~sum(.x == .y)) %>% min()
}
