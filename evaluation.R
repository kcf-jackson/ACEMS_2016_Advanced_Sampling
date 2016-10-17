# evaluate_train_nn <- function(train_data, oracle) {
#   oracle$FUN(train_data)$net.result %>% `>=`(0.5) %>% as.numeric()
# }
# evaluate_test_nn <- function(test_data, oracle) {
#   map(
#     seq_along(test_data), 
#     ~oracle(test_data[[.x]])$net.result %>% `>=`(0.5) %>% as.numeric()
#   )
# }
# 

evaluate_test <- function(test_data, oracle) {
  map(seq_along(test_data), ~evaluate_df_by_row(test_data[[.x]], oracle))
}
evaluate_df_by_row <- function(df0, oracle) {
  map_dbl(1:nrow(df0), ~( df0[.x, ] %>% oracle$FUN() ))
}
evaluate_train <- evaluate_df_by_row


print_result_summary <- function(oracle_1, my_fun_1, test_data_1) {
  test_perf <- map2_dbl(
    evaluate_test(test_data_1, oracle_1), 
    evaluate_test(test_data_1, my_fun_1),
    ~sum(.x == .y)
  )
 
  cat("Oracle\n")
  cat("z: \n")
  print(oracle_1$z)
  cat("threshold:", oracle_1$threshold, "\n\n")
  
  cat("My function\n")
  cat("z: \n")
  print(my_fun_1$z)
  cat("threshold:", my_fun_1$threshold, "\n\n")
  
  cat("Differences\n")
  cat("z:", sum(xor(oracle_1$z, my_fun_1$z)), "\n")
  cat("threshold:", abs(oracle_1$threshold - my_fun_1$threshold), "\n")
  cat("test data:", min(test_perf), "\n")
  
  return(data.frame(
    z = sum(xor(oracle_1$z, my_fun_1$z)),
    threshold = abs(oracle_1$threshold - my_fun_1$threshold),
    test_perf = min(test_perf)
  ))
}
