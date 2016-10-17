sample_binary_vector <- function(len, num_ones) {
  if (missing(num_ones)) 
    return(sample(c(0,1), len, replace = TRUE))
  
  x <- rep(0, len)
  pos_of_ones <- sample(1:len, num_ones)
  x[pos_of_ones] <- 1
  x
}


create_train_data <- function(n, d) {
  map(1:n, ~sample_binary_vector(d)) %>% do.call(rbind, .)
}


# create 10 batches of 100 test data
create_test_data <- function(n) {
  num_ones <- (1:10) * floor(n / 11)
  purrr::map(
    .x = num_ones, 
    .f = function(num_ones) {
      map(1:100, ~sample_binary_vector(n, num_ones)) %>% do.call(rbind, .)
    }
  )
}


create_artifical_data <- function(train_data_1, y, n) {
  if (n == 0) return(NULL)
  seq(n) %>% 
    map(function(.x) {
      sample_index <- sample(nrow(train_data_1), 1)
      
      train_vec <- train_data_1[sample_index, ]
      flip_index <- sample(length(train_vec), 1)
      
      train_vec[flip_index] %<>% c_flip_bit()
      c(X = train_vec, y = y[sample_index])
    }) %>% 
    do.call(rbind, .)
}
