exploration_sampling <- function(center_vec, num_sample, near_range_sampling) {
  len <- length(center_vec)
  if (missing(near_range_sampling)) near_range_sampling <- len
  map(seq(num_sample), function(k) {
    dist_away_from_center <- rbinom(1, size = near_range_sampling, prob = 0.5)
    flip_index <- sample(len, dist_away_from_center)
    center_vec[flip_index] %<>% flip_bit()
    center_vec
  }) %>% 
    do.call(rbind, .)
}


exploration_train_data <- function(n, d, batch = 100, oracle, near_range = 32) {
  my_train <- create_train_data(batch, d = d)
  my_y <- evaluate_train(my_train, oracle)
  
  full_train <- current_train <- my_train
  current_y <- my_y
  while(nrow(full_train) < n) {
    index_set <- current_y == 1
    if (length(index_set) > 0) {
      new_train <- map(
        which(current_y == 1),
        ~rbind(exploration_sampling(current_train[.x, ], 50, near_range))
      ) %>% 
        do.call(rbind, .)
    } else { 
      new_train <- map(1:50, ~sample_binary_vector(d)) %>% do.call(rbind, .)
    }
    current_train <- new_train
    current_y <- evaluate_train(new_train, oracle)
    full_train %<>% rbind(new_train)
    print(nrow(full_train))
  }
  
  full_train <- full_train[sample(nrow(full_train), n),]
  list(x = full_train, y = evaluate_train(full_train, oracle))
}
