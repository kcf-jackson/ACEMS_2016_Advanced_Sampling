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


exploration_train_data <- function(n, d, batch = 100, oracle) {
  my_train <- create_train_data(batch, d = d)
  my_y <- evaluate_train(my_train, oracle)
  
  full_train <- current_train <- my_train
  current_y <- my_y
  while(nrow(full_train) < n) {
    new_train <- map(
      which(current_y == 1),
      ~rbind(exploration_sampling(current_train[i, ], 50, 20))
    ) %>% 
      do.call(rbind, .)
    current_y <- evaluate_train(new_train, oracle)
    full_train %<>% rbind(new_train)
    print(nrow(full_train))
  }
  
  full_train <- full_train[seq(n), ]
  list(x = full_train, y = evaluate_train(full_train, oracle))
}