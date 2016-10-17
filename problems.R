# ===== Problem 1 =============================================================
init_binary_ftn_1 <- function(z, threshold) {
  return( function(x) {
    exceed_threshold <- sum(xor(x, z)) > threshold
    as.numeric(exceed_threshold)  
  } )
}


init_oracle_1 <- function(d, z, threshold) {
  if (missing(z)) 
    z <- sample(c(0,1), d, replace = TRUE)
  
  if (missing(threshold)) 
    threshold <- sample(0:d, 1)
  
  list(z = z, threshold = threshold, 
       FUN = init_binary_ftn_1(z, threshold))
}


# ===== Problem 2 =============================================================
init_binary_ftn_2 <- function() {}
init_oracle_2 <- function(n = 128, num_train = 1000) {
  nn_train <- create_train_data(n = num_train, d = n)
  # nn_y <- init_oracle_1(d = n, threshold = sample(53:75, 1)) %>% 
    # evaluate_train(nn_train, .)
  nn_y <- sample(c(0,1), num_train, replace = TRUE)
  nn_data <- data.frame(x = nn_train, y = nn_y)
  nn_form <- nn_data %>% names() %>% head(-1) %>% 
    paste(collapse = " + ") %>% paste("y ~", .)
  oracle_2 <- neuralnet(nn_form, nn_data, hidden = c(64, 32, 16, 8, 4, 2),
                        act.fct = "logistic", linear.output = FALSE)
  return(
    list(FUN = function(newx) {
      if (is.null(dim(newx))) 
        return(
          compute(oracle_2, t(newx)) %>% 
            `$`("net.result") %>% 
            `>=`(0.5) %>% 
            as.numeric() 
          )
      compute(oracle_2, newx) %>% 
        `$`("net.result") %>% 
        `>=`(0.5) %>% 
        as.numeric() 
    })
  )
}


# ===== Problem 3 =============================================================
init_binary_ftn_3 <- function(dict, resp) {
  return( function(x) {
    ret_index <- 1:nrow(dict) %>% 
      map_dbl(~sum(x != dict[.x, ])) %>% 
      which_min_distance()
    resp[ret_index]
  } )
}


init_oracle_3 <- function(n = 64, m = 10, dict, resp) {
  if (missing(dict)) 
    dict <- c(0,1) %>% 
      sample(n * m, replace = TRUE) %>% 
      matrix(ncol = n, nrow = m)
  
  if (missing(resp)) 
    resp <- c(0,1) %>% sample(m, replace = TRUE)
  
  list(dict = dict, resp = resp,
       FUN = init_binary_ftn_3(dict, resp))
}
