# ===== Problem 1 =============================================================
init_oracle_1 <- function(d, z, threshold) {
  if (missing(z)) 
    z <- sample(c(0,1), d, replace = TRUE)
  
  if (missing(threshold)) 
    threshold <- sample(0:d, 1)
  
  list(z = z, threshold = threshold, 
       FUN = init_binary_ftn_1(z, threshold))
}
init_binary_ftn_1 <- function(z, threshold) {
  return( function(x) {
    exceed_threshold <- sum(xor(x, z)) > threshold
    as.numeric(exceed_threshold)  
  } )
}


# ===== Problem 2 =============================================================
init_oracle_2 <- function(d = 128, threshold = 2, threshold_prob = 0.6,
                          num_nodes = 1) {
  nn <- create_network(n = d, threshold = threshold, 
                       threshold_prob = threshold_prob,
                       num_nodes = num_nodes) 
  return(list(
    model = nn,
    FUN = function(x) {
      myX <- as.matrix(x)
      map_dbl(1:nrow(myX), ~evaluate_network(nn, myX[.x, ]))
    }))
}
source("neural_net_creation.R")


# ===== Problem 3 =============================================================
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


init_binary_ftn_3 <- function(dict, resp) {
  return( function(x) {
    ret_index <- 1:nrow(dict) %>% 
      map_dbl(~sum(x != dict[.x, ])) %>% 
      which_min_distance()
    resp[ret_index]
  } )
}
