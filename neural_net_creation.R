create_network <- function(n = 128, threshold = 2, threshold_prob = 0.6, 
                           num_nodes = 1) {
  m <- log(n) / log(2) # number of layers
  
  threshold_network <- map(seq(m + 1), function(i) {
    arr = numeric(2^(m - i + 1))
    if (i > 1) {
      arr <- arr + 1
      for (j in seq(length(arr))) {
        if (runif(1) <= threshold_prob) {
          arr[j] <- threshold
        }
      }
    }
    arr
  })
  
  connection_network <- 
    map(1:m, function(level) {
      mylevel <- threshold_network[[level]]
      nextlevel <- threshold_network[[level+1]]
      l1sz <- length(mylevel)
      l2sz <- length(nextlevel)
      connection_arr <- matrix(0, nrow = l1sz, ncol = l2sz)
      if (l2sz <= 2) {
        connection_arr <- matrix(1, nrow = l1sz, ncol = l2sz)
      } else {
        for (j in 1:l1sz) {
          on_node <- sample(l2sz, num_nodes)
          connection_arr[j, on_node] <- 1
        }
      }
      connection_arr
    })
  
  list(threshold = threshold_network, connection = connection_network)
}


evaluate_network <- function(network, x, n, m) {
  if (missing(n)) n <- length(network[[1]][[1]])
  if (missing(m)) m <- log(n) / log(2)
  
  threshold <- network[[1]]
  connections <- network[[2]]
  for (i in 1:m) {
    threshold_vec <- threshold[[i + 1]]
    conn_matrix <- connections[[i]]
    # print(rbind(
    #   as.vector(t(conn_matrix) %*% as.matrix(x)),
    #   threshold_vec,
    #   as.numeric(t(conn_matrix) %*% as.matrix(x) >= threshold_vec))
    # )
    x <- as.numeric(t(conn_matrix) %*% as.matrix(x) >= threshold_vec)
  }
  x
}
