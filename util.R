common_pos <- function(x, y) {
  which(x == y)
}


list_addition <- function(l1, l2) {
  map2(l1, l2, ~.x + .y)
}


list_divide_by_constant <- function(l1, k) {
  map(l1, ~.x / k)
}


list_binarise <- function(l1) {
  map(l1, ~binarise(.x)) 
}
binarise <- function(v0) {
  as.numeric(v0 >= 0.5)
}


df_remove_last_column <- function(df0) {
  df0[, 1:(ncol(df0) - 1)]
}


df_get_last_column <- function(df0) {
  df0[, ncol(df0)]
}


c_flip_bit <- function(x) {
  if (x == 0) return(1)
  if (x == 1) return(0)
}
flip_bit <- Vectorize(c_flip_bit)


df0_flip_bit <- function(df0) {
  map(1:nrow(df0), ~df0[.x, ] %>% flip_bit) %>% 
    do.call(rbind, .)
}


which_min_distance <- function(v0) {
  which(v0 == min(v0)) %>% min()
}


rename_df <- function(df0, new_names) {
  names(df0) <- new_names
  df0
}
