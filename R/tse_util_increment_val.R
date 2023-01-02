tse_util_increment_val <- function(vec) {
  c <- vec[1]
  s <- 1
  for(i in 2:length(vec)) {
    s_n <- s[i - 1]
    if(c == vec[i]) {
      s <- c(s, s_n)
    } else {
      s <- c(s, s_n + 1)
    }
    c <- vec[i]
  }
  return(s)
}
