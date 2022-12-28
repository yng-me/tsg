tsg_get_config <- function(label) {
  x <- NULL
  if(exists('tsg_config')) {
    x <- tsg_config[[label]]
  }
  return(x)
}
