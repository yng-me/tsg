tsg_get_config <- function(label) {

  x <- NULL

  if(exists('tsg_config')) {
    config <- eval(as.name('tsg_config'))
    x <- config[[label]]
  }
  return(x)
}
