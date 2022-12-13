tsg_util_get_label <- function(col, g_val, label) {
  if(g_val == 'indicators' | g_val == 'indicator') {
    p <- paste0(col, '||', label)
  } else {
    p <- paste0(label, '||', col)
  }
  return(p)
}
