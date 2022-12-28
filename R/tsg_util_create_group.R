tsg_util_create_group <- function(.d, g, ...) {
  if(is.character(g) & length(g) > 0) {
    for(i in 1:length(g)) {
      .d <- .d |> dplyr::group_by(!!as.name(g[i]), ..., .add = T)
    }
    return(.d)
  } else {
    stop('Grouping variable is invalid.')
  }
}
