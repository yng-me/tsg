tsg_select <- function(
    data,
    x_group = NULL,
    y_group = NULL,
    ...
) {
  data |> dplyr::select(
      dplyr::matches(paste0('^', x_group, '$')),
      dplyr::matches(paste0('^', y_group, '$')),
      ...
    )
}
