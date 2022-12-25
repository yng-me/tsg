tsg_select <- function(
    data,
    group_rows = NULL,
    group_cols = NULL,
    ...
) {
  data |> dplyr::select(
      dplyr::matches(paste0('^', group_rows, '$')),
      dplyr::matches(paste0('^', group_cols, '$')),
      ...
    )
}
