tse_util_extract_col <- function(
  df,
  start_col,
  start_row,
  separator = '>'
) {

  value <- NULL
  n <- NULL
  col_from <- NULL
  row_from <- NULL
  data <- NULL

  dplyr::as_tibble(names(df)) |>
    dplyr::mutate(
      value = stringr::str_split(value, separator),
      col_from = 1:n()
    ) |>
    dplyr::mutate(col_from = col_from + start_col - 1) |>
    tidyr::unnest(value) |>
    dplyr::group_by(col_from) |>
    dplyr::mutate(row_from = 1:n()) |>
    dplyr::mutate(row_from = row_from + start_row - 1) |>
    tidyr::nest() |>
    dplyr::mutate(depth = purrr::map_int(data, nrow)) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::group_by(value) |>
    tidyr::nest() |>
    dplyr::mutate(r = purrr::map_int(data, nrow)) |>
    tidyr::unnest(data) |>
    dplyr::arrange(col_from)
}
