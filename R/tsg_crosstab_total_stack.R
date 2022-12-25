tsg_crosstab_total_stack <- function(
    data,
    separator,
    ...
) {

  value <- NULL

  cols <- dplyr::as_tibble(names(data)) |>
    dplyr::filter(grepl('pivot_', value))

  ex_cols <- dplyr::as_tibble(names(data)) |>
    dplyr::filter(!grepl('pivot_', value)) |>
    dplyr::pull(value)

  ex_col_names <-  paste0('^', ex_cols, '$')

  x <- cols |>
    dplyr::mutate(value = stringr::str_remove(value, 'pivot_')) |>
    dplyr::mutate(value = stringr::str_remove(value,  paste0(separator, '.*$') )) |>
    dplyr::distinct() |>
    dplyr::pull(value)

  df <- data |>
    dplyr::select(
      dplyr::matches(ex_col_names),
      dplyr::starts_with(paste0('pivot_', x[1], separator))
    ) |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(ex_cols)), as.character) |>
    tsg_crosstab_total(separator = separator, total_label = x[1], ...) |>
    dplyr::tibble()

  for(i in 2:length(x)) {

    df_y <- data |>
      dplyr::select(
        dplyr::matches(ex_cols),
        dplyr::starts_with(paste0('pivot_', x[i], separator))
      ) |>
      dplyr::mutate_at(dplyr::vars(dplyr::matches(ex_cols)), as.character) |>
      tsg_crosstab_total(separator = separator, total_label = x[i], ...) |>
      dplyr::select(-dplyr::matches(ex_cols)) |>
      dplyr::tibble()

    df <- df |>  tibble::add_column(df_y)
  }

  return(df)

}
