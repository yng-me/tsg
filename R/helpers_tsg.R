frequency_inclusion <- function(
  .data_piped,
  excluded_cols,
  include_total,
  include_cumulative,
  exclude_zero_value
) {

  Frequency <- NULL
  Percent <- NULL

  df <- .data_piped |> dplyr::tibble()

  if(include_cumulative == T) {
    cumulative <- .data_piped |>
      dplyr::select(-dplyr::any_of(excluded_cols)) |>
      cumsum() |>
      dplyr::rename(
        'Cumulative Total' = Frequency,
        'Cumulative Percent' = Percent
      )
    df <- .data_piped |> dplyr::bind_cols(cumulative)
  }

  if(include_total == T) {
    df <- df |>
      janitor::adorn_totals(
        where = 'row',
        fill = '-',
        na.rm = T,
        name = 'Total',
        -dplyr::any_of(excluded_cols),
        -dplyr::contains('Cumulative')
      ) |>
      convert_to_na('-') |>
      dplyr::mutate_at(dplyr::vars(dplyr::contains('Cumulative')), as.numeric)
  }

  if(exclude_zero_value == T) {
    df <- df |> dplyr::filter(Frequency > 0)
  }

  return(df)

}

convert_to_na <- function(.data, value_to_be_replaced = '') {
  .data |>
    dplyr::mutate_if(
      is.character,
      ~ dplyr::if_else(
        stringr::str_trim(.) == value_to_be_replaced,
        NA_character_,
        .
      )
    )
}

