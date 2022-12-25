tsg_frequency_inclusion <- function(
    data,
    excluded_cols,
    exclude_total = FALSE,
    exclude_cumulative = FALSE,
    exclude_zero_value = TRUE
) {

  Frequency <- NULL
  Percent <- NULL

  df <- data |> dplyr::tibble()

  if(exclude_cumulative == F) {
    cumulative <- data |>
      dplyr::select(!dplyr::matches(paste0('^', excluded_cols, '$'))) |>
      cumsum() |>
      dplyr::rename(
        'Cumulative Total' = Frequency,
        'Cumulative Percent' = Percent
      )
    df <- data |> dplyr::bind_cols(cumulative)
  }

  if(exclude_total == F) {
    df <- df |>
      janitor::adorn_totals(
        where = 'row',
        fill = '-',
        na.rm = T,
        name = 'Total',
        -dplyr::matches(paste0('^', excluded_cols, '$')),
        -dplyr::contains('Cumulative')
      ) |>
      dplyr::na_if('-') |>
      dplyr::mutate_at(dplyr::vars(dplyr::contains('Cumulative')), as.numeric)
  }

  if(exclude_zero_value == T) {
    df <- df |> dplyr::filter(Frequency > 0)
  }

  return(df)

}
