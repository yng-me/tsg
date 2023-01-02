tsg_crosstab_inclusion <- function(
    data,
    y_group_separator,
    p_label = 'Percent',
    exclude_frequency = FALSE,
    exclude_proportion = FALSE,
    exclude_total = FALSE
) {

  # Check if both values of exclude_frequency and include_prop are valid.
  if(exclude_frequency == T & exclude_proportion == T) {
    exclude_frequency <- T
    warning("'exclude_frequency' and 'include_prop' cannot be both 'FALSE'. Defaulted back to 'exclude_frequency = TRUE'")
  }

  df <- data

  p <- paste0(y_group_separator, p_label, '|', p_label, y_group_separator)
  f <- paste0(y_group_separator, 'Frequency|Frequency', y_group_separator)

  # Check if to include frequency
  if(exclude_proportion == T) {

    df <- df |>
      dplyr::rename_at(dplyr::vars(dplyr::contains('Total')), ~ stringr::str_remove_all(., f)) |>
      dplyr::select(-dplyr::matches(p)) |>
      dplyr::rename_all(~ stringr::str_remove_all(., f))
  }

  # Check if to include percent calculation
  if(exclude_frequency == T) {
    df <- df |>
      dplyr::rename_at(
        dplyr::vars(dplyr::contains('Total')),
        ~ stringr::str_remove_all(., p)
      ) |>
      dplyr::select(-dplyr::matches(f)) |>
      dplyr::rename_all(~ stringr::str_remove_all(., p))
  }

  return(df)
}
