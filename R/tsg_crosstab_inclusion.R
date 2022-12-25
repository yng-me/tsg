tsg_crosstab_inclusion <- function(
    data,
    separator,
    p_label = 'Percent',
    exclude_freq = FALSE,
    exclude_prop = FALSE,
    exclude_total = FALSE
) {

  # Check if both values of exclude_freq and include_prop are valid.
  if(exclude_freq == T & exclude_prop == T) {
    exclude_freq <- T
    warning("'exclude_freq' and 'include_prop' cannot be both 'FALSE'. Defaulted back to 'exclude_freq = TRUE'")
  }

  df <- data

  p <- paste0(separator, p_label, '|', p_label, separator)
  f <- paste0(separator, 'Frequency|Frequency', separator)

  # Check if to include frequency
  if(exclude_prop == T) {

    df <- df |>
      dplyr::rename_at(dplyr::vars(dplyr::contains('Total')), ~ stringr::str_remove_all(., f)) |>
      dplyr::select(-dplyr::matches(p)) |>
      dplyr::rename_all(~ stringr::str_remove_all(., f))
  }

  # Check if to include percent calculation
  if(exclude_freq == T) {
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
