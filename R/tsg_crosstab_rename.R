tsg_crosstab_rename <- function(
    data,
    replace_na_with = 'Missing',
    y_group_separator
) {

  `.` <- NULL

  df <- data |> dplyr::rename_at(
    dplyr::vars(dplyr::contains('pivot_')),
    ~ stringr::str_remove_all(., 'pivot_|_pl')
  ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches(paste0(y_group_separator, 'NA|NA', y_group_separator))),
      ~ stringr::str_remove(stringr::str_replace(., 'NA', replace_na_with), 'NA')
    )

  df_names_sorted <- paste0('^', stringr::str_subset(sort(names(df)), y_group_separator), '$')

  df <- df |>
    dplyr::select(!dplyr::contains(y_group_separator), dplyr::matches(df_names_sorted)) |>
    # dplyr::rename_all(~ stringr::str_remove_all(., '^\\d+ - ')) |>
    dplyr::tibble()

  return(df)
}

