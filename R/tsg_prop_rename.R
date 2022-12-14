#' tsg_prop_rename
#'
#' @param data a
#' @param replace_na_with b
#'
#' @return
#' @export
#'
#' @examples


tsg_prop_rename <- function(
    data,
    replace_na_with = 'Missing'
) {

  `.` <- NULL

  df <- data |> dplyr::rename_at(
    dplyr::vars(dplyr::contains('pivot_')),
    ~ stringr::str_remove_all(., 'pivot_|_pl')
  ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('\\|\\|NA|NA\\|\\|')),
      ~ stringr::str_remove(stringr::str_replace(., 'NA', replace_na_with), 'NA')
    )

  df_names_sorted <- paste0('^', stringr::str_subset(sort(names(df)), '||'), '$')

  df <- df |>
    dplyr::select(!dplyr::contains('||'), dplyr::matches(df_names_sorted)) |>
    # dplyr::rename_all(~ stringr::str_remove_all(., '^\\d+ - ')) |>
    dplyr::tibble()
}
