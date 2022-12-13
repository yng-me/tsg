#' tsg_prop_inclusion
#'
#' @param data a
#' @param exclude_freq b
#' @param exclude_prop c
#' @param exclude_total d
#' @param prop_label e
#'
#' @return
#' @export
#'
#' @examples
#'

tsg_prop_inclusion <- function(
    data,
    exclude_freq = F,
    exclude_prop = F,
    exclude_total = F,
    prop_label = 'Percent'
) {

  # Check if both values of include_freq and include_prop are valid.
  if(exclude_freq == T & exclude_prop == T) {
    include_freq <- T
    warning("'include_freq' and 'include_prop' cannot be both 'FALSE'. Defaulted back to 'include_freq = TRUE'")
  }

  df <- data

  p <- paste0('\\|\\|', prop_label, '|', prop_label, '\\|\\|')
  f <- '\\|\\|Frequency|Frequency\\|\\|'

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
