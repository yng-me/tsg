#' tsg_list
#'
#' @param data a
#' @param list e
#' @param var_row b
#' @param ... c
#' @param fn d
#' @param list_name_overall f
#' @param exclude_overall g
#' @param label_var_row h
#' @param label_list i
#'
#' @return
#' @export
#'
#' @examples

tsg_list <- function(
    data,
    list_group,
    var_row,
    ...,
    fn = 'tsg_freq',
    list_name_overall = 'ALL',
    collapse_overall = F,
    exclude_overall = F
) {

  # `:=` <- NULL

  valid_fn <- c('tsg_prop', 'tsg_freq')

  if(!(fn %in% valid_fn)) {
    fn <- 'tsg_freq'
    warning("You have entered invalid agrument for 'func' parameter. It only accepts: 'tsg_prop' | 'tsg_freq'")
  }

  # as_string <- function(to_str) {
  #   stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
  # }

  f <- eval(as.name(fn))

  list_names <- dplyr::distinct(data, {{list_group}}) |>
    dplyr::pull({{list_group}})

  df <- list()

  if(exclude_overall == F) {

    if(collapse_overall == T) {
      df_all <- data |>
        f({{var_row}}, use_var_row_as_group = !collapse_overall, ...)

    } else {
      df_all <- data |>
        dplyr::select(-{{var_row}}) |>
        f({{list_group}}, use_var_row_as_group = !collapse_overall, ...)

    }

    df[[list_name_overall]] <- df_all

  }

  for(i in 1:length(list_names)) {
    df_d <- data |>
      dplyr::filter({{list_group}} == list_names[i]) |>
      dplyr::select(-{{list_group}})

    if(nrow(df_d) > 0) {
      df[[list_names[i]]] <- df_d |> f({{var_row}}, ...)
    }
  }

  return(df)
}
