#' tsg_list
#'
#' @param data a
#' @param var_row b
#' @param ... c
#' @param fn d
#' @param list_grouping e
#' @param list_name_overall f
#' @param exclude_overall g
#' @param label_var_row h
#' @param label_list_grouping i
#'
#' @return
#' @export
#'
#' @examples

tsg_list <- function(
    data,
    var_row,
    ...,
    fn = 'tsg_freq',
    list_grouping = NULL,
    list_name_overall = 'ALL',
    exclude_overall = F,
    label_var_row = NULL,
    label_list_grouping = NULL
) {

  valid_fn <- c('tsg_prop', 'tsg_freq')

  if(!(fn %in% valid_fn)) {
    fn <- 'tsg_prop'
    warning("You have entered invalid agrument for 'func' parameter. It only accepts: 'tsg_prop' | 'tsg_freq'")
  }

  f <- eval(as.name(fn))

  g <-stringr::str_remove(rlang::expr_text(rlang::enquo(list_grouping)), '~')

  if(g != 'NULL') {

    list_names <- dplyr::distinct(data, {{list_grouping}}) |>
      dplyr::pull({{list_grouping}})

    df <- list()

    if(exclude_overall == F) {

      df[[list_name_overall]] <- data |>
        dplyr::select(-{{var_row}}) |>
        f({{list_grouping}}, use_var_row_as_group = T, ...) |>
        tsg_util_rename_label(label_list_grouping, list_grouping)
    }

    for(i in 1:length(list_names)) {

      df_d <- data |>
        dplyr::filter({{list_grouping}} == list_names[i]) |>
        dplyr::select(-dplyr::matches(paste0('^', g, '$')))

      if(nrow(df_d) > 0) {
        df[[list_names[i]]] <- df_d |>
          f({{var_row}}, ...) |>
          tsg_util_rename_label(label_var_row, var_row)
      }

    }
  } else {
    df <- data |> f({{var_row}}, ...)
  }

  return(df)
}
