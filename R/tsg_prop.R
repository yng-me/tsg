#' tsg_prop
#'
#' @param data a
#' @param var_row b
#' @param var_col c
#' @param group_rows d
#' @param group_cols e
#' @param use_var_row_as_group f
#' @param ... g
#'
#' @return
#' @export
#'
#' @examples

tsg_prop <- function(
  data,
  var_row,
  var_col,
  group_rows = NULL,
  group_cols = NULL,
  use_var_row_as_group = F,
  ...
) {

  n <- NULL
  `.` <- NULL

  # Check if the input data is a valid data frame
  if (!is.data.frame(data)) {
    stop(paste0("Data input must be a valid data frame."))
  }

  # Convert data frame to tibble format
  df <- data |> dplyr::tibble()

  # [Function] Create column grouping variable/s if defined.
  create_group <- function(d, g, ...) {
    if(is.character(g) & length(g) > 0) {
      for(i in 1:length(g)) {
        d <- d |> dplyr::group_by(!!as.name(g[i]), ..., .add = T)
      }
      return(d)
    } else {
      stop('Grouping variable is invalid.')
    }
  }

  # Convert tidy_var to string
  var_row_string <- stringr::str_remove(rlang::expr_text(rlang::enquo(var_row)), '~')
  var_col_string <- stringr::str_remove(rlang::expr_text(rlang::enquo(var_col)), '~')

  # Check if group_rows are defined
  if(!is.null(group_rows)) {
    if(use_var_row_as_group == T) {
      df <- create_group(df, c(var_row_string, group_rows))
    } else {
      df <- create_group(df, group_rows, {{var_row}})
    }
    var_row_string <- c(group_rows, var_row_string)
  }

  # Check if group_cols are defined
  if(!is.null(group_cols)) {

    df <- create_group(df, group_cols, {{var_row}}, {{var_col}}) |>
      dplyr::count() |>
      tidyr::pivot_wider(
        names_from = dplyr::matches(paste0('^', c(group_cols, var_col_string), '$')),
        values_from = n,
        values_fill = 0,
        names_sep = '||',
        names_prefix = 'pivot_'
      ) |>
      dplyr::select(dplyr::matches(paste0('^', var_row_string, '$')), order(names(.))) |>
      tsg_prop_total_stack(...)

  } else {
    df <- df |>
      dplyr::group_by({{var_row}}, .add = T) |>
      dplyr::count({{var_col}}) |>
      tidyr::pivot_wider(
        names_from = {{var_col}},
        values_from = n,
        values_fill = 0,
        names_sep = '||',
        names_prefix = 'pivot_'
      ) |>
      tsg_prop_total(...)
  }

  return(df |> tsg_prop_rename())

}
