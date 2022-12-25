#' @title Generate cross-tabulation
#'
#' @description The function can generate a cross-tabulation that supports more than one (1) variable specified in the column.

#' @param data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#'
#' @param var_row \strong{Required}. Row variable to be used as categories.
#' @param var_col \strong{Required}. Column variable.
#' @param group_rows Row grouping variable/s.
#' @param group_cols Column grouping variable/s.
#' @param use_var_row_as_group Use row variable as grouping.
#' @param separator Column separator that defines the table hierarchy.
#' @param ... Accepts valid arguments for \code{tsg_crosstab_inclusion}.
#'
#' @return Returns a cross-table of type \code{tibble}
#'
#' @export
#'
#' @examples


tsg_crosstab <- function(
  data,
  var_row,
  var_col,
  group_rows = NULL,
  group_cols = NULL,
  use_var_row_as_group = FALSE,
  separator = '>',
  ...
) {

  n <- NULL
  `.` <- NULL

  # Check if the input data is a valid data frame
  if (!is.data.frame(data) && !is.data.frame(dplyr::collect(data))) {
    stop(paste0("Data input must be a valid data frame or Arrow format."))
  }

  # Convert data frame to tibble format
  df <- data |>
    tsg_select(group_rows = group_rows, group_cols = group_cols, {{var_row}}, {{var_col}})

  as_string <- function(to_str) {
    stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
  }

  g <- as_string({{var_row}})

  # Check if group_rows are defined
  if(!is.null(group_rows)) {

    if(use_var_row_as_group == T) {
      g <- c(g, group_rows)
    } else {
      g <- c(group_rows, g)
    }

    df <- df |> tsg_util_create_group(g)
  }

  # Check if group_cols are defined
  if(!is.null(group_cols)) {

    gc <- c(group_cols, as_string({{var_col}}))

    df <- df |>
      tsg_util_create_group(c(g, gc)) |>
      dplyr::count(name = 'n') |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = dplyr::matches(paste0('^', gc, '$')),
        values_from = n,
        values_fill = 0,
        names_sep = separator,
        names_prefix = 'pivot_'
      )

    df_names_sorted <- paste0('^', stringr::str_subset(sort(names(df)), separator), '$')

    df <- df |>
      dplyr::select(dplyr::matches(paste0('^', g, '$')), dplyr::matches(df_names_sorted)) |>
      tsg_crosstab_total_stack(separator = separator, ...)

  } else {
    df <- df |>
      dplyr::group_by({{var_row}}, .add = T) |>
      dplyr::count({{var_col}}) |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = {{var_col}},
        values_from = n,
        values_fill = 0,
        names_sep = separator,
        names_prefix = 'pivot_'
      ) |>
      dplyr::ungroup() |>
      tsg_crosstab_total(separator = separator, ...)
  }

  df <- df |>
    dplyr::tibble() |>
    tsg_crosstab_rename(separator = separator)

  return(df)
}

