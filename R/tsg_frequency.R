#' @title Generate frequency table
#' @description Generate a frequency table with optional cumulative total and percent.
#'
#' @param data \strong{Required}. A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param var_row \strong{Required}. Variable to be used as categories.
#' @param group_rows \strong{Required}. Accepts a vector of string/character as grouping variables.
#' @param group_cols Column grouping variable/s.
#' @param label Accepts a string or character value to be used as label for variable or grouping variable.
#' @param code_ref Code reference that will be used to replace the coded value with proper label.
#' @param sort_frequency Whether to sort the output. It accepts \code{TRUE} (descending) | \code{FALSE} (ascending). The default is \code{FALSE}
#' @param use_var_row_as_group Use row variable as grouping.
#' @param ... Accepts valid arguments for \code{tsg_frequency_inclusion}.
#'
#' @return Returns a frequency table of type \code{tibble} with optional cumulative total and percent.
#' @export
#'
#' @examples
#' mtcars_cyl_freq <- mtcars |>
#'   tsg_frequency(cyl)
#'
#' mtcars_cyl_freq
#'

tsg_frequency <- function(
    data,
    var_row,
    group_rows = NULL,
    group_cols = NULL,
    label = NULL,
    code_ref = NULL,
    sort_frequency = FALSE,
    use_var_row_as_group = FALSE,
    ...
) {

  Percent <- NULL
  Frequency <- NULL
  percent <- NULL
  n <- NULL
  `:=` <- NULL

  if (!is.data.frame(data) && !is.data.frame(dplyr::collect(data))) {
    stop(paste0("Data input must be a valid data frame."))
  }

  var_row_string <- stringr::str_remove(rlang::expr_text(rlang::enquo(var_row)), '~')

  df <- data |>
    # tsg_select(group_rows = group_rows, group_cols = group_cols, {{var_row}}) |>
    dplyr::count({{var_row}}) |>
    dplyr::collect() |>
    dplyr::mutate(percent = n / sum(n))

  if(!is.null(group_rows)) {

    if(use_var_row_as_group == T) {
      var_row_string <- c(var_row_string, group_rows)
    } else {
      var_row_string <- c(group_rows, var_row_string)
    }

    df <- data |>
      tsg_select(group_rows = group_rows, group_cols = group_cols, {{var_row}}) |>
      tsg_util_create_group(var_row_string) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      dplyr::mutate(percent = n / sum(n))
  }

  if(sort_frequency == T) {
    df <- df |> dplyr::arrange(dplyr::desc(n))
  }

  df <- df |> dplyr::mutate(Percent := percent * 100) |>
    dplyr::select(
      dplyr::matches(paste0('^', var_row_string, '$')),
      Frequency := n,
      Percent
    ) |>
    tsg_frequency_inclusion(excluded_cols = var_row_string, ...)

  if(!is.null(label)) {
    df <- df |> dplyr::rename((!!as.name(label)) := {{var_row}})
  }

  return(df |> dplyr::tibble())

}










