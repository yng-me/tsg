#' tsg_freq
#'
#' @param data a
#' @param var_row b
#' @param group_rows c
#' @param label d
#' @param code_ref e
#' @param sort_frequency f
#' @param use_var_row_as_group g
#' @param ... h
#'
#' @return x
#' @export
#'
#' @examples


tsg_freq <- function(
    data,
    var_row,
    group_rows = NULL,
    label = NULL,
    code_ref = NULL,
    sort_frequency = F,
    use_var_row_as_group = F,
    ...
) {

  # Percent <- NULL
  # Frequency <- NULL
  # percent <- NULL
  # n <- NULL
  # `:=` <- NULL

  # utils::globalVariables(c('Percent', 'Frequency', 'n'))

  if (!is.data.frame(data)) {
    stop(paste0("Data input must be a valid data frame."))
  }

  var_row_string <- stringr::str_remove(rlang::expr_text(rlang::enquo(var_row)), '~')
  df <- data |> janitor::tabyl({{var_row}})

  if(!is.null(group_rows)) {

    if(use_var_row_as_group == T) {
      var_row_string <- c(var_row_string, group_rows)
    } else {
      var_row_string <- c(group_rows, var_row_string)
    }

    df <- data |>
        tsg_util_create_group(var_row_string) |>
        dplyr::count() |>
        dplyr::ungroup() |>
        dplyr::mutate(percent := n / sum(n))
  }

  if(sort_frequency == T) {
    df <- df |> dplyr::arrange(dplyr::desc(n))
  }

  df <- df |> dplyr::mutate(Percent := percent * 100) |>
    dplyr::select(dplyr::matches(paste0('^', var_row_string, '$')), Frequency := n, Percent) |>
    tsg_freq_inclusion(excluded_cols = var_row_string, ...)

  if(!is.null(code_ref)) {
    df <- df |> recode_ts(code_ref)
  }

  if(!is.null(label)) {
    df <- df |> dplyr::rename((!!as.name(label)) := {{var_row}})
  }

  return(df %>% dplyr::tibble())

}










