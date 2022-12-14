#' Title
#'
#' @param data
#' @param var_row
#' @param var_col
#' @param group_rows
#' @param group_cols
#' @param use_var_row_as_group
#' @param ...
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

  # n <- NULL
  `.` <- NULL

  # Check if the input data is a valid data frame
  if (!is.data.frame(data)) {
    stop(paste0("Data input must be a valid data frame."))
  }

  # Convert data frame to tibble format
  df <- data |> dplyr::tibble()

  as_string <- function(to_str) {
    stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
  }

  g <- as_string({{var_row}})

  # Check if group_rows are defined
  if(!is.null(group_rows)) {

    g <- dplyr::if_else(
      use_var_row_as_group == T,
      c(g, group_rows),
      c(group_rows, g)
    )

    df <- df |> tsg_util_create_group(g)
  }

  # Check if group_cols are defined
  if(!is.null(group_cols)) {

    gc <- c(group_cols, as_string({{var_col}}))

    df <- df |>
      tsg_util_create_group(c(g, gc)) |>
      dplyr::count(name = 'n') |>
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from = dplyr::matches(paste0('^', gc, '$')),
        values_from = n,
        values_fill = 0,
        names_sep = '||',
        names_prefix = 'pivot_'
      )

    df_names_sorted <- paste0('^', stringr::str_subset(sort(names(df)), '||'), '$')

    df <- df %>%
      dplyr::select(dplyr::matches(df_names_sorted)) |>
      dplyr::select(dplyr::matches(paste0('^', g, '$')), everything()) |>
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
      dplyr::ungroup() |>
      tsg_prop_total(...)
  }

  return(df |> tsg_prop_rename())

}

