#' @title Generate cross-tabulation
#'
#' @description The function can generate a cross-tabulation that supports more than one (1) variable specified in the column.

#' @param .data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param x \strong{Required}. Row variable to be used as categories.
#' @param y \strong{Required}. Column variable.
#' @param x_group Row grouping variable/s.
#' @param y_group Column grouping variable/s.
#' @param x_label Stubhead label (first column).
#' @param y_group_separator Column separator that defines the table hierarchy.
#' @param x_as_group Use row variable as grouping.
#' @param ... Accepts valid arguments for \code{tsg_crosstab_inclusion}.
#'
#' @return Returns a cross-table of type \code{tibble}
#'
#' @export
#'
#' @examples
#'
#' dplyr::starwars |> tsg_crosstab(species, sex)


tsg_crosstab <- function(
  .data,
  x,
  y,
  x_group = NULL,
  y_group = NULL,
  x_label = tsg_get_config('x_label'),
  y_group_separator = '>',
  x_as_group = FALSE,
  ...
) {

  n <- NULL
  `.` <- NULL
  `:=` <- NULL

  # Check if the input .data is a valid .data frame
  if (!is.data.frame(.data) && !is.data.frame(dplyr::collect(.data))) {
    stop(paste0(".data input must be a valid .data frame or Arrow format."))
  }

  # Convert .data frame to tibble format
  df <- .data |>
    tsg_select(x_group = x_group, y_group = y_group, {{x}}, {{y}})

  as_string <- function(to_str) {
    stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
  }

  g <- as_string({{x}})

  # Check if x_group are defined
  if(!is.null(x_group)) {

    if(x_as_group == T) {
      g <- c(g, x_group)
    } else {
      g <- c(x_group, g)
    }

    df <- df |> tsg_util_create_group(g)
  }

  # Check if y_group are defined
  if(!is.null(y_group)) {

    gc <- c(y_group, as_string({{y}}))

    df <- df |>
      tsg_util_create_group(c(g, gc)) |>
      dplyr::count(name = 'n') |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = dplyr::matches(paste0('^', gc, '$')),
        values_from = n,
        values_fill = 0,
        names_sep = y_group_separator,
        names_prefix = 'pivot_'
      )

    df_names_sorted <- paste0('^', stringr::str_subset(sort(names(df)), y_group_separator), '$')

    df <- df |>
      dplyr::select(dplyr::matches(paste0('^', g, '$')), dplyr::matches(df_names_sorted)) |>
      tsg_crosstab_total_stack(y_group_separator = y_group_separator, ...)

  } else {
    df <- df |>
      dplyr::group_by({{x}}, .add = T) |>
      dplyr::count({{y}}) |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = {{y}},
        values_from = n,
        values_fill = 0,
        names_sep = y_group_separator,
        names_prefix = 'pivot_'
      ) |>
      dplyr::ungroup() |>
      tsg_crosstab_total(y_group_separator = y_group_separator, ...)
  }

  df <- df |>
    dplyr::tibble() |>
    tsg_crosstab_rename(y_group_separator = y_group_separator)

  if(!is.null(x_label)) {
    df <- df |> dplyr::rename((!!as.name(x_label)) := {{x}})
  }

  return(df)
}

