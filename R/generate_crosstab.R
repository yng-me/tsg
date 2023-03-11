#' @title Generate cross-tabulation
#'
#' @description This function extends the functionality of \code{generate_frequency} by allowing you to generate cross-tabulations of two (2) or more categorical variables.

#' @param .data A data frame, data frame extension (e.g. a \code{tibble}), a lazy data frame (e.g. from \code{dbplyr} or \code{dtplyr}), or Arrow data format.
#' @param x \strong{Required}. Variable to be used as categories.
#' @param y Variable to be used as columns (like in \code{pivot_wider}). If not supplied, \code{generate_frequency} will used in the function call. \code{NA} will be automatically renamed to \code{Missing}.
#' @param x_group Row grouping variable/s.
#' @param y_group Column grouping variable/s.
#' @param x_label Stubhead label (first column).
#' @param y_group_separator Column separator that defines the table hierarchy. Default is \code{>}.
#' @param x_as_group Use \code{x} variable as top level grouping.
#' @param total_by Accepts \code{row} | \code{column}. Whether to apply the sum columnwise or rowwise.
#' @param group_values_by Accepts \code{statistics} | \code{indicators}.
#' @param include_frequency Whether to include frequency columns. Default is \code{TRUE}.
#' @param include_proportion Whether to include proportion/percentage columns. Default is \code{TRUE}.
#' @param include_column_total Whether to include column total. Default is \code{TRUE}.
#' @param include_row_total Whether to include row total. Default is \code{TRUE}.
#' @param convert_to_percent Whether to format to percent or proportion. Default is \code{TRUE}.
#' @param format_precision Specify the precision of rounding the percent or proportion. Default is \code{2}.
#' @param total_label Whether to rename the column total.
#' @param ... Additional agruments.
#'
#' @return Returns a cross-table of type \code{tibble}
#'
#' @export
#'
#' @examples
#'
#' dplyr::starwars |> generate_crosstab(species, sex)


generate_crosstab <- function(
  .data,
  x,
  y = NULL,
  x_group = NULL,
  y_group = NULL,
  x_label = get_config('x_label'),
  x_as_group = FALSE,
  y_group_separator = '>',
  total_by = 'row',
  group_values_by = 'statistics',
  include_frequency = TRUE,
  include_proportion = TRUE,
  include_column_total = TRUE,
  include_row_total = TRUE,
  convert_to_percent = TRUE,
  format_precision = 2,
  total_label = NULL,
  ...
) {

  # Check the if input data is valid
  check_input_data_validity(.data)

  # If no argument passed to y, `generate_frequency` will be invoked
  if(set_as_string({{y}}) == 'NULL') {
    return(
      .data |>
        generate_frequency(
          x = {{x}},
          x_group = x_group,
          x_label = x_label,
          x_as_group = x_as_group,
          ...
        )
    )
  }

  overall_total <- NULL
  n <- NULL
  `.` <- NULL
  `:=` <- NULL
  stub_var <- NULL

  # Convert .data frame to tibble format and select only specified columns for efficiency
  df <- .data |>
    select_only(
      x_group = x_group,
      y_group = y_group,
      {{x}},
      {{y}}
    )

  # Set first variable as grouping
  grouping_row_vars <- set_as_string({{x}})

  # Check if x_group is defined
  if(!is.null(x_group)) {

    if(x_as_group == T) grouping_row_vars <- c(grouping_row_vars, x_group)
    else grouping_row_vars <- c(x_group, grouping_row_vars)

    # Use to identify first variable returned in the table

    df <- df |> create_group(grouping_row_vars)
  }

  total_fn <- eval(as.name('get_crosstab_total'))

  stub_var <- grouping_row_vars[1]

  # Check if y_group are defined
  if(!is.null(y_group)) {

    total_fn <- eval(as.name('get_crosstab_total_stack'))

    grouping_col_vars <- c(y_group, set_as_string({{y}}))

    df <- df |>
      create_group(c(grouping_row_vars, grouping_col_vars)) |>
      dplyr::count(name = 'n') |>
      dplyr::ungroup() |>
      dplyr::arrange({{y}}) |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = dplyr::matches(paste0('^', grouping_col_vars, '$')),
        values_from = n,
        names_sort = T,
        values_fill = 0,
        names_sep = y_group_separator,
        names_prefix = 'pivot_'
      )

  } else {

    df <- df |>
      dplyr::group_by({{x}}, .add = T) |>
      dplyr::count({{y}}) |>
      dplyr::collect() |>
      dplyr::arrange({{y}}) |>
      tidyr::pivot_wider(
        names_from = {{y}},
        values_from = n,
        names_sort = T,
        values_fill = 0,
        names_sep = y_group_separator,
        names_prefix = 'pivot_'
      )

  }

  df <- df |>
    dplyr::ungroup() |>
    total_fn(
      y_group_separator,
      total_by,
      group_values_by,
      convert_to_percent,
      format_precision,
      total_label,
      include_frequency,
      include_proportion
    ) |>
    dplyr::tibble() |>
    crosstab_rename(y_group_separator = y_group_separator)


  if(include_column_total == F) {
    df <- df |>
      dplyr::select(-dplyr::matches('^(Frequency|Total).*(>Total|>Frequency)$'))
  }

  if(include_row_total == F & !is.null(stub_var)) {
    df <- df |>
      dplyr::filter(!!as.name(stub_var) != 'Total')
  }

  if(!is.null(x_label)) {
    df <- df |> dplyr::rename((!!as.name(x_label)) := {{x}})
  }


  return(df)
}

