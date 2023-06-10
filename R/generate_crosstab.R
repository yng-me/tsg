#' @title Generate cross-tabulation
#'
#' @description The function can generate a cross-tabulation that supports more than one (1) variable specified in the column.

#' @param .data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#'
#' @param x \strong{Required}. Row variable to be used as categories.
#' @param y Variable to be used as columns (like in pivot_wider). If not supplied, \code{generate_frequency} will used in the function call. \code{NA} will be automatically renamed to \code{Missing}.
#' @param x_group Row grouping variable/s.
#' @param y_group Column grouping variable/s.
#' @param x_label Stubhead label (first column).
#' @param y_group_separator Column separator that defines the table hierarchy.
#' @param x_as_group Use row variable as grouping.
#' @param total_by Accepts \code{row} | \code{column}. Whether to apply the sum columnwise or rowwise.
#' @param group_values_by Accepts \code{statistics} | \code{indicators}.
#' @param include_frequency Whether to include frequency columns.
#' @param include_proportion Whether to include proportion/percentage columns.
#' @param include_column_total Whether to include column total.
#' @param convert_to_percent Whether to format to percent or proportion.
#' @param format_precision Specify the precision of rounding the percent or proportion. Default is \code{2}.
#' @param total_label Whether to rename the column total.
#' @param ... Additional arguments
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
  x_label = get_config(set_as_string({{x}})),
  y_group_separator = '>',
  x_as_group = FALSE,
  total_by = 'row',
  group_values_by = 'statistics',
  include_frequency = TRUE,
  include_proportion = TRUE,
  include_column_total = TRUE,
  convert_to_percent = TRUE,
  format_precision = 2,
  exclude_zero_value = TRUE,
  total_label = NULL,
  ...
) {

  # Check the if input data is valid
  check_input_data_validity(.data)

  # set factor
  .data <- .data |> get_factor(set_as_string({{x}}), set_as_string({{y}}))

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

  # Convert .data frame to tibble format
  df <- .data |>
    select_only(x_group = x_group, y_group = y_group, {{x}}, {{y}})

  g <- set_as_string({{x}})

  # Check if x_group are defined
  if(!is.null(x_group)) {

    if(x_as_group == T) g <- c(g, x_group)
    else g <- c(x_group, g)

    df <- df |> create_group(g)
  }

  total_fn <- eval(as.name('crosstab_total'))

  # Check if y_group are defined
  if(!is.null(y_group)) {

    total_fn <- eval(as.name('crosstab_total_stack'))

    gc <- c(y_group, set_as_string({{y}}))

    df <- df |>
      create_group(c(g, gc)) |>
      dplyr::count(name = 'n') |>
      dplyr::ungroup() |>
      dplyr::arrange({{y}}) |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = dplyr::matches(paste0('^', gc, '$')),
        values_from = n,
        values_fill = 0,
        names_sep = y_group_separator,
        names_sort = T,
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
        names_expand = exclude_zero_value,
        names_sep = y_group_separator,
        names_prefix = 'pivot_'
      ) |>
      dplyr::ungroup()
  }

  df <- df |>
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

  # if(include_column_total == T) {
  #   df <- df |>
  #     dplyr::rowwise() |>
  #     dplyr::mutate(
  #       overall_total = rowSums(
  #         dplyr::across(
  #           dplyr::matches('^(Frequency|Total).*(Total|Frequency)$')
  #         ),
  #         na.rm = T
  #       )
  #     ) |>
  #     dplyr::select(
  #       dplyr::matches(paste0('^', g, '$')),
  #       Total = overall_total,
  #       dplyr::everything()
  #     )
  # }

  if(include_column_total == F) {
    df <- df |>
      dplyr::select(-dplyr::matches('^(Frequency|Total).*(>Total|>Frequency)$'))
  }

  if(!is.null(x_label)) {
    df <- df |> dplyr::rename((!!as.name(x_label)) := {{x}})
  }

  return(df)
}

