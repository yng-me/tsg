#' Generate summary table from a multiple response category
#'
#' @param .data A .data frame, .data frame extension (e.g. a tibble), a lazy .data frame (e.g. from dbplyr or dtplyr), or Arrow .data format.
#' @param ... tidyselect columns
#' @param x \strong{Required}. Row variable to be used as categories.
#' @param y Column variable to specify for a letter-coded response.
#' @param y_group_separator Column separator that defines the table hierarchy.
#' @param x_group Column grouping variable/s.
#' @param x_label Stubhead label (first column).
#' @param x_as_group Use row variable as grouping.
#' @param group_values_by Whether to group column variables by \code{statistics} or \code{indicators}.
#' @param value_to_count Value used as basis for counting the frequencies.
#' @param include_frequency Whether to include frequency columns.
#' @param include_proportion Whether to include proportion/percentage columns.
#' @param convert_to_percent Whether to format to \code{percent} or \code{proportion}.
#' @param format_precision Specify the precision of rounding the percent or proportion. Default is \code{2}.
#' @param recode Whether to recode the variable name first.
#' @param total_label Column name for the total.
#' @param clean_name Whether to output a clean column name.
#'
#' @return Returns a cross-table of type \code{tibble}
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'    category = c("G1", "G1", "G2", "G1", "G2", "G1"),
#'    response = c("AB", "AC", "B", "ABC", "AB", "C"),
#'    A = c(1, 1, 0, 1, 1, 0),
#'    B = c(1, 0, 1, 1, 1, 0),
#'    C = c(0, 1, 0, 1, 0, 1)
#'  )


generate_multiple_response <- function(
  .data,
  x,
  ...,
  y = NULL,
  x_group = NULL,
  x_label = get_config(set_as_string({{x}})),
  x_as_group = FALSE,
  y_group_separator = '>',
  group_values_by = 'statistics',
  value_to_count = 1,
  include_frequency = TRUE,
  include_proportion = TRUE,
  convert_to_percent = TRUE,
  format_precision = 2,
  total_label = NULL,
  recode = TRUE,
  clean_name = TRUE
) {

  # Check the if input data is valid
  check_input_data_validity(.data)

  type <- NULL
  n <- NULL
  `:=` <- NULL

  g_val <- tolower(group_values_by)

  get_label <- function(col, label) {
    if(g_val == 'indicators' | g_val == 'indicator') return(paste0(col, y_group_separator, label))
    else return(paste0(label, y_group_separator, col))
  }

  p_divisor <- dplyr::if_else(convert_to_percent == T, 100, 1)
  p_label <- dplyr::if_else(convert_to_percent == T, 'Percent', 'Proportion')

  df <- .data |>
    select_only(x_group, y_group = NULL, {{x}}, {{y}}, ...)

  g <- set_as_string({{x}})

  # Check if x_group are defined
  if(!is.null(x_group)) {
    if(x_as_group == T) g <- c(g, x_group)
    else g <- c(x_group, g)
  }

  join_with <- df |>
    create_group(g) |>
    dplyr::count() |>
    dplyr::collect()

  y_as_str <- set_as_string({{y}})

  if(y_as_str == 'NULL') {

    if(recode == T) {
      df <- df |>
        dplyr::rename_at(
          dplyr::vars(...),
          ~ paste0('<<', toupper(stringr::str_sub(., 5, 5)), '>>')
        ) |>
        create_group(g) |>
        dplyr::collect() |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^<<[A-Z]>>$')),
          ~ dplyr::if_else(. != as.integer(value_to_count), 0L, 1L, NA_integer_)
        ) |>
        dplyr::summarise_at(
          dplyr::vars(dplyr::matches('^<<[A-Z]>>$')),
          ~ sum(., na.rm = T)
        )

    } else {

      df <- df |>
        create_group(g) |>
        dplyr::collect() |>
        dplyr::mutate_at(
          dplyr::vars(...),
          ~ dplyr::if_else(. != as.integer(value_to_count), 0L, 1L, NA_integer_)
        ) |>
        dplyr::summarise_at(
          dplyr::vars(...),
          ~ sum(., na.rm = T)
        )
    }

  } else if(typeof(df[[y_as_str]]) == 'list') {

    df <- df |>
      tidyr::unnest({{y}}) |>
      dplyr::filter(!is.na({{y}})) |>
      create_group(g, {{y}}) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        names_from = {{y}},
        values_from = n,
        values_fill = 0,
        names_sort = F
      )

  } else {

    df <- df |>
      dplyr::mutate(type = toupper(stringr::str_trim({{y}}))) |>
      convert_to_na() |>
      dplyr::collect() |>
      dplyr::mutate(type = strsplit(type, split = '')) |>
      tidyr::unnest(type) |>
      dplyr::filter(!is.na(type), grepl('^[A-Z]$', type)) |>
      dplyr::mutate(type = paste0('<<', type, '>>')) |>
      create_group(g, type) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::arrange(type) |>
      tidyr::pivot_wider(
        names_from = type,
        values_from = n,
        values_fill = 0,
        names_sort = F
      )
  }

  df_cols <- df |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::matches(paste0('^', g, '$')), -n) |>
    names()

  df <- df |>
    dplyr::inner_join(join_with, by = g) |>
    janitor::adorn_totals() |>
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches(paste0('^', df_cols, '$'))),
      list(percent = ~ (. / n) * p_divisor)
    ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('_percent$')),
      ~ get_label(stringr::str_remove(., '_percent$'), p_label)
    ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches(paste0('^', df_cols, '$'))),
      ~ get_label(., 'Frequency')
    ) |>
    dplyr::select(dplyr::matches(paste0('^', g, '$')), n, dplyr::everything()) |>
    dplyr::rename(Total = n) |>
    crosstab_inclusion(
      y_group_separator,
      p_label,
      include_frequency,
      include_proportion
    ) |>
    dplyr::tibble()

  if(!is.null(x_label)) {
    df <- df |> dplyr::rename((!!as.name(x_label)) := {{x}})
  }

  if(!is.null(total_label)) {
    df <- df |>
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('Total')),
        ~ stringr::str_replace(., 'Total', total_label)
      )
  }

  if(clean_name == T) {
    df <- df |>
      dplyr::rename_all(~ stringr::str_remove_all(., '<<|>>'))
  }

  return(df)

}

