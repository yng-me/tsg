#' Generate summary table from a multiple response category
#'
#' @param .data A .data frame, .data frame extension (e.g. a tibble), a lazy .data frame (e.g. from dbplyr or dtplyr), or Arrow .data format.
#' @param x \strong{Required}. Row variable to be used as categories.
#' @param ... Columns with binary-coded response. Use tidyselect specification.
#' @param y Column variable to specify for a letter-coded response.
#' @param y_group_separator Column separator that defines the table hierarchy.
#' @param x_group Column grouping variable/s.
#' @param x_label Stubhead label (first column).
#' @param x_as_group Use row variable as grouping.
#' @param group_values_by Whether to group column variables by \code{statistics} or \code{indicators}.
#' @param format_to_percent Whether to format to \code{percent} or \code{proportion}.
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
#'
#' df |> tsg_crosstab_multi_response(category, A:C)
#' df |> tsg_crosstab_multi_response(category, y = response)


tsg_crosstab_multi_response <- function(
    .data,
    x,
    ...,
    y = NULL,
    x_group = NULL,
    x_label = get_config('x_label'),
    x_as_group = FALSE,
    y_group_separator = '>',
    format_to_percent = TRUE,
    group_values_by = 'statistics'
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

  p_divisor <- dplyr::if_else(format_to_percent == T, 100, 1)
  p_label <- dplyr::if_else(format_to_percent == T, 'Percent', 'Proportion')

  df <- .data |>
    select_only(x_group, y_group = NULL, {{x}}, {{y}}, ...)

  g <- set_as_string({{x}})

  # Check if x_group are defined
  if(!is.null(x_group)) {

    if(x_as_group == T) {
      g <- c(g, x_group)
    } else {
      g <- c(x_group, g)
    }
  }

  join_with <- df |>
    create_group(g) |>
    dplyr::count() |>
    dplyr::collect()

  y_as_str <- set_as_string({{y}})
  if(y_as_str == 'NULL') {
    df <- df |>
      create_group(g) |>
      dplyr::collect() |>
      dplyr::mutate_at(dplyr::vars(...), ~ dplyr::if_else(. > 1, 0L, as.integer(.))) |>
      dplyr::summarise_at(dplyr::vars(...), ~ sum(., na.rm = T))

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
      dplyr::na_if('') |>
      dplyr::collect() |>
      dplyr::mutate(type = strsplit(type, split = '')) |>
      tidyr::unnest(type) |>
      dplyr::filter(!is.na(type), grepl('^[A-Z]$', type)) |>
      create_group(g, type) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        names_from = type,
        values_from = n,
        values_fill = 0,
        names_sort = F
      )
  }

  df_cols <- df |>
    dplyr::select(-1, -n) |>
    names()

  total_name <- get_label('Total', 'Frequency')

  df <- df |>
    dplyr::inner_join(join_with, by = g) |>
    janitor::adorn_totals() |>
    dplyr::mutate_at(
      dplyr::vars(-c(1, n)),
      list(percent = ~ (. / n) * p_divisor)
    ) |>
    dplyr::select(1, n, dplyr::everything()) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('_percent$')),
      ~ get_label(stringr::str_remove(., '_percent$'), p_label)
    ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches(paste0('^', df_cols, '$'))),
      ~ get_label(., 'Frequency')
    ) |>
    dplyr::rename((!!as.name(total_name)) := n) |>
    dplyr::tibble()

  if(!is.null(x_label)) {
    df <- df |> dplyr::rename((!!as.name(x_label)) := {{x}})
  }

  return(df)

}

