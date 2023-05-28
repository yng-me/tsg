#' Generate summary table from a multiple response category
#'
#' @param .data A .data frame, .data frame extension (e.g. a tibble), a lazy .data frame (e.g. from dbplyr or dtplyr), or Arrow .data format.
#' @param x \strong{Required}. Row variable to be used as categories.
#' @param ... tidyselect columns. If only one variable is specified, it will be treated as letter-coded response; otherwise, it will be parsed as columns with binary-coded response.
#' @param value_to_count Value used as basis for counting the frequencies.
#' @param include_frequency Whether to include frequency columns.
#' @param include_proportion Whether to include proportion/percentage columns.
#' @param convert_to_percent Whether to format to \code{percent} or \code{proportion}.
#' @param format_precision Specify the precision of rounding the percent or proportion.
#' @param label_stub Stubhead label (first column).
#' @param label_total Column name for the total.
#' @param recode Whether to recode the variable name first.
#' @param recode_variable_position Indicate the position of letter of variable that uniquely identifies each variable.
#' @param clean_name Whether to output a clean column name.
#' @param names_separator Column separator that defines the table hierarchy.
#'
#' @return Returns a cross-table of type \code{tibble}
#' @export
#'
#' @examples
#'

generate_multiple_response <- function(
  .data,
  x,
  ...,
  value_to_count = 1,
  include_frequency = TRUE,
  include_proportion = TRUE,
  convert_to_percent = TRUE,
  format_precision = 2,
  label_stub = get_config('label_stub'),
  label_total = NULL,
  names_separator = '>',
  recode = TRUE,
  recode_variable_position = 5,
  clean_name = TRUE
) {

  # Check the if input data is valid
  check_input_data_validity(.data)

  type <- NULL
  n <- NULL
  `:=` <- NULL

  dots_name <- .data |>
    dplyr::ungroup() |>
    dplyr::select(...) |>
    names()

  if(rlang::dots_n(...) == 0 | length(dots_name) == 0) {
    stop('Multiple response variable is required!')
  }

  grouping_cols <- .data |> dplyr::select(dplyr::group_cols())
  grouping_col_names <- names(dplyr::collect(grouping_cols))

  .df_selected <- .data |>
    dplyr::select(any_of(grouping_col_names), {{x}}, ...)


  p_label <- dplyr::if_else(convert_to_percent, 'Percent', 'Proportion')
  p_multiplier <- dplyr::if_else(convert_to_percent, 100, 1)


  join_with <- .df_selected |>
    dplyr::group_by({{x}}, .add = T) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::collect()

  if(length(dots_name) == 1 & rlang::dots_n(...) == 1) {

    y <- sapply(substitute(list(...))[-1], deparse)[1]

    df <- .df_selected |>
      dplyr::mutate(type = toupper(stringr::str_trim(!!as.name(y)))) |>
      dplyr::collect() |>
      dplyr::mutate(type = dplyr::if_else(type == '', NA_character_, type)) |>
      dplyr::mutate(type = strsplit(type, split = '')) |>
      tidyr::unnest(type) |>
      dplyr::filter(!is.na(type), grepl('^[A-Z]$', type)) |>
      dplyr::mutate(type = paste0('<<', type, '>>')) |>
      dplyr::group_by({{x}}, type, .add = T, .drop = T) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::arrange(type) |>
      tidyr::pivot_wider(
        names_from = type,
        values_from = n,
        values_fill = 0,
        names_sort = F
      )

  } else {

    if(recode == T) {

      recode_var <- stringr::str_sub(
        dots_name,
        recode_variable_position,
        recode_variable_position
      )

      if(length(dots_name) != length(unique(recode_var))) {
        stop('Indicate the position of letter of variable that uniquely identifies each mult-response variable: \n
             `recode_variable_position`')
      }

      df <- .df_selected |>
        dplyr::group_by({{x}}, .add = T) |>
        dplyr::rename_at(
          dplyr::vars(...),
          ~ paste0('<<', toupper(
            stringr::str_sub(., recode_variable_position, recode_variable_position)), '>>'
          )
        ) |>
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

      df <- .df_selected |>
        dplyr::group_by({{x}}, .add = T) |>
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

  }

  g <- c(grouping_col_names, set_as_string({{x}}))

  df_cols <- df |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(g), -n) |>
    names()

  df <- df |>
    dplyr::inner_join(join_with, by = g) |>
    janitor::adorn_totals() |>
    dplyr::mutate_at(
      dplyr::vars(dplyr::any_of(df_cols)),
      list(percent = ~ (. / n) * p_multiplier)
    ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('_percent$')),
      ~ paste0(p_label, names_separator, stringr::str_remove(., '_percent$'))
    ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::any_of(df_cols)), ~ paste0('Frequency', names_separator, .)
    ) |>
    dplyr::select(dplyr::any_of(g), n, dplyr::everything()) |>
    dplyr::rename(Total = n)

  if(!is.null(label_stub)) {
    df <- df |> dplyr::rename((!!as.name(label_stub)) := {{x}})
  }

  if(!is.null(label_total)) {
    df <- df |>
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('Total')),
        ~ stringr::str_replace(., 'Total', label_total)
      )
  }

  if(clean_name == T) {
    df <- df |>
      dplyr::rename_all(~ stringr::str_remove_all(., '<<|>>'))
  }

  return(dplyr::tibble(df))

}

