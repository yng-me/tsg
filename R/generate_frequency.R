#' @title Generate frequency table
#' @description Generate a frequency table with optional cumulative total and percent.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param x \strong{Required}. Variable to be used as categories.
#' @param x_group Accepts a vector of string/character as grouping variables.
#' @param x_label Stubhead label (first column).
#' @param x_as_group Use row variable as grouping.
#' @param sort_frequency Whether to sort the output. If set to \code{TRUE}, the frequency will be sorted in descending order.
#' @param include_total Whether to include row total.
#' @param include_cumulative Whether to cumulative frequencies.
#' @param exclude_zero_value Whether to drop categories with zero (0) values
#'
#' @return Returns a frequency table of type \code{tibble}.
#' @export
#'
#' @examples
#' library(palmerpenguins)
#'
#' # Example 1: Basic usage
#'
#' generate_frequency(penguins, species)
#'
#'
#' # Example 2: Add grouping variable and define label for x
#'
#' penguins |>
#'   generate_frequency(
#'     x = sex,
#'     x_group = 'species',
#'     x_label = 'Sex'
#'    )
#'
#'
#' # Example 3: Add grouping variable, use x as group, and exclude column total
#'
#' penguins |>
#'   generate_frequency(
#'     x = sex,
#'     x_group = 'species',
#'     x_as_group = TRUE,
#'     include_total = FALSE
#'  )
#'
#'
#' # Example 4: Exclude cumulative values and sort the output by frequency
#'
#' penguins |>
#'   generate_frequency(
#'    x = species,
#'    x_label = 'Species',
#'    sort_frequency = TRUE,
#'    include_cumulative = FALSE
#'  )
#'

generate_frequency <- function(
  .data,
  x,
  x_group = NULL,
  x_label = get_config('x_label'),
  sort_frequency = FALSE,
  x_as_group = FALSE,
  include_total = TRUE,
  include_cumulative = TRUE,
  exclude_zero_value = FALSE
) {

  check_input_data_validity(.data)

  Percent <- NULL
  Frequency <- NULL
  percent <- NULL
  n <- NULL
  `:=` <- NULL

  x_string <- set_as_string({{x}})

  df <- .data |>
    dplyr::count({{x}}) |>
    dplyr::collect() |>
    dplyr::mutate(percent = n / sum(n))

  if(!is.null(x_group)) {

    if(x_as_group == T) x_string <- c(x_string, x_group)
    else x_string <- c(x_group, x_string)

    df <- .data |>
      select_only(x_group = x_group, NULL, {{x}}) |>
      create_group(x_string) |>
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
      dplyr::matches(paste0('^', x_string, '$')),
      Frequency := n,
      Percent
    ) |>
    frequency_inclusion(
      excluded_cols = x_string,
      include_total,
      include_cumulative,
      exclude_zero_value
    )

  if(!is.null(x_label)) df <- df |> dplyr::rename((!!as.name(x_label)) := {{x}})

  return(df |> dplyr::tibble())

}










