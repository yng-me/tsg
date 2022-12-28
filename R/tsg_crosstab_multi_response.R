#' Generate summary table from a multiple response category
#'
#' @param data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param x \strong{Required}. Row variable to be used as categories.
#' @param y \strong{Required}. Column variable.
#' @param x_group Column grouping variable/s.
#' @param filter_var Use of screening variable.
#' @param use_x_as_group Use row variable as grouping.
#'
#' @return Returns a cross-table of type \code{tibble}
#' @export
#'
#' @examples

tsg_crosstab_multi_response <- function(
    .data,
    x,
    ...,
    y = NULL,
    x_group = NULL,
    x_label = tsg_get_config('x_label'),
    use_x_as_group = F
) {

  type <- NULL
  n <- NULL

  if (!is.data.frame(.data) && !is.data.frame(dplyr::collect(.data))) {
    stop(paste0("Data input must be a valid data frame."))
  }

  df <- .data |>
    tsg_select(x_group, y_group = NULL, {{x}}, {{y}}, ...)

  as_string <- function(to_str) {
    stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
  }

  g <- as_string({{x}})

  # Check if x_group are defined
  if(!is.null(x_group)) {

    if(use_x_as_group == T) {
      g <- c(g, x_group)
    } else {
      g <- c(x_group, g)
    }
  }

  join_with <- df |>
    tsg_util_create_group(g) |>
    dplyr::count() |>
    dplyr::collect()

  if(as_string({{y}}) == 'NULL') {
    df <- df |>
      tsg_util_create_group(g) |>
      dplyr::collect() |>
      dplyr::mutate_at(dplyr::vars(...), ~ dplyr::if_else(. > 1L, 0L, .)) |>
      dplyr::summarise_at(dplyr::vars(...), ~ sum(., na.rm = T))
  } else {
    df <- df |>
      dplyr::mutate(type = toupper(stringr::str_trim({{y}}))) |>
      dplyr::na_if('') |>
      dplyr::collect() |>
      dplyr::mutate(type = strsplit(type, split = '')) |>
      tidyr::unnest(type) |>
      dplyr::filter(!is.na(type), grepl('^[A-Z]$', type)) |>
      tsg_util_create_group(g, type) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        names_from = type,
        values_from = n,
        values_fill = 0,
        names_sort = F
      )
  }

  df <- df |>
    dplyr::inner_join(join_with, by = g) |>
    dplyr::mutate_at(
      dplyr::vars(-c(1, n)),
      list(percent = ~ (. / n) * 100)
    ) |>
    dplyr::select(1, n, dplyr::everything()) |>
    janitor::adorn_totals() |>
    dplyr::tibble()

  if(!is.null(x_label)) {
    df <- df |> rename((!!as.name(x_label)) := {{x}})
  }

  return(df)

}

