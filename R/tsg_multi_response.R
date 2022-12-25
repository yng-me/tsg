#' Generate summary table from a multiple response category
#'
#' @param data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param var_row \strong{Required}. Row variable to be used as categories.
#' @param var_col \strong{Required}. Column variable.
#' @param group_rows Column grouping variable/s.
#' @param filter_var Use of screening variable.
#' @param use_var_row_as_group Use row variable as grouping.
#'
#' @return Returns a cross-table of type \code{tibble}
#' @export
#'
#' @examples

tsg_multi_response <- function(
    data,
    var_row,
    var_col,
    group_rows = NULL,
    # code_ref = NULL,
    # rename_total = 'Total Households',
    filter_var = NULL,
    use_var_row_as_group = F
    # label = NULL
) {

  # if(is.null(label)) label <- d_label

  # if(!is.null(code_ref)) {
  #   code_refs <- ref_codes |>
  #     filter(list_name == code_ref) |>
  #     select(label, type = value)
  #
  #   is_all_unique <- code_refs |>
  #     distinct(label) |>
  #     group_by(label) |>
  #     count() |>
  #     filter(n > 1)
  #
  #   if(nrow(is_all_unique) > 0) stop('Code refs contains duplicate values/labels')
  #
  # }

  type <- NULL
  n <- NULL

  if (!is.data.frame(data) && !is.data.frame(dplyr::collect(data))) {
    stop(paste0("Data input must be a valid data frame."))
  }

  df <- data |>
    tsg_select({{var_row}}, {{var_col}}, group_rows)

  as_string <- function(to_str) {
    stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
  }

  g <- as_string({{var_row}})

  # Check if group_rows are defined
  if(!is.null(group_rows)) {

    if(use_var_row_as_group == T) {
      g <- c(g, group_rows)
    } else {
      g <- c(group_rows, g)
    }
  }

  if(!is.null(filter_var)) {
    df <- df |> dplyr::filter({{filter_var}} == 1)
  }

  join_with <- data |>
    tsg_select({{var_row}}, {{var_col}}, group_rows) |>
    tsg_util_create_group(g) |>
    dplyr::count() |>
    dplyr::collect() #|>
    #janitor::adorn_totals()

  df <- data |>
    tsg_select({{var_row}}, {{var_col}}, group_rows) |>
    dplyr::mutate(type = toupper(stringr::str_trim({{var_col}}))) |>
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
    ) |>
    # janitor::adorn_totals() |>
    dplyr::inner_join(join_with, by = g) |>
    dplyr::mutate_at(
      dplyr::vars(-c(1, n)),
      list(percent = ~ (. / n) * 100)
    ) |>
    dplyr::select(1, n, dplyr::everything()) |>
    dplyr::tibble()

  return(df |> janitor::adorn_totals())
    # rename((!!as.name(rename_total)) := 2)
}

