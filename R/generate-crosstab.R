#' @title Generate cross-tabulation
#'
#' @description This function extends the functionality of \code{generate_frequency} by allowing you to generate cross-tabulations of two (2) or more categorical variables.

#' @param .data A data frame, data frame extension (e.g. a \code{tibble}), a lazy data frame (e.g. from \code{dbplyr} or \code{dtplyr}), or Arrow data format.
#'
#' @param x \strong{Required}. Variable to be used as categories.
#' @param ... Tidy-select column names.
#' @param total_by_col Whether to apply the sum columnwise if \code{TRUE} or rowwise \code{FALSE}. Default is \code{FALSE}
#' @param include_frequency Whether to include frequency columns. Default is \code{TRUE}.
#' @param include_proportion Whether to include proportion/percentage columns. Default is \code{TRUE}.
#' @param include_column_total Whether to include column total. Default is \code{TRUE}.
#' @param include_row_total Whether to include row total. Default is \code{TRUE}.
#' @param include_subtotal CURRENTLY IGNORE Whether to include subtotal. Default is \code{FALSE}.
#' @param include_zero_value Whether to drop columns with zero (0) values
#' @param convert_to_percent Whether to format to percent or proportion. Default is \code{TRUE}.
#' @param decimal_precision Specify the precision of rounding the percent or proportion.
#' @param label_stub stubhead label (first column).
#' @param label_total Label for the overall total.
#' @param label_subtotal Label for the subtotal.
#' @param weights Weights to be applied in the aggregation.
#' @param names_separator Column separator that defines the table hierarchy.
#'
#' @return Returns a cross-table of type \code{tibble}
#'
#' @export
#'
#' @examples
#'

generate_crosstab <- function(
  .data,
  x,
  ...,
  total_by_col = FALSE,
  include_frequency = TRUE,
  include_proportion = TRUE,
  include_column_total = TRUE,
  include_row_total = TRUE,
  include_subtotal = FALSE,
  include_zero_value = FALSE,
  convert_to_percent = TRUE,
  decimal_precision = NULL,
  label_stub = NULL,
  label_total = 'Total',
  label_subtotal = NULL,
  names_separator = '__',
  x_as_stub = FALSE,
  remove_cols_from_group = NULL,
  weights = NULL
) {

  check_input_data_validity(.data)
  if("ArrowObject" %in% class(.data) | "arrow_dplyr_query" %in% class(.data)) {
    .data <- .data |> dplyr::collect()
  }

  grouping_col_names <- .data |> dplyr::group_vars()

  df_selected <- .data |>
    dplyr::select(any_of(grouping_col_names), {{x}}, ...)

  expr_cols <- rlang::expr(c(...))
  cols_to_pivot <- tidyselect::eval_select(
    expr = expr_cols,
    data = dplyr::collect(df_selected)
  )

  v <- names(df_selected)
  df_names <- list()
  for(i in seq_along(v)) {
    y <- v[i]
    attr_i <- attributes(df_selected[[y]])
    label <- attr_i$label
    if(is.null(label)) label <- y
    df_names[[i]] <- dplyr::tibble(
      value = y,
      label = label
    )
  }
  df_names <- df_names |> dplyr::bind_rows()

  for(i in seq_along(v)) {
    y <- v[i]
    df_selected <- df_selected |>
      factor_col(y, .keep_cols = F)
  }

  if(rlang::dots_n(...) == 0) {
    return(
      .data |>
        generate_frequency(
          x = {{x}},
          label_stub = label_stub
        )
    )
  }

  p_label <- dplyr::if_else(convert_to_percent, 'Percent', 'Proportion')
  p_multiplier <- dplyr::if_else(convert_to_percent, 100, 1)

  add_total <- function(.df) {

    df <- .df |>
      dplyr::select({{x}}, dplyr::everything()) |>
      janitor::adorn_totals(c('row', 'col')) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::matches('^Frequency')),
        list(PV_TOTAL_ALL_INTERNAL = ~ p_multiplier * (. / Total))
      ) |>
      dplyr::select(
        {{x}},
        Total,
        dplyr::everything()
      )

    if(total_by_col) {
      df <- .df |>
        janitor::adorn_totals('col', name = "Frequency__Total") |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^Frequency')),
          list(PV_TOTAL_ALL_INTERNAL = ~ p_multiplier * (. / sum(., na.rm = T)))
        ) |>
        dplyr::mutate_if(is.numeric, ~ dplyr::if_else(is.nan(.), 0, .))
    }

    if(is.null(decimal_precision) & is.numeric(decimal_precision)) {
      df <- df |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('PV_TOTAL_ALL_INTERNAL$')),
          ~ round(., as.integer(decimal_precision))
        )
    }

    df |>
      dplyr::rename_all(~ stringr::str_replace(., 'Total', label_total)) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::ends_with('PV_TOTAL_ALL_INTERNAL')),
        ~ stringr::str_remove(
          stringr::str_replace(., '^Frequency', p_label), '_PV_TOTAL_ALL_INTERNAL$'
        )
      )

  }

  add_subtotal <- function(.df) {

    dots_name <- .data |>
      dplyr::ungroup() |>
      dplyr::select(...) |>
      names()

    if(include_subtotal == F & length(dots_name) < 2) return(.df)

    if(is.null(label_subtotal)) label_subtotal <- 'Subtotal'

    m <- dots_name[c(1:(length(dots_name) - 1))]

    for(k in seq_along(m)) {

      s <- unique(.data[[m[k]]])

      for(j in seq_along(s)) {

        .df <- .df |>
          dplyr::mutate(
            !!as.name(paste0('Frequency__', s[j], '__', label_subtotal)) := rowSums(dplyr::select(., dplyr::matches(paste0('^Frequency__', s[j])))),
            !!as.name(paste0('Percent__', s[j], '__', label_subtotal)) := rowSums(dplyr::select(., dplyr::matches(paste0('^Percent__', s[j]))))
          )
      }
    }

    return(.df)

  }

  set_inclusion <- function(.df) {

    if(include_frequency == F & include_proportion == F)  {
      include_frequency <- T
    }

    if(include_frequency == F) {
      .df <- .df |>
        dplyr::select(-dplyr::matches('^Frequency')) |>
        dplyr::rename_at(
          dplyr::vars(dplyr::matches(paste0('^', p_label))),
          ~ stringr::str_remove(., paste0('^', p_label, names_separator))
        )
    }

    if(include_proportion == F) {
      .df <- .df |>
        dplyr::select(-dplyr::matches(paste0('^', p_label))) |>
        dplyr::rename_at(
          dplyr::vars(dplyr::matches('^Frequency')),
          ~ stringr::str_remove(., paste0('^Frequency', names_separator))
        )
    }

    if(include_column_total == F) {
      .df <- .df |> dplyr::select(-dplyr::matches('^(Total|Frequency__Total|Percent__Total|Proportion__Total)'))
    }
    if(include_row_total == F) .df <- .df |>
        dplyr::filter({{x}} != 'Total' | is.na({{x}}) | {{x}} == '-')

    if(!is.null(label_stub)) {
      .df <- .df |> dplyr::rename((!!as.name(label_stub)) := {{x}})
    }

    if(total_by_col) .df <- .df |> janitor::adorn_totals()

    return(.df)

  }

  cross_tab <- df_selected |>
    dplyr::group_by({{x}}, ..., .add = T) |>
    dplyr::count(name = 'Frequency') |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(names(cols_to_pivot)),
      values_from = Frequency,
      names_sep = names_separator,
      names_sort = T,
      values_fill = 0,
      names_expand = include_zero_value,
      names_prefix = paste0('Frequency', names_separator)
    ) |>
    dplyr::arrange(as.integer({{x}}), {{x}}) |>
    add_total() |>
    # add_subtotal() |>
    set_inclusion() |>
    dplyr::tibble()


  if(length(grouping_col_names) > 0 & !is.null(remove_cols_from_group)) {
    vv <- grouping_col_names[!(grouping_col_names %in% remove_cols_from_group)]
    if(length(vv) > 0) {
      cross_tab <- cross_tab |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(vv)))
    }
  }

  if(x_as_stub) {

    if(!is.null(label_stub)) {

      cross_tab <- cross_tab |>
        dplyr::select(dplyr::any_of(label_stub), dplyr::any_of(grouping_col_names), dplyr::everything())

    } else {
      cross_tab <- cross_tab |>
        dplyr::select({{x}}, dplyr::any_of(grouping_col_names), dplyr::everything())
    }

  } else {
    cross_tab <- cross_tab |>
      dplyr::select(dplyr::any_of(grouping_col_names), dplyr::everything())
  }


  for(j in seq_along(df_names$value)) {
    z <- df_names$value[j]
    if(z %in% names(cross_tab)) {
      cross_tab <- cross_tab |>
        dplyr::rename(!!as.name(df_names$label[j]) := !!as.name(z))
    }
  }

  for(i in seq_along(remove_cols_from_group)) {
    xx <- remove_cols_from_group[i]
    if(remove_cols_from_group[i] %in% names(cross_tab)) {
      cross_tab <- cross_tab |>
        dplyr::mutate(
          !!as.name(xx) := dplyr::if_else(
            !!as.name(xx) == '-',
            NA_character_,
            !!as.name(xx)
          )
        )
    }
  }

  cross_tab |>
    dplyr::select(-dplyr::any_of('PV_TOTAL_ALL_INTERNAL'))

}
