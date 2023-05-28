#' @title Generate cross-tabulation
#'
#' @description This function extends the functionality of \code{generate_frequency} by allowing you to generate cross-tabulations of two (2) or more categorical variables.

#' @param .data A data frame, data frame extension (e.g. a \code{tibble}), a lazy data frame (e.g. from \code{dbplyr} or \code{dtplyr}), or Arrow data format.
#' @param x \strong{Required}. Variable to be used as categories.
#' @param ... Tidy-select column names.
#' @param total_by_col Whether to apply the sum columnwise if \code{TRUE} or rowwise \code{FALSE}. Default is \code{FALSE}
#' @param total_option CURRENTLY IGNORE. For future implementation.
#' @param include_frequency Whether to include frequency columns. Default is \code{TRUE}.
#' @param include_proportion Whether to include proportion/percentage columns. Default is \code{TRUE}.
#' @param include_column_total Whether to include column total. Default is \code{TRUE}.
#' @param include_row_total Whether to include row total. Default is \code{TRUE}.
#' @param include_subtotal Whether to include subtotal. Default is \code{FALSE}.
#' @param convert_to_percent Whether to format to percent or proportion. Default is \code{TRUE}.
#' @param decimal_precision Specify the precision of rounding the percent or proportion.
#' @param label_stub Stubhead label (first column).
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
    total_option = 'default',
    include_frequency = TRUE,
    include_proportion = TRUE,
    include_column_total = TRUE,
    include_row_total = TRUE,
    include_subtotal = FALSE,
    convert_to_percent = TRUE,
    decimal_precision = NULL,
    label_stub = get_config('label_stub'),
    label_total = 'Total',
    label_subtotal = NULL,
    weights = NULL,
    names_separator = '>'
) {

  # Check the if input data is valid
  check_input_data_validity(.data)

  grouping_cols <- .data |> dplyr::select(dplyr::group_cols())
  grouping_col_names <- names(dplyr::collect(grouping_cols))

  .df_selected <- .data |> dplyr::select(any_of(grouping_col_names), {{x}}, ...)

  `:=` <- NULL
  `.` <- NULL
  Total <- NULL
  Frequency <- NULL

  expr <- rlang::expr(c(...))
  cols_to_pivot <- tidyselect::eval_select(
    expr, data = dplyr::collect(.df_selected)
  )

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
      janitor::adorn_totals(c('row', 'col')) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::matches('^Frequency')),
        list(pv = ~ p_multiplier * (. / Total))
      ) |>
      dplyr::select(
        {{x}},
        Total,
        dplyr::everything()
      )

    if(total_by_col) {
      df <- .df |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^Frequency')),
          list(pv = ~ p_multiplier * (. / sum(.)))
        )
    }

    if(is.null(decimal_precision) & is.numeric(decimal_precision)) {
      df <- df |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('_pv$')),
          ~ round(., as.integer(decimal_precision))
        )
    }

    df |>
      dplyr::rename_all(~ stringr::str_replace(., 'Total', label_total)) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::ends_with('_pv')),
        ~ stringr::str_remove(
          stringr::str_replace(., '^Frequency', p_label), '_pv$'
        )
      )

  }

  add_subtotal <- function(.df) {

    if(include_subtotal == F) return(.df)
    if(is.null(label_subtotal)) label_subtotal <- 'Subtotal'

    get_cols <- function(x) {
      stringr::str_flatten(x[-h], collapse = names_separator)
    }

    get_unique_cols <- function(x) {
      v <- x[-h]
      v[-c(1:i)]
    }

    df_names <- names(.df)[grepl(names_separator, names(.df))]
    categories <- stringr::str_split(df_names, names_separator)
    h <- length(categories[[1]])
    levels <- h - 1
    col_first <- categories[[1]][h]

    cols_to_subtotal <- unique(
      unlist(lapply(categories, get_cols))
    )

    for(i in 1:levels) {

      unique_categories <- unique(
        unlist(lapply(categories, get_unique_cols))
      )

      for(j in seq_along(unique_categories)) {

        get_current_col <- function(k) {
          selected <- paste0(k, names_separator, unique_categories[j])
          list(
            subtotal =  paste0('^', selected),
            sub = paste0(selected, names_separator, label_subtotal),
            before = paste0(selected, names_separator, col_first)
          )
        }

        v <- get_current_col(k = 'Frequency')

        .df <- .df |>
          dplyr::mutate(
            (!!as.name(v$sub)) := rowSums(
              dplyr::select(., dplyr::matches(v$subtotal))
            )
          ) |>
          dplyr::relocate((!!as.name(v$sub)), .before = !!as.name(v$before))

        w <- get_current_col(k = p_label)

        .df <- .df |>
          dplyr::mutate(total_by_col = total_by_col) |>
          dplyr::mutate(
            (!!as.name(w$sub)) := dplyr::if_else(
              total_by_col,
              p_multiplier * (!!as.name(v$sub) / sum(!!as.name(v$sub))),
              rowSums(dplyr::select(., dplyr::matches(w$subtotal)))
            )
          ) |>
          dplyr::relocate((!!as.name(w$sub)), .before = !!as.name(w$before)) |>
          dplyr::select(-t)
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
      .df <- .df |> dplyr::select(-dplyr::matches('^Total'))
    }
    if(include_row_total == F) .df <- .df |> dplyr::filter({{x}} != 'Total')
    if(!is.null(label_stub)) {
      .df <- .df |> dplyr::rename((!!as.name(label_stub)) := {{x}})
    }

    if(total_by_col) .df <- .df |> janitor::adorn_totals()

    return(.df)

  }

  cross_tab <- .df_selected |>
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
      names_prefix = paste0('Frequency', names_separator)
    ) |>
    add_total() |>
    add_subtotal() |>
    set_inclusion() |>
    dplyr::select(dplyr::any_of(grouping_col_names), dplyr::everything())

  return(dplyr::tibble(cross_tab))

}
