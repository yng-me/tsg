#' @title Generate cross-tabulation
#'
#' @description This function extends the functionality of \code{generate_frequency} by allowing you to generate cross-tabulations of two (2) or more categorical variables.

#' @param data A data frame, data frame extension (e.g. a \code{tibble}), a lazy data frame (e.g. from \code{dbplyr} or \code{dtplyr}), or Arrow data format.
#'
#' @param x \strong{Required}. Variable to be used as categories.
#' @param ... Tidy-select column names.
#' @param total_by_col Whether to apply the sum columnwise if \code{TRUE} or rowwise \code{FALSE}. Default is \code{FALSE}
#' @param include_frequency Whether to include frequency columns. Default is \code{TRUE}.
#' @param include_percent Whether to include proportion/percentage columns. Default is \code{TRUE}.
#' @param include_column_total Whether to include column total. Default is \code{TRUE}.
#' @param include_row_total Whether to include row total. Default is \code{TRUE}.
#' @param include_subtotal CURRENTLY IGNORE Whether to include subtotal. Default is \code{FALSE}.
#' @param include_zero_value Whether to drop columns with zero (0) values
#' @param as_proportion Whether to format to percent or proportion. Default is \code{TRUE}.
#' @param decimal_precision Specify the precision of rounding the percent or proportion.
#' @param label_stub stubhead label (first column).
#' @param label_total Label for the overall total.
#' @param label_subtotal Label for the subtotal.
#' @param names_separator Column separator that defines the table hierarchy.
#' @param label_na Logical. If \code{TRUE}, includes missing values in the frequency table.
#' @param metadata A named list with optional metadata to attach as attributes: \code{title}, \code{subtitle}, \code{source_note}, and \code{footnotes}.
#'
#' @return Returns a cross-table of type \code{tibble}
#'
#' @export
#'
#' @examples
#'

generate_crosstab <- function(
  data,
  x,
  ...,
  total_by_col = FALSE,
  include_frequency = TRUE,
  include_percent = TRUE,
  include_column_total = TRUE,
  include_row_total = TRUE,
  include_subtotal = FALSE,
  include_zero_value = FALSE,
  as_proportion = FALSE,
  decimal_precision = NULL,
  label_stub = NULL,
  label_total = 'Total',
  label_na = "Not reported",
  label_subtotal = NULL,
  names_separator = '_',
  metadata = list(
    title = NULL,
    subtitle = NULL,
    source_note = NULL,
    footnotes = NULL
  )
) {

  n_args <- rlang::dots_n(...)
  if(n_args == 0) {
    return(generate_frequency(data, {{x}}))
  }

  data <- collect_data(data)

  grouping_col_names <- dplyr::group_vars(data)
  data <- dplyr::select(data, any_of(grouping_col_names), {{x}}, ...)

  expr_cols <- rlang::expr(c(...))
  cols_to_pivot <- tidyselect::eval_select(
    expr = expr_cols,
    data = dplyr::collect(data)
  )

  df_colnames <- names(data)
  df_names <- list()

  for(i in seq_along(df_colnames)) {

    df_colname <- df_colnames[i]
    attr_i <- attributes(data[[df_colname]])
    label <- attr_i$label

    if(is.null(label)) label <- df_colname

    df_names[[df_colname]] <- list(
      value = df_colname,
      label = label,
      order = i,
      type = typeof(data[[df_colname]]),
      labels = attr_i$labels
    )
  }

  label_separator <- paste0(rep(names_separator, 2), collapse = "")

  p_multiplier <- 100
  p_label <- "percent"
  if(as_proportion) {
    p_multiplier <- 1
    p_label <- "proportion"
  }

  add_total <- function(.df, .x) {

    df <- .df |>
      dplyr::select({{.x}}, dplyr::everything()) |>
      janitor::adorn_totals(c("col", "row"), name = c("Total", "total")) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::matches('^frequency')),
        list(PV_TOTAL_ALL_INTERNAL = ~ p_multiplier * (. / total))
      ) |>
      dplyr::select(
        {{.x}},
        total,
        dplyr::everything()
      )

    if(total_by_col) {
      df <- .df |>
        janitor::adorn_totals('col', name = glue::glue("frequency{names_separator}total")) |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::matches('^frequency')),
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
      # dplyr::rename_all(~ stringr::str_replace(., 'total', label_total)) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::ends_with('PV_TOTAL_ALL_INTERNAL')),
        ~ stringr::str_remove(
          stringr::str_replace(., '^frequency', p_label), '_PV_TOTAL_ALL_INTERNAL$'
        )
      )

  }

  set_included_stats <- function(.df) {

    if(include_frequency == FALSE & include_percent == FALSE)  {
      include_frequency <- TRUE
    }

    if(include_frequency == FALSE) {
      .df <- .df |>
        dplyr::select(-dplyr::matches('^frequency')) |>
        dplyr::rename_at(
          dplyr::vars(dplyr::matches(paste0('^', p_label))),
          ~ stringr::str_remove(., paste0('^', p_label, names_separator))
        )
    }

    if(include_percent == FALSE) {
      .df <- .df |>
        dplyr::select(-dplyr::matches(paste0('^', p_label))) |>
        dplyr::rename_at(
          dplyr::vars(dplyr::matches('^frequency')),
          ~ stringr::str_remove(., paste0('^frequency', names_separator))
        )
    }

    if(include_column_total == FALSE) {
      .df <- .df |> dplyr::select(
        -dplyr::matches(
          glue::glue(
            '^(total|frequency{names_separator}total|percent{names_separator}total|proportion{names_separator}total)'
          )
        )
      )
    }

    if(include_row_total == FALSE) .df <- .df |>
        dplyr::filter({{x}} != 'total' | is.na({{x}}) | {{x}} == '-')

    if(!is.null(label_stub)) {
      .df <- .df |> dplyr::rename((!!as.name(label_stub)) := {{x}})
    }

    if(total_by_col) .df <- .df |> janitor::adorn_totals()

    return(.df)

  }

  tabs <- list()

  for(i in seq_along(cols_to_pivot)) {

    col_i <- names(cols_to_pivot[i])

    tab_i <- data |>
      dplyr::select(any_of(grouping_col_names), {{x}}, !!as.name(col_i)) |>
      dplyr::group_by({{x}}, !!as.name(col_i), .add = TRUE) |>
      dplyr::count(name = 'frequency') |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      tidyr::pivot_wider(
        names_from = !!as.name(col_i),
        values_from = frequency,
        names_sep = names_separator,
        names_sort = TRUE,
        values_fill = 0,
        names_expand = include_zero_value,
        names_prefix = paste0('frequency', names_separator)
      ) |>
      # add_total({{x}}) |>
      # add_column_total() |>
      set_included_stats() |>
      dplyr::select(-dplyr::any_of('PV_TOTAL_ALL_INTERNAL')) |>
      dplyr::tibble()

    attr(tab_i, "col") <- col_i
    label_i <- df_names[[col_i]]$label

    tab_labels <- names(tab_i)
    tab_labels <- tab_labels[grepl(glue::glue("^(frequency|{p_label}){names_separator}"), tab_labels)]

    for(j in seq_along(tab_labels)) {
      tab_label_j <- tab_labels[j]
      new_label_j <- stringr::str_remove(tab_label_j, glue::glue("^(frequency|{p_label}){names_separator}"))
      prefix_label_j <- stringr::str_remove(tab_label_j, glue::glue("{names_separator}{new_label_j}"))

      if(new_label_j == "NA") new_label_j <- label_na

      value_j <- as.integer(new_label_j)
      if(!is.na(value_j) & !is.null(df_names[[col_i]]$labels)) {
        if(value_j %in% df_names[[col_i]]$labels) {
          new_label_j <- names(which(df_names[[col_i]]$labels == value_j))
        }
      }
      new_label_j <- paste0(stringr::str_to_title(prefix_label_j), label_separator, new_label_j)
      attr(tab_i[[tab_label_j]], "label") <- new_label_j

    }

    if("total" %in% names(tab_i)) {
      attr(tab_i$total, "label") <- label_total
    }

    tabs[[label_i]] <- tab_i

    class(tabs[[label_i]]) <- c(class(tabs[[label_i]]), "tsg")

  }

  df <- tabs
  if(length(tabs) == 1) { df <- tabs[[1]] }

  metadata_names <- names(metadata)
  for(i in seq_along(metadata_names)) {
    metadata_i <- metadata_names[i]
    attr(df, metadata_i) <- metadata[[i]]
  }

  class(df) <- c("tsg", class(df)[!(class(df) %in% "tsg")])

  return(df)

}
