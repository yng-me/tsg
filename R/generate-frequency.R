#' @title Generate frequency table
#' @description This function allows you to generates a frequency distribution table (marginal table) of a categorical variable \code{x} specified in its second argument.
#'
#' @param .data A data frame, data frame extension (e.g. a \code{tibble}), a lazy data frame (e.g. from \code{dbplyr} or \code{dtplyr}), or Arrow data format.
#' @param x \strong{Required}. Variable to be used as categories.
#' @param label_stub Stubhead label (first column).
#' @param sort_frequency Whether to sort the output. If set to \code{TRUE}, the frequency will be sorted in descending order.
#' @param include_total Whether to include row total.
#' @param include_cumulative Whether to include cumulative frequencies.
#' @param exclude_zero_value Whether to drop categories with zero (0) values
#'
#' @return \code{generate_frequency} returns five (5) columns by default if \code{x_group} is not specified. These include (1) categories of \code{x}, (2) frequency of each category, (3) percent to total, (4) cumulative frequency, and (5) cumulative percent to total.
#' @export
#'
#'

generate_frequency <- function(
  .data,
  x,
  label_stub = NULL,
  sort_frequency = FALSE,
  sort_by = 'desc',
  include_total = TRUE,
  include_cumulative = TRUE,
  include_zero_value = FALSE
) {

  # Check if input data is valid
  check_input_data_validity(.data)

  Percent <- NULL
  Frequency <- NULL
  percent <- NULL
  n <- NULL
  `:=` <- NULL

  grouping_cols <- .data |> dplyr::select(dplyr::group_cols())
  grouping_col_names <- names(dplyr::collect(grouping_cols))
  x_col <- set_as_string({{x}})
  first_cols <- c(x_col, grouping_col_names)

  df_selected <- .data |>
    dplyr::select({{x}}, dplyr::any_of(grouping_col_names))

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


  # Compute frequency distribution
  df <- df_selected |>
    dplyr::count({{x}}) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    dplyr::mutate(percent = n / sum(n))

  # Whether to sort the frequency
  if(sort_frequency == T) {
    if(sort_by == 'desc') {
      df <- dplyr::arrange(df, dplyr::desc(n))
    } else {
      df <- dplyr::arrange(df, n)
    }
  }

  df <- df |>
    dplyr::arrange(as.integer({{x}}), {{x}})

  gc <- grouping_col_names[grouping_col_names != x_col]
  gc <- gc[gc != 'x']

  # Check what to include/exclude in the final output
  df <- df |>
    dplyr::mutate(Percent := percent * 100) |>
    dplyr::select(
      dplyr::any_of(first_cols),
      Frequency = n,
      Percent
    ) |>
    frequency_inclusion(
      excluded_cols = first_cols,
      include_total,
      include_cumulative,
      include_zero_value
    ) |>
    dplyr::group_by(dplyr::pick(dplyr::any_of(gc)))

  # Check if stub head label is specified
  if(!is.null(label_stub)) {
    df <- df |>
      dplyr::rename((!!as.name(label_stub)) := {{x}})
  }

  for(j in seq_along(df_names$value)) {
    z <- df_names$value[j]
    if(z %in% names(df)) {
      df <- df |>
        dplyr::rename(!!as.name(df_names$label[j]) := !!as.name(z))
    }
  }


  return(df)

}
