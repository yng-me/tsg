tsg_crosstab_total <- function(
    data,
    total_by = 'row',
    y_group_separator,
    group_values_by = 'statistics',
    format_to_percent = TRUE,
    format_precision = 2,
    total_label = NULL,
    ...
  ) {

  `.` <- NULL

  g_val <- tolower(group_values_by)

  tsg_util_get_label <- function(col, label) {
    if(g_val == 'indicators' | g_val == 'indicator') {
      p <- paste0(col, y_group_separator, label)
    } else {
      p <- paste0(label, y_group_separator, col)
    }
    return(p)
  }

  # Check if the argument for 'direction' is valid
  if(!(tolower(total_by) %in% c('row', 'col', 'column'))) {
    total_by <- 'row'
    warning("You have entered invalid agrument for 'total_by' parameter. It reverts back to default value: 'row'.")
  }

  if(!(tolower(g_val) %in% c('indicators', 'statistics', 'statistic'))) g_val <- 'statistics'

  p_divisor <- dplyr::if_else(format_to_percent == T, 100, 1)
  p_label <- dplyr::if_else(format_to_percent == T, 'Percent', 'Proportion')

  # get_prop <- function(var, total) {
  #   t <- if_else(total == 0, 0, var / total)
  #   if(format_to_percent == T) t <- t * 100
  #   return(round(t, format_precision))
  # }

  t_label <- 'Total'
  if(!is.null(total_label)) {
    t_label <- paste0('pivot_', total_label, y_group_separator, 'Total')
  }

  if(tolower(total_by) == 'col' | total_by == 'column') {
    pct <- data |>
      janitor::adorn_totals('col', name = t_label) |>
      janitor::adorn_percentages('col', na.rm = T, dplyr::matches('^pivot_|Total')) |>
      dplyr::select(dplyr::starts_with('pivot_'), dplyr::contains('Total')) |>
      # replace(is.na(.), 0) |>
      dplyr::mutate_all(~ . * p_divisor) |>
      dplyr::rename_all(~ tsg_util_get_label(., p_label)) |>
      dplyr::tibble()

    df <- data |>
      janitor::adorn_totals('col', name = t_label) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('^pivot_')),
        ~ tsg_util_get_label(., 'Frequency')
      ) |>
      dplyr::bind_cols(pct) |>
      janitor::adorn_totals() |>
      dplyr::tibble()

  } else {

    df <- data |>
      janitor::adorn_totals(c('row', 'col')) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains('pivot_')),
        list(pl = ~ dplyr::if_else(Total == 0, 0, (. / Total) * p_divisor))
        # list(pl = ~ get_prop(., Total))
      ) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::ends_with('_pl')),
        ~ tsg_util_get_label(., p_label)
      ) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::starts_with('pivot_'), -dplyr::contains(p_label)),
        ~ tsg_util_get_label(., 'Frequency')
      )

    if(!is.null(total_label)) {
      df <- df |>
        dplyr::rename_at(
          dplyr::vars(dplyr::matches('^Total$')),
          ~ tsg_util_get_label(paste0('pivot_', total_label, y_group_separator, .), 'Frequency')
        )
    }
  }

  df <- df |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('^Total$')),
      ~ tsg_util_get_label('Total', 'Frequency')
    )

  return(df |> tsg_crosstab_inclusion(y_group_separator = y_group_separator, p_label = p_label, ...))
}
