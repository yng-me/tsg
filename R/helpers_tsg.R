crosstab_total <- function(
  .data_piped,
  y_group_separator,
  total_by,
  group_values_by,
  format_to_percent,
  format_precision,
  total_label,
  include_frequency,
  include_proportion,
  include_column_total
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
    pct <- .data_piped |>
      janitor::adorn_totals('col', name = t_label) |>
      janitor::adorn_percentages('col', na.rm = T, dplyr::matches('^pivot_|Total')) |>
      dplyr::select(dplyr::starts_with('pivot_'), dplyr::contains('Total')) |>
      # replace(is.na(.), 0) |>
      dplyr::mutate_all(~ . * p_divisor) |>
      dplyr::rename_all(~ tsg_util_get_label(., p_label)) |>
      dplyr::tibble()

    df <- .data_piped |>
      janitor::adorn_totals('col', name = t_label) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('^pivot_')),
        ~ tsg_util_get_label(., 'Frequency')
      ) |>
      dplyr::bind_cols(pct) |>
      janitor::adorn_totals() |>
      dplyr::tibble()

  } else {

    df <- .data_piped |>
      janitor::adorn_totals(c('row', 'col')) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains('pivot_')),
        list(pl = ~ dplyr::if_else(Total == 0, 0, (. / Total) * p_divisor))
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

  return(
    df |> crosstab_inclusion(
      y_group_separator,
      p_label,
      include_frequency,
      include_proportion,
      include_column_total
    )
  )
}


crosstab_total_stack <- function(
  data,
  y_group_separator,
  total_by,
  group_values_by,
  format_to_percent,
  format_precision,
  total_label,
  include_frequency,
  include_proportion,
  include_column_total
) {

  value <- NULL

  cols <- dplyr::as_tibble(names(data)) |>
    dplyr::filter(grepl('pivot_', value))

  ex_cols <- dplyr::as_tibble(names(data)) |>
    dplyr::filter(!grepl('pivot_', value)) |>
    dplyr::pull(value)

  ex_col_names <-  paste0('^', ex_cols, '$')

  x <- cols |>
    dplyr::mutate(value = stringr::str_remove(value, 'pivot_')) |>
    dplyr::mutate(value = stringr::str_remove(value,  paste0(y_group_separator, '.*$') )) |>
    dplyr::distinct() |>
    dplyr::pull(value)

  df <- data |>
    dplyr::select(
      dplyr::matches(ex_col_names),
      dplyr::starts_with(paste0('pivot_', x[1], y_group_separator))
    ) |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches(ex_cols)), as.character) |>
    crosstab_total(
      y_group_separator,
      total_by,
      group_values_by,
      format_to_percent,
      format_precision,
      total_label = x[1],
      include_frequency,
      include_proportion,
      include_column_total
    ) |>
    dplyr::tibble()

  for(i in 2:length(x)) {

    df_y <- data |>
      dplyr::select(
        dplyr::matches(ex_cols),
        dplyr::starts_with(paste0('pivot_', x[i], y_group_separator))
      ) |>
      dplyr::mutate_at(dplyr::vars(dplyr::matches(ex_cols)), as.character) |>
      crosstab_total(
        y_group_separator,
        total_by,
        group_values_by,
        format_to_percent,
        format_precision,
        total_label = x[i],
        include_frequency,
        include_proportion,
        include_column_total
      ) |>
      dplyr::select(-dplyr::matches(ex_cols)) |>
      dplyr::tibble()

    df <- df |>  tibble::add_column(df_y)
  }

  return(df)

}


crosstab_rename <- function(
  .data_piped,
  replace_na_with = 'Missing',
  y_group_separator
) {

  `.` <- NULL

  df <- .data_piped |> dplyr::rename_at(
    dplyr::vars(dplyr::contains('pivot_')),
    ~ stringr::str_remove_all(., 'pivot_|_pl')
  ) |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches(paste0(y_group_separator, 'NA|NA', y_group_separator))),
      ~ stringr::str_remove(stringr::str_replace(., 'NA', replace_na_with), 'NA')
    )

  df_names_sorted <- paste0('^', stringr::str_subset(sort(names(df)), y_group_separator), '$')

  df <- df |>
    dplyr::select(!dplyr::contains(y_group_separator), dplyr::matches(df_names_sorted)) |>
    # dplyr::rename_all(~ stringr::str_remove_all(., '^\\d+ - ')) |>
    dplyr::tibble()

  return(df)
}


crosstab_inclusion <- function(
  .data_piped,
  y_group_separator,
  p_label,
  include_frequency,
  include_proportion,
  include_column_total
) {

  if(include_frequency == T & include_proportion == T) {
    return(.data_piped)
  }

  # Check if both values of exclude_frequency and include_prop are valid.
  if(include_frequency == F & include_proportion == F) {
    include_frequency <- T
    warning("'include_frequency' and 'include_proportion' cannot be both 'FALSE'. Defaulted back to 'include_frequency = TRUE'")
  }

  prop_column_label <- paste0(y_group_separator, p_label, '|', p_label, y_group_separator)
  freq_column_label <- paste0(y_group_separator, 'Frequency|Frequency', y_group_separator)

  column_label_retain <- dplyr::if_else(include_frequency == F,  prop_column_label, freq_column_label)
  column_label_remove <- dplyr::if_else(include_frequency == F,  freq_column_label, prop_column_label)

  .data_piped |>
    dplyr::rename_at(
      dplyr::vars(dplyr::contains('Total')),
      ~ stringr::str_remove_all(., column_label_retain)
    ) |>
    dplyr::select(-dplyr::matches(column_label_remove)) |>
    dplyr::rename_all(~ stringr::str_remove_all(., column_label_retain))

}


frequency_inclusion <- function(
  .data_piped,
  excluded_cols,
  include_total,
  include_cumulative,
  exclude_zero_value
) {

  Frequency <- NULL
  Percent <- NULL

  df <- .data_piped |> dplyr::tibble()

  if(include_cumulative == T) {
    cumulative <- .data_piped |>
      dplyr::select(!dplyr::matches(paste0('^', excluded_cols, '$'))) |>
      cumsum() |>
      dplyr::rename(
        'Cumulative Total' = Frequency,
        'Cumulative Percent' = Percent
      )
    df <- .data_piped |> dplyr::bind_cols(cumulative)
  }

  if(include_total == T) {
    df <- df |>
      janitor::adorn_totals(
        where = 'row',
        fill = '-',
        na.rm = T,
        name = 'Total',
        -dplyr::matches(paste0('^', excluded_cols, '$')),
        -dplyr::contains('Cumulative')
      ) |>
      dplyr::na_if('-') |>
      dplyr::mutate_at(dplyr::vars(dplyr::contains('Cumulative')), as.numeric)
  }

  if(exclude_zero_value == T) {
    df <- df |> dplyr::filter(Frequency > 0)
  }

  return(df)

}
