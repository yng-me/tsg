#' tsg_prop_total
#'
#' @param data a
#' @param total_by b
#' @param group_values_by c
#' @param format_to_percent d
#' @param format_precision e
#' @param total_label f
#' @param ... g
#'
#' @return
#' @export
#'
#' @examples

tsg_prop_total <- function(
    data,
    total_by = 'row',
    group_values_by = 'statistics',
    format_to_percent = T,
    format_precision = 2,
    total_label = NULL,
    ...
  ) {

  `.` <- NULL

  g_val <- tolower(group_values_by)

  tsg_util_get_label <- function(col, label) {
    if(g_val == 'indicators' | g_val == 'indicator') {
      p <- paste0(col, '||', label)
    } else {
      p <- paste0(label, '||', col)
    }
    return(p)
  }

  # Check if the argument for 'direction' is valid
  if(!(total_by %in% c('col', 'row'))) {
    total_by <- 'row'
    warning("You have entered invalid agrument for 'add_by' parameter. It reverts back to default value: 'row'.")
  }

  if(!(g_val %in% c('indicators', 'statistics', 'statistic'))) g_val <- 'statistics'

  p_label <- dplyr::if_else(format_to_percent == T, 'Percent', 'Proportion')

  # get_prop <- function(var, total) {
  #   t <- if_else(total == 0, 0, var / total)
  #   if(format_to_percent == T) t <- t * 100
  #   return(round(t, format_precision))
  # }

  t_label <- 'Total'
  if(!is.null(total_label)) {
    t_label <- paste0('pivot_', total_label, '||Total')
  }

  if(total_by == 'col') {
    pct <- data |>
      janitor::adorn_totals('col', name = t_label) |>
      janitor::adorn_percentages('col', na.rm = T, dplyr::matches('^pivot_|Total')) |>
      dplyr::select(dplyr::starts_with('pivot_'), dplyr::contains('Total')) |>
      # replace(is.na(.), 0) |>
      dplyr::mutate_all(~ . * 100) |>
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
        list(pl = ~ if_else(Total == 0, 0, (. / Total) * 100))
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
          ~ tsg_util_get_label(paste0('pivot_', total_label, '||', .), 'Frequency')
        )
    }
  }

  df <- df |>
    dplyr::rename_at(
      dplyr::vars(dplyr::matches('^Total$')),
      ~ tsg_util_get_label('Total', 'Frequency')
    )

  return(df |> tsg_prop_inclusion(...))
}
