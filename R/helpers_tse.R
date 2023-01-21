# ------------------------------------------------------------------------------
increment_inner_depth <- function(vec) {
  c <- vec[1]
  s <- 1
  for(i in 2:length(vec)) {
    s_n <- s[i - 1]
    if(c == vec[i]) {
      s <- c(s, s_n)
    } else {
      s <- c(s, s_n + 1)
    }
    c <- vec[i]
  }
  return(s)
}


# ------------------------------------------------------------------------------
set_export_facade <- function(
  ...,
  header_depth,
  start_row_init,
  start_row,
  start_col,
  end_row,
  end_col,
  options = NULL
) {

  options_default <- list()

  options_default$first_col_width <- 20
  options_default$row_height <- 20

  options_default$title <- openxlsx::createStyle(
    fontSize = 13,
    textDecoration = 'bold'
  )

  options_default$style_indent <- openxlsx::createStyle(
    indent = 1,
    numFmt = '#,##0',
    valign = 'center'
  )

  options_default$style_header <- openxlsx::createStyle(
    wrapText = T,
    valign = 'center',
    fgFill = '#f5f5f5',
    border = c('top', 'bottom', 'left', 'right'),
    borderStyle = 'dashed',
    borderColour = 'gray'
  )

  options_default$border_right_outer <- openxlsx::createStyle(
    border = 'right',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_left_outer <- openxlsx::createStyle(
    border = 'left',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_top_outer <- openxlsx::createStyle(
    border = 'top',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_bottom_outer <- openxlsx::createStyle(
    border = 'bottom',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_header <- openxlsx::createStyle(
    border = 'bottom',
    borderColour = '#757575',
    borderStyle = 'double'
  )

  options_default$footnote <- openxlsx::createStyle(
    fontSize = 10,
    textDecoration = 'italic'
  )

  options <- c(options, options_default)

  # Default width of the first column
  start_col_width <- start_col - 1
  openxlsx::setColWidths(
    ...,
    cols = 1:start_col,
    widths = c(rep(2, start_col_width), options$first_col_width)
  )

  openxlsx::setRowHeights(
    ...,
    rows = 1:end_row,
    heights = options$row_height
  )

  end_row_header <- header_depth + start_row - 1
  openxlsx::addStyle(
    ...,
    style = options$style_header,
    rows = start_row:end_row_header,
    cols = start_col:end_col,
    gridExpand = T,
    stack = T
  )

  openxlsx::addStyle(
    ...,
    style = options$style_indent,
    rows = 1:end_row,
    cols = start_col:end_col,
    gridExpand = T,
    stack = T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_right_outer,
    rows = start_row:end_row,
    cols = end_col,
    gridExpand = T,
    stack = T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_left_outer,
    rows = start_row:end_row,
    cols = start_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_top_outer,
    rows = start_row,
    cols = start_col:end_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_bottom_outer,
    rows = end_row,
    cols = start_col:end_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_header,
    rows = end_row_header,
    cols = start_col:end_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$title,
    rows = start_row_init,
    cols = start_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$footnote,
    rows = end_row + 2,
    cols = start_col,
    gridExpand =  T,
    stack =  T
  )

}

# ------------------------------------------------------------------------------
extract_column_names <- function(
  df,
  start_col,
  start_row,
  y_group_separator = '>'
) {

  value <- NULL
  n <- NULL
  col_from <- NULL
  row_from <- NULL
  data <- NULL

  dplyr::as_tibble(names(df)) |>
    dplyr::mutate(
      value = stringr::str_split(value, y_group_separator),
      col_from = 1:n()
    ) |>
    dplyr::mutate(col_from = col_from + start_col - 1) |>
    tidyr::unnest(value) |>
    dplyr::group_by(col_from) |>
    dplyr::mutate(row_from = 1:n()) |>
    dplyr::mutate(row_from = row_from + start_row - 1) |>
    tidyr::nest() |>
    dplyr::mutate(depth = purrr::map_int(data, nrow)) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::group_by(value) |>
    tidyr::nest() |>
    dplyr::mutate(r = purrr::map_int(data, nrow)) |>
    tidyr::unnest(data) |>
    dplyr::arrange(col_from)
}

# ------------------------------------------------------------------------------
set_sheet_name <- function(wb) {
  sheet <- paste0('Sheet ', length(names(wb)) + 1)
  return(sheet)
}
