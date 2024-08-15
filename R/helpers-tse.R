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
  start_row_note,
  decimal_format_cols = NULL,
  format_precision = 2,
  options = NULL
) {

  options_default <- list()

  options_default$col_width_first <- 35
  options_default$col_width_all <- 20
  options_default$row_height <- 20
  options_default$row_height_header <- 30

  # options_default$decimal <- openxlsx::createStyle(
  #   indent = 1,
  #   numFmt = paste0('#,#0.', paste0(rep(0, as.integer(format_precision)), collapse = ''))
  # )

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

  options <- c(options, options_default)

  # Default width of the first column
  start_col_width <- start_col - 1
  start_col_plus <- start_col + 1

  # Start col
  openxlsx::setColWidths(..., cols = 1:start_col_width, widths = rep(2, start_col_width))
  openxlsx::setColWidths(..., cols = start_col, widths = options$col_width_first)

  # Middle cols
  if(length(options$col_width_all) > length(start_col_plus:end_col)) {
    col_width_all_fit <- options$col_width_all[1:length(start_col_plus:end_col)]
  } else {
    col_width_all_fit <- options$col_width_all
  }
  openxlsx::setColWidths(..., cols = start_col_plus:end_col, widths = col_width_all_fit)

  # End col
  if(!is.null(options$col_width_last) | length(is.na(options$col_width_last)) > 0) {
    openxlsx::setColWidths(..., cols = end_col, widths = options$col_width_last)
  }

  end_row_header <- header_depth + start_row - 1
  start_row_tb <- end_row_header + 1


  openxlsx::setRowHeights(..., rows = 1, heights = 15)
  openxlsx::setRowHeights(..., rows = start_row:end_row_header, heights = options$row_height_header)
  openxlsx::setRowHeights(..., rows = start_row_tb:end_row, heights = options$row_height)

  # if(!is.null())
  openxlsx::setRowHeights(..., rows = start_row_tb:end_row, heights = options$row_height)

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
    rows = start_row:end_row,
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
    style = openxlsx::createStyle(
      textDecoration = NULL,
      fontColour = '#07403b',
      fontSize = 9
    ),
    rows = 1,
    cols = start_col,
    gridExpand =  T,
    stack =  T
  )

  # Decimal
  if(length(decimal_format_cols) > 0) {

    add_decimal_style <- function(cols, precise) {
      openxlsx::addStyle(
        ...,
        style = openxlsx::createStyle(
          indent = 1,
          numFmt = paste0('#,#0.', paste0(rep(0, as.integer(precise)), collapse = ''))
        ),
        rows = end_row_header:end_row,
        cols = cols + start_col - 1,
        gridExpand = T,
        stack = T
      )
    }

    if(inherits(decimal_format_cols, 'list')) {

      for(d in 1:length(format_precision)) {
        add_decimal_style(decimal_format_cols[[d]], format_precision[d])
      }

    } else {

      add_decimal_style(decimal_format_cols, format_precision)

    }

  }

}

# ------------------------------------------------------------------------------
extract_column_names <- function(
  df,
  start_col = 1,
  start_row = 1,
  names_separator = '>'
) {

  value <- NULL
  n <- NULL
  col_from <- NULL
  row_from <- NULL
  data <- NULL

  dplyr::as_tibble(names(df)) |>
    dplyr::mutate(
      value = stringr::str_split(value, names_separator),
      col_from = 1:dplyr::n()
    ) |>
    dplyr::mutate(col_from = col_from + start_col - 1) |>
    tidyr::unnest(value) |>
    dplyr::group_by(col_from) |>
    dplyr::mutate(row_from = 1:dplyr::n()) |>
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
