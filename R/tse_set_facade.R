tse_set_facade <- function(
  ...,
  header_depth,
  start_row_init,
  start_row,
  start_col,
  end_row,
  end_col,
  options = NULL
  ) {

  if(is.null(options)) {
    options <- list()
    options$first_col_width <- 20
    options$row_height <- 20

    options$title <- openxlsx::createStyle(
      fontSize = 13,
      textDecoration = 'bold'
    )

    options$style_indent <- openxlsx::createStyle(
      indent = 1,
      numFmt = '#,##0',
      valign = 'center'
    )

    options$style_header <- openxlsx::createStyle(
      wrapText = T,
      valign = 'center',
      fgFill = '#f5f5f5',
      border = c('top', 'bottom', 'left', 'right'),
      borderStyle = 'dashed',
      borderColour = 'gray'
    )

    options$border_right_outer <- openxlsx::createStyle(
      border = 'right',
      borderColour = '#757575',
      borderStyle = 'medium'
    )

    options$border_left_outer <- openxlsx::createStyle(
      border = 'left',
      borderColour = '#757575',
      borderStyle = 'medium'
    )

    options$border_top_outer <- openxlsx::createStyle(
      border = 'top',
      borderColour = '#757575',
      borderStyle = 'medium'
    )

    options$border_bottom_outer <- openxlsx::createStyle(
      border = 'bottom',
      borderColour = '#757575',
      borderStyle = 'medium'
    )

    options$border_header <- openxlsx::createStyle(
      border = 'bottom',
      borderColour = '#757575',
      borderStyle = 'double'
    )

    options$footnote <- openxlsx::createStyle(
      fontSize = 10,
      textDecoration = 'italic'
    )
  }

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
