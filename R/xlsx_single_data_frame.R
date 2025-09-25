xlsx_single_data_frame <- function(
  wb,
  data,
  sheet_name,
  offset_row,
  offset_col,
  start_col,
  names_separator,
  facade,
  source_note = NULL,
  footnotes = NULL
) {

  wb <- xlsx_header_merge(
    wb = wb,
    data = data,
    sheet = sheet_name,
    offset_row = offset_row,
    offset_col = offset_col,
    names_separator = names_separator
  )

  header_depth <- attributes(wb)$header_depth
  header_width_pad <- 0
  if(header_depth == 1) { header_width_pad <- 6 }

  # header style
  xlsx_eval_style(
    wb = wb,
    sheet = sheet_name,
    style = facade$styles$header,
    rows = (1:header_depth) + offset_row,
    cols = start_col:(ncol(data) + start_col - 1)
  )

  openxlsx::writeData(
    wb = wb,
    x = dplyr::mutate_if(data, haven::is.labelled, haven::as_factor),
    sheet = sheet_name,
    startRow = header_depth + offset_row + 1,
    startCol = start_col,
    colNames = F
  )

  # body
  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = facade$styles$body,
    rows = 1:(header_depth + nrow(data)) + offset_row,
    cols = start_col:(ncol(data) + start_col - 1)
  )

  xlsx_decimal_format(
    wb = wb,
    data = data,
    sheet_name = sheet_name,
    rows = 1:(header_depth + nrow(data)) + offset_row,
    offset = start_col - 1
  )

  if(facade$lastRowBold) {
    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      style = openxlsx::createStyle(textDecoration = "bold"),
      rows = header_depth + nrow(data) + offset_row,
      cols = start_col:(ncol(data) + start_col - 1),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::setRowHeights(
    wb = wb,
    sheet = sheet_name,
    rows = 1:(header_depth + nrow(data)) + offset_row,
    heights = facade$heights$body
  )

  openxlsx::setRowHeights(
    wb = wb,
    sheet = sheet_name,
    rows = (1:header_depth) + offset_row,
    heights = facade$heights$header + header_width_pad
  )

  # header border
  xlsx_eval_style(
    wb = wb,
    sheet = sheet_name,
    style = facade$styles$border_header,
    rows = header_depth + offset_row,
    cols = start_col:(ncol(data) + start_col - 1)
  )

  if(header_depth > 1) {
    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = header_depth + offset_row,
      heights = facade$heights$bottomHeader
    )
  }

  xlsx_corner_borders(
    wb = wb,
    sheet_name = sheet_name,
    row_start = offset_row + 1,
    row_end = nrow(data) + header_depth + offset_row,
    col_start = start_col,
    col_end = ncol(data) + start_col - 1,
    facade = facade
  )

  xlsx_write_endnotes(
    wb = wb,
    sheet_name = sheet_name,
    source_note = source_note,
    footnotes = footnotes,
    start_col = start_col,
    start_row = header_depth + offset_row + 1 + nrow(data),
    facade = facade
  )

  xlsx_colwidths(
    wb = wb,
    sheet_name = sheet_name,
    facade = facade,
    cols = start_col:(ncol(data) + start_col - 1),
    offset = offset_col
  )

}
