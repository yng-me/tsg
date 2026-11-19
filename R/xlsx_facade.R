xlsx_eval_style <- function(wb, sheet_name, style, cols, rows) {

  style <- purrr::discard(style, function(x) length(x) == 0)

  if(length(style) > 0) {
    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      stack = TRUE,
      gridExpand = TRUE,
      cols = cols,
      rows = rows,
      style = do.call(openxlsx::createStyle, style)
    )
  }

  return(wb)
}


xlsx_decimal_format <- function(wb, data, sheet_name, rows, offset, cols = NULL, precision = 3) {

  if(!is.numeric(precision) || length(precision) != 1) precision <- 2
  formats <- number_format_cols(data, cols)

  number_format <- function(columns, num_fmt) {
    if(length(columns) == 0) return(invisible())
    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      cols = match(columns, names(data)) + offset,
      rows = rows,
      gridExpand = TRUE,
      stack = TRUE,
      style = openxlsx::createStyle(numFmt = num_fmt)
    )
  }

  number_format(formats$integer, "#,##0")
  number_format(formats$decimal, if(precision > 0) paste0("#,##0.", strrep("0", precision)) else "#,##0")

  return(wb)
}

xlsx_corner_borders <- function(
  wb,
  sheet_name,
  row_start,
  row_end,
  col_start,
  col_end,
  facade
) {

  corners <- c("top", "bottom", "left", "right")

  corner_rows <- list(row_start, row_end, row_start:row_end, row_start:row_end)
  corner_cols <- list(col_start:col_end, col_start:col_end, col_start, col_end)

  # border_outer.border picks the sides; a facade setting it to ~ draws none.
  for(i in which(corners %in% facade$border_outer.border)) {

    facade$border_outer.border <- corners[i]

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'border_outer'),
      rows = corner_rows[[i]],
      cols = corner_cols[[i]]
    )
  }

  return(wb)
}


# table.* style properties (font, fill, alignment, ...) over the whole table.
xlsx_apply_table_style <- function(wb, sheet_name, facade, rows, cols) {
  table_style <- extract_facade(facade, 'table')
  table_style <- table_style[!names(table_style) %in% c("locked", "hidden")]
  xlsx_eval_style(wb, sheet_name, style = table_style, rows = rows, cols = cols)
  wb
}


xlsx_apply_col_styles <- function(wb, sheet_name, facade, start_col, end_col, rows) {
  xlsx_eval_style(wb, sheet_name, extract_facade(facade, 'col_first'), rows = rows, cols = start_col)
  xlsx_eval_style(wb, sheet_name, extract_facade(facade, 'col_last'), rows = rows, cols = end_col)
  wb
}


# Fill every other body row (the 2nd, 4th, ...) with body.bandFill, when set.
xlsx_band_rows <- function(wb, sheet_name, rows, cols, facade) {
  fill <- extract_facade(facade, 'body', 'bandFill')
  if(is.null(fill) || length(rows) < 2) return(wb)

  openxlsx::addStyle(
    wb = wb,
    sheet = sheet_name,
    cols = cols,
    rows = rows[seq(2, length(rows), by = 2)],
    gridExpand = TRUE,
    stack = TRUE,
    style = openxlsx::createStyle(fgFill = fill)
  )
  wb
}


# `data` gives the columns: `category` gets `col_first.width` and the grouping columns
# before it get `row_group.width`; a width left unset fits the column's content.
xlsx_colwidths <- function(wb, sheet_name, cols, facade = get_tsg_facade(), offset = 0, data = NULL) {

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols,
    widths = extract_facade(facade, 'table', 'width')
  )

  category <- match("category", names(data))
  if(is.na(category)) category <- 1L

  widths <- list()
  widths[[category]] <- extract_facade(facade, 'col_first', 'width') %||% xlsx_fit_width(data, category)
  for(j in seq_len(category - 1)) {
    widths[[j]] <- extract_facade(facade, 'row_group', 'width') %||% xlsx_fit_width(data, j)
  }
  for(j in seq_along(widths)) {
    if(!is.null(widths[[j]])) {
      openxlsx::setColWidths(wb = wb, sheet = sheet_name, cols = cols[j], widths = widths[[j]])
    }
  }

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[length(cols)],
    widths = extract_facade(facade, 'col_last', 'width')
  )

  if(offset > 0) {
    openxlsx::setColWidths(
      wb = wb,
      sheet = sheet_name,
      cols = 1:offset,
      widths = extract_facade(facade, 'table', 'widthOffset')
    )
  }

  return(wb)
}


# Width (in characters) that fits the longest value or header text of column j.
# shortcut: counts characters, not font size or bold; large or wide fonts may still clip.
xlsx_fit_width <- function(data, j, max_width = 60) {
  if(is.null(data) || j > ncol(data)) return(NULL)
  values <- as.character(convert_factor(data[j])[[1]])
  header <- unlist(get_header(data[j]))
  chars <- max(nchar(c(values, header)), 0, na.rm = TRUE)
  min(max(chars + 4, 8), max_width)
}
