xlsx_eval_style <- function(wb, sheet_name, style, cols, rows) {

  style_fn_args <- purrr::discard(style, is.null)

  if(length(style_fn_args) > 0) {

    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      stack = TRUE,
      gridExpand = TRUE,
      cols = cols,
      rows = rows,
      style = do.call(openxlsx::createStyle, style_fn_args)
    )
  }

  return(wb)
}


xlsx_decimal_format <- function(wb, data, sheet_name, rows, offset, cols = NULL, precision = 3) {

  is_int <- names(dplyr::select(data, dplyr::where(is.integer)))
  maybe_int <- names(dplyr::select(data, dplyr::where(is.numeric)))
  maybe_int <- maybe_int[!(maybe_int %in% is_int)]

  for(i in maybe_int) {

    if(sum(data[[i]], na.rm = TRUE) == sum(as.integer(data[[i]]), na.rm = TRUE)) {
      is_int <- c(is_int, i)
    }
  }

  which_int <- which(names(data) %in% c(is_int, maybe_int))

  if(length(which_int) > 0) {
    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      cols = which_int + offset,
      rows = rows,
      gridExpand = TRUE,
      stack = TRUE,
      style = openxlsx::createStyle(numFmt = "#,##0")
    )
  }

  if(is.null(precision)) { precision <- 2 }
  if(!is.numeric(precision)) { precision <- 2 }

  is_dbl <- names(dplyr::select(data, dplyr::where(is.double)))
  is_dbl <- is_dbl[!(is_dbl %in% is_int)]

  which_dbl <- which(names(data) %in% is_dbl)

  if(length(which_dbl) > 0) {
    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      cols = which_dbl + offset,
      rows = rows,
      gridExpand = TRUE,
      stack = TRUE,
      style = openxlsx::createStyle(
        numFmt = paste0('#,##0.', paste0(rep(0, as.integer(precision)), collapse = ''))
      )
    )
  }

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

  for(i in 1:4) {

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


xlsx_apply_table_style <- function(wb, sheet_name, facade, rows, cols) {
  table_style <- extract_facade(facade, 'table')
  table_style <- table_style[!names(table_style) %in% c("locked", "hidden")]
  xlsx_eval_style(wb, sheet_name, style = table_style, rows = rows, cols = cols)
  wb
}


xlsx_apply_spanner_style <- function(wb, sheet_name, facade, header_depth, offset_row, start_col, end_col) {
  if (header_depth > 1) {
    xlsx_eval_style(
      wb = wb, sheet_name = sheet_name,
      style = extract_facade(facade, 'spanner'),
      rows = (1:(header_depth - 1)) + offset_row,
      cols = start_col:end_col
    )
    spanner_height <- extract_facade(facade, 'spanner', 'height')
    if (!is.null(spanner_height)) {
      openxlsx::setRowHeights(wb, sheet_name,
        rows = (1:(header_depth - 1)) + offset_row,
        heights = spanner_height
      )
    }
  }
  wb
}


xlsx_apply_col_styles <- function(wb, sheet_name, facade, start_col, end_col, rows) {
  xlsx_eval_style(wb, sheet_name, extract_facade(facade, 'col_first'), rows = rows, cols = start_col)
  xlsx_eval_style(wb, sheet_name, extract_facade(facade, 'col_last'), rows = rows, cols = end_col)
  wb
}


xlsx_colwidths <- function(wb, sheet_name, cols, facade = get_tsg_facade(), offset = 0) {

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols,
    widths = extract_facade(facade, 'table', 'width')
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[1],
    widths = extract_facade(facade, 'col_first', 'width')
  )

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
