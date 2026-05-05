xlsx_write_title <- function(
  wb,
  sheet_name,
  title = NULL,
  subtitle = NULL,
  offset_row = 0,
  offset_col = 0,
  start_col = 1,
  start_row = 1,
  facade = get_tsg_facade()
) {

  if(!is.null(title)) {

    openxlsx::writeData(
      wb = wb,
      x = title,
      sheet = sheet_name,
      startRow = start_row + offset_row,
      startCol = start_col + offset_col,
      colNames = FALSE
    )

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'title'),
      cols = start_col + offset_col,
      rows = start_row + offset_row
    )

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = start_row + offset_row,
      heights = extract_facade(facade, 'title', 'height')
    )

    offset_row <- offset_row + 1

    if(!is.null(subtitle)) {

      if(!is.na(subtitle) & subtitle != '') {

        openxlsx::writeData(
          wb = wb,
          x = subtitle,
          sheet = sheet_name,
          startRow = start_row + offset_row,
          startCol = start_col + offset_col,
          colNames = FALSE
        )

        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'subtitle'),
          rows = start_row + offset_row,
          cols = start_col + offset_col
        )

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = start_row + offset_row,
          heights = extract_facade(facade, 'subtitle', 'height')
        )

        offset_row <- offset_row + 1
      }

    }

    offset_row <- offset_row + 1
  }

  attr(wb, "offset_row") <- offset_row

  return(wb)

}


xlsx_write_footnotes <- function(
  wb,
  footnotes,
  sheet_name,
  offset_row = 0,
  offset_col = 0,
  n_cols = 1,
  facade = get_tsg_facade()
) {

  footnotes <- .normalize_footnotes(footnotes)
  if (is.null(footnotes) || length(footnotes$text) == 0L) return(wb)

  n <- length(footnotes$text)

  for (i in seq_len(n)) {

    fn_text      <- footnotes$text[[i]]
    fn_placement <- footnotes$placement[[i]] %||% "auto"

    # For right-aligned footnotes, write starting from the right-most data column
    start_col <- if (fn_placement == "right") {
      offset_col + max(1L, n_cols)
    } else {
      offset_col + 1L
    }

    openxlsx::writeData(
      wb       = wb,
      x        = fn_text,
      sheet    = sheet_name,
      startRow = offset_row + i,
      startCol = start_col,
      colNames = FALSE
    )

    fn_halign <- if (fn_placement == "right") "right" else "left"

    footnote_style <- extract_facade(facade, 'footnotes')
    footnote_style[["halign"]] <- fn_halign

    xlsx_eval_style(
      wb         = wb,
      sheet_name = sheet_name,
      style      = footnote_style,
      cols       = start_col,
      rows       = offset_row + i
    )

  }

  # Apply base footnote style to the whole footnote row range (for borders etc.)
  xlsx_eval_style(
    wb         = wb,
    sheet_name = sheet_name,
    style      = extract_facade(facade, 'footnotes'),
    cols       = (offset_col + 1L):(offset_col + max(1L, n_cols)),
    rows       = (offset_row + 1L):(offset_row + n)
  )

  attr(wb, "offset_row") <- n

  return(wb)

}


resolve_source_note <- function(data, source_note) {

  source_note <- source_note %||% attributes(data)$source_note

  if(!is.null(source_note)) {
    if(!grepl('^source', source_note, ignore.case = TRUE)) {
      source_note <- glue::glue("Source: {source_note}")
    }
  }

  source_note

}


