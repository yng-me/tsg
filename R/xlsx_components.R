xlsx_write_title <- function(
  wb,
  sheet_name,
  title = NULL,
  subtitle = NULL,
  offset_row = 0,
  offset_col = 0,
  start_col = 1,
  start_row = 1,
  facade = getOption("tsg.options.facade")
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
      style = facade$styles$title,
      cols = start_col + offset_col,
      rows = start_row + offset_row
    )

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = start_row + offset_row,
      heights = facade$heights$title
    )

    offset_row <- offset_row + 1

    if(!is.null(subtitle)) {

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
        style = facade$styles$subtitle,
        rows = start_row + offset_row,
        cols = start_col + offset_col
      )

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = start_row + offset_row,
        heights = facade$heights$subtitle
      )

      offset_row <- offset_row + 1

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
  facade = getOption("tsg.options.facade")
) {

  if(length(footnotes) == 0) return(wb)

  for(i in seq_along(footnotes)) {

    footnote <- footnotes[[i]]

    if(inherits(footnote, 'list')) {
      footnote_text <- footnote$text
    } else {
      footnote_text <- footnote
    }

    openxlsx::writeData(
      wb = wb,
      x = footnote_text,
      sheet = sheet_name,
      startRow =  offset_row + i,
      startCol = offset_col + 1,
      colNames = FALSE
    )

  }

  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = facade$styles$footnote,
    cols = offset_col + 1,
    rows = offset_row:(offset_row + length(footnotes))
  )


  attr(wb, "offset_row") <- length(footnotes)

  return(wb)

}
