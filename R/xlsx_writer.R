xlsx_write_data <- function(
  wb,
  data,
  sheet_name,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  offset_row = 0,
  offset_col = 0,
  collapse_list = FALSE,
  names_separator = "__",
  row_group_as_column = FALSE,
  include_table_list = FALSE,
  facade = get_tsg_facade()
) {

  facade <- resolve_facade(facade, attributes(data)$facade)

  table_hidden <- FALSE %||% facade$table.hidden

  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name,
    gridLines = facade$table.gridLines,
    tabColour = facade$table.tabColour,
    visible = !table_hidden,
    ...
  )

  if(include_table_list) {

    list_title <- 'List of Tables' %||% facade$label.titleTableList
    back_label <- '< Back' %||% facade$label.backHyperlink

    openxlsx::writeFormula(
      wb = wb,
      sheet = sheet_name,
      startCol = 1,
      startRow = 1,
      x = openxlsx::makeHyperlinkString(
        sheet = list_title,
        text = back_label
      )
    )

    offset_row <- offset_row + 1

  }

  start_col <- offset_col + 1

  title <- title %||% attributes(data)$title
  subtitle <- subtitle %||% attributes(data)$subtitle
  footnotes <- footnotes %||% attributes(data)$footnotes$text
  source_note <- resolve_source_note(data, source_note)

  groups <- attributes(data)$groups

  wb <- xlsx_write_title(
    wb = wb,
    sheet_name = sheet_name,
    title = title,
    subtitle = subtitle,
    offset_row = offset_row,
    offset_col = offset_col,
    facade = facade
  )

  offset_row <- attributes(wb)$offset_row

  corners <- c("top", "bottom", "left", "right")

  if(inherits(data, "list")) {

    row_titles <- names(data)
    offset_row_i <- offset_row

    if(!is.null(groups)) {

      data_first <- dplyr::ungroup(data[[1]])

      if(!row_group_as_column) {
        data_first <- dplyr::select(data_first, -dplyr::any_of(groups))
      }

      wb <- xlsx_header_merge(
        wb = wb,
        data = data_first,
        sheet = sheet_name,
        offset_row = offset_row_i,
        offset_col = offset_col,
        names_separator = names_separator
      )

      header_depth_i <- attributes(wb)$header_depth
      header_width_pad_i <- 0
      if(header_depth_i == 1) { header_width_pad_i <- 6 }

      xlsx_eval_style(
        wb = wb,
        sheet_name = sheet_name,
        style = extract_facade(facade, 'header'),
        rows = (1:header_depth_i) + offset_row_i,
        cols = start_col:(ncol(data_first) + start_col - 1)
      )

      for(i in seq_along(row_titles)) {

        row_title <- row_titles[i]

        data_i <- dplyr::ungroup(data[[i]])
        border_outer <- extract_facade(facade, 'border_outer')

        if(!row_group_as_column) {

          offset_row_i <- offset_row_i + 1
          data_i <- dplyr::select(data_i, -dplyr::any_of(groups))

          openxlsx::writeData(
            wb = wb,
            x = row_title,
            sheet = sheet_name,
            startRow = header_depth_i + offset_row_i,
            startCol = start_col,
            colNames = FALSE
          )

          openxlsx::addStyle(
            wb = wb,
            sheet = sheet_name,
            style = openxlsx::createStyle(textDecoration = "bold"),
            rows = header_depth_i + offset_row_i,
            cols = start_col:(ncol(data_first) + start_col - 1),
            gridExpand = TRUE,
            stack = TRUE
          )

          openxlsx::mergeCells(
            wb = wb,
            sheet = sheet_name,
            rows = header_depth_i + offset_row_i,
            cols = start_col:(ncol(data_first) + start_col - 1)
          )

          border_outer$border <- corners

        } else {
          border_outer$border <- "bottom"
        }

        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = border_outer,
          rows = header_depth_i + offset_row_i,
          cols = start_col:(ncol(data_first) + start_col - 1)
        )

        openxlsx::writeData(
          wb = wb,
          x = convert_factor(data_i),
          sheet = sheet_name,
          startRow = header_depth_i + offset_row_i + 1,
          startCol = start_col,
          colNames = FALSE
        )

        if(extract_facade(facade, 'table', 'lastRowBold')) {

          openxlsx::addStyle(
            wb = wb,
            sheet = sheet_name,
            style = openxlsx::createStyle(textDecoration = "bold"),
            rows = header_depth_i + offset_row_i + nrow(data_i),
            cols = start_col:(ncol(data_first) + start_col - 1),
            gridExpand = TRUE,
            stack = TRUE
          )
        }

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = (header_depth_i + offset_row_i) + 1:nrow(data_i),
          heights = extract_facade(facade, 'body', 'height')
        )

        if(!row_group_as_column) {

          openxlsx::setRowHeights(
            wb = wb,
            sheet = sheet_name,
            rows = header_depth_i + offset_row_i,
            heights = extract_facade(facade, 'row_group', 'height')
          )

        } else {

          which_group_cols <- which(names(data_i) %in% groups)

          for(j in seq_along(which_group_cols)) {
            openxlsx::mergeCells(
              wb = wb,
              sheet = sheet_name,
              rows = (header_depth_i + offset_row_i) + 1:nrow(data_i),
              cols = start_col + (which_group_cols[j] - 1)
            )
          }
        }

        offset_row_i <- offset_row_i + nrow(data_i)

      }

      xlsx_eval_style(
        wb = wb,
        sheet_name = sheet_name,
        style = extract_facade(facade, 'border_header'),
        rows = header_depth_i + offset_row,
        cols = start_col:(ncol(data_first) + start_col - 1)
      )

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = offset_row + 1,
        heights = extract_facade(facade, 'header', 'height') + header_width_pad_i
      )

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = header_depth_i + offset_row,
        heights = extract_facade(facade, 'border_bottom', 'height')
      )

      # body
      xlsx_eval_style(
        wb = wb,
        sheet_name = sheet_name,
        style = extract_facade(facade, 'body'),
        rows = (offset_row + 1):(header_depth_i + offset_row_i),
        cols = start_col:(ncol(data_first) + start_col - 1)
      )

      # outer borders
      # c("top", "bottom", "left", "right")
      corner_rows <- list(
        offset_row + 1,
        offset_row_i + header_depth_i,
        (offset_row + 1):(offset_row_i + header_depth_i),
        (offset_row + 1):(offset_row_i + header_depth_i)
      )

      corner_cols <- list(
        start_col:(ncol(data_first) + start_col - 1),
        start_col:(ncol(data_first) + start_col - 1),
        start_col,
        (ncol(data_first) + start_col - 1)
      )

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

      xlsx_decimal_format(
        wb = wb,
        data = data_i,
        sheet_name = sheet_name,
        rows = (offset_row + header_depth_i):(offset_row_i + header_depth_i),
        offset = start_col - 1,
        cols = extract_facade(facade, 'table', 'decimalCols'),
        precision = extract_facade(facade, 'table', 'decimalPrecision')
      )

      xlsx_colwidths(
        wb = wb,
        sheet_name = sheet_name,
        facade = facade,
        cols = start_col:(ncol(data_first) + start_col - 1),
        offset = offset_col
      )

      if(!is.null(source_note)) {

        openxlsx::writeData(
          wb = wb,
          x = source_note,
          sheet = sheet_name,
          startRow = offset_row_i + header_depth_i + 1,
          startCol = start_col,
          colNames = FALSE
        )

        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'source_note'),
          rows = offset_row_i + header_depth_i + 1,
          cols = start_col
        )

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = offset_row_i + header_depth_i + 1,
          heights = extract_facade(facade, 'source_note', 'height')
        )

        offset_row_i <- offset_row_i + 1

      }

      xlsx_write_footnotes(
        wb = wb,
        sheet_name = sheet_name,
        footnotes = footnotes,
        offset_row = offset_row_i + header_depth_i + 1,
        offset_col = offset_col,
        facade = facade
      )

      xlsx_colwidths(
        wb = wb,
        sheet_name = sheet_name,
        facade = facade,
        cols = offset_row_i + header_depth_i + 1,
        offset = offset_col
      )

    } else {

      for(i in seq_along(row_titles)) {

        row_title <- row_titles[i]
        data_i <- dplyr::select(data[[i]], -1)

        openxlsx::writeData(
          wb = wb,
          x = row_title,
          sheet = sheet_name,
          startRow = offset_row_i + 1,
          startCol = start_col,
          colNames = FALSE
        )

        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'sub_group'),
          rows = offset_row_i + 1,
          cols = start_col
        )

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = offset_row_i + 1,
          heights = extract_facade(facade, 'subtitle', 'height')
        )

        # subtitle style
        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'subtitle'),
          rows = offset_row_i + 1,
          cols = start_col
        )

        offset_row_i <- offset_row_i + 1

        wb <- xlsx_header_merge(
          wb = wb,
          data = data_i,
          sheet = sheet_name,
          offset_row = offset_row_i,
          offset_col = offset_col,
          names_separator = names_separator
        )

        header_depth_i <- attributes(wb)$header_depth
        header_width_pad_i <- 0
        if(header_depth_i == 1) { header_width_pad_i <- 6 }

        # header style
        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'header'),
          rows = (1:header_depth_i) + offset_row_i,
          cols = start_col:(ncol(data_i) + start_col - 1)
        )

        openxlsx::writeData(
          wb = wb,
          x = convert_factor(dplyr::ungroup(data_i)),
          sheet = sheet_name,
          startRow = header_depth_i + offset_row_i + 1,
          startCol = start_col,
          colNames = FALSE
        )

        # body
        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'body'),
          rows = 1:(header_depth_i + nrow(data_i)) + offset_row_i,
          cols = start_col:(ncol(data_i) + start_col - 1)
        )

        if(extract_facade(facade, 'table', 'lastRowBold')) {
          openxlsx::addStyle(
            wb = wb,
            sheet = sheet_name,
            style = openxlsx::createStyle(textDecoration = "bold"),
            rows = header_depth_i + nrow(data_i) + offset_row_i,
            cols = start_col:(ncol(data_i) + start_col - 1),
            gridExpand = TRUE,
            stack = TRUE
          )
        }

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = 1:(header_depth_i + nrow(data_i)) + offset_row_i,
          heights = extract_facade(facade, 'body', 'height')
        )

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = (1:header_depth_i) + offset_row_i,
          heights = extract_facade(facade, 'header', 'height') + header_width_pad_i
        )

        # header border
        xlsx_eval_style(
          wb = wb,
          sheet_name = sheet_name,
          style = extract_facade(facade, 'border_header'),
          rows = header_depth_i + offset_row_i,
          cols = start_col:(ncol(data_i) + start_col - 1)
        )

        openxlsx::setRowHeights(
          wb = wb,
          sheet = sheet_name,
          rows = header_depth_i + offset_row_i,
          heights = extract_facade(facade, 'border_bottom', 'height')
        )

        # outer borders
        # c("top", "bottom", "left", "right")
        corner_rows <- list(
          offset_row_i + 1,
          nrow(data_i) + header_depth_i + offset_row_i,
          (offset_row_i + 1):(nrow(data_i) + header_depth_i + offset_row_i),
          (offset_row_i + 1):(nrow(data_i) + header_depth_i + offset_row_i)
        )

        corner_cols <- list(
          start_col:(ncol(data_i) + start_col - 1),
          start_col:(ncol(data_i) + start_col - 1),
          start_col,
          ncol(data_i) + start_col - 1
        )

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

        offset_row_dec <- offset_row_i

        if(!is.null(source_note)) {

          offset_row_i <- offset_row_i + 1

          openxlsx::writeData(
            wb = wb,
            x = source_note,
            sheet = sheet_name,
            startRow = header_depth_i + offset_row_i + nrow(data_i),
            startCol = start_col,
            colNames = FALSE
          )

          xlsx_eval_style(
            wb = wb,
            sheet_name = sheet_name,
            style = extract_facade(facade, 'source_note'),
            rows = header_depth_i + offset_row_i + nrow(data_i),
            cols = start_col
          )

          openxlsx::setRowHeights(
            wb = wb,
            sheet = sheet_name,
            rows = header_depth_i + offset_row_i + nrow(data_i),
            heights = extract_facade(facade, 'source_note', 'height')
          )

          wb <- xlsx_write_footnotes(
            wb = wb,
            sheet_name = sheet_name,
            footnotes = footnotes,
            offset_row = header_depth_i + offset_row_i + nrow(data_i),
            offset_col = offset_col,
            facade = facade
          )

          offset_row_i <- offset_row_i + attributes(wb)$offset_row

        } else {
          offset_row_i <- offset_row_i + 3
        }

        xlsx_decimal_format(
          wb = wb,
          data = data_i,
          sheet_name = sheet_name,
          rows = (offset_row_dec + 1):(nrow(data_i) + header_depth_i + offset_row_i),
          offset = start_col - 1,
          cols = extract_facade(facade, 'table', 'decimalCols'),
          precision = extract_facade(facade, 'table', 'decimalPrecision')
        )

        offset_row_i <- offset_row_i + nrow(data_i)

        xlsx_colwidths(
          wb = wb,
          sheet_name = sheet_name,
          facade = facade,
          cols = start_col:(ncol(data_i) + start_col - 1),
          offset = offset_col
        )

      }

    }

  } else {

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
      sheet_name = sheet_name,
      style = extract_facade(facade, 'header'),
      rows = (1:header_depth) + offset_row,
      cols = start_col:(ncol(data) + start_col - 1)
    )

    openxlsx::writeData(
      wb = wb,
      x = convert_factor(data),
      sheet = sheet_name,
      startRow = header_depth + offset_row + 1,
      startCol = start_col,
      colNames = FALSE
    )

    # body
    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'body'),
      rows = 1:(header_depth + nrow(data)) + offset_row,
      cols = start_col:(ncol(data) + start_col - 1)
    )

    xlsx_decimal_format(
      wb = wb,
      data = data,
      sheet_name = sheet_name,
      rows = 1:(header_depth + nrow(data)) + offset_row,
      offset = start_col - 1,
      cols = extract_facade(facade, 'table', 'decimalCols'),
      precision = extract_facade(facade, 'table', 'decimalPrecision')
    )

    if(extract_facade(facade, 'table', 'lastRowBold')) {
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
      heights = extract_facade(facade, 'body', 'height')
    )

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = (1:header_depth) + offset_row,
      heights = extract_facade(facade, 'header', 'height') + header_width_pad
    )

    # header border
    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'border_header'),
      rows = header_depth + offset_row,
      cols = start_col:(ncol(data) + start_col - 1)
    )

    if(header_depth > 1) {
      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = header_depth + offset_row,
        heights = extract_facade(facade, 'border_bottom', 'height')
      )
    }

    # outer borders
    # c("top", "bottom", "left", "right")
    corner_rows <- list(
      offset_row + 1,
      nrow(data) + header_depth + offset_row,
      (offset_row + 1):(nrow(data) + header_depth + offset_row),
      (offset_row + 1):(nrow(data) + header_depth + offset_row)
    )

    corner_cols <- list(
      start_col:(ncol(data) + start_col - 1),
      start_col:(ncol(data) + start_col - 1),
      start_col,
      ncol(data) + start_col - 1
    )

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

    if(!is.null(source_note)) {

      openxlsx::writeData(
        wb = wb,
        x = source_note,
        sheet = sheet_name,
        startRow = header_depth + offset_row + 1 + nrow(data),
        startCol = start_col,
        colNames = FALSE
      )

      xlsx_eval_style(
        wb = wb,
        sheet_name = sheet_name,
        style = extract_facade(facade, 'source_note'),
        rows = header_depth + offset_row + 1 + nrow(data),
        cols = start_col
      )

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = header_depth + offset_row + 1 + nrow(data),
        heights = extract_facade(facade, 'source_note', 'height')
      )

    }

    xlsx_write_footnotes(
      wb = wb,
      sheet_name = sheet_name,
      footnotes = footnotes,
      offset_row = header_depth + offset_row + 1 + nrow(data),
      offset_col = offset_col,
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

  return(wb)

}
