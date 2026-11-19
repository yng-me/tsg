xlsx_write_data <- function(
  wb,
  data,
  sheet_name,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  offset_row = NULL,
  offset_col = NULL,
  names_separator = "__",
  row_group_as_column = FALSE,
  include_table_list = FALSE,
  facade = get_tsg_facade()
) {

  meta <- table_meta(data)
  facade <- resolve_facade(facade, meta$facade)

  # A sheet of a list nested 3+ groups deep holds lists of tables; the writers below take
  # a list of tables, so flatten the deeper levels into tables named by their path.
  if (inherits(data, "list") && !all(vapply(data, is.data.frame, logical(1)))) {
    data <- copy_table_attrs(flatten_tables(data), data, except = "names")
  }

  # The sheets of a list nested 2+ groups deep hold per-group tables, but only the tables
  # carry their groups. Group columns need the shared-header layout, so take the groups
  # from the tables when they all have the same columns.
  if (row_group_as_column && inherits(data, "list") && is.null(meta$groups)) {
    same_cols <- all(vapply(data, function(d) is.data.frame(d) && identical(names(d), names(data[[1]])), logical(1)))
    if (same_cols && length(attr(data[[1]], "groups")) > 0) {
      attr(data, "groups") <- attr(data[[1]], "groups")
      meta$groups <- attr(data, "groups")
    }
  }

  # A stacked crosstab (multiple_columns_type = "stacked") joins its header levels with
  # its label_separator; split the header at that separator.
  data_for_sep <- if (inherits(data, "list")) data[[1]] else data
  if (isTRUE(attr(data_for_sep, "multiple_columns_type") == "stacked")) {
    names_separator <- attr(data_for_sep, "label_separator") %||% names_separator
  }

  offset_row <- offset_row %||% facade$table.offsetRow
  offset_col <- offset_col %||% facade$table.offsetCol

  # `...` may also hold createWorkbook() arguments; keep the addWorksheet() ones.
  dots <- list(...)
  do.call(
    openxlsx::addWorksheet,
    c(
      list(
        wb = wb,
        sheetName = sheet_name,
        gridLines = facade$table.gridLines,
        tabColour = facade$table.tabColour,
        visible = !isTRUE(facade$table.hidden)
      ),
      dots[names(dots) %in% names(formals(openxlsx::addWorksheet))]
    )
  )

  if(isTRUE(facade$table.locked)) {
    openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE)
  }

  if(include_table_list) {

    openxlsx::writeFormula(
      wb = wb,
      sheet = sheet_name,
      startCol = 1,
      startRow = 1,
      x = openxlsx::makeHyperlinkString(
        sheet = facade$label.titleTableList %||% 'List of Tables',
        text = facade$label.backHyperlink %||% '< Back'
      )
    )

    offset_row <- offset_row + 1
  }

  notes <- list(
    source_note = resolve_source_note(source_note %||% meta$source_note),
    footnotes = footnotes %||% meta$footnotes
  )

  wb <- xlsx_write_title(
    wb = wb,
    sheet_name = sheet_name,
    title = title %||% meta$title,
    subtitle = subtitle %||% meta$subtitle,
    offset_row = offset_row,
    offset_col = offset_col,
    facade = facade
  )

  offset_row <- attributes(wb)$offset_row

  if(!inherits(data, "list")) {

    wb <- rlang::inject(
      xlsx_write_table(wb, data, sheet_name, offset_row, offset_col, names_separator, facade, !!!notes)
    )

  } else if(!is.null(meta$groups)) {

    wb <- rlang::inject(
      xlsx_write_grouped(
        wb, data, sheet_name, offset_row, offset_col, names_separator,
        row_group_as_column, facade, !!!notes
      )
    )

  } else {

    # Tables stacked vertically, each under its own sub-heading.
    for(i in seq_along(data)) {

      data_i <- dplyr::ungroup(data[[i]])
      data_i <- dplyr::select(data_i, -dplyr::any_of(attributes(data[[i]])$groups))

      openxlsx::writeData(
        wb = wb,
        x = names(data)[i],
        sheet = sheet_name,
        startRow = offset_row + 1,
        startCol = offset_col + 1,
        colNames = FALSE
      )

      xlsx_eval_style(
        wb = wb,
        sheet_name = sheet_name,
        style = extract_facade(facade, 'sub_group'),
        rows = offset_row + 1,
        cols = offset_col + 1
      )

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = offset_row + 1,
        heights = extract_facade(facade, 'subtitle', 'height')
      )

      xlsx_eval_style(
        wb = wb,
        sheet_name = sheet_name,
        style = extract_facade(facade, 'subtitle'),
        rows = offset_row + 1,
        cols = offset_col + 1
      )

      wb <- rlang::inject(
        xlsx_write_table(wb, data_i, sheet_name, offset_row + 1, offset_col, names_separator, facade, !!!notes)
      )

      offset_row <- attributes(wb)$last_row + 2
    }
  }

  return(wb)

}


# One table: header, body, borders, notes and column widths.
# Sets attr(wb, "last_row") to the last row written.
xlsx_write_table <- function(
  wb,
  data,
  sheet_name,
  offset_row,
  offset_col,
  names_separator,
  facade,
  source_note = NULL,
  footnotes = NULL
) {

  start_col <- offset_col + 1
  cols <- start_col:(ncol(data) + offset_col)

  wb <- xlsx_header_merge(
    wb = wb,
    data = data,
    sheet = sheet_name,
    offset_row = offset_row,
    offset_col = offset_col,
    names_separator = names_separator
  )

  header_depth <- attributes(wb)$header_depth
  last_row <- offset_row + header_depth + nrow(data)
  rows <- (offset_row + 1):last_row

  # first, so header and body styles stack on top of it
  xlsx_apply_table_style(wb, sheet_name, facade, rows, cols)
  xlsx_header_style(wb, sheet_name, offset_row, header_depth, cols, facade)

  openxlsx::writeData(
    wb = wb,
    x = convert_factor(data),
    sheet = sheet_name,
    startRow = offset_row + header_depth + 1,
    startCol = start_col,
    colNames = FALSE
  )

  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = extract_facade(facade, 'body'),
    rows = rows,
    cols = cols
  )

  xlsx_band_rows(wb, sheet_name, (offset_row + header_depth + 1):last_row, cols, facade)
  xlsx_apply_col_styles(wb, sheet_name, facade, start_col, max(cols), rows)

  xlsx_decimal_format(
    wb = wb,
    data = data,
    sheet_name = sheet_name,
    rows = rows,
    offset = offset_col,
    cols = extract_facade(facade, 'table', 'decimalCols'),
    precision = extract_facade(facade, 'table', 'decimalPrecision')
  )

  if(isTRUE(extract_facade(facade, 'table', 'lastRowBold'))) {
    xlsx_bold_row(wb, sheet_name, last_row, cols)
  }

  openxlsx::setRowHeights(
    wb = wb,
    sheet = sheet_name,
    rows = rows,
    heights = extract_facade(facade, 'body', 'height')
  )

  xlsx_header_heights(wb, sheet_name, offset_row, header_depth, facade)

  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = extract_facade(facade, 'border_header'),
    rows = offset_row + header_depth,
    cols = cols
  )

  xlsx_corner_borders(wb, sheet_name, offset_row + 1, last_row, start_col, max(cols), facade)

  wb <- xlsx_write_notes(wb, sheet_name, last_row, offset_col, length(cols), source_note, footnotes, facade)

  xlsx_colwidths(
    wb = wb,
    sheet_name = sheet_name,
    facade = facade,
    cols = cols,
    offset = offset_col,
    data = data
  )

  wb
}


# A list of per-group tables under one shared header, each group introduced by a
# merged row-group title (or, with row_group_as_column, by its group columns).
xlsx_write_grouped <- function(
  wb,
  data,
  sheet_name,
  offset_row,
  offset_col,
  names_separator,
  row_group_as_column,
  facade,
  source_note = NULL,
  footnotes = NULL
) {

  groups <- table_meta(data)$groups
  start_col <- offset_col + 1

  data_first <- dplyr::ungroup(data[[1]])
  if(!row_group_as_column) {
    data_first <- dplyr::select(data_first, -dplyr::any_of(groups))
  }

  cols <- start_col:(ncol(data_first) + offset_col)

  wb <- xlsx_header_merge(
    wb = wb,
    data = data_first,
    sheet = sheet_name,
    offset_row = offset_row,
    offset_col = offset_col,
    names_separator = names_separator
  )

  header_depth <- attributes(wb)$header_depth

  n_rows <- header_depth + sum(vapply(data, nrow, integer(1))) + if(row_group_as_column) 0 else length(data)
  # first, so header and body styles stack on top of it
  xlsx_apply_table_style(wb, sheet_name, facade, offset_row + seq_len(n_rows), cols)
  xlsx_header_style(wb, sheet_name, offset_row, header_depth, cols, facade)

  row <- offset_row + header_depth # last row written
  member_rows <- integer() # body rows under a row-group title

  for(i in seq_along(data)) {

    data_i <- dplyr::ungroup(data[[i]])
    border_outer <- extract_facade(facade, 'border_outer')

    if(!row_group_as_column) {

      row <- row + 1
      data_i <- dplyr::select(data_i, -dplyr::any_of(groups))

      openxlsx::writeData(
        wb = wb,
        x = names(data)[i],
        sheet = sheet_name,
        startRow = row,
        startCol = start_col,
        colNames = FALSE
      )

      xlsx_bold_row(wb, sheet_name, row, cols)
      xlsx_eval_style(wb, sheet_name, extract_facade(facade, 'row_group'), rows = row, cols = cols)
      openxlsx::mergeCells(wb = wb, sheet = sheet_name, rows = row, cols = cols)

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheet_name,
        rows = row,
        heights = extract_facade(facade, 'row_group', 'height')
      )

      border_outer$border <- intersect(c("top", "bottom", "left", "right"), border_outer$border)

    } else {
      border_outer$border <- intersect("bottom", border_outer$border)
    }

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = border_outer,
      rows = row,
      cols = cols
    )

    body_rows <- row + seq_len(nrow(data_i))
    if(!row_group_as_column) member_rows <- c(member_rows, body_rows)
    xlsx_band_rows(wb, sheet_name, body_rows, cols, facade)

    openxlsx::writeData(
      wb = wb,
      x = convert_factor(data_i),
      sheet = sheet_name,
      startRow = row + 1,
      startCol = start_col,
      colNames = FALSE
    )

    if(isTRUE(extract_facade(facade, 'table', 'lastRowBold'))) {
      xlsx_bold_row(wb, sheet_name, row + nrow(data_i), cols)
    }

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = body_rows,
      heights = extract_facade(facade, 'body', 'height')
    )

    if(row_group_as_column) {
      for(j in start_col + which(names(data_i) %in% groups) - 1) {
        openxlsx::mergeCells(wb = wb, sheet = sheet_name, rows = body_rows, cols = j)
      }
    }

    row <- row + nrow(data_i)
  }

  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = extract_facade(facade, 'border_header'),
    rows = offset_row + header_depth,
    cols = cols
  )

  xlsx_header_heights(wb, sheet_name, offset_row, header_depth, facade)

  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = extract_facade(facade, 'body'),
    rows = (offset_row + 1):row,
    cols = cols
  )

  xlsx_apply_col_styles(wb, sheet_name, facade, start_col, max(cols), (offset_row + 1):row)

  # categories under a row-group title sit one level deeper than the title
  if(length(member_rows) > 0) {
    category_col <- start_col + max(match("category", names(data_first)), 1, na.rm = TRUE) - 1
    indent <- (extract_facade(facade, 'col_first', 'indent') %||% extract_facade(facade, 'body', 'indent') %||% 0) + 1
    xlsx_eval_style(wb, sheet_name, list(indent = indent), rows = member_rows, cols = category_col)
  }

  xlsx_corner_borders(wb, sheet_name, offset_row + 1, row, start_col, max(cols), facade)

  # every group's rows, so the number formats and widths fit all of them
  all_rows <- dplyr::bind_rows(lapply(data, function(d) {
    convert_factor(dplyr::select(dplyr::ungroup(d), dplyr::all_of(names(data_first))))
  }))
  for(cn in names(data_first)) {
    for(a in c("label", "label_xlsx")) attr(all_rows[[cn]], a) <- attr(data_first[[cn]], a, exact = TRUE)
  }

  xlsx_decimal_format(
    wb = wb,
    data = all_rows,
    sheet_name = sheet_name,
    rows = (offset_row + header_depth):row,
    offset = offset_col,
    cols = extract_facade(facade, 'table', 'decimalCols'),
    precision = extract_facade(facade, 'table', 'decimalPrecision')
  )

  xlsx_colwidths(
    wb = wb,
    sheet_name = sheet_name,
    facade = facade,
    cols = cols,
    offset = offset_col,
    data = all_rows
  )

  xlsx_write_notes(wb, sheet_name, row, offset_col, length(cols), source_note, footnotes, facade)
}


# Spanner rows are the upper rows of a multi-level header.
xlsx_header_style <- function(wb, sheet_name, offset_row, header_depth, cols, facade) {

  xlsx_eval_style(
    wb = wb,
    sheet_name = sheet_name,
    style = extract_facade(facade, 'header'),
    rows = offset_row + seq_len(header_depth),
    cols = cols
  )

  if(header_depth > 1) {
    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'spanner'),
      rows = offset_row + seq_len(header_depth - 1),
      cols = cols
    )
  }

  wb
}


xlsx_header_heights <- function(wb, sheet_name, offset_row, header_depth, facade) {

  pad <- if(header_depth == 1) 6 else 0

  openxlsx::setRowHeights(
    wb = wb,
    sheet = sheet_name,
    rows = offset_row + seq_len(header_depth),
    heights = extract_facade(facade, 'header', 'height') + pad
  )

  if(header_depth > 1) {
    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = offset_row + header_depth,
      heights = extract_facade(facade, 'border_bottom', 'height')
    )
  }

  spanner_height <- extract_facade(facade, 'spanner', 'height')
  if(header_depth > 1 & !is.null(spanner_height)) {
    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = offset_row + seq_len(header_depth - 1),
      heights = spanner_height
    )
  }

  wb
}


xlsx_bold_row <- function(wb, sheet_name, row, cols) {
  openxlsx::addStyle(
    wb = wb,
    sheet = sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold"),
    rows = row,
    cols = cols,
    gridExpand = TRUE,
    stack = TRUE
  )
}


# Source note on the row below the table, footnotes below that.
# Sets attr(wb, "last_row") to the last row written.
xlsx_write_notes <- function(wb, sheet_name, last_row, offset_col, n_cols, source_note, footnotes, facade) {

  if(!is.null(source_note)) {

    openxlsx::writeData(
      wb = wb,
      x = source_note,
      sheet = sheet_name,
      startRow = last_row + 1,
      startCol = offset_col + 1,
      colNames = FALSE
    )

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'source_note'),
      rows = last_row + 1,
      cols = offset_col + 1
    )

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet_name,
      rows = last_row + 1,
      heights = extract_facade(facade, 'source_note', 'height')
    )
  }

  wb <- xlsx_write_footnotes(
    wb = wb,
    sheet_name = sheet_name,
    footnotes = footnotes,
    offset_row = last_row + 1,
    offset_col = offset_col,
    n_cols = n_cols,
    facade = facade
  )

  n_footnotes <- length(.normalize_footnotes(footnotes)$text)
  has_notes <- !is.null(source_note) | n_footnotes > 0
  attr(wb, "last_row") <- last_row + has_notes + n_footnotes

  wb
}
