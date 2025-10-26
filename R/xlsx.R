#' Write Data to Excel with Titles, Notes, and Styling
#'
#' Exports a data frame or a list of data frames to one or multiple Excel files,
#' with support for titles, subtitles, source notes, footnotes, grouping, and custom styles.
#' It leverages the \code{openxlsx} package to create styled Excel reports suitable for presentation.
#'
#' @param data A \code{data.frame}, tibble, or a named \code{list} of them. When a list is provided:
#'   \itemize{
#'     \item If \code{separate_files = FALSE}, each element is written to a separate sheet in one Excel file.
#'     \item If \code{separate_files = TRUE}, each element is written to its own Excel file.
#'   }
#' @param path A file path (if \code{separate_files = FALSE}) or directory path (if \code{separate_files = TRUE})
#'   where the Excel file(s) will be saved. File extension \code{.xlsx} is automatically added if missing.
#' @param ... Additional arguments passed to \code{openxlsx::createWorkbook()} and \code{openxlsx::addWorksheet()}.
#' @param sheet_name Optional name for the Excel sheet. Ignored if \code{data} is a list and \code{separate_files = FALSE}.
#' @param title Optional title displayed above the data in each sheet or file.
#' @param subtitle Optional subtitle displayed under the title.
#' @param source_note Optional source note displayed below the data.
#' @param footnotes Optional character vector of footnotes to display below the source note.
#' @param separate_files Logical. If \code{TRUE}, each list item in \code{data} is saved as a separate Excel file.
#' @param collapse_list Logical. If \code{TRUE}, a list of data frames will be merged into one sheet (if applicable).
#' @param row_group_as_column Logical. If \code{TRUE}, row groupings are included as columns instead of grouped titles.
#' @param names_separator Character used to separate column names when dealing with nested or grouped headers.
#' @param facade A list of styling options (colors, fonts, sizes, border styles, etc.). Defaults to the global option \code{tsg.options.facade}.
#' @param include_table_list
#' @param table_list_reference
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side-effect of writing Excel file(s).
#'
#' @details
#' This function supports advanced Excel formatting including:
#' \itemize{
#'   \item Grouped headers
#'   \item Dynamic column widths
#'   \item Styled titles, subtitles, source notes, and footnotes
#'   \item Border styling (inner, outer, header)
#' }
#'
#' The function is designed to handle export needs in professional and reporting contexts.
#'
#' @export
#'
#' @examples
#' data <- tsg::generate_frequency(dplyr::starwars, sex)
#'
#' dir_to <- tempfile()
#' write_xlsx(
#'   data,
#'   file.path(dir_to, "starwars_frequency.xlsx")
#'  )
#'

write_xlsx <- function(
  data,
  path,
  ...,
  sheet_name = NULL,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = FALSE,
  collapse_list = FALSE,
  row_group_as_column = FALSE,
  names_separator = "__",
  include_table_list = FALSE,
  table_list_reference = NULL,
  facade = get_tsg_facade()
) {

  offset_row <- facade$table.offsetRow
  offset_col <- facade$table.offsetCol

  # --- Data inherits a "list" class and data is
  # --- to be as separate file per item in the list

  if(separate_files & !inherits(data, "list")) {
    stop("When `separate_files = TRUE`, `data` must be a list of data frames.")
  }

  if(inherits(data, "list") & separate_files) {
    return(
      write_xlsx_multiple_files(
        data = data,
        path = path,
        ...,
        title = title,
        subtitle = subtitle,
        source_note = source_note,
        footnotes = footnotes,
        offset_row = offset_row,
        offset_col = offset_col,
        names_separator = names_separator,
        row_group_as_column = row_group_as_column,
        facade = facade
      )
    )
  }


  if(!grepl("\\.xlsx$", path) & !separate_files) {
    path <- glue::glue("{path}.xlsx")
  }

  wb <- openxlsx::createWorkbook(...)
  openxlsx::modifyBaseFont(
    wb,
    fontName = facade$table.fontName,
    fontSize = facade$table.fontSize
  )

  # --- Data inherits a "list" class and data is to be exported as one file
  # --- and each item in the list is written in its own sheet
  if(inherits(data, "list") & !collapse_list) {

    if(include_table_list) {

      sheet_summary <- 'List of Tables'

      if(!is.null(table_list_reference)) {
        table_list_reference <- table_list_reference |>
          dplyr::filter(table_id %in% names(data)) |>
          dplyr::mutate(table_name = xlsx_set_valid_sheet_name(table_name))
      } else {
        table_list_reference <- create_table_list(data)
      }

      wb <- xlsx_write_data(
        wb,
        dplyr::select(table_list_reference, table_name, title) |>
          rename_label(
            table_number = "Table number",
            title = "Title"
          ),
        sheet_name = sheet_summary,
        title = sheet_summary,
        offset_col = 1,
        offset_row = 1
      )

      for (s in 1:nrow(table_list_reference)) {

        hyperlink <- table_list_reference$table_name[s]
        hyperlink_name <- glue::glue("Table {table_list_reference$table_number[s]}")

        openxlsx::writeFormula(
          wb,
          sheet = sheet_summary,
          startCol = 2,
          startRow =  4 + s,
          x = openxlsx::makeHyperlinkString(sheet = hyperlink, text = hyperlink_name)
        )
      }
    }

    sheet_names <- names(data)

    for(i in seq_along(sheet_names)) {

      sheet_name_i <- xlsx_set_valid_sheet_name(sheet_names[i])

      if(include_table_list) {
        table_list_reference_i <- table_list_reference |>
          dplyr::filter(table_id == sheet_names[i])

        if(nrow(table_list_reference_i) > 0) {
          sheet_name_i <- xlsx_set_valid_sheet_name(table_list_reference_i$table_name[1])
        }
      }

      title_i <- NULL
      if(!is.null(title)) {
        title_i <- glue::glue("{title}: {sheet_name_i}")
      }

      data_i <- data[[i]]

      groups <- attributes(data_i)$groups

      wb <- xlsx_write_data(
        wb = wb,
        data = data_i,
        sheet_name = sheet_name_i,
        title = title_i,
        subtitle = subtitle,
        source_note = source_note,
        footnotes = footnotes,
        offset_row = offset_row,
        offset_col = offset_row,
        names_separator = names_separator,
        collapse_list = FALSE,
        row_group_as_column = row_group_as_column,
        facade = facade
      )
    }

  } else {

    if(is.null(sheet_name)) { sheet_name <- "Sheet1" }

    wb <- xlsx_write_data(
      wb = wb,
      data = data,
      sheet_name = sheet_name,
      ...,
      title = title,
      subtitle = subtitle,
      source_note = source_note,
      footnotes = footnotes,
      offset_row = offset_row,
      offset_col = offset_row,
      collapse_list = collapse_list,
      names_separator = names_separator,
      row_group_as_column = row_group_as_column,
      facade = facade
    )

  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

}


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
  facade = get_tsg_facade()
) {

  facade <- resolve_facade(facade, attributes(data)$facade)

  openxlsx::addWorksheet(
    wb,
    sheetName = sheet_name,
    gridLines = facade$table.gridLines,
    ...
  )

  start_col <- offset_col + 1

  title <- title %||% attributes(data)$title
  subtitle <- subtitle %||% attributes(data)$subtitle
  footnotes <- footnotes %||% attributes(data)$footnotes
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
          x = dplyr::mutate_if(data_i, haven::is.labelled, haven::as_factor),
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
        data_i <- data[[i]]

        openxlsx::writeData(
          wb = wb,
          x = row_title,
          sheet = sheet_name,
          startRow = offset_row_i + 1,
          startCol = start_col,
          colNames = FALSE
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
          x = dplyr::mutate_if(dplyr::ungroup(data_i), haven::is.labelled, haven::as_factor),
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

        }

        xlsx_decimal_format(
          wb = wb,
          data = data_i,
          sheet_name = sheet_name,
          rows = (offset_row_i + 1):(nrow(data_i) + header_depth_i + offset_row_i),
          offset = start_col - 1,
          cols = extract_facade(facade, 'table', 'decimalCols'),
          precision = extract_facade(facade, 'table', 'decimalPrecision')
        )

        offset_row_i <- offset_row_i + nrow(data_i) + 3

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
      x = dplyr::mutate_if(data, haven::is.labelled, haven::as_factor),
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



write_xlsx_multiple_files <- function(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  offset_row = 0,
  offset_col = 0,
  names_separator = "__",
  row_group_as_column = FALSE,
  facade = get_tsg_facade()
) {

  if(grepl("\\.xlsx$", path)) {
    path <- stringr::str_remove(path, "\\.xlsx$")
  }

  fs::dir_create(path)
  sheet_names <- names(data)

  for(i in seq_along(sheet_names)) {

    wb <- openxlsx::createWorkbook(...)
    openxlsx::modifyBaseFont(
      wb,
      fontName = facade$table.fontName,
      fontSize = facade$table.fontSize
    )

    sheet_name_i <- xlsx_set_valid_sheet_name(sheet_names[i])
    path_i <- file.path(path, glue::glue("{sheet_name_i}.xlsx"))
    path_i <- fs::path_norm(path_i)

    title_i <- NULL
    if(!is.null(title)) {
      title_i <- glue::glue("{title}: {sheet_name_i}")
    }

    wb <- xlsx_write_data(
      wb = wb,
      data = data[[i]],
      title = title_i,
      subtitle = subtitle,
      sheet_name = "Sheet1",
      source_note = source_note,
      footnotes = footnotes,
      offset_row = offset_row,
      offset_col = offset_col,
      names_separator = names_separator,
      row_group_as_column = row_group_as_column,
      facade = facade
    )

    openxlsx::saveWorkbook(wb, path_i, overwrite = TRUE)

  }

}


tsg_write_table_list <- function(wb, data, table_list_reference = NULL, include_table_list = FALSE) {

  if(!include_table_list) return(wb)

  sheet_summary <- 'List of Tables'

  if(!is.null(table_list_reference)) {
    table_list_reference <- table_list_reference |>
      dplyr::filter(table_id %in% names(data)) |>
      dplyr::mutate(table_name = xlsx_set_valid_sheet_name(table_name))
  } else {
    table_list_reference <- create_table_list(data)
  }

  wb <- xlsx_write_data(
    wb,
    dplyr::select(table_list_reference, table_name, title) |>
      rename_label(
        table_number = "Table number",
        title = "Title"
      ),
    sheet_name = sheet_summary,
    title = sheet_summary,
    offset_col = 1,
    offset_row = 1
  )

  for (s in 1:nrow(table_list_reference)) {

    hyperlink <- table_list_reference$table_name[s]
    hyperlink_name <- glue::glue("Table {table_list_reference$table_number[s]}")

    openxlsx::writeFormula(
      wb,
      sheet = sheet_summary,
      startCol = 2,
      startRow =  4 + s,
      x = openxlsx::makeHyperlinkString(sheet = hyperlink, text = hyperlink_name)
    )
  }

}


