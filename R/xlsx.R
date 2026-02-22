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
#' @param include_table_list Logical. If \code{TRUE}, a table list reference is included in the Excel file.
#' @param table_list_reference A data frame containing the table list reference. If \code{NULL}, it will be generated from \code{data}.
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

  facade <- facade %||% get_tsg_facade()

  offset_row <- attributes(data)$facade$table.offsetRow %||% facade$table.offsetRow
  offset_col <- attributes(data)$facade$table.offsetCol %||% facade$table.offsetCol

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
      table_list_reference <- resolve_table_list(data, table_list_reference)
      wb <- tsg_write_table_list(wb, data, table_list_reference)

    }

    sheet_names <- names(data)

    for(i in seq_along(sheet_names)) {

      sheet_name_i <- xlsx_set_valid_sheet_name(sheet_names[i])

      data_i <- data[[i]]

      attr_i <- resolve_table_meta(
        attr = attributes(data[[i]]),
        sheet_name = sheet_name_i,
        title = title,
        subtitle = subtitle,
        source_note = source_note,
        footnotes = footnotes
      )

      table_meta <- resolve_table_ref(
        refs = table_list_reference,
        include_ref = include_table_list,
        sheet_name = sheet_names[i],
        attr = attr_i
      )

      wb <- xlsx_write_data(
        wb = wb,
        data = data_i,
        sheet_name = table_meta$sheet_name,
        title = table_meta$title,
        subtitle = table_meta$subtitle,
        source_note = table_meta$source_note,
        footnotes = table_meta$footnotes,
        offset_row = offset_row,
        offset_col = offset_col,
        names_separator = names_separator,
        collapse_list = FALSE,
        row_group_as_column = row_group_as_column,
        include_table_list = include_table_list,
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
      offset_col = offset_col,
      collapse_list = collapse_list,
      names_separator = names_separator,
      row_group_as_column = row_group_as_column,
      facade = facade
    )

  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

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



tsg_write_table_list <- function(wb, data, table_list_reference = NULL, facade = get_tsg_facade()) {

  sheet_summary <- 'List of Tables' %||% facade$label.titleTableList

  ref <- dplyr::select(table_list_reference, table_number, table_name, title)

  wb <- xlsx_write_data(
    wb,
    data = ref |>
      dplyr::transmute(
        name = table_name,
        number = dplyr::if_else(
          grepl('^table', table_number, ignore.case = TRUE),
          as.character(table_number),
          paste("Table", table_number)
        ),
        title = title
      ) |>
      rename_label(
        name = "Tab Name",
        number = "Table",
        title = "Title"
      ),
    sheet_name = sheet_summary,
    title = sheet_summary,
    offset_col = 1,
    offset_row = 1
  )

  openxlsx::setColWidths(wb, sheet = sheet_summary, cols = c(2, 3, 4), widths = c(36, 12, 148))

  for (s in 1:nrow(table_list_reference)) {

    openxlsx::writeFormula(
      wb,
      sheet = sheet_summary,
      startCol = 2,
      startRow =  4 + s,
      x = openxlsx::makeHyperlinkString(
        sheet = table_list_reference$table_name[s],
        text = table_list_reference$table_name[s]
      )
    )
  }

  return(wb)

}


resolve_table_list <- function(data, table_list_reference) {

  if(!is.null(table_list_reference)) {

    if(inherits(table_list_reference, 'character')) {

      if(!fs::is_file(table_list_reference)) {
        stop('table_list_reference is invalid')
      }

      if(grepl('\\.json$', table_list_reference)) {
        table_list_reference <- jsonlite::read_json(table_list_reference, simplifyVector = TRUE)
      } else if (grepl('\\.csv$', table_list_reference)) {
        table_list_reference <- utils::read.csv(table_list_reference)
      } else if (grepl('\\.xlsx$', table_list_reference)) {
        table_list_reference <- openxlsx::read.xlsx(table_list_reference)
      }

    }

    if(!("table_id" %in% names(table_list_reference))) {
      stop("`table_list_reference` must contain a `table_id` column.")
    }

    if(!("table_name" %in% names(table_list_reference))) {
      stop("`table_list_reference` must contain a `table_name` column.")
    }

    if(!("table_number" %in% names(table_list_reference))) {
      stop("`table_list_reference` must contain a `table_number` column.")
    }

    if(!("title" %in% names(table_list_reference))) {
      stop("`table_list_reference` must contain a `title` column.")
    }

    table_list_reference <- table_list_reference |>
      dplyr::mutate(
        table_id = stringr::str_trim(table_id),
        table_name = stringr::str_trim(table_name),
        title = stringr::str_trim(title)
      ) |>
      dplyr::filter(table_id %in% names(data)) |>
      dplyr::mutate(table_name = xlsx_set_valid_sheet_name(table_name))

    if("subtitle" %in% names(table_list_reference)) {

      table_list_reference <- table_list_reference |>
        dplyr::mutate(subtitle = stringr::str_trim(subtitle))

    }

  } else {
    table_list_reference <- create_table_list(data)
  }

  table_list_reference

}

resolve_table_ref <- function(refs, sheet_name, include_ref, attr) {

  title <- attr$title
  subtitle <- attr$subtitle
  source_note <- attr$source_note
  footnotes <- attr$footnotes

  if(include_ref) {

    ref <- dplyr::filter(refs, table_id == sheet_name)

    if(nrow(ref) > 0) {

      sheet_name <- ref$table_name[1]
      title <- ref$title[1]

      if("table_number" %in% names(ref)) {

        table_number <- ref$table_number[1]

        if(!is.na(table_number) & table_number != '') {

          if(!grepl('^table', table_number, ignore.case = TRUE)) {
            table_number <- paste("Table", table_number)
          }

          title <- paste0(table_number, ". ", title)
        }

      }

      if("subtitle" %in% names(ref)) {
        subtitle <- ref$subtitle[1]
      }

      if("source_note" %in% names(ref)) {
        source_note <- ref$source_note[1]
      }

      if("footnotes" %in% names(ref)) {
        footnotes <- ref$footnotes[1]
      }

    }

  }

  return(
    list(
      sheet_name = sheet_name,
      title = title,
      subtitle = subtitle,
      source_note = source_note,
      footnotes = footnotes
    )
  )

}

resolve_table_meta <- function(attr, sheet_name, title, subtitle, source_note, footnotes) {

  if(!is.null(title) & is.null(attr$title)) {
    attr$title <- glue::glue("{title}: {sheet_name}")
  }

  if(!is.null(subtitle) & is.null(attr$subtitle)) {
    attr$subtitle <- subtitle
  }

  if(!is.null(source_note) & is.null(attr$source_note)) {
    attr$source_note <- source_note
  }

  if(!is.null(subtitle) & is.null(attr$footnotes)) {
    attr$footnotes <- footnotes
  }

  attr

}


