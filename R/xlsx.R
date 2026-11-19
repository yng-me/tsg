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
#' @param ... Additional arguments passed to \code{openxlsx::createWorkbook()} (e.g. \code{creator}) or \code{openxlsx::addWorksheet()} (e.g. \code{zoom}), matched by name.
#' @param sheet_name Optional name for the Excel sheet. Ignored if \code{data} is a list and \code{separate_files = FALSE}.
#' @param trim_sheet_names Logical. Excel limits sheet names to 31 characters. If \code{FALSE} (default), a longer sheet name (from \code{sheet_name}, a list name or the \code{table_name} of \code{table_list_reference}) is an error. If \code{TRUE}, it is cut to 31 characters, and names that then repeat are numbered (\code{"... (2)"}). File names (\code{separate_files = TRUE}) are never cut.
#' @param title Optional title displayed above the data in each sheet or file.
#' @param subtitle Optional subtitle displayed under the title.
#' @param source_note Optional source note displayed below the data.
#' @param footnotes Optional character vector of footnotes to display below the source note.
#' @param separate_files Logical. If \code{TRUE}, each list item in \code{data} is saved as a separate Excel file.
#' @param collapse_list Logical. If \code{TRUE}, a list of data frames will be merged into one sheet (if applicable).
#' @param row_group_as_column Logical. If \code{TRUE}, row groupings are included as columns instead of grouped titles. Applies to a grouped list written on one sheet (\code{collapse_list = TRUE}) and to the sheets of a list nested two or more groups deep.
#' @param names_separator Character used to separate column names when dealing with nested or grouped headers.
#' @param facade A built-in facade name (e.g. \code{"booktabs"}; see [get_tsg_facade()]), the path to a \code{.yaml} or \code{.json} facade file, or a facade list. A built-in facade looks the same in every output format. Keys missing from it fall back to this format's default facade; settings added with [add_facade()] take precedence over it.
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
  trim_sheet_names = FALSE,
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

  facade <- resolve_facade(facade, table_meta(data)$facade)
  is_list <- inherits(data, "list")

  if(separate_files & !is_list) {
    stop("When `separate_files = TRUE`, `data` must be a list of data frames.")
  }

  # sheets and files are named after list items; unnamed ones would write nothing
  if(is_list) {
    item_names <- names(data) %||% rep("", length(data))
    missing_name <- is.na(item_names) | item_names == ""
    item_names[missing_name] <- paste("Table", which(missing_name))
    names(data) <- item_names
  }

  write_args <- list(
    names_separator = names_separator,
    row_group_as_column = row_group_as_column,
    facade = facade
  )

  # --- one file per list item
  if(is_list & separate_files) {

    path <- stringr::str_remove(path, "\\.xlsx$")
    fs::dir_create(path)

    # files have no 31-character limit
    sheet_names <- xlsx_set_valid_sheet_name(names(data), trim = FALSE)

    for(i in seq_along(data)) {
      sheet_name_i <- sheet_names[i]
      wb <- xlsx_create_workbook(..., facade = facade)
      meta <- resolve_table_meta(data[[i]], sheet_name_i, title, subtitle, source_note, footnotes)
      wb <- rlang::inject(xlsx_write_data(wb, data[[i]], "Sheet1", ..., !!!meta[-1], !!!write_args))
      openxlsx::saveWorkbook(wb, fs::path_norm(file.path(path, glue::glue("{sheet_name_i}.xlsx"))), overwrite = TRUE)
    }

    return(invisible(NULL))
  }

  if(!grepl("\\.xlsx$", path)) {
    path <- glue::glue("{path}.xlsx")
  }

  wb <- xlsx_create_workbook(..., facade = facade)

  # --- one sheet per list item
  if(is_list & !collapse_list) {

    sheet_names <- names(data)
    valid_sheet_names <- xlsx_sheet_names(sheet_names, trim_sheet_names)

    if(include_table_list) {
      table_list_reference <- resolve_table_list(data, table_list_reference)
      table_list_reference$table_name <- xlsx_sheet_names(table_list_reference$table_name, trim_sheet_names)
      wb <- tsg_write_table_list(wb, data, table_list_reference, facade = facade)
    }

    for(i in seq_along(sheet_names)) {

      meta <- resolve_table_meta(
        data[[i]],
        valid_sheet_names[i],
        title, subtitle, source_note, footnotes
      )

      if(include_table_list) {
        meta <- resolve_table_ref(table_list_reference, sheet_names[i], meta)
      }

      wb <- rlang::inject(
        xlsx_write_data(
          wb,
          data[[i]],
          ...,
          !!!meta,
          !!!write_args,
          include_table_list = include_table_list
        )
      )
    }

  } else {

    wb <- rlang::inject(
      xlsx_write_data(
        wb,
        data,
        xlsx_sheet_names(sheet_name %||% "Sheet1", trim_sheet_names),
        ...,
        title = title,
        subtitle = subtitle,
        source_note = source_note,
        footnotes = footnotes,
        !!!write_args
      )
    )
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}


# Valid sheet names; a name over Excel's 31-character limit is cut when `trim` is TRUE
# and an error otherwise.
xlsx_sheet_names <- function(names, trim) {
  if(!trim) {
    long <- names[nchar(xlsx_set_valid_sheet_name(names, trim = FALSE)) > 31]
    if(length(long) > 0) {
      stop(
        "Sheet names can have at most 31 characters: ", paste0("\"", long, "\"", collapse = ", "),
        ". Use shorter names, or set `trim_sheet_names = TRUE` to cut them.",
        call. = FALSE
      )
    }
  }
  xlsx_set_valid_sheet_name(names)
}


# `...` holds both createWorkbook() and addWorksheet() arguments; keep the former.
xlsx_create_workbook <- function(..., facade) {
  dots <- list(...)
  wb <- do.call(
    openxlsx::createWorkbook,
    dots[names(dots) %in% names(formals(openxlsx::createWorkbook))]
  )
  openxlsx::modifyBaseFont(wb, fontName = facade$table.fontName, fontSize = facade$table.fontSize)
  wb
}


tsg_write_table_list <- function(wb, data, table_list_reference = NULL, facade = get_tsg_facade()) {

  sheet_summary <- facade$label.titleTableList %||% 'List of Tables'

  ref <- dplyr::select(table_list_reference, table_number, table_name, title)

  listing <- ref |>
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
    )

  wb <- xlsx_write_data(
    wb,
    data = listing,
    sheet_name = sheet_summary,
    title = sheet_summary,
    offset_col = 1,
    offset_row = 1,
    facade = facade
  )

  # fit the content; long titles stop at 148 characters
  widths <- c(xlsx_fit_width(listing, 1), xlsx_fit_width(listing, 2), xlsx_fit_width(listing, 3, max_width = 148))
  openxlsx::setColWidths(wb, sheet = sheet_summary, cols = c(2, 3, 4), widths = widths)

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
      dplyr::mutate(table_name = xlsx_set_valid_sheet_name(table_name, trim = FALSE))

    if("subtitle" %in% names(table_list_reference)) {

      table_list_reference <- table_list_reference |>
        dplyr::mutate(subtitle = stringr::str_trim(subtitle))

    }

  } else {
    table_list_reference <- create_table_list(data)
  }

  table_list_reference

}

# A table-list reference row overrides the table's metadata.
resolve_table_ref <- function(refs, table_id, meta) {

  ref <- dplyr::filter(refs, table_id == !!table_id)
  if(nrow(ref) == 0) return(meta)

  meta$sheet_name <- ref$table_name[1]
  meta$title <- ref$title[1]

  if("table_number" %in% names(ref)) {

    table_number <- ref$table_number[1]

    if(!is.na(table_number) & table_number != '') {

      if(!grepl('^table', table_number, ignore.case = TRUE)) {
        table_number <- paste("Table", table_number)
      }

      meta$title <- paste0(table_number, ". ", meta$title)
    }
  }

  # an empty cell (NA, or "" read from a file) keeps the table's own text
  for(key in c("subtitle", "source_note", "footnotes")) {
    value <- ref[[key]][1]
    if(key %in% names(ref) && !is.na(value) && nzchar(value)) {
      meta[[key]] <- as.character(value)
    }
  }

  meta
}


# For a table in a list, its own metadata wins; write_xlsx() arguments fill the gaps
# (a title argument becomes "<title>: <sheet name>").
resolve_table_meta <- function(data, sheet_name, title, subtitle, source_note, footnotes) {

  meta <- table_meta(data)

  if(!is.null(title)) {
    title <- glue::glue("{title}: {sheet_name}")
  }

  list(
    sheet_name = sheet_name,
    title = meta$title %||% title,
    subtitle = meta$subtitle %||% subtitle,
    source_note = meta$source_note %||% source_note,
    footnotes = meta$footnotes %||% footnotes
  )
}
