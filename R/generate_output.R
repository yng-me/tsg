#' Generate output in specified format (e.g., xlsx, html, pdf, word)
#'
#' @param data Preferably a \code{tsg} class object for best results. A data frame, tibble, and list are also supported.
#' @param path File path to save the output. For HTML/PDF with a list and \code{separate_files = TRUE}
#'   (HTML) or any list (PDF), this is treated as a directory path. For \code{"gsheet"}, a Google
#'   Sheet name or an existing sheet; see [write_gsheet()].
#' @param format Output format. One of \code{"xlsx"}, \code{"gsheet"} (Google Sheets), \code{"html"}, \code{"pdf"}, or \code{"word"}.
#' @param ... Additional arguments passed to specific format functions:
#'   \itemize{
#'     \item \code{"xlsx"}: passed to \code{\link{write_xlsx}}
#'     \item \code{"gsheet"}: passed to \code{\link{write_gsheet}} (requires the \pkg{googledrive} package)
#'     \item \code{"html"}: passed to \code{\link{write_html}} (requires the \pkg{gt} package)
#'     \item \code{"pdf"}: passed to \code{\link{write_pdf}} (requires the \pkg{gt} and \pkg{webshot2} packages)
#'     \item \code{"word"}: passed to \code{\link{write_docx}} (requires the \pkg{officer} and \pkg{flextable} packages)
#'   }
#'
#' @return Invisibly returns \code{NULL}. Called for its side-effect of writing output file(s).
#' @export
#'
#' @examples
#' # Generate an xlsx file from a tsg object
#' data <- generate_frequency(dplyr::starwars, sex)
#'
#' dir_to <- tempdir()
#' generate_output(
#'   data,
#'   file.path(dir_to, "starwars_frequency.xlsx"),
#'   format = "xlsx"
#' )
#'
#' unlink(file.path(dir_to, "starwars_frequency.xlsx"))
#'

generate_output <- function(
  data,
  path,
  ...,
  format = c("xlsx", "gsheet", "html", "pdf", "word")
) {

  match.arg(format[1], c("xlsx", "gsheet", "html", "pdf", "word"), several.ok = FALSE)
  format <- format[1]

  if (format == "xlsx") {

    write_xlsx(data, path, ...)

  } else if (format == "gsheet") {

    write_gsheet(data, path, ...)

  } else if (format == "html") {

    write_html(data, path, ...)

  } else if (format == "pdf") {

    write_pdf(data, path, ...)

  } else if (format == "word") {

    write_docx(data, path, ...)

  }

}


create_table_list <- function(data) {

  if(!inherits(data, "list")) {
    stop("Data must be a list to create a table list.")
  }

  tables <- names(data)
  titles <- vapply(
    seq_along(data),
    function(i) as.character(table_meta(data[[i]])$title %||% tables[i]),
    character(1)
  )

  # table_id and table_name must match the list names and the sheets write_xlsx() creates
  dplyr::tibble(
    table_id = tables,
    table_number = seq_along(tables),
    table_name = xlsx_set_valid_sheet_name(tables, trim = FALSE),
    title = titles
  )
}


