#' Generate output in specified format (e.g., xlsx, html, pdf, word)
#'
#' @param data Preferably a \code{tsg} class object for best results. A data frame, tibble, and list are also supported.
#' @param path File path to save the output.
#' @param format Output format. One of \code{"xlsx"}, \code{"html"}, \code{"pdf"}, or \code{"word"}.
#' @param ... Additional arguments passed to specific format functions.
#'
#' @return Generates and saves the output file in the specified format at the given path.
#' @export
#'
#' @examples
#' #' # Generate an xlsx file from a tsg object
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
  format = c("xlsx", "html", "pdf", "word")
) {

  match.arg(format[1], c("xlsx", "html", "pdf", "word"), several.ok = FALSE)
  format <- format[1]

  if (format == "xlsx") {

    list_depth <- purrr::pluck_depth(data)

    if(list_depth < 5) {
      tsg::write_xlsx(data, path, ...)
    } else {
      stop("Data structure too deep for xlsx output")
    }

  } else if (format == "html") {

    stop("HTML format not yet implemented")

  } else if (format == "pdf") {

    stop("PDF format not yet implemented")

  } else if (format == "word") {

    stop("Word format not yet implemented")

  }

}


create_table_list <- function(data) {

  if(!inherits(data, "list")) {
    stop("Data must be a list to create a table list.")
  }

  tables <- names(data)

  table_list <- dplyr::tibble(
    table_id = character(),
    table_number = integer(),
    table_name = character(),
    title = character()
  )

  for(i in seq_along(tables)) {

    attr_i <- attributes(data[[i]])
    title_i <- attr_i$title %||% tables[i]

    table_list_i <- dplyr::tibble(
      table_number = i,
      table_id = title_i,
      table_name = xlsx_set_valid_sheet_name(title_i),
      title = title_i
    )

    table_list <- dplyr::bind_rows(
      table_list,
      table_list_i
    )
  }

  table_list
}


