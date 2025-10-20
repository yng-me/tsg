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
#' dir_to <- tempfile()
#' generate_output(
#'   data,
#'   file.path(dir_to, "starwars_frequency.xlsx"),
#'   format = "xlsx"
#'  )
#'

generate_output <- function(data, path, format, ...) {

  match.arg(format, c("xlsx", "html", "pdf", "word"))

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
