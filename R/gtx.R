#' GT Extended
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param title Table title
#' @param subtitle Table subtitle
#' @param source_note Table source note
#' @param y_group_separator Column separator that defines the table hierarchy.
#'
#' @return GT table
#' @export
#'
#' @examples

gtx <- function(
  .data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  y_group_separator = '>'
) {

  `:=` <- NULL
  cols <- names(.data)

  gty <- .data |>
    gt::gt()

  for(i in 1:length(cols)) {

    col <-stringr::str_replace_all(cols[i], y_group_separator, '<br>')

    gty <- gty |>
      gt::cols_label(
        (!!as.name(cols[i])) := gt::md(col)
      )
  }

  return(gty)

}
