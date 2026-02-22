#' Add a row total
#'
#' @param data A data frame, tibble, or \code{tsg} object to which a total row will be added.
#' @param position Position to add the total row. Either "bottom" (default) or "top".
#' @param label_total Label for the total row in the category column. Default is "Total".
#' @param fill Character. Value to fill in for missing numeric columns in the total row. Default is "-".
#'
#' @return The input data frame with an additional row representing the total of numeric columns.
#' @export
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'  category = c("A", "B", "C"),
#'  value1 = c(10, 20, 30),
#'  value2 = c(5, 15, 25)
#' )
#'
#' df_with_total <- add_row_total(df)
#' df_with_total_top <- add_row_total(df, position = "top")
#'

add_row_total <- function(data, position = c("bottom", "top"), label_total = "Total", fill = "-") {

  nn_cols <- ncol(dplyr::select(data, dplyr::where(is.numeric)))

  if(nn_cols == 0) {
    stop("No numeric columns in the dataset")
  }

  position <- match.arg(position[1], c("top", "bottom"))

  col <- attributes(data)$category
  groups <- attributes(data)$groups
  is_grouped_df <- FALSE

  if(!is.null(groups)) {
    if("grouped_df" %in% class(data)) {
      is_grouped_df <- TRUE
      groups <- dplyr::group_vars(data)
      data <- dplyr::ungroup(data)
    }
  }

  exclude_cols <- unique(
    c(
      groups,
      names(dplyr::select(data, -dplyr::where(is.numeric)))
    )
  )

  if(!is.null(col)) {
    exclude_cols <- unique(c(exclude_cols, col))
    data <- coerce_category(data, !!as.name(col))
  }

  total_row <-dplyr::summarise(
    data,
    dplyr::across(-dplyr::any_of(exclude_cols), ~ sum(.x, na.rm = TRUE))
  )

  if(!is.null(col)) {
    total_row <- coerce_total(total_row, col, data[[col]], label_total = label_total)
  }

  total_row <- dplyr::select(total_row, dplyr::any_of(exclude_cols), dplyr::everything())

  if(!is.null(groups) & !is.null(col)) {
    total_row <- data |>
      dplyr::select(dplyr::all_of(groups)) |>
      dplyr::first() |>
      dplyr::bind_cols(total_row)
  } else if (length(exclude_cols) == 1 & is.null(col)) {
    total_row[[exclude_cols]] <- label_total
  }

  names_order <- names(data)

  if(position[1] == "bottom") {
    data <- dplyr::bind_rows(data, total_row)
  } else {
    data <- dplyr::bind_rows(total_row, data)
  }

  if(is_grouped_df) {
    data <- dplyr::group_by(data, dplyr::pick(dplyr::all_of(groups)))
  }

  data <- dplyr::select(data, dplyr::any_of(names_order))

  if(is.null(col) & ncol(data) > nn_cols) {

    if(position[1] == "bottom") {

      data[nrow(data), 1] <- label_total
      x <- which(is.na(data[nrow(data), ]))
      if(length(x) > 0) { data[nrow(data), x] <- fill }

    } else {
      data[1, 1] <- label_total
      x <- which(is.na(data[1, ]))
      if(length(x) > 0) { data[1, x] <- fill }
    }
  }

  data

}


#' Add a column total
#'
#' @param data A data frame, tibble, or \code{tsg} object to which a column row will be added.
#' @param name Column name for total. Default value is \code{"total"}.
#' @param label_total Label for the total column. Default is "Total".
#' @param ... Additional named arguments to be added as columns alongside the total column.
#'
#' @return The input data frame with an additional column representing the total of each row.
#' @export
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'  category = c("A", "B", "C"),
#'  value1 = c(10, 20, 30),
#'  value2 = c(5, 15, 25)
#'  )
#' add_column_total(df)
#'

add_column_total <- function(data, name = 'total', label_total = "Total", ...) {
  data <- dplyr::mutate(
    data,
    !!as.name(name) := rowSums(dplyr::across(dplyr::where(is.numeric)), na.rm = TRUE),
    ...
  )

  attr(data$total, "label") <- label_total

  data

}

#' Add a footnote attribute to a table
#'
#' @param data A data frame, tibble, or \code{tsg} object to which a footnote attribute will be added.
#' @param footnote The footnote text to be added.
#' @param locations Locations where the footnote should be applied. Default is NULL (applies to entire table).
#' @param placement Placement of the footnote. One of "auto" (default), "right", or "left".
#'
#' @return The input data frame with an added footnote attribute.
#' @export
#'
#' @examples
#' add_footnote(
#'   dplyr::starwars,
#'   footnote = "This is a footnote.",
#'   locations = c("A1", "B2"),
#'   placement = "right"
#')

add_footnote <- function(data, footnote, locations = NULL, placement = c("auto", "right", "left")) {

  match.arg(placement, c("auto", "right", "left"))

  if(!is.character(footnote)) stop("Footnote must be a character string.")
  if(length(footnote) > 1) stop("Footnote must be a single character string.")

  value <- list(
    text = c(attributes(data)$footnotes$text, footnote),
    locations = c(attributes(data)$footnotes$locations, locations),
    placement = c(attributes(data)$footnotes$placement, placement[1])
  )

  attr(data, "footnotes") <- value

  data

}

#' Add a source note attribute to a table
#'
#' @param data A data frame, tibble, or \code{tsg} object to which a source note attribute will be added.
#' @param source_note The source note text to be added.
#'
#' @return The input data frame with an added source note attribute.
#' @export
#'
#' @examples
#' add_source_note(
#'   dplyr::starwars,
#'   source_note = "Source: Star Wars API (SWAPI)."
#' )

add_source_note <- function(data, source_note) {

  if(!is.character(source_note)) stop("Source note must be a character string.")
  if(length(source_note) > 1) stop("Source note must be a single character string.")

  attr(data, "source_note") <- source_note

  data

}

#' Add a title attribute to a table
#'
#' @param data A data frame, tibble, or \code{tsg} object to which a title attribute will be added.
#' @param title The title text to be added.
#'
#' @return The input data frame with an added title attribute.
#' @export
#'
#' @examples
#' add_table_title(
#'   dplyr::starwars,
#'   title = "Star Wars Character Data"
#' )

add_table_title <- function(data, title) {

  if(!is.character(title)) stop("Table title must be a character string.")
  if(length(title) > 1) stop("Table title must be a single character string.")

  attr(data, "title") <- title

  data

}

#' Add a subtitle attribute to a table
#'
#' @param data A data frame, tibble, or \code{tsg} object to which a subtitle attribute will be added.
#' @param subtitle The subtitle text to be added.
#'
#' @return The input data frame with an added subtitle attribute.
#' @export
#'
#' @examples
#' add_table_subtitle(
#'   dplyr::starwars,
#'   subtitle = "Star Wars Character Data"
#' )
#'

add_table_subtitle <- function(data, subtitle) {

  if(!is.character(subtitle)) stop("Table subtitle must be a character string.")
  if(length(subtitle) > 1) stop("Table subtitle must be a single character string.")

  attr(data, "subtitle") <- subtitle

  data
}


add_table_number <- function(data, table_number) {

  if(!is.numeric(table_number)) stop("Table number must be numeric.")
  if(length(table_number) > 1) stop("Table number must be a single numeric value.")

  attr(data, "table_number") <- table_number

  data

}


#' Convert labelled factors to regular factors
#'
#' @param data A data frame, tibble, or \code{tsg} object containing labelled factors.
#'
#' @returns A data frame with labelled factors converted to regular factors.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   category = haven::labelled(
#'     c(1, 2, 3),
#'     c("One" = 1, "Two" = 2, "Three" = 3)
#'    )
#'  )
#'
#' df_converted <- convert_factor(df)

convert_factor <- function(data) {
  dplyr::mutate_if(data, haven::is.labelled, haven::as_factor)
}
