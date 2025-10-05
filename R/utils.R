`%||%` <- function(a, b) if (!is.null(a)) a else b

is_valid_input_data <- function(x) {
  error_message <- "data input must be a valid data frame or Arrow format."

  if(is.vector(x) | is.character(x)) stop(error_message)
  if(!(inherits(x, 'data.frame') |
       inherits(x, 'ArrowObject') |
       inherits(x, 'arrow_dplyr_query')
  )
  ) stop(error_message)
}

collect_data <- function(data, ...) {

  is_valid_input_data(data)

  if("ArrowObject" %in% class(data) | "arrow_dplyr_query" %in% class(data)) {
    dplyr::collect(data)
  } else {
    data
  }
}
