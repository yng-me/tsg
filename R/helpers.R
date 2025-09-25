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


factor_col <- function(data, .col, .keep_cols = TRUE, .rename_cols = FALSE) {

  attr_i <- attributes(data[[.col]])

  if (!is.null(attr_i$labels)) {
    col_fct <- .col
    if (.keep_cols) col_fct <- paste0(.col, "_fct")

    if(is.numeric(as.integer(attr_i$labels$value[1]))) {

      data <- data |>
        dplyr::mutate(
          !!as.name(col_fct) := factor(
            as.integer(!!as.name(.col)),
            as.integer(attr_i$lables$value),
            attr_i$labels$label
          )
        )

    } else {
      data <- data |>
        dplyr::mutate(
          !!as.name(col_fct) := factor(
            !!as.name(.col),
            attr_i$labels$value,
            attr_i$labels$label
          )
        )
    }
  }

  if(!is.null(attr_i$label) & .rename_cols & !.keep_cols) {
    data <- data |>
      dplyr::rename(!!as.name(attr_i$label) := !!as.name(col_fct))
  }
  data
}
