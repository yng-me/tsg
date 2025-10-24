write_html <- function(data, path, ...) {

}


gtx <- function(data, ...) {

  x <- get_header(data)
  y <- names(x)

  data <- data |>
    dplyr::mutate_if(haven::is.labelled, haven::as_factor) |>
    gt::gt()

  spans <- list()

  for(i in seq_along(x)) {

    lab <- x[[i]]

    data <- data |>
      gt::cols_label(
        !!as.name(y[i]) := lab[length(lab)]
      )

    if(length(lab) > 1) {

      spans[[i]] <- list(
        label = lab[length(lab)],
        columns = y[i],
        id = lab[1]
      )

      data <- data |>
        gt::tab_spanner(

        )
    }
  }

  data

}
