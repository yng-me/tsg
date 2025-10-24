xlsx_header_merge <- function(
  wb,
  data,
  ...,
  offset_row = 0,
  offset_col = 0,
  names_separator = "__"
) {

  depths <- get_header_depth(data, names_separator = names_separator)
  depth_max <- max(depths, na.rm = TRUE)

  for(i in seq_len(depth_max)) {

    header <- get_header_split(data, i = i, names_separator = names_separator)
    header_text <- header[1]
    header_offset <- header[2:length(header)]

    header_cols_value <- 1
    header_cols_label <- list()

    for(j in seq_along(header_offset)) {

      header_j <- header_offset[j]
      header_j_label <- paste0("h__", j, "__", header_text)

      if(header_j == header_text) {
        header_cols_value <- c(header_cols_value, j + 1)
      } else {

        header_cols_label[[header_j_label]] <- header_cols_value
        header_text <- header_j
        header_cols_value <- j + 1

      }

      if(j == length(header_offset)) {
        header_cols_label[[paste0("h__", j, "__", header_text)]] <- header_cols_value
      }

    }

    openxlsx::writeData(
      wb = wb,
      x = t(header),
      ...,
      startRow = offset_row + i,
      startCol = offset_col + 1,
      colNames = FALSE
    )

    if(i < depth_max) {

      h_labels <- names(header_cols_label)

      for(k in seq_along(h_labels)) {

        h_label <- h_labels[k]
        h_value <- header_cols_label[[k]]

        if(length(h_value) > 1 & !grepl("__$", h_label)) {
          openxlsx::mergeCells(wb, ..., cols = h_value + offset_col, rows = i + offset_row)
        }
      }

    } else {

      for(k in seq_along(depths)) {
        d <- depths[k]
        if(d >= depth_max) next
        openxlsx::mergeCells(wb, ..., cols = k + offset_col, rows = d:depth_max + offset_row)
      }
    }
  }

  attr(wb, "header_depth") <- depth_max

  return(wb)

}

xlsx_set_valid_sheet_name <- function(sheet) {
  pattern <- '[^0-9a-zA-Z\\(\\)@#$%^&!\\-\\_\\=\\"\\~\\`\\,\\.\\{\\}\\;\\<\\> ]'
  sheet_name <- stringr::str_replace_all(stringr::str_trim(sheet), "\\/", " or ")
  sheet_name <- stringr::str_sub(sheet_name, 1, 31)
  stringr::str_remove_all(sheet_name, pattern)
}

get_header <- function(data, names_separator = "__") {
  columns <- names(data)
  column_labels <- NULL

  for(i in seq_along(columns)) {
    column <- columns[i]
    label <- attributes(data[[column]])$label_xlsx
    label <- label %||% attributes(data[[column]])$label
    label <- label %||% column
    column_labels <- c(column_labels, label)
  }

  x <- stats::setNames(
    stringr::str_split(column_labels, pattern = names_separator),
    columns
  )

  x
}

get_header_split <- function(data, i = 1, ...) {
  header <- get_header(data, ...)
  header <- as.character(purrr::map_vec(header, \(x) x[i]))
  dplyr::if_else(is.na(header), '', header)
}

get_header_depth <- function(data, ...) {
  header <- get_header(data, ...)
  purrr::map_vec(header, \(x) length(x))
}

get_facade_prop <- function(object, key, fallback = NULL) {
  value <- object[[key]]
  if(is.null(value)) { value <- fallback }
  return(value)
}
