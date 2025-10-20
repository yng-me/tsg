rename_label <- function(data, ...) {

  rename_list <- rlang::list2(...)
  if(length(rename_list) == 0) { return(data) }

  add_data_label(data, rename_list)

}

remove_label <- function(data, ...) {

  if(inherits(data, "list")) {

    list_names <- names(data)

    for(j in seq_along(list_names)) {
      data[[list_names[j]]] <- remove_label(data[[list_names[j]]], ...)
    }

  } else {

    if(rlang::dots_n(...) == 0) {
      data_labels <- data
    } else {
      data_labels <- dplyr::select(data, ...)
    }

    cols <- names(data_labels)

    for(i in cols) {

      if(i %in% names(data)) {

        attr(data[[i]], "label") <- NULL
      } else {
        warning(paste0("Column '", i, "' not found in data."))
      }

    }
  }

  return(data)

}

remove_labels <- function(data, ...) {

  if(inherits(data, "list")) {

    list_names <- names(data)

    for(j in seq_along(list_names)) {
      data[[list_names[j]]] <- remove_label(data[[list_names[j]]], ...)
    }

  } else {

    if(rlang::dots_n(...) == 0) {
      data_labels <- data
    } else {
      data_labels <- dplyr::select(data, ...)
    }

    cols <- names(data_labels)

    for(i in cols) {

      if(i %in% names(data)) {

        attr(data[[i]], "label") <- NULL
        attr(data[[i]], "labels") <- NULL
        attr(data[[i]], "xlsx_label") <- NULL

      } else {
        warning(paste0("Column '", i, "' not found in data."))
      }

    }
  }

  return(data)

}

add_data_label <- function(data, labels) {

  labels_name <- names(labels)

  for(i in labels_name) {

    if(inherits(data, "list")) {

      list_names <- names(data)

      for(j in seq_along(list_names)) {
        data[[list_names[j]]] <- add_data_label(data[[list_names[j]]], labels)
      }

    } else {

      if(i %in% names(data)) {

        attr(data[[i]], "label") <- labels[[i]]
      } else {
        warning(paste0("Column '", i, "' not found in data."))
      }
    }
  }

  return(data)

}



