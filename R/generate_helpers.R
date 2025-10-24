#' Collapse a list of data frames or tibbles into a single data frame
#'
#' @param data A list of data frames or tibbles to be collapsed.
#' @param ... Additional arguments passed to \code{dplyr::filter()}.
#' @param col_id The name of the column to be created for the category.
#' @param label  A label for the category column. If `NULL`, defaults to "Category".
#' @param pluck A character vector of column names to pluck from the data frames. If \code{NULL}, all columns are retained.
#' @param as_proportion  If \code{TRUE}, the frequency values will be converted to proportions. Default is \code{FALSE}.
#' @param name_separator A string to separate the names of the columns in the output data frame. Default is "_".
#' @param label_separator A string to separate the labels of the columns in the output data frame. Default is "__".
#'
#' @returns
#' @export
#'
#' @examples
#'

collapse_list <- function(
    data,
    ...,
    col_id = "category",
    label = NULL,
    pluck = NULL,
    as_proportion = FALSE,
    name_separator = "_",
    label_separator = "__"
) {

  if(!inherits(data, "list")) {
    stop("Data must be a list")
  }

  multiplier_col <- get_multiplier(as_proportion, key = "col")
  cols_to_pivot <- c("frequency", multiplier_col)

  class(data) <- "list"

  data <- dplyr::bind_rows(data, .id = paste0('.', col_id))

  groups <- attributes(data)$groups
  group_attrs <- attributes(data)$group_attrs

  for(i in groups) {
    data <- dplyr::filter(data, !is.na(!!as.name(i)))
  }

  pluck_col <- function(df) {
    if(length(pluck) == 1) {
      cols_to_remove <- paste0("^", setdiff(c("frequency", "percent"), pluck), "_")
      df <- dplyr::select(df, -dplyr::matches(cols_to_remove))
    }
    df
  }

  data <- data |>
    dplyr::mutate(value = category) |>
    dplyr::filter(...) |>
    dplyr::select(-dplyr::any_of("value"), -dplyr::matches("^cumulative")) |>
    tidyr::pivot_wider(
      names_from = category,
      values_from = dplyr::any_of(cols_to_pivot),
      names_sep = name_separator,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    pluck_col() |>
    add_column_label(
      x = "category",
      column_name = col_id,
      data_attr = attributes(data$category),
      multiplier_col = multiplier_col,
      name_separator = name_separator,
      label_separator = label_separator,
      prefixed = multiplier_col %in% names(data)
    ) |>
    set_group_attrs(groups, group_attrs) |>
    dplyr::rename(!!as.name(col_id) := !!as.name(paste0('.', col_id))) |>
    dplyr::select(dplyr::any_of(groups), dplyr::any_of(col_id), dplyr::everything())

  col_label <- label %||% "Category"

  attr(data[[col_id]], "label") <- col_label

  attr(data, "groups") <- groups
  attr(data, "group_attrs") <- group_attrs

  class(data) <- unique(c("tsg", "tsgf", class(data)))

  data

}


#' Rename data labels
#'
#' @param data
#' @param ... A named list of labels to rename. The names should match the column names in the data, and the values should be the new labels.
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
rename_label <- function(data, ...) {

  rename_list <- rlang::list2(...)
  if(length(rename_list) == 0) { return(data) }

  add_data_label(data, rename_list)

}

#' Remnove data labels
#'
#' @param data
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#'

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

#' Remove all labels
#'
#' @param data
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
#'

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



