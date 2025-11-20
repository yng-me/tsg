#' Collapse a list of data frames or tibbles into a single data frame
#'
#' @param data A list of data frames or tibbles to be collapsed.
#' @param ... Additional arguments passed to \code{dplyr::filter()}.
#' @param col_id The name of the column to be created for the category.
#' @param label  A label for the category column. If \code{NULL}, defaults to "Category".
#' @param pluck A character vector of column names to pluck from the data frames. If \code{NULL}, all columns are retained.
#' @param as_proportion  If \code{TRUE}, the frequency values will be converted to proportions. Default is \code{FALSE}.
#' @param name_separator A string to separate the names of the columns in the output data frame. Default is "_".
#' @param label_separator A string to separate the labels of the columns in the output data frame. Default is "__".
#'
#' @returns A data frame with the specified category column and the frequency and percent columns for each category, along with any additional columns specified in \code{pluck}.
#' @export
#'
#' @examples
#' person_record |>
#'   generate_frequency(
#'     seeing,
#'     hearing,
#'     walking,
#'     remembering,
#'     self_caring,
#'     communicating
#'   ) |>
#'   collapse_list()

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
  cols_to_pivot <- paste0("^", c("frequency", multiplier_col))

  is_tsgc <- inherits(data, "tsgc")
  is_tsgf <- inherits(data, "tsgf")

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
      values_from = dplyr::matches(cols_to_pivot),
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
#' @param data A data frame or tibble to rename labels in.
#' @param ... A named list of labels to rename. The names should match the column names in the data, and the values should be the new labels.
#'
#' @returns A data frame or tibble with the specified labels renamed.
#' @export
#'
#' @examples
#' person_record |>
#'   generate_frequency(
#'     seeing,
#'     hearing,
#'     walking,
#'     remembering,
#'     self_caring,
#'     communicating
#'   ) |>
#'   collapse_list() |>
#'   rename_label(category = "Functional difficulty")


rename_label <- function(data, ...) {

  rename_list <- rlang::list2(...)
  if(length(rename_list) == 0) { return(data) }

  add_data_label(data, rename_list)

}

#' Remove data labels
#'
#' @param data A data frame or tibble from which to remove labels.
#' @param ... A character vector of column names from which to remove labels. If no columns are specified, all labels will be removed.
#'
#' @returns A data frame or tibble with the specified labels removed. If no columns are specified, all labels will be removed.
#' @export
#'
#' @examples
#' person_record |>
#'   generate_frequency(
#'     seeing,
#'     hearing,
#'     walking,
#'     remembering,
#'     self_caring,
#'     communicating
#'   ) |>
#'   collapse_list() |>
#'   remove_label()

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
#' @param data A data frame or tibble from which to remove all labels.
#' @param ... A character vector of column names from which to remove labels. If no columns are specified, all labels will be removed.
#'
#' @returns A data frame or tibble with all labels removed. If no columns are specified, all labels will be removed.
#' @export
#'
#' @examples
#' person_record |>
#'   generate_frequency(
#'     seeing,
#'     hearing,
#'     walking,
#'     remembering,
#'     self_caring,
#'     communicating
#'   ) |>
#'   collapse_list() |>
#'   remove_labels()

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


separate_cols <- function(data, cols, data_attrs, label_total = "Total", add_total = TRUE, convert_factor = FALSE) {

  if(inherits(data, "list")) {
    for(i in seq_along(data)) {
      data[[i]] <- separate_cols(
        data = data[[i]],
        cols = cols,
        data_attrs = data_attrs,
        label_total = label_total,
        add_total = add_total,
        convert_factor = convert_factor
      )
    }

    return(data)

  } else {

    data <- tidyr::separate(
      data,
      col = category,
      into = cols,
      sep = "__",
      remove = TRUE,
      fill = "right"
    )

    for(i in seq_along(cols)) {
      col_name <- cols[i]
      data_attr <- data_attrs[[col_name]]

      if(!is.null(data_attr)) {

        if(!is.null(data_attr$labels)) {

          if(data_attr$type == "integer") {
            data[[col_name]] <- suppressWarnings(as.integer(data[[col_name]]))
          } else if(data_attr$type == "double") {
            data[[col_name]] <- suppressWarnings(as.double(data[[col_name]]))
          } else if(data_attr$type == "character") {
            data[[col_name]] <- suppressWarnings(as.character(data[[col_name]]))
          }

          attr_labels <- data_attr$labels

          if(add_total) {
            .value <- 0L
            if(min(as.integer(data[[col_name]]), na.rm = TRUE) == 0) { .value <- -1L }
            data[[col_name]][is.na(data[[col_name]])] <- .value

            attr_labels <- c(attr_labels, stats::setNames(.value, label_total))

          }

          data[[col_name]] <- haven::labelled(
            data[[col_name]],
            label = data_attr$label,
            labels = attr_labels
          )

        } else {
          attr(data[[col_name]], "label") <- data_attr$label
        }
      } else {
        attr(data[[col_name]], "label") <- col_name
      }
    }

    if(convert_factor) {
      data <- dplyr::mutate_if(data, haven::is.labelled, haven::as_factor)
    }

    return(data)
  }

}
