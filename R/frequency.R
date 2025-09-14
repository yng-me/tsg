#' Generate Frequency Tables with Optional Grouping and Summary Statistics
#'
#' Creates frequency tables for one or more categorical variables, optionally grouped by other variables.
#' The function supports various enhancements such as sorting, totals, percentages, cumulative statistics,
#' handling of missing values, and label customization. It returns a single table or a list of frequency tables.
#'
#' @param data A data frame (typically \code{tibble}) containing the variables to summarize.
#' @param ... One or more unquoted variable names (passed via tidy evaluation) for which to compute frequency tables.
#' @param sort_value Logical. If \code{TRUE}, frequency values will be sorted.
#' @param sort_desc Logical. If \code{TRUE}, sorts in descending order of frequency. Ignored if \code{sort_value = FALSE}.
#' @param sort_except Optional character vector. Variables to exclude from sorting.
#' @param add_total Logical. If \code{TRUE}, adds a total row or value to the frequency table.
#' @param add_percent Logical. If \code{TRUE}, adds percent or proportion values to the table.
#' @param add_cumulative Logical. If \code{TRUE}, adds cumulative frequency counts.
#' @param add_cumulative_percent Logical. If \code{TRUE}, adds cumulative percentages (or proportions if \code{as_proportion = TRUE}).
#' @param include_na Logical. If \code{TRUE}, includes missing values in the frequency table.
#' @param recode_na Character or \code{NULL}. Value used to replace missing values in labelled vectors; \code{"auto"} will determine a code automatically.
#' @param calculate_per_group Logical. If \code{TRUE}, calculates frequencies within groups defined in \code{data} (from \code{group_by()} or existing grouping).
#' @param as_proportion Logical. If \code{TRUE}, displays proportions instead of percentages (range 0–1).
#' @param position_total Character. Where to place the total row: \code{"top"} or \code{"bottom"}.
#' @param group_separator Character. Separator used when concatenating group values in list output (if \code{group_as_list = TRUE}).
#' @param group_as_list Logical. If \code{TRUE}, output is a list of frequency tables for each group combination.
#' @param label_stub Optional character vector used for labeling output tables (e.g., for export or display).
#' @param label_na Character. Label to use for missing (\code{NA}) values.
#' @param label_total Character. Label used for the total row/category.
#' @param metadata A named list with optional metadata to attach as attributes: \code{title}, \code{subtitle}, \code{source_note}, and \code{footnotes}.
#'
#' @return A frequency table (\code{tibble}, possibly nested) or a list of such tables. Additional attributes such as labels, metadata, and grouping information may be attached. The returned object is of class \code{"tsg"}.
#'
#' @export
#'
#' @examples
#'

generate_frequency <- function(
  data,
  ...,
  sort_value = TRUE,
  sort_desc = TRUE,
  sort_except = NULL,
  add_total = TRUE,
  add_percent = TRUE,
  add_cumulative = FALSE,
  add_cumulative_percent = FALSE,
  include_na = TRUE,
  recode_na = "auto",
  calculate_per_group = TRUE,
  as_proportion = FALSE,
  position_total = "bottom",
  group_separator = " - ",
  group_as_list = FALSE,
  label_stub = NULL,
  label_na = "Not reported",
  label_total = "Total",
  metadata = list(
    title = NULL,
    subtitle = NULL,
    source_note = NULL,
    footnotes = NULL
  )
) {

  n_args <- rlang::dots_n(...)
  df <- list()

  groups <- dplyr::group_vars(data)
  group_attrs <- stats::setNames(lapply(groups, \(x) attributes(data[[x]])), groups)

  multiplier <- 100
  multiplier_label <- "Percent"
  if(as_proportion) {
    multiplier <- 1
    multiplier_label <- "Proportion"
  }
  multiplier_col <- tolower(multiplier_label)
  cumulative_col <- glue::glue("cumulative_{multiplier_col}")
  cumulative_label <- glue::glue("Cumulative {multiplier_col}")

  if(n_args > 0) {
    data <- dplyr::select(data, dplyr::any_of(groups), ...)
  }

  column_names <- names(data)

  label_stubs <- NULL
  if(length(label_stub) > 0) {
    stub_eq <- length(column_names) == length(label_stub)
    if(length(label_stub) == 1 | (length(label_stub) > 1 & !stub_eq)) {
      label_stubs <- rep(label_stub, length(column_names))
    } else if (length(label_stub) > 1 & stub_eq) {
      label_stubs <- label_stub
    }
  }

  if(length(groups) > 0) {
    column_names <- column_names[!(column_names %in% groups)]
  }

  for(i in seq_along(column_names)) {

    column_name <- column_names[i]
    label <- attributes(data[[column_name]])$label

    if(is.null(label)) {
      label <- column_name
    }

    if(!include_na) {
      data <- dplyr::filter(data, !is.na(!!as.name(column_name)))
    }

    df_i <- data |>
      dplyr::rename(category := !!as.name(column_name)) |>
      dplyr::group_by(category, .add = TRUE) |>
      dplyr::count(name = "frequency") |>
      dplyr::ungroup() |>
      dplyr::collect()

    sort_value_i <- sort_value
    if(!is.null(sort_except)) {
      sort_value_i <- !(column_name %in% sort_except)
    }

    if(sort_value_i & length(groups) == 0) {
      if(sort_desc) {
        df_i <- dplyr::arrange(df_i, dplyr::desc(frequency))
      } else {
        df_i <- dplyr::arrange(df_i, frequency)
      }
    }

    if(group_as_list & length(groups) > 0) {

      glue_arg <- paste0(
        paste0("{haven::as_factor(", groups, ")}"),
        collapse = group_separator
      )

      df_groups <- df_i |>
        dplyr::select(dplyr::any_of(groups)) |>
        dplyr::distinct(.keep_all = T) |>
        dplyr::mutate(list_group = glue::glue(glue_arg))

      df_ij <- list()

      for(j in seq_along(df_groups$list_group)) {

        list_group_j <- df_groups$list_group[j]

        df_j <- dplyr::filter(df_i, glue::glue(glue_arg) == list_group_j)

        if(add_percent) { df_j[[multiplier_col]] <- multiplier * (df_j$frequency / sum(df_j$frequency, na.rm = T)) }
        if(add_cumulative) { df_j$cumulative <- cumsum(df_j$frequency) }
        if(add_cumulative_percent & add_percent) { df_j[[cumulative_col]] <- cumsum(df_j[[multiplier_col]]) }
        if(add_total) {
          df_j <- add_column_total(
            data = df_j,
            position = position_total,
            colname = multiplier_col,
            label_total = label_total
          )
        }

        if(is.null(attributes(df_j$category)$label)) {
          attr(df_j$category, "label") <- column_name
        }

        attr(df_j$frequency, "label") <- "Frequency"

        if(multiplier_col %in% names(df_j)) { attr(df_j[[multiplier_col]], "label") <- multiplier_label }
        if("cumulative" %in% names(df_j)) { attr(df_j$cumulative, "label") <- "Cumulative frequency" }
        if(cumulative_col %in% names(df_j)) { attr(df_j[[cumulative_col]], "label") <- cumulative_label }

        attr_names <- names(group_attrs)

        for(k in seq_along(attr_names)) {

          attr_k <- attr_names[k]
          attr_label <- group_attrs[[attr_k]]$label
          attr_labels <- group_attrs[[attr_k]]$labels

          if(!is.null(attr_labels)) {

            df_j[[attr_k]] <- haven::labelled(
              x = df_j[[attr_k]],
              label = group_attrs[[attr_k]]$label,
              labels = group_attrs[[attr_k]]$labels
            )

          } else if (!is.null(attr_label)) {

            attr(df_j[[attr_k]], "label") <- attr_label

          }
        }

        attr(df_j, "groups") <- groups

        df_ij[[list_group_j]] <- df_j

      }

      df_i <- df_ij


    } else {

      if(calculate_per_group & length(groups) > 0) {

        df_i <- df_i |>
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
          tidyr::nest(data = -dplyr::all_of(groups)) |>
          dplyr::mutate(data = purrr::map(data, function(x) {

            if(add_percent) { x[[multiplier_col]] <- multiplier * (x$frequency / sum(x$frequency, na.rm = T)) }
            if(add_cumulative) { x$cumulative <- cumsum(x$frequency) }
            if(add_cumulative_percent & add_percent) { x[[cumulative_col]] <- cumsum(x[multiplier_col]) }
            if(add_total) {
              x <- add_column_total(
                data = x,
                position = position_total,
                colname = multiplier_col,
                label_total = label_total
              )
            }

            return(x)

          })) |>
          tidyr::unnest(cols = c(data)) |>
          dplyr::ungroup()

      } else {

        if(add_percent) { df_i[[multiplier_col]] <- multiplier * (df_i$frequency / sum(df_i$frequency, na.rm = T)) }
        if(add_cumulative) { df_i$cumulative <- cumsum(df_i$frequency) }
        if(add_cumulative_percent & add_percent) { df_i[[cumulative_col]] <- cumsum(df_i[[multiplier_col]]) }
        if(add_total & length(groups) == 0) {
          df_i <- add_column_total(
            data = df_i,
            position = position_total,
            colname = multiplier_col,
            label_total = label_total
          )
        }
      }

      attr(df_i$frequency, "label") <- "Frequency"

      if(multiplier_col %in% names(df_i)) { attr(df_i[[multiplier_col]], "label") <- multiplier_label }
      if("cumulative" %in% names(df_i)) { attr(df_i$cumulative, "label") <- "Cumulative frequency" }
      if(cumulative_col %in% names(df_i)) { attr(df_i[[cumulative_col]], "label") <- cumulative_label }

      if(!is.null(attributes(data[[column_name]])$label)) {
        attr(df_i$category, "label") <- label
      } else {
        attr(df_i$category, "label") <- column_name
      }
    }


    # if with missing category
    if(include_na & length(df_i$category[is.na(df_i$category)]) > 0) {

      df_i$category <- add_missing_label(
        value = df_i$category,
        label_na = label_na,
        recode_na = recode_na
      )

    }

    if(length(label_stubs) > 0) {
      attr(df_i, "label_xlsx") <- label_stubs[i]
    }

    df[[label]] <- df_i

  }

  if(length(df) == 1) { df <- df[[1]] }

  if(group_as_list & length(groups) > 0) {
    attr(df, "groups") <- groups
  }

  metadata_names <- names(metadata)
  for(i in seq_along(metadata_names)) {
    metadata_i <- metadata_names[i]
    attr(df, metadata_i) <- metadata[[i]]
  }

  class(df) <- c("tsg", class(df))
  return(df)

}


#' Add a Label for Missing Values in a Variable
#'
#' Assigns a custom label for missing (`NA`) values in a variable. Handles labelled vectors (via `haven::labelled`),
#' factors (via `forcats::fct_na_value_to_level()`), and other vectors by replacing `NA` with a user-defined value or label.
#'
#' @param value A vector (labelled, factor, or atomic) in which missing values should be labeled.
#' @param label_na Character. The label to assign to missing values (default is `"Not reported"`).
#' @param recode_na Character or `"auto"`. If the vector is `haven::labelled`, defines the numeric code used to represent missing values.
#' `"auto"` will automatically generate a suitable value beyond the current max code.
#'
#' @return A vector with missing values replaced and labeled appropriately. Preserves variable attributes.
#'
#' @export
#'
#' @examples
#'
#'
#'
add_missing_label <- function(value, label_na = "Not reported", recode_na = "auto") {

  if(haven::is.labelled(value)) {

    get_na_value <- function() {

      if(recode_na != "auto") return(recode_na)

      max_value <- as.integer(max(as.integer(value), na.rm = TRUE))
      if(grepl("^9+$", max_value)) {
        as.integer(paste0(rep(9, 1 + nchar(max_value)), collapse = ""))
      } else {
        as.integer(paste0(rep(9, nchar(max_value)), collapse = ""))
      }
    }

    value_na <- get_na_value()

    labels <- attributes(value)$labels
    labels_with_na <- c(labels, value_na)
    names(labels_with_na) <- c(names(labels), label_na)

    value[is.na(value)] <- value_na
    value <- haven::labelled(
      value,
      labels = labels_with_na,
      label = attributes(value)$label
    )

  } else if(is.factor(value)) {

    value <- forcats::fct_na_value_to_level(value, level = label_na)

  } else {

    value[is.na(value)] <- label_na

  }

  return(value)

}


#' Add a Total Row to a Frequency Table
#'
#' Adds a total row to a frequency table, computing the sum of the `frequency` column and (optionally) a percentage or proportion column.
#' Handles different types of category variables (e.g., labelled, factor, character, numeric).
#'
#' @param data A data frame containing a `category` column and a `frequency` column (and optionally a percent/proportion column).
#' @param position Character. Where to place the total row: `"top"` or `"bottom"`.
#' @param colname Character. Name of the column to sum along with frequency (usually `"percent"` or `"proportion"`).
#' @param label_total Character. Label to assign to the total row/category (default is `"Total"`).
#'
#' @return A data frame with a total row added. The object includes an attribute `"total"` with metadata about the operation.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   category = factor(c("A", "B", "C")),
#'   frequency = c(10, 20, 30),
#'   percent = c(16.7, 33.3, 50)
#' )
#'
#' add_column_total(df, position = "bottom", colname = "percent", label_total = "Total")
#'
#'
#'

add_column_total <- function(data, position = "bottom", colname = "percent", label_total = "Total") {

  total <- data |>
    dplyr::select(dplyr::any_of(c("frequency", colname))) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum, na.rm = TRUE), .groups = 'drop')

  if(haven::is.labelled(data$category)) {

    value__ <- 0L
    if(min(data$category, na.rm = T) == 0) {
      value__ <- -1L
    }

    total <- total |>
      dplyr::mutate(category = value__) |>
      dplyr::mutate(category = haven::labelled(category, labels = stats::setNames(value__, label_total)))

  } else if(is.factor(data$category)) {

    value__ <- 0L
    if(min(as.integer(data$category), na.rm = T) == 0) {
      value__ <- -1L
    }

    total <- total |>
      dplyr::mutate(category = value__) |>
      dplyr::mutate(category = factor(category, levels = value__, labels = label_total))

  } else if (is.numeric(data$category)) {

    value__ <- label_total
    data <- dplyr::mutate(data, category = as.character(category))
    total <- dplyr::mutate(total, category = "Total")

  } else if (is.character(data$category))  {

    value__ <- label_total
    total <- dplyr::mutate(total, category = label_total)

  }

  if(position == "top") {
    data <- dplyr::bind_rows(total, data) |>
      dplyr::select(category, dplyr::any_of(c("frequency", colname)))
  } else if(position == "bottom") {
    data <- dplyr::bind_rows(data, total)
  } else {
    stop("Invalid position argument. Use 'top' or 'bottom'.")
  }

  attr(data, "total") <- list(
    value = value__,
    label = label_total,
    position = position,
    colname = colname
  )

  data

}
