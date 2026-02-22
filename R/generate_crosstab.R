#' Generate cross-tabulation
#'
#' @param data A data frame (typically \code{tibble}) containing the variables to summarize.
#' @param x The variable to use for the rows of the cross-tabulation.
#' @param ... Additional variable(s) to use for the columns of the cross-tabulation. If none are provided, a frequency table for \code{x} will be returned.
#' @param add_total Logical. If \code{TRUE}, adds total row and/or column.
#' @param add_total_row Logical. If \code{TRUE}, adds a total row.
#' @param add_total_column Logical. If \code{TRUE}, adds a total column.
#' @param add_percent Logical. If \code{TRUE}, adds percent or proportion values to the table.
#' @param as_proportion Logical. If \code{TRUE}, displays proportions instead of percentages (range 0–1).
#' @param percent_by_column Logical. If \code{TRUE}, percentages are calculated by column; otherwise, by row.
#' @param name_separator Character. Separator used when constructing variable names in the output.
#' @param label_separator Character. Separator used when constructing labels in the output.
#' @param label_total Character. Label used for the total row/category.
#' @param label_total_column Character. Label used for the total column/category.
#' @param label_total_row Character. Label used for the total row/category.
#' @param label_na Character. Label to use for missing (\code{NA}) values.
#' @param include_na Logical. If \code{TRUE}, includes missing values in the cross table.
#' @param label_as_group_name Logical. If \code{TRUE}, uses the variable label of the grouping variable(s) as the name in the output list.
#' @param group_separator Character. Separator used when constructing group names in the output list.
#' @param group_as_list Logical. If \code{TRUE}, the output will be a list of data frames, one for each combination of grouping variable(s).
#' @param group_grand_total `r lifecycle::badge("experimental")` Logical. Compute grand total based on the grouping variable.
#' @param group_grand_total_label `r lifecycle::badge("experimental")` Character. Apply label to the grand total if \code{group_grand_total} is set to \code{TRUE}.
#' @param recode_na Character or \code{NULL}. Value used to replace missing values in labelled vectors; \code{"auto"} will determine a code automatically.
#' @param sort_column_names Logical. If \code{TRUE}, sorts the column names in the output.
#' @param calculate_per_group Logical. If \code{TRUE}, calculates the cross-tabulation separately for each group defined by the grouping variable(s).
#' @param expand_categories Logical. If \code{TRUE}, ensures that all categories of \code{x} are represented in the output, even if they have zero counts.
#' @param position_total Character. Position of the total row/column; either \code{"bottom"} or \code{"top"} for rows, and \code{"right"} or \code{"left"} for columns.
#' @param metadata A named list with optional metadata to attach as attributes, e.g. \code{title}, \code{subtitle}, and \code{source_note}.
#' @param collapse_list Logical (NOT YET IMPLEMENTED). If \code{TRUE} and \code{group_as_list = TRUE}, collapses the list of frequency tables into a single data frame with group identifiers. See also [collapse_list()].
#' @param convert_factor Logical. If \code{TRUE}, converts labelled variables to factors in the output. See also [convert_factor()].
#'
#' @return A data frame or a list of data frames containing the cross-tabulation results. If \code{group_as_list} is \code{TRUE}, the output will be a list of data frames, one for each combination of grouping variable(s). Otherwise, a single data frame is returned. Each data frame includes counts and, if specified, percentages or proportions for each combination of \code{x} and the additional variables provided in \code{...}.
#'
#' @seealso [generate_frequency()], [generate_output()], [rename_label()], [remove_label()]
#'
#' @export
#'
#' @examples
#' # Using built-in dataset `person_record`
#'
#' # Basic usage
#' person_record |>
#'  generate_crosstab(marital_status, sex)
#'
#'
#' # Multiple variables
#' person_record |>
#'  generate_crosstab(
#'   sex,
#'   seeing,
#'   hearing,
#'   walking,
#'   remembering,
#'   self_caring,
#'   communicating
#'  )
#'
#'  # Grouping
#'  person_record |>
#'    dplyr::group_by(sex) |>
#'    generate_crosstab(marital_status, employed, group_as_list = TRUE)
#'
#' # # Percent or proportion by row or column
#' person_record |>
#'  generate_crosstab(
#'    marital_status,
#'    sex,
#'    percent_by_column = TRUE
#'  )

generate_crosstab <- function(
  data,
  x,
  ...,
  add_total = TRUE,
  add_total_row = TRUE,
  add_total_column = TRUE,
  add_percent = TRUE,
  as_proportion = FALSE,
  percent_by_column = FALSE,
  name_separator = "_",
  label_separator = "__",
  label_total = "Total",
  label_total_column = NULL,
  label_total_row = NULL,
  label_na = "Not reported",
  include_na = TRUE,
  recode_na = "auto",
  label_as_group_name = TRUE,
  group_separator = " - ",
  group_as_list = FALSE,
  group_grand_total = FALSE,
  group_grand_total_label = "All",
  calculate_per_group = TRUE,
  expand_categories = TRUE,
  position_total = "bottom",
  sort_column_names = TRUE,
  collapse_list = FALSE,
  convert_factor = FALSE,
  metadata = NULL
) {

  n_args <- rlang::dots_n(...)
  cols_grouping <- names(dplyr::select(dplyr::ungroup(data), {{x}}))

  data <- dplyr::select(data, dplyr::group_cols(), {{x}}, ...)
  groups <- dplyr::group_vars(data)

  group_attrs <- get_group_attrs(data, groups)

  data_attrs <- get_data_attrs(data)
  x_attr <- data_attrs[[rlang::as_label(rlang::enquo(x))]]

  column_names <- names(dplyr::select(dplyr::ungroup(data), ...))

  if(length(cols_grouping) > 1) {

    separated_cols <- names(dplyr::select(dplyr::ungroup(data), {{x}}))
    united_names <- paste0(separated_cols, collapse = "__")

    data <- tidyr::unite(data, category, {{x}}, remove = FALSE, sep = "__")
    data <- dplyr::rename(data, !!as.name(united_names) := category)

    # nested_cols <- separated_cols[-length(separated_cols)]
    # if(length(nested_cols) > 1) {
    #
    #   united_nested_cols <- paste0(nested_cols, collapse = "__")
    #   data <- tidyr::unite(data, nested_category, dplyr::any_of(nested_cols), remove = FALSE, sep = "__")
    #   data <- dplyr::rename(data, !!as.name(united_nested_cols) := nested_category)
    # }

    df <- generate_crosstab(
      data,
      x = !!as.name(united_names),
      ...,
      add_total = add_total,
      add_total_row = FALSE,
      add_total_column = add_total_column,
      add_percent = add_percent,
      as_proportion = as_proportion,
      percent_by_column = percent_by_column,
      name_separator = name_separator,
      label_separator = label_separator,
      label_total = label_total,
      label_total_column = label_total_column,
      label_total_row = label_total_row,
      label_na = label_na,
      include_na = include_na,
      recode_na = recode_na,
      label_as_group_name = label_as_group_name,
      group_separator = group_separator,
      group_as_list = group_as_list,
      calculate_per_group = calculate_per_group,
      expand_categories = expand_categories,
      position_total = position_total,
      sort_column_names = sort_column_names,
      collapse_list = FALSE,
      convert_factor = convert_factor,
      metadata = metadata
    )

    df <- separate_cols(
      data = df,
      cols = separated_cols,
      data_attrs = data_attrs,
      label_total = label_total,
      add_total = add_total | add_total_row,
      convert_factor = convert_factor
    )

    return(df)
  }

  if(n_args == 0) {

    df <- generate_frequency(
      data,
      {{x}},
      add_total = add_total,
      as_proportion = as_proportion,
      include_na = include_na,
      recode_na = recode_na,
      position_total = position_total,
      calculate_per_group = calculate_per_group,
      group_separator = group_separator,
      group_as_list = group_as_list,
      group_grand_total = group_grand_total,
      group_grand_total_label = group_grand_total_label,
      label_as_group_name = label_as_group_name,
      label_na = label_na,
      label_total = label_total,
      expand_categories = expand_categories,
      collapse_list = collapse_list,
      convert_factor = convert_factor,
      metadata = metadata
    )

    return(df)

  }

  df_list <- list()
  categories <- unique(data[[rlang::as_label(rlang::enquo(x))]])

  for(column_name in column_names) {

    list_name <- column_name
    if(label_as_group_name) {
      list_name <- attributes(data[[column_name]])$label
      if(is.null(list_name)) { list_name <- column_name }
    }

    data_i <- tsg_get_crosstab(data, {{x}}, column_name, include_na)

    if(group_as_list & length(groups) > 0) {

      glue_arg <- paste0(
        paste0("{haven::as_factor(", groups, ")}"),
        collapse = group_separator
      )

      df_groups <- data_i |>
        dplyr::select(dplyr::any_of(groups)) |>
        dplyr::distinct(.keep_all = TRUE) |>
        dplyr::mutate(list_group = glue::glue(glue_arg))

      data_ij <- list()

      if(group_grand_total) {

        data_g <- dplyr::ungroup(data)

        for(g in groups) {

          data_g <- coerce_total(
            data = data_g,
            col = g,
            x = data_g[[g]],
            label_total = group_grand_total_label,
            default_code = -1L
          )
        }

        data_ij[[group_grand_total_label]] <- data_g |>
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
          generate_crosstab(
            {{x}},
            !!as.name(column_name),
            add_total = add_total,
            add_total_row = add_total_row,
            add_total_column = add_total_column,
            add_percent = add_percent,
            as_proportion = as_proportion,
            percent_by_column = percent_by_column,
            name_separator = name_separator,
            label_separator = label_separator,
            label_total = label_total,
            label_total_column = label_total_column,
            label_total_row = label_total_row,
            label_na = label_na,
            include_na = include_na,
            recode_na = recode_na,
            label_as_group_name = label_as_group_name,
            group_separator = group_separator,
            group_as_list = FALSE,
            group_grand_total = FALSE,
            group_grand_total_label = FALSE,
            calculate_per_group = calculate_per_group,
            expand_categories = expand_categories,
            position_total = position_total,
            sort_column_names = sort_column_names,
            collapse_list = collapse_list,
            convert_factor = convert_factor,
            metadata = metadata
          )
      }

      for(j in seq_along(df_groups$list_group)) {

        list_group_j <- df_groups$list_group[j]

        data_j <- data_i |>
          dplyr::filter(glue::glue(glue_arg) == list_group_j) |>
          expand_category_values(
            categories = categories,
            df_groups[j, groups],
            expand = expand_categories
          ) |>
          tsg_pivot_table(
            column_name,
            groups = groups,
            data_attr = data_attrs[[column_name]],
            x_attr = x_attr,
            add_percent = add_percent,
            add_total = add_total,
            add_total_row = add_total_row,
            add_total_column = add_total_column,
            as_proportion = as_proportion,
            percent_by_column = percent_by_column,
            position_total = position_total,
            label_total = label_total_row %||% label_total,
            name_separator = name_separator,
            label_separator = label_separator,
            label_na = label_na,
            sort_column_names = sort_column_names
          ) |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
          add_total_label(
            label = label_total_column %||% label_total,
            label_separator = label_separator,
            name_separator = name_separator,
            percent_by_column = percent_by_column & add_percent
          ) |>
          set_group_attrs(groups, group_attrs, resolve = FALSE)

        if(include_na & length(data_j$category[is.na(data_j$category)]) > 0) {

          data_j$category <- add_missing_label(
            value = data_j$category,
            label_na = label_na,
            recode_na = recode_na
          )
        }

        if(convert_factor) {
          data_j <- dplyr::mutate_if(data_j, haven::is.labelled, haven::as_factor)
        }

        if(!add_total | !add_total_row) {
          if(position_total[1] == 'top') {
            data_j <- data_j[-1, ]
          } else {
            data_j <- data_j[-nrow(data_j), ]
          }
        }

        if(!add_total | !add_total_column) {
          data_j <- dplyr::select(data_j, -dplyr::any_of('total'))
        }

        data_ij[[list_group_j]] <- data_j

      }

      data_i <- data_ij


    } else {

      multiplier <- get_multiplier(as_proportion)

      if(calculate_per_group & length(groups) > 0) {

        data_i <- data_i |>
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
          tidyr::nest(data = -dplyr::all_of(groups)) |>
          dplyr::mutate(data = purrr::map(data, function(x) {
            x |>
              expand_category_values(
                categories = categories,
                expand = expand_categories
              ) |>
              tsg_pivot_table(
                column_name,
                data_attr = data_attrs[[column_name]],
                x_attr = x_attr,
                add_percent = add_percent,
                add_total = add_total,
                add_total_row = add_total_row,
                add_total_column = add_total_column,
                as_proportion = as_proportion,
                percent_by_column = percent_by_column,
                position_total = position_total,
                label_total = label_total_row %||% label_total,
                name_separator = name_separator,
                label_separator = label_separator,
                label_na = label_na,
                sort_column_names = sort_column_names
              )
          })) |>
          tidyr::unnest(cols = c(data), keep_empty = expand_categories) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with("frequency"),
              ~ dplyr::if_else(is.na(.), 0L, .)
            )
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::matches(glue::glue("^(percent|proportion){name_separator}")),
              ~ dplyr::if_else(is.na(.), 0, .)
            )
          )


        if(group_grand_total) {

          data_g <- dplyr::ungroup(data)

          for(g in groups) {

            data_g <- coerce_total(
              data = data_g,
              col = g,
              x = data_g[[g]],
              label_total = group_grand_total_label,
              default_code = -1L
            )

          }

          data_i <- dplyr::bind_rows(
            data_g |>
              dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
              tsg_get_crosstab({{x}}, column_name, include_na) |>
              tidyr::nest(data = -dplyr::all_of(groups)) |>
              dplyr::mutate(data = purrr::map(data, function(x) {
                x |>
                  expand_category_values(
                    categories = categories,
                    expand = expand_categories
                  ) |>
                  tsg_pivot_table(
                    column_name,
                    data_attr = data_attrs[[column_name]],
                    x_attr = x_attr,
                    add_percent = add_percent,
                    add_total = add_total,
                    add_total_row = add_total_row,
                    add_total_column = add_total_column,
                    as_proportion = as_proportion,
                    percent_by_column = percent_by_column,
                    position_total = position_total,
                    label_total = label_total_row %||% label_total,
                    name_separator = name_separator,
                    label_separator = label_separator,
                    label_na = label_na,
                    sort_column_names = sort_column_names
                  )
              })) |>
              tidyr::unnest(cols = c(data), keep_empty = expand_categories) |>
              dplyr::ungroup() |>
              dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with("frequency"),
                  ~ dplyr::if_else(is.na(.), 0L, .)
                )
              ) |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::matches(glue::glue("^(percent|proportion){name_separator}")),
                  ~ dplyr::if_else(is.na(.), 0, .)
                )
              ),
            data_i
          )


        }


        data_i <- add_column_label(
            data_i,
            x = 'category',
            x_attr = x_attr,
            column_name = column_name,
            data_attr = data_attrs[[column_name]],
            multiplier_col = multiplier$col,
            name_separator = name_separator,
            label_separator = label_separator,
            label_na = label_na,
            prefixed = add_percent
          )

        if(any(grepl(glue::glue("^(frequency|percent|proportion){name_separator}"), names(data_i)))) {

          data_i <- data_i |>
            dplyr::select(
              dplyr::any_of(groups),
              dplyr::any_of(c(".category", "category", "total")),
              dplyr::matches("^frequency"),
              dplyr::matches("^(percent|proportion)"),
              dplyr::everything()
            )
        } else {

          data_i <- data_i |>
            dplyr::select(
              dplyr::any_of(groups),
              dplyr::any_of(c(".category", "category")),
              dplyr::matches("^frequency"),
              dplyr::matches("^(percent|proportion)"),
              dplyr::everything(),
              dplyr::any_of("total")
            )
        }

        if(!add_total | !add_total_column) {
          data_i <- dplyr::select(data_i, -dplyr::any_of('total'))
        }

        if(!include_na) {
          data_i <- dplyr::filter(data_i, !is.na(category))
        }


      } else {

        data_i <- data_i |>
          tsg_pivot_table(
            column_name,
            data_attr = data_attrs[[column_name]],
            x_attr = x_attr,
            add_percent = add_percent,
            add_total = add_total,
            add_total_row = add_total_row,
            add_total_column = add_total_column,
            as_proportion = as_proportion,
            percent_by_column = percent_by_column,
            position_total = position_total,
            label_total = label_total_row %||% label_total,
            name_separator = name_separator,
            label_separator = label_separator,
            label_na = label_na,
            sort_column_names = sort_column_names,
            groups = groups
          ) |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
          set_group_attrs(groups, group_attrs, resolve = FALSE)
      }

      data_i <- add_total_label(
        data_i,
        label = label_total_column %||% label_total,
        label_separator = label_separator,
        name_separator = name_separator,
        percent_by_column = percent_by_column & add_percent
      )

      if(include_na & length(data_i$category[is.na(data_i$category)]) > 0) {

        data_i$category <- add_missing_label(
          value = data_i$category,
          label_na = label_na,
          recode_na = recode_na
        )
      }

      if(convert_factor) {
        data_i <- dplyr::mutate_if(data_i, haven::is.labelled, haven::as_factor)
      }

      if(!add_total | !add_total_row) {

        if(position_total[1] == 'top') {
          data_i <- data_i[-1, ]
        } else {
          data_i <- data_i[-nrow(data_i), ]
        }
      }

      if(!add_total | !add_total_column) {
        data_i <- dplyr::select(data_i, -dplyr::any_of('total'))
      }

    }

    df_list[[list_name]] <- data_i

  }

  if(length(df_list) == 1) {
    df_list <- df_list[[1]]
  } #else if (length(df_list) > 1 & collapse_list) {
  #   df_list <- collapse_list(data = df_list)
  # }

  if(group_as_list & length(groups) > 0) {
    attr(df_list, "groups") <- groups
  }

  for(meta in names(metadata)) {
    attr(df_list, meta) <- metadata[[meta]]
  }

  class(df_list) <- c("tsg", "tsgc", class(df_list))

  return(df_list)

}



tsg_pivot_table <- function(
  data,
  column_name,
  add_percent,
  add_total,
  add_total_row,
  add_total_column,
  data_attr,
  x_attr,
  as_proportion,
  position_total,
  label_total,
  label_separator,
  name_separator,
  label_na,
  sort_column_names,
  percent_by_column = FALSE,
  groups = NULL
) {

  multiplier <- get_multiplier(as_proportion)

  col_prefix <- ""
  if(add_percent) {
    col_prefix <- glue::glue("frequency{name_separator}")
  }
  col_prefix_p <- glue::glue("{multiplier$col}{name_separator}")

  data <- data |>
    tidyr::pivot_wider(
      names_from = !!as.name(column_name),
      values_from = frequency,
      values_fill = 0,
      names_prefix = col_prefix,
      names_expand = TRUE,
      names_sort = sort_column_names
    ) |>
    dplyr::select(
      dplyr::any_of(groups),
      dplyr::where(~ !all(. == 0))
    )

  total_col <- "total"

  if(add_percent) {

    if(percent_by_column) { total_col <- glue::glue("{col_prefix}total") }

    data[[total_col]] <- as.integer(
      rowSums(data[, grepl(glue::glue("^{col_prefix}"), names(data))], na.rm = TRUE)
    )
  } else {
    data[[total_col]] <- as.integer(
      rowSums(data[, which(names(data) != ".category" & !(names(data) %in% groups))], na.rm = TRUE)
    )
  }

  if(add_percent) {

    if(!percent_by_column) {
      data <- tsg_add_row_total(
        data,
        .category,
        position = position_total,
        label_total = label_total,
        groups = groups
      )
    }

    data <- data |>
      dplyr::select(.category, dplyr::any_of(total_col), dplyr::everything()) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with(col_prefix),
          function(x) {
            if(percent_by_column) {
              (x / sum(x, na.rm = TRUE)) * multiplier$value
            } else {
              (x / !!as.name(total_col)) * multiplier$value
            }

          },
          .names = glue::glue("{col_prefix_p}{{col}}")
        )
      )
  }

  if(percent_by_column | !add_percent) {
    data <- tsg_add_row_total(
      data,
      .category,
      position = position_total,
      label_total = label_total,
      groups = groups
    )
  }

  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(col_prefix_p),
        ~ dplyr::if_else(is.nan(.), 0, .)
      )
    ) |>
    dplyr::rename_all(
      ~ stringr::str_replace(
        .,
        glue::glue("^{col_prefix_p}{col_prefix}"),
        glue::glue("{col_prefix_p}")
      )
    ) |>
    add_column_label(
      x = '.category',
      x_attr = x_attr,
      column_name = column_name,
      data_attr = data_attr,
      multiplier_col = multiplier$col,
      name_separator = name_separator,
      label_separator = label_separator,
      label_na = label_na,
      prefixed = add_percent
    ) |>
    dplyr::rename(category = .category)

}


add_column_label <- function(
  data,
  x,
  x_attr,
  column_name,
  data_attr,
  multiplier_col,
  name_separator,
  label_separator,
  label_na,
  prefixed = TRUE,
  excluded = NULL
) {

  attr(data, "col") <- column_name
  label_pattern <- glue::glue("^(frequency|{multiplier_col}){name_separator}")

  tab_labels <- names(data)
  if(!is.null(excluded)) {
    tab_labels <- tab_labels[!(tab_labels %in% excluded)]
  }

  if(prefixed) {
    tab_labels <- tab_labels[grepl(label_pattern, tab_labels)]
  }

  for(tab_label in tab_labels) {

    new_label <- stringr::str_remove(tab_label, label_pattern)
    prefix_label <- ""
    if(prefixed) {
      prefix_label <- stringr::str_remove(tab_label, glue::glue("{name_separator}{new_label}"))
    }

    if(new_label == "NA") new_label <- label_na

    value <- new_label
    if(grepl("^[0-9]+$", new_label)) {
      value <- as.integer(new_label)
    }

    if(!is.na(value) & !is.null(data_attr$labels)) {
      if(value %in% data_attr$labels) {
        new_label <- names(which(data_attr$labels == value))
      }
    }

    if(prefixed) {
      new_label <- paste0(stringr::str_to_title(prefix_label), label_separator, new_label)
    }

    attr(data[[tab_label]], "label") <- new_label

  }

  if(x %in% names(data)) {
    attr(data[[x]], "label") <- x_attr$label
  }

  data

}


add_total_label <- function(data, label, name_separator, label_separator, percent_by_column = FALSE) {

  total_col <- "total"
  if(percent_by_column) {
    label <- glue::glue("Frequency{label_separator}{label}")
    total_col <- glue::glue("frequency{name_separator}{total_col}")
  }

  attr(data[[total_col]], "label") <- label

  data

}
