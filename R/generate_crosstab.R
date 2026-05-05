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
#' @param group_separator Character. Separator used when concatenating group values in list output (if \code{group_as_list = TRUE} with a single group).
#' @param group_as_list Logical. If \code{TRUE}, output is a named list of cross-tabulation tables keyed by group value. With a single group the list is flat; with 2+ groups the list is nested. When combined with \code{group_as_hierarchy = TRUE}, a nested list with totals at each level is returned.
#' @param group_as_hierarchy Logical. When \code{TRUE} (without \code{group_as_list}), inserts grand-total rows into the output. When \code{TRUE} together with \code{group_as_list = TRUE}, returns a nested named list with a total entry at each level; the total key is formatted as \code{"{var_label}: {label_group_hierarchy}"}.
#' @param label_group_hierarchy Character. Label applied to grand-total entries when \code{group_as_hierarchy = TRUE}. Can be a single string (applied to all group levels) or a named character vector keyed by group column name for per-variable labels (e.g. \code{c(sex = "All sexes", employed = "All workers")}). Defaults to \code{"All"}.
#' @param recode_na Character or \code{NULL}. Value used to replace missing values in labelled vectors; \code{"auto"} will determine a code automatically.
#' @param sort_column_names Logical. If \code{TRUE}, sorts the column names in the output.
#' @param calculate_per_group Logical. If \code{TRUE}, calculates the cross-tabulation separately for each group defined by the grouping variable(s).
#' @param expand_categories Logical. If \code{TRUE}, ensures that all categories of \code{x} are represented in the output, even if they have zero counts.
#' @param position_total Character. Position of the total row/column; either \code{"bottom"} or \code{"top"} for rows, and \code{"right"} or \code{"left"} for columns.
#' @param metadata A named list with optional metadata to attach as attributes, e.g. \code{title}, \code{subtitle}, and \code{source_note}.
#' @param multiple_columns `r lifecycle::badge("experimental")` Logical or `NULL`. If `TRUE`, each column in `...` is treated as a binary indicator variable. Rows where the column equals `multiple_columns_filter` are counted per `x` category and presented as side-by-side frequency/percent columns in a single wide table. Requires at least 2 columns in `...`; if fewer are supplied a warning is issued and the function falls back to regular cross-tabulation mode.
#' @param multiple_columns_type Character. Controls how `multiple_columns = TRUE` handles the additional columns. `"filtered"` (default) treats each column as a binary indicator and produces a wide table with one column-pair per variable. `"stacked"` stacks results hierarchically: each column in `...` becomes a row group with `x` categories as columns; `multiple_columns_filter` is ignored in this mode.
#' @param multiple_columns_filter Scalar value (default `1L`). The value to filter on when `multiple_columns = TRUE` and `multiple_columns_type = "filtered"`. Ignored when `multiple_columns_type = "stacked"`.
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
#'  # Nested list with totals at each level (group_as_list + group_as_hierarchy)
#'  person_record |>
#'    dplyr::group_by(sex) |>
#'    generate_crosstab(marital_status, employed,
#'      group_as_list = TRUE, group_as_hierarchy = TRUE)
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
  label_as_group_name = TRUE,
  label_group_hierarchy = "All",
  include_na = TRUE,
  recode_na = "auto",
  group_separator = " - ",
  group_as_list = FALSE,
  group_as_hierarchy = FALSE,
  calculate_per_group = TRUE,
  expand_categories = TRUE,
  position_total = "bottom",
  sort_column_names = TRUE,
  collapse_list = FALSE,
  convert_factor = FALSE,
  multiple_columns = FALSE,
  multiple_columns_type = c("filtered", "stacked"),
  multiple_columns_filter = 1L,
  metadata = NULL
) {

  n_args <- rlang::dots_n(...)
  cols_grouping <- names(dplyr::select(dplyr::ungroup(data), {{x}}))

  data <- dplyr::select(data, dplyr::group_cols(), {{x}}, ...)
  groups <- dplyr::group_vars(data)

  # Collect Arrow/lazy frames once to avoid N separate round-trips in the column loop
  data <- collect_data(data)

  group_attrs <- get_group_attrs(data, groups)

  data_attrs <- get_data_attrs(data)
  x_attr <- data_attrs[[rlang::as_label(rlang::enquo(x))]]

  column_names <- names(dplyr::select(dplyr::ungroup(data), ...))
  n_cols <- length(column_names)

  x_col <- rlang::as_label(rlang::enquo(x))

  if(length(cols_grouping) > 1) {

    separated_cols <- names(dplyr::select(dplyr::ungroup(data), {{x}}))
    united_names <- paste0(separated_cols, collapse = "__")

    data <- tidyr::unite(data, category, {{x}}, remove = FALSE, sep = "__")
    data <- dplyr::rename(data, !!as.name(united_names) := category)

    x_col <- united_names
    add_total_row <- FALSE
    do_separate_cols <- TRUE

  } else {
    separated_cols <- NULL
    do_separate_cols <- FALSE
  }

  if(n_cols == 0) {

    warning('No column variable/s specified or no matching columns found. Fallback to `generate_frequency()`.')

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
      group_as_hierarchy = group_as_hierarchy,
      label_group_hierarchy = label_group_hierarchy,
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

  if(isTRUE(multiple_columns)) {
    if(n_cols < 2) {
      warning("'multiple_columns = TRUE' requires at least 2 columns in '...'. Falling back to regular mode.")
    } else {
      multiple_columns_type <- match.arg(multiple_columns_type)
      if(multiple_columns_type == "stacked") {
        return(crosstab_stacked_multiple_cols(
          data = data,
          x_col = x_col,
          column_names = column_names,
          data_attrs = data_attrs,
          x_attr = x_attr,
          groups = groups,
          group_attrs = group_attrs,
          group_as_list = group_as_list,
          group_separator = group_separator,
          group_as_hierarchy = group_as_hierarchy,
          label_group_hierarchy = label_group_hierarchy,
          calculate_per_group = calculate_per_group,
          add_percent = add_percent,
          add_total = add_total,
          add_total_row = add_total_row,
          add_total_column = add_total_column,
          as_proportion = as_proportion,
          percent_by_column = percent_by_column,
          position_total = position_total,
          label_total = label_total,
          label_total_column = label_total_column,
          label_total_row = label_total_row,
          name_separator = name_separator,
          label_separator = label_separator,
          label_na = label_na,
          include_na = include_na,
          recode_na = recode_na,
          convert_factor = convert_factor,
          sort_column_names = sort_column_names,
          label_as_group_name = label_as_group_name,
          metadata = metadata
        ))
      } else {
        return(crosstab_compute_multiple_cols(
          data = data,
          x_col = x_col,
          column_names = column_names,
          data_attrs = data_attrs,
          x_attr = x_attr,
          groups = groups,
          group_attrs = group_attrs,
          group_as_list = group_as_list,
          group_separator = group_separator,
          group_as_hierarchy = group_as_hierarchy,
          label_group_hierarchy = label_group_hierarchy,
          calculate_per_group = calculate_per_group,
          multiple_columns_filter = multiple_columns_filter,
          add_percent = add_percent,
          add_total = add_total,
          add_total_row = add_total_row,
          add_total_column = add_total_column,
          as_proportion = as_proportion,
          position_total = position_total,
          label_total = label_total,
          label_total_column = label_total_column,
          label_total_row = label_total_row,
          name_separator = name_separator,
          label_separator = label_separator,
          label_na = label_na,
          include_na = include_na,
          recode_na = recode_na,
          convert_factor = convert_factor,
          label_as_group_name = label_as_group_name,
          metadata = metadata
        ))
      }
    }
  }

  df_list <- list()
  categories <- unique(data[[x_col]])

  for(column_name in column_names) {

    list_name <- column_name
    if(label_as_group_name) {
      list_name <- attributes(data[[column_name]])$label
      if(is.null(list_name)) { list_name <- column_name }
    }

    data_i <- tsg_get_crosstab(data, !!rlang::sym(x_col), column_name, include_na)

    if(group_as_list && length(groups) > 0) {

      multiplier <- get_multiplier(as_proportion)

      crosstab_nested_fn <- function(sub_data) {
        sub_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
          tsg_get_crosstab(!!rlang::sym(x_col), column_name, include_na) |>
          tidyr::nest(data = -dplyr::all_of(groups)) |>
          dplyr::mutate(data = purrr::map(data, function(x) {
            crosstab_compute_group(
              x, categories, NULL, column_name,
              data_attr = data_attrs[[column_name]],
              x_attr = x_attr,
              add_percent = add_percent,
              add_total = add_total,
              add_total_row = add_total_row,
              add_total_column = add_total_column,
              as_proportion = as_proportion,
              percent_by_column = percent_by_column,
              position_total = position_total,
              label_total = label_total,
              label_total_row = label_total_row,
              name_separator = name_separator,
              label_separator = label_separator,
              label_na = label_na,
              sort_column_names = sort_column_names,
              expand_categories = expand_categories
            )
          })) |>
          tidyr::unnest(cols = c(data), keep_empty = expand_categories) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
          add_column_label(
            x = 'category',
            x_attr = x_attr,
            column_name = column_name,
            data_attr = data_attrs[[column_name]],
            multiplier_col = multiplier$col,
            name_separator = name_separator,
            label_separator = label_separator,
            label_na = label_na,
            prefixed = add_percent
          ) |>
          add_total_label(
            label = label_total_column %||% label_total,
            label_separator = label_separator,
            name_separator = name_separator,
            percent_by_column = percent_by_column & add_percent
          ) |>
          set_group_attrs(groups, group_attrs, resolve = FALSE)
      }

      apply_post_proc <- function(x) {
        if (is.data.frame(x)) {
          if (include_na & anyNA(x$category)) {
            x$category <- add_missing_label(x$category, label_na, recode_na)
          }
          if (convert_factor) {
            x <- dplyr::mutate(x, dplyr::across(dplyr::where(haven::is.labelled), haven::as_factor))
          }
          if (!add_total | !add_total_row) {
            x <- dplyr::filter(x, as.character(category) != (label_total_row %||% label_total))
          }
          if (!add_total | !add_total_column) {
            x <- dplyr::select(x, -dplyr::any_of('total'))
          }
          return(x)
        }
        lapply(x, apply_post_proc)
      }

      if(group_as_hierarchy) {

        data_i <- build_nested_group_list(
          raw_data    = dplyr::ungroup(data),
          groups_rem  = groups,
          group_attrs = group_attrs,
          add_totals  = TRUE,
          total_label = label_group_hierarchy,
          compute_fn  = crosstab_nested_fn
        )
        data_i <- apply_post_proc(data_i)

      } else if(length(groups) > 1) {

        # Properly nested list for 2+ groups (no totals)
        data_i <- build_nested_group_list(
          raw_data    = dplyr::ungroup(data),
          groups_rem  = groups,
          group_attrs = group_attrs,
          add_totals  = FALSE,
          total_label = "",
          compute_fn  = crosstab_nested_fn
        )
        data_i <- apply_post_proc(data_i)

      } else {

        glue_arg <- paste0("{haven::as_factor(", groups, ")}")

        df_groups <- data_i |>
          dplyr::select(dplyr::any_of(groups)) |>
          dplyr::distinct(.keep_all = TRUE) |>
          dplyr::mutate(list_group = glue::glue(glue_arg))

        data_ij <- list()

        for(j in seq_along(df_groups$list_group)) {

          list_group_j <- df_groups$list_group[j]

          data_j <- data_i |>
            dplyr::filter(glue::glue(glue_arg) == list_group_j) |>
            crosstab_compute_group(
              categories, df_groups[j, groups], column_name,
              data_attr = data_attrs[[column_name]],
              x_attr = x_attr,
              add_percent = add_percent,
              add_total = add_total,
              add_total_row = add_total_row,
              add_total_column = add_total_column,
              as_proportion = as_proportion,
              percent_by_column = percent_by_column,
              position_total = position_total,
              label_total = label_total,
              label_total_row = label_total_row,
              name_separator = name_separator,
              label_separator = label_separator,
              label_na = label_na,
              sort_column_names = sort_column_names,
              expand_categories = expand_categories,
              groups = groups
            ) |>
            dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
            add_total_label(
              label = label_total_column %||% label_total,
              label_separator = label_separator,
              name_separator = name_separator,
              percent_by_column = percent_by_column & add_percent
            ) |>
            set_group_attrs(groups, group_attrs, resolve = FALSE)

          if(include_na & anyNA(data_j$category)) {

            data_j$category <- add_missing_label(
              value = data_j$category,
              label_na = label_na,
              recode_na = recode_na
            )
          }

          if(convert_factor) {
            data_j <- dplyr::mutate(data_j, dplyr::across(dplyr::where(haven::is.labelled), haven::as_factor))
          }

          if(!add_total | !add_total_row) {
            data_j <- dplyr::filter(
              data_j,
              as.character(category) != (label_total_row %||% label_total)
            )
          }

          if(!add_total | !add_total_column) {
            data_j <- dplyr::select(data_j, -dplyr::any_of('total'))
          }

          data_ij[[list_group_j]] <- data_j

        }

        data_i <- data_ij

      }


    } else {

      multiplier <- get_multiplier(as_proportion)

      if(calculate_per_group & length(groups) > 0) {

        data_i <- data_i |>
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
          tidyr::nest(data = -dplyr::all_of(groups)) |>
          dplyr::mutate(data = purrr::map(data, function(x) {
            crosstab_compute_group(
              x, categories, NULL, column_name,
              data_attr = data_attrs[[column_name]],
              x_attr = x_attr,
              add_percent = add_percent,
              add_total = add_total,
              add_total_row = add_total_row,
              add_total_column = add_total_column,
              as_proportion = as_proportion,
              percent_by_column = percent_by_column,
              position_total = position_total,
              label_total = label_total,
              label_total_row = label_total_row,
              name_separator = name_separator,
              label_separator = label_separator,
              label_na = label_na,
              sort_column_names = sort_column_names,
              expand_categories = expand_categories
            )
          })) |>
          tidyr::unnest(cols = c(data), keep_empty = expand_categories) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::starts_with(c(
                "frequency",
                paste0("percent", name_separator),
                paste0("proportion", name_separator)
              )),
              ~ replace(., is.na(.), 0)
            )
          )


        if(group_as_hierarchy) {

          # Closure: computes grouped crosstab for an arbitrary (possibly coerced) data subset.
          crosstab_compute_grouped <- function(data_subset) {
            data_subset |>
              dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
              tsg_get_crosstab(!!rlang::sym(x_col), column_name, include_na) |>
              tidyr::nest(data = -dplyr::all_of(groups)) |>
              dplyr::mutate(data = purrr::map(data, function(x) {
                crosstab_compute_group(
                  x, categories, NULL, column_name,
                  data_attr = data_attrs[[column_name]],
                  x_attr = x_attr,
                  add_percent = add_percent,
                  add_total = add_total,
                  add_total_row = add_total_row,
                  add_total_column = add_total_column,
                  as_proportion = as_proportion,
                  percent_by_column = percent_by_column,
                  position_total = position_total,
                  label_total = label_total,
                  label_total_row = label_total_row,
                  name_separator = name_separator,
                  label_separator = label_separator,
                  label_na = label_na,
                  sort_column_names = sort_column_names,
                  expand_categories = expand_categories
                )
              })) |>
              tidyr::unnest(cols = c(data), keep_empty = expand_categories) |>
              dplyr::ungroup() |>
              dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
              dplyr::mutate(dplyr::across(
                dplyr::starts_with(c(
                  "frequency",
                  paste0("percent", name_separator),
                  paste0("proportion", name_separator)
                )),
                ~ replace(., is.na(.), 0)
              ))
          }

          grand_total_df <- crosstab_compute_grouped(
            coerce_groups_total(data, groups, label_group_hierarchy)
          )

          # Intermediate subtotals for 2+ groups
          subtotals <- list()
          if(length(groups) >= 2) {
            for(k in seq_len(length(groups) - 1)) {
              parent_groups <- groups[seq_len(k)]
              child_groups  <- groups[(k + 1):length(groups)]
              combos <- dplyr::distinct(
                dplyr::select(dplyr::ungroup(data), dplyr::all_of(parent_groups))
              )
              parts <- vector("list", nrow(combos))
              for(i in seq_len(nrow(combos))) {
                data_k <- dplyr::ungroup(data)
                for(pg in parent_groups) {
                  pv     <- combos[[pg]][i]
                  data_k <- dplyr::filter(data_k, !!rlang::sym(pg) == pv)
                }
                for(cg in child_groups) {
                  data_k <- coerce_total(
                    data_k, cg, data_k[[cg]],
                    .resolve_hierarchy_label(label_group_hierarchy, cg),
                    default_code = -1L
                  )
                }
                parts[[i]] <- crosstab_compute_grouped(data_k)
              }
              subtotals[[k]] <- dplyr::bind_rows(parts)
            }
          }

          data_i <- assemble_group_hierarchy(grand_total_df, subtotals, data_i, groups)
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

        if(any(
          startsWith(names(data_i), paste0("frequency", name_separator)) |
          startsWith(names(data_i), paste0("percent", name_separator)) |
          startsWith(names(data_i), paste0("proportion", name_separator))
        )) {

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

      if(add_total & add_total_column) {
        data_i <- add_total_label(
          data_i,
          label = label_total_column %||% label_total,
          label_separator = label_separator,
          name_separator = name_separator,
          percent_by_column = percent_by_column & add_percent
        )
      }

      if(include_na & anyNA(data_i$category)) {

        data_i$category <- add_missing_label(
          value = data_i$category,
          label_na = label_na,
          recode_na = recode_na
        )
      }

      if(convert_factor) {
        data_i <- dplyr::mutate(data_i, dplyr::across(dplyr::where(haven::is.labelled), haven::as_factor))
      }

      if(!add_total | !add_total_row) {
        data_i <- dplyr::filter(
          data_i,
          as.character(category) != (label_total_row %||% label_total)
        )
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

  if((group_as_list | group_as_hierarchy) && length(groups) > 0) {
    attr(df_list, "groups") <- groups
  }

  attr(df_list, "label_separator") <- label_separator

  for(meta in names(metadata)) {
    attr(df_list, meta) <- metadata[[meta]]
  }

  class(df_list) <- c("tsg", "tsgc", class(df_list))

  if(do_separate_cols) {
    df_list <- separate_cols(
      data = df_list,
      cols = separated_cols,
      data_attrs = data_attrs,
      label_total = label_total,
      add_total = add_total | add_total_row,
      convert_factor = convert_factor
    )
  }

  return(df_list)

}



crosstab_compute_group <- function(
  data, categories, group_row = NULL, column_name,
  data_attr, x_attr,
  add_percent, add_total, add_total_row, add_total_column,
  as_proportion, percent_by_column, position_total,
  label_total, label_total_row = NULL,
  name_separator, label_separator, label_na,
  sort_column_names, expand_categories, groups = NULL
) {
  data |>
    expand_category_values(
      categories = categories,
      group_row,
      expand = expand_categories
    ) |>
    tsg_pivot_table(
      column_name,
      groups = groups,
      data_attr = data_attr,
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
}


crosstab_compute_multiple_cols <- function(
  data, x_col, column_names, data_attrs, x_attr,
  groups, group_attrs, group_as_list, group_separator,
  group_as_hierarchy, label_group_hierarchy, calculate_per_group,
  multiple_columns_filter,
  add_percent, add_total, add_total_row, add_total_column, as_proportion,
  position_total, label_total, label_total_column, label_total_row,
  name_separator, label_separator, label_na, include_na, recode_na, convert_factor,
  label_as_group_name, metadata
) {

  multiplier <- get_multiplier(as_proportion)

  mc_one_table <- function(df) {

    # Single group_by + summarise pass: total N and per-column filtered counts
    result <- df |>
      dplyr::group_by(!!rlang::sym(x_col)) |>
      dplyr::summarise(
        .total_n = dplyr::n(),
        dplyr::across(
          dplyr::all_of(column_names),
          ~ sum(. == multiple_columns_filter, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    # Rename indicator columns to frequency_<cn>
    for(cn in column_names) {
      freq_col_name <- paste0("frequency", name_separator, cn)
      result <- dplyr::rename(result, !!freq_col_name := !!rlang::sym(cn))
    }

    if(add_percent) {
      for(cn in column_names) {
        freq_col_name <- paste0("frequency", name_separator, cn)
        pct_col_name  <- paste0(multiplier$col, name_separator, cn)
        result <- dplyr::mutate(
          result,
          !!pct_col_name := (!!rlang::sym(freq_col_name) / .total_n) * multiplier$value
        )
      }
    }

    result <- dplyr::rename(result, total = .total_n, category = !!rlang::sym(x_col))

    if(add_total && add_total_row) {
      total_label_val <- label_total_row %||% label_total
      result <- coerce_category(result, category)
      total_row <- dplyr::summarise(
        result,
        total = sum(total, na.rm = TRUE),
        dplyr::across(
          dplyr::starts_with(paste0("frequency", name_separator)),
          ~ sum(., na.rm = TRUE)
        )
      )
      total_row <- coerce_total(total_row, "category", result$category, label_total = total_label_val)
      if(add_percent) {
        for(cn in column_names) {
          freq_col_name <- paste0("frequency", name_separator, cn)
          pct_col_name <- paste0(multiplier$col, name_separator, cn)
          total_row[[pct_col_name]] <- total_row[[freq_col_name]] / total_row$total * multiplier$value
        }
      }
      total_row <- dplyr::select(total_row, category, dplyr::everything())
      if(position_total == "bottom") {
        result <- dplyr::bind_rows(result, total_row)
      } else {
        result <- dplyr::bind_rows(total_row, result)
      }
    }

    if(!add_total || !add_total_column) {
      result <- dplyr::select(result, -dplyr::any_of("total"))
    }

    if(!add_total || !add_total_row) {
      result <- dplyr::filter(result, as.character(category) != (label_total_row %||% label_total))
    }

    if(include_na && anyNA(result$category)) {
      result$category <- add_missing_label(result$category, label_na, recode_na)
    } else if(!include_na) {
      result <- dplyr::filter(result, !is.na(category))
    }

    if(convert_factor) {
      result <- dplyr::mutate(result, dplyr::across(dplyr::where(haven::is.labelled), haven::as_factor))
    }

    # Set column labels after all structural ops to prevent dplyr from stripping them
    attr(result$category, "label") <- x_attr$label
    if("total" %in% names(result)) {
      attr(result$total, "label") <- label_total_column %||% label_total
    }

    for(cn in column_names) {
      col_label <- data_attrs[[cn]]$label %||% cn
      freq_col_name <- paste0("frequency", name_separator, cn)
      if(freq_col_name %in% names(result)) {
        attr(result[[freq_col_name]], "label") <- paste0("Frequency", label_separator, col_label)
      }
      if(add_percent) {
        pct_col_name <- paste0(multiplier$col, name_separator, cn)
        if(pct_col_name %in% names(result)) {
          attr(result[[pct_col_name]], "label") <- paste0(
            stringr::str_to_title(multiplier$col), label_separator, col_label
          )
        }
      }
    }

    result
  }

  finalize_mc <- function(result, is_list = FALSE) {
    attr(result, "label_separator") <- label_separator
    attr(result, "name_separator") <- name_separator
    attr(result, "multiple_columns") <- TRUE
    attr(result, "multiple_columns_filter") <- multiple_columns_filter
    if(is_list && length(groups) > 0) {
      attr(result, "groups") <- groups
    }
    for(meta in names(metadata)) {
      attr(result, meta) <- metadata[[meta]]
    }
    class(result) <- c("tsg", "tsgc", class(result))
    result
  }

  if(group_as_list && length(groups) > 0) {

    mc_nested_fn <- function(sub_data) {
      result_j  <- mc_one_table(dplyr::ungroup(sub_data))
      group_row <- dplyr::select(dplyr::ungroup(sub_data), dplyr::all_of(groups))[1, , drop = FALSE]
      group_df  <- group_row[rep(1, nrow(result_j)), , drop = FALSE]
      rownames(group_df) <- NULL
      dplyr::bind_cols(group_df, result_j) |>
        set_group_attrs(groups, group_attrs, resolve = FALSE)
    }

    if(group_as_hierarchy) {

      result_list <- build_nested_group_list(
        raw_data    = dplyr::ungroup(data),
        groups_rem  = groups,
        group_attrs = group_attrs,
        add_totals  = TRUE,
        total_label = label_group_hierarchy,
        compute_fn  = mc_nested_fn
      )

      return(finalize_mc(result_list, is_list = TRUE))

    } else if(length(groups) > 1) {

      # Properly nested list for 2+ groups (no totals)
      result_list <- build_nested_group_list(
        raw_data    = dplyr::ungroup(data),
        groups_rem  = groups,
        group_attrs = group_attrs,
        add_totals  = FALSE,
        total_label = "",
        compute_fn  = mc_nested_fn
      )

      return(finalize_mc(result_list, is_list = TRUE))

    } else {

      glue_arg <- paste0("{haven::as_factor(", groups, ")}")

      df_groups <- dplyr::ungroup(data) |>
        dplyr::select(dplyr::any_of(groups)) |>
        dplyr::distinct(.keep_all = TRUE) |>
        dplyr::mutate(list_group = glue::glue(glue_arg))

      result_list <- list()

      for(j in seq_along(df_groups$list_group)) {
        list_group_j <- df_groups$list_group[j]
        data_subset <- dplyr::ungroup(data) |>
          dplyr::filter(glue::glue(glue_arg) == list_group_j)
        result_j <- mc_one_table(data_subset)
        group_df_j <- df_groups[rep(j, nrow(result_j)), groups, drop = FALSE]
        rownames(group_df_j) <- NULL
        data_j_mc <- dplyr::bind_cols(group_df_j, result_j) |>
          set_group_attrs(groups, group_attrs, resolve = FALSE)
        result_list[[list_group_j]] <- data_j_mc
      }

      return(finalize_mc(result_list, is_list = TRUE))

    }

  } else if(calculate_per_group && length(groups) > 0) {

    mc_compute_grouped <- function(data_subset) {
      data_subset |>
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
        tidyr::nest(data = -dplyr::all_of(groups)) |>
        dplyr::mutate(data = purrr::map(data, mc_one_table)) |>
        tidyr::unnest(cols = c(data), keep_empty = TRUE) |>
        dplyr::ungroup() |>
        dplyr::select(dplyr::any_of(groups), dplyr::everything())
    }

    restore_mc_labels <- function(df) {
      attr(df$category, "label") <- x_attr$label
      if ("total" %in% names(df)) {
        attr(df$total, "label") <- label_total_column %||% label_total
      }
      for (cn in column_names) {
        col_label <- data_attrs[[cn]]$label %||% cn
        freq_col_name <- paste0("frequency", name_separator, cn)
        if (freq_col_name %in% names(df)) {
          attr(df[[freq_col_name]], "label") <- paste0("Frequency", label_separator, col_label)
        }
        if (add_percent) {
          pct_col_name <- paste0(multiplier$col, name_separator, cn)
          if (pct_col_name %in% names(df)) {
            attr(df[[pct_col_name]], "label") <- paste0(
              stringr::str_to_title(multiplier$col), label_separator, col_label
            )
          }
        }
      }
      df
    }

    result_df <- mc_compute_grouped(data)

    if(group_as_hierarchy) {
      grand_total_df <- mc_compute_grouped(
        coerce_groups_total(data, groups, label_group_hierarchy)
      )

      subtotals <- list()
      if(length(groups) >= 2) {
        for(k in seq_len(length(groups) - 1)) {
          parent_groups <- groups[seq_len(k)]
          child_groups  <- groups[(k + 1):length(groups)]
          combos <- dplyr::distinct(
            dplyr::select(dplyr::ungroup(data), dplyr::all_of(parent_groups))
          )
          parts <- vector("list", nrow(combos))
          for(i in seq_len(nrow(combos))) {
            data_k <- dplyr::ungroup(data)
            for(pg in parent_groups) {
              pv     <- combos[[pg]][i]
              data_k <- dplyr::filter(data_k, !!rlang::sym(pg) == pv)
            }
            for(cg in child_groups) {
              data_k <- coerce_total(
                data_k, cg, data_k[[cg]],
                .resolve_hierarchy_label(label_group_hierarchy, cg),
                default_code = -1L
              )
            }
            parts[[i]] <- mc_compute_grouped(data_k)
          }
          subtotals[[k]] <- dplyr::bind_rows(parts)
        }
      }

      result_df <- assemble_group_hierarchy(grand_total_df, subtotals, result_df, groups)

      # Restore column labels stripped by bind_rows inside assemble_group_hierarchy
      result_df <- restore_mc_labels(result_df)
    } else {
      # Restore column labels stripped by tidyr::unnest inside mc_compute_grouped
      result_df <- restore_mc_labels(result_df)
    }

    return(finalize_mc(result_df, is_list = FALSE))

  } else {

    result_df <- mc_one_table(dplyr::ungroup(data))
    return(finalize_mc(result_df, is_list = FALSE))

  }

}



crosstab_stacked_multiple_cols <- function(
  data, x_col, column_names, data_attrs, x_attr,
  groups, group_attrs, group_as_list, group_separator,
  group_as_hierarchy, label_group_hierarchy, calculate_per_group,
  add_percent, add_total, add_total_row, add_total_column, as_proportion, percent_by_column,
  position_total, label_total, label_total_column, label_total_row,
  name_separator, label_separator, label_na, include_na, recode_na, convert_factor,
  sort_column_names, label_as_group_name, metadata
) {

  multiplier <- get_multiplier(as_proportion)

  # Internal separator: SOH character, never appears in real data values
  ISEP <- "\x01"

  # Build ordered list of list(value, label) for one column vector.
  get_col_cats <- function(col_vec) {
    lbs <- attr(col_vec, "labels")
    if (!is.null(lbs)) {
      present <- unique(col_vec)
      if (!include_na) present <- present[!is.na(present)]
      in_order   <- lbs[lbs %in% present]
      unlabelled <- present[!(present %in% lbs) & !is.na(present)]
      cats <- c(
        lapply(seq_along(in_order), function(i)
          list(value = unname(in_order[[i]]), label = names(in_order)[[i]])),
        lapply(unlabelled, function(v) list(value = v, label = as.character(v)))
      )
      if (include_na && anyNA(unique(col_vec)))
        cats <- c(cats, list(list(value = NA, label = label_na)))
    } else if (is.factor(col_vec)) {
      cats <- lapply(levels(col_vec), function(l) list(value = l, label = l))
    } else {
      vals <- sort(unique(col_vec[!is.na(col_vec)]))
      cats <- lapply(vals, function(v) list(value = v, label = as.character(v)))
      if (include_na && anyNA(col_vec))
        cats <- c(cats, list(list(value = NA, label = label_na)))
    }
    cats
  }

  # Build category maps from the full (ungrouped) data so indices are globally consistent.
  col_cats <- stats::setNames(
    lapply(column_names, function(cn) get_col_cats(data[[cn]])),
    column_names
  )

  val_to_idx <- lapply(col_cats, function(cats) {
    keys <- vapply(cats, function(c) if (is.na(c$value)) "NA" else as.character(c$value), character(1))
    stats::setNames(seq_along(cats), keys)
  })

  val_to_lbl <- lapply(col_cats, function(cats) {
    keys <- vapply(cats, function(c) if (is.na(c$value)) "NA" else as.character(c$value), character(1))
    stats::setNames(vapply(cats, function(c) c$label, character(1)), keys)
  })

  # Translate a raw pivot-wide column name (e.g. "1\x012") to named+labelled column info.
  # Returns NULL if the name doesn't decompose into exactly length(column_names) parts.
  parse_wide_col <- function(raw_name) {
    parts <- strsplit(raw_name, ISEP, fixed = TRUE)[[1]]
    if (length(parts) != length(column_names)) return(NULL)
    indices <- vapply(seq_along(column_names), function(j) {
      key <- if (is.na(parts[j]) || parts[j] == "NA") "NA" else parts[j]
      as.character(val_to_idx[[j]][[key]] %||% parts[j])
    }, character(1))
    labels <- vapply(seq_along(column_names), function(j) {
      key <- if (is.na(parts[j]) || parts[j] == "NA") "NA" else parts[j]
      val_to_lbl[[j]][[key]] %||% parts[j]
    }, character(1))
    idx_str <- paste(indices, collapse = name_separator)
    lbl_str <- paste(labels, collapse = label_separator)
    list(
      freq_name  = paste0("frequency", name_separator, idx_str),
      freq_label = paste0("Frequency", label_separator, lbl_str),
      pct_name   = paste0(multiplier$col, name_separator, idx_str),
      pct_label  = paste0(multiplier$label, label_separator, lbl_str)
    )
  }

  # Compute the stacked wide table for one (ungrouped) data frame.
  # Rows = x categories; columns = all combinations of column_names values,
  # plus a subtotal column before each first-level category group (when >= 2 col vars).
  stacked_one_table <- function(df) {

    # N-way count then pivot wide on all column variables simultaneously
    result <- df |>
      dplyr::select(!!rlang::sym(x_col), dplyr::all_of(column_names)) |>
      dplyr::group_by(!!rlang::sym(x_col), !!!rlang::syms(column_names)) |>
      dplyr::summarise(frequency = dplyr::n(), .groups = "drop") |>
      dplyr::rename(.category = !!rlang::sym(x_col)) |>
      tidyr::pivot_wider(
        names_from  = dplyr::all_of(column_names),
        values_from = frequency,
        values_fill = 0L,
        names_sep   = ISEP,
        names_sort  = FALSE  # manual sort by index below
      )

    # Drop all-zero columns
    result   <- deselect_zero_cols(result)
    raw_cols <- setdiff(names(result), ".category")

    # NA handling for x (category) column
    if (include_na && anyNA(result$.category)) {
      result$.category <- add_missing_label(result$.category, label_na, recode_na)
    } else if (!include_na) {
      result <- dplyr::filter(result, !is.na(.category))
    }

    # Parse raw_cols into index tuples and sort by those tuples (category-defined order)
    raw_parsed <- lapply(raw_cols, function(rcn) {
      parts <- strsplit(rcn, ISEP, fixed = TRUE)[[1]]
      indices <- vapply(seq_along(column_names), function(j) {
        key <- parts[j]
        as.integer(val_to_idx[[j]][[key]] %||% .Machine$integer.max)
      }, integer(1))
      list(raw = rcn, parts = parts, indices = indices)
    })

    sort_key   <- vapply(raw_parsed, function(p)
      paste(sprintf("%09d", p$indices), collapse = "."), character(1))
    sorted_idx <- order(sort_key)
    raw_cols   <- raw_cols[sorted_idx]
    raw_parsed <- raw_parsed[sorted_idx]

    # Build subtotal info: one subtotal per unique first-level value (only for >= 2 col vars).
    # Subtotal for first-level cat i = rowSums of all leaf columns sharing that first index.
    # Subtotal labels are padded to the same depth as leaf labels (n_cols + 1 levels total)
    # by appending label_total for each missing intermediate level. This prevents xlsx
    # header merge conflicts between spanner rows and merge-down for short-depth columns.
    sub_info <- list()  # keyed by first-level raw value string
    if (length(column_names) >= 2) {
      # depth padding: leaf labels have (1 + length(column_names)) parts;
      # subtotal has 2 parts -> needs (length(column_names) - 1) extra parts
      n_pad     <- length(column_names) - 1L
      pad_suffix <- paste(rep(paste0(label_separator, label_total), n_pad), collapse = "")

      seen_first <- character(0)
      for (p in raw_parsed) {
        fv <- p$parts[1]
        if (!fv %in% seen_first) {
          seen_first <- c(seen_first, fv)
          fv_idx <- val_to_idx[[1]][[fv]] %||% fv
          fv_lbl <- val_to_lbl[[1]][[fv]] %||% fv
          sub_info[[fv]] <- list(
            first_val       = fv,
            temp_freq_name  = paste0("__sf__", fv),
            temp_pct_name   = paste0("__sp__", fv),
            final_freq_name = paste0("frequency", name_separator, fv_idx),
            final_freq_lbl  = paste0("Frequency", label_separator, fv_lbl, pad_suffix),
            final_pct_name  = paste0(multiplier$col, name_separator, fv_idx),
            final_pct_lbl   = paste0(multiplier$label, label_separator, fv_lbl, pad_suffix),
            matching_raws   = character(0)
          )
        }
        sub_info[[fv]]$matching_raws <- c(sub_info[[fv]]$matching_raws, p$raw)
      }

      # Add subtotal frequency columns
      for (si in sub_info) {
        result[[si$temp_freq_name]] <- as.integer(
          rowSums(result[, si$matching_raws, drop = FALSE], na.rm = TRUE)
        )
      }
    }

    # Row totals: sum raw leaf columns only (not subtotals) to avoid double-counting
    result$total <- as.integer(rowSums(result[, raw_cols, drop = FALSE], na.rm = TRUE))

    sub_temp_freq_names <- vapply(sub_info, function(si) si$temp_freq_name, character(1))

    # Percent helper: compute pct cols for both leaf and subtotal cols
    pct_tmps <- character(0)

    add_percent_cols <- function(res) {
      for (rcn in raw_cols) {
        info <- parse_wide_col(rcn)
        if (is.null(info)) next
        pct_val <- if (percent_by_column) {
          col_sum <- sum(res[[rcn]], na.rm = TRUE)
          if (col_sum > 0) res[[rcn]] / col_sum * multiplier$value else rep(0, nrow(res))
        } else {
          res[[rcn]] / res$total * multiplier$value
        }
        pct_val[is.nan(pct_val)] <- 0
        tmp <- paste0(".pct.", rcn)
        res[[tmp]] <- pct_val
        if (!tmp %in% pct_tmps) pct_tmps <<- c(pct_tmps, tmp)
      }
      for (si in sub_info) {
        pct_val <- if (percent_by_column) {
          col_sum <- sum(res[[si$temp_freq_name]], na.rm = TRUE)
          if (col_sum > 0) res[[si$temp_freq_name]] / col_sum * multiplier$value else rep(0, nrow(res))
        } else {
          res[[si$temp_freq_name]] / res$total * multiplier$value
        }
        pct_val[is.nan(pct_val)] <- 0
        res[[si$temp_pct_name]] <- pct_val
        if (!si$temp_pct_name %in% pct_tmps) pct_tmps <<- c(pct_tmps, si$temp_pct_name)
      }
      res
    }

    all_freq_for_total <- c(raw_cols, sub_temp_freq_names)

    if (add_percent && !percent_by_column) {
      if (add_total && add_total_row) {
        result <- coerce_category(result, .category)
        num_cols  <- c(all_freq_for_total, "total")
        total_row <- as.data.frame(
          as.list(colSums(result[, num_cols, drop = FALSE], na.rm = TRUE)),
          check.names = FALSE
        )
        total_row <- coerce_total(
          total_row, ".category", result$.category,
          label_total = label_total_row %||% label_total
        )
        total_row <- dplyr::select(total_row, .category, dplyr::everything())
        result <- if (position_total == "bottom") {
          dplyr::bind_rows(result, total_row)
        } else {
          dplyr::bind_rows(total_row, result)
        }
      }
      result <- add_percent_cols(result)
    } else if (add_percent && percent_by_column) {
      result <- add_percent_cols(result)
      if (add_total && add_total_row) {
        result <- coerce_category(result, .category)
        num_cols  <- c(all_freq_for_total, "total", pct_tmps)
        total_row <- as.data.frame(
          as.list(colSums(result[, num_cols, drop = FALSE], na.rm = TRUE)),
          check.names = FALSE
        )
        total_row <- coerce_total(
          total_row, ".category", result$.category,
          label_total = label_total_row %||% label_total
        )
        total_row <- dplyr::select(total_row, .category, dplyr::everything())
        result <- if (position_total == "bottom") {
          dplyr::bind_rows(result, total_row)
        } else {
          dplyr::bind_rows(total_row, result)
        }
      }
    } else {
      if (add_total && add_total_row) {
        result <- coerce_category(result, .category)
        num_cols  <- c(all_freq_for_total, "total")
        total_row <- as.data.frame(
          as.list(colSums(result[, num_cols, drop = FALSE], na.rm = TRUE)),
          check.names = FALSE
        )
        total_row <- coerce_total(
          total_row, ".category", result$.category,
          label_total = label_total_row %||% label_total
        )
        total_row <- dplyr::select(total_row, .category, dplyr::everything())
        result <- if (position_total == "bottom") {
          dplyr::bind_rows(result, total_row)
        } else {
          dplyr::bind_rows(total_row, result)
        }
      }
    }

    # Remove total column / total row if not requested
    if (!add_total || !add_total_column) {
      result <- dplyr::select(result, -dplyr::any_of("total"))
    }
    if (!add_total || !add_total_row) {
      result <- dplyr::filter(
        result,
        as.character(.category) != (label_total_row %||% label_total)
      )
    }

    if (convert_factor) {
      result <- dplyr::mutate(
        result,
        dplyr::across(dplyr::where(haven::is.labelled), haven::as_factor)
      )
    }

    # Build final column order and rename.
    # Order: category, [total (if !pct_by_col)],
    #   ALL frequency columns (all groups): subtotal_freq_g, leaf_freq_g_1, leaf_freq_g_2, ...
    #   ALL percent  columns (all groups): subtotal_pct_g,  leaf_pct_g_1,  leaf_pct_g_2, ...
    #   [total (if pct_by_col)]
    # This ensures the top-level xlsx header spanner groups all "Frequency" columns
    # together before all "Percent/Proportion" columns.
    new_order <- ".category"
    if (add_total && add_total_column && !percent_by_column) {
      new_order <- c(new_order, "total")
    }

    if (length(column_names) >= 2) {
      seen_first <- unique(vapply(raw_parsed, function(p) p$parts[1], character(1)))

      # --- Pass 1: ALL frequency columns across all first-level groups ---
      for (fv in seen_first) {
        si <- sub_info[[fv]]
        if (!is.null(si) && si$temp_freq_name %in% names(result)) {
          result <- dplyr::rename(result, !!si$final_freq_name := !!rlang::sym(si$temp_freq_name))
          attr(result[[si$final_freq_name]], "label") <- si$final_freq_lbl
          new_order <- c(new_order, si$final_freq_name)
        }
        for (p in raw_parsed) {
          if (p$parts[1] != fv) next
          rcn  <- p$raw
          info <- parse_wide_col(rcn)
          if (is.null(info) || !rcn %in% names(result)) next
          result <- dplyr::rename(result, !!info$freq_name := !!rlang::sym(rcn))
          attr(result[[info$freq_name]], "label") <- info$freq_label
          new_order <- c(new_order, info$freq_name)
        }
      }

      # --- Pass 2: ALL percent columns across all first-level groups ---
      if (add_percent) {
        for (fv in seen_first) {
          si <- sub_info[[fv]]
          if (!is.null(si) && si$temp_pct_name %in% names(result)) {
            result <- dplyr::rename(result, !!si$final_pct_name := !!rlang::sym(si$temp_pct_name))
            attr(result[[si$final_pct_name]], "label") <- si$final_pct_lbl
            new_order <- c(new_order, si$final_pct_name)
          }
          for (p in raw_parsed) {
            if (p$parts[1] != fv) next
            rcn  <- p$raw
            info <- parse_wide_col(rcn)
            if (is.null(info)) next
            tmp  <- paste0(".pct.", rcn)
            if (!tmp %in% names(result)) next
            result <- dplyr::rename(result, !!info$pct_name := !!rlang::sym(tmp))
            attr(result[[info$pct_name]], "label") <- info$pct_label
            new_order <- c(new_order, info$pct_name)
          }
        }
      }
    } else {
      # Single column variable — no subtotals; all freqs then all pcts (sorted order)
      for (p in raw_parsed) {
        rcn  <- p$raw
        info <- parse_wide_col(rcn)
        if (is.null(info) || !rcn %in% names(result)) next
        result <- dplyr::rename(result, !!info$freq_name := !!rlang::sym(rcn))
        attr(result[[info$freq_name]], "label") <- info$freq_label
        new_order <- c(new_order, info$freq_name)
      }
      if (add_percent) {
        for (p in raw_parsed) {
          rcn  <- p$raw
          info <- parse_wide_col(rcn)
          if (is.null(info)) next
          tmp  <- paste0(".pct.", rcn)
          if (!tmp %in% names(result)) next
          result <- dplyr::rename(result, !!info$pct_name := !!rlang::sym(tmp))
          attr(result[[info$pct_name]], "label") <- info$pct_label
          new_order <- c(new_order, info$pct_name)
        }
      }
    }

    if (add_total && add_total_column && percent_by_column) {
      new_order <- c(new_order, "total")
    }

    result <- dplyr::select(result, dplyr::any_of(new_order))

    attr(result$.category, "label") <- x_attr$label
    if ("total" %in% names(result)) {
      attr(result$total, "label") <- label_total_column %||% label_total
    }

    dplyr::rename(result, category = .category)
  }

  finalize_stacked <- function(result, is_list = FALSE) {
    attr(result, "label_separator")       <- label_separator
    attr(result, "name_separator")        <- name_separator
    attr(result, "multiple_columns")      <- TRUE
    attr(result, "multiple_columns_type") <- "stacked"
    if (is_list && length(groups) > 0) attr(result, "groups") <- groups
    for (meta in names(metadata)) attr(result, meta) <- metadata[[meta]]
    class(result) <- c("tsg", "tsgc", class(result))
    result
  }

  # --- group_as_list path ---
  if (group_as_list && length(groups) > 0) {

    stacked_nested_fn <- function(sub_data) {
      result_j  <- stacked_one_table(dplyr::ungroup(sub_data))
      group_row <- dplyr::select(dplyr::ungroup(sub_data), dplyr::all_of(groups))[1, , drop = FALSE]
      group_df  <- group_row[rep(1, nrow(result_j)), , drop = FALSE]
      rownames(group_df) <- NULL
      dplyr::bind_cols(group_df, result_j) |>
        set_group_attrs(groups, group_attrs, resolve = FALSE)
    }

    if (group_as_hierarchy) {

      result_list <- build_nested_group_list(
        raw_data    = dplyr::ungroup(data),
        groups_rem  = groups,
        group_attrs = group_attrs,
        add_totals  = TRUE,
        total_label = label_group_hierarchy,
        compute_fn  = stacked_nested_fn
      )
      return(finalize_stacked(result_list, is_list = TRUE))

    } else if (length(groups) > 1) {

      result_list <- build_nested_group_list(
        raw_data    = dplyr::ungroup(data),
        groups_rem  = groups,
        group_attrs = group_attrs,
        add_totals  = FALSE,
        total_label = "",
        compute_fn  = stacked_nested_fn
      )
      return(finalize_stacked(result_list, is_list = TRUE))

    } else {

      glue_arg <- paste0("{haven::as_factor(", groups, ")}")

      df_groups <- dplyr::ungroup(data) |>
        dplyr::select(dplyr::any_of(groups)) |>
        dplyr::distinct(.keep_all = TRUE) |>
        dplyr::mutate(list_group = glue::glue(glue_arg))

      result_list <- list()
      for (j in seq_along(df_groups$list_group)) {
        list_group_j <- df_groups$list_group[j]
        data_subset  <- dplyr::filter(dplyr::ungroup(data), glue::glue(glue_arg) == list_group_j)
        result_j     <- stacked_one_table(data_subset)
        group_df_j   <- df_groups[rep(j, nrow(result_j)), groups, drop = FALSE]
        rownames(group_df_j) <- NULL
        result_list[[list_group_j]] <- dplyr::bind_cols(group_df_j, result_j) |>
          set_group_attrs(groups, group_attrs, resolve = FALSE)
      }
      return(finalize_stacked(result_list, is_list = TRUE))
    }

  # --- calculate_per_group path ---
  } else if (calculate_per_group && length(groups) > 0) {

    stacked_compute_grouped <- function(data_subset) {
      data_subset |>
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
        tidyr::nest(data = -dplyr::all_of(groups)) |>
        dplyr::mutate(data = purrr::map(data, stacked_one_table)) |>
        tidyr::unnest(cols = c(data), keep_empty = TRUE) |>
        dplyr::ungroup() |>
        dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::starts_with(c(
              "frequency",
              paste0(multiplier$col, name_separator)
            )),
            ~ replace(., is.na(.), 0)
          )
        )
    }

    result_df <- stacked_compute_grouped(data)

    if (group_as_hierarchy) {

      grand_total_df <- stacked_compute_grouped(
        coerce_groups_total(data, groups, label_group_hierarchy)
      )

      subtotals <- list()
      if (length(groups) >= 2) {
        for (k in seq_len(length(groups) - 1)) {
          parent_groups <- groups[seq_len(k)]
          child_groups  <- groups[(k + 1):length(groups)]
          combos <- dplyr::distinct(
            dplyr::select(dplyr::ungroup(data), dplyr::all_of(parent_groups))
          )
          parts <- vector("list", nrow(combos))
          for (i in seq_len(nrow(combos))) {
            data_k <- dplyr::ungroup(data)
            for (pg in parent_groups) {
              pv     <- combos[[pg]][i]
              data_k <- dplyr::filter(data_k, !!rlang::sym(pg) == pv)
            }
            for (cg in child_groups) {
              data_k <- coerce_total(
                data_k, cg, data_k[[cg]],
                .resolve_hierarchy_label(label_group_hierarchy, cg),
                default_code = -1L
              )
            }
            parts[[i]] <- stacked_compute_grouped(data_k)
          }
          subtotals[[k]] <- dplyr::bind_rows(parts)
        }
      }

      result_df <- assemble_group_hierarchy(grand_total_df, subtotals, result_df, groups)
    }

    return(finalize_stacked(result_df, is_list = FALSE))

  # --- simple (no groups) path ---
  } else {

    result_df <- stacked_one_table(dplyr::ungroup(data))
    return(finalize_stacked(result_df, is_list = FALSE))

  }

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
  groups = NULL,
  apply_labels = TRUE,
  deselect_zero = TRUE
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
      values_fill = 0L,
      names_prefix = col_prefix,
      names_expand = TRUE,
      names_sort = sort_column_names
    ) |>
    dplyr::select(
      dplyr::any_of(groups),
      dplyr::everything()
    )

  if(deselect_zero) {
    data <- deselect_zero_cols(data)
  }

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

  data <- data |>
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
    )

  if(apply_labels) {
    data <- add_column_label(
      data,
      x = '.category',
      x_attr = x_attr,
      column_name = column_name,
      data_attr = data_attr,
      multiplier_col = multiplier$col,
      name_separator = name_separator,
      label_separator = label_separator,
      label_na = label_na,
      prefixed = add_percent
    )
  }

  dplyr::rename(data, category = .category)

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
  freq_prefix <- paste0("frequency", name_separator)
  mult_prefix <- paste0(multiplier_col, name_separator)

  tab_labels <- names(data)
  if(!is.null(excluded)) {
    tab_labels <- tab_labels[!(tab_labels %in% excluded)]
  }

  if(prefixed) {
    tab_labels <- tab_labels[startsWith(tab_labels, freq_prefix) | startsWith(tab_labels, mult_prefix)]
  }

  for(tab_label in tab_labels) {

    prefix_label <- ""
    if(prefixed) {
      if(startsWith(tab_label, freq_prefix)) {
        prefix_label <- "frequency"
        new_label <- substr(tab_label, nchar(freq_prefix) + 1L, nchar(tab_label))
      } else {
        prefix_label <- multiplier_col
        new_label <- substr(tab_label, nchar(mult_prefix) + 1L, nchar(tab_label))
      }
    } else {
      new_label <- tab_label
    }

    if(new_label == "NA") new_label <- label_na

    value <- new_label

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
