#' Generate frequency table
#'
#' Creates frequency tables for one or more categorical variables, optionally grouped by other variables.
#' The function supports various enhancements such as sorting, totals, percentages, cumulative statistics,
#' handling of missing values, and label customization. It returns a single table or a list of frequency tables.
#'
#' @param data A data frame (typically \code{tibble}) containing the variables to summarize.
#' @param ... One or more unquoted variable names (passed via tidy evaluation) for which to compute frequency tables.
#' @param sort_value Logical. If \code{TRUE}, frequency values will be sorted.
#' @param sort_desc Logical. If \code{TRUE}, sorts in descending order of frequency. If \code{sort_value = FALSE}, the category is sorted in ascending order.
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
#' @param group_grand_total `r lifecycle::badge("experimental")` Logical. Compute grand total based on the grouping variable.
#' @param group_grand_total_label `r lifecycle::badge("experimental")` Character. Apply label to the grand total if \code{group_grand_total} is set to \code{TRUE}.
#' @param label_stub Optional character vector used for labeling output tables (e.g., for export or display).
#' @param label_na Character. Label to use for missing (\code{NA}) values.
#' @param label_total Character. Label used for the total row/category.
#' @param label_as_group_name Logical. If \code{TRUE}, uses variable labels as names in the output list; otherwise, uses variable names.
#' @param expand_categories Logical. If \code{TRUE}, ensures all categories (including those with zero counts) are included in the output.
#' @param top_n Integer or \code{NULL}. If specified, limits the output to the top \code{n} categories by frequency.
#' @param top_n_only Logical. If \code{TRUE} and \code{top_n} is specified, only the top \code{n} categories are included, excluding others.
#' @param collapse_list Logical. If \code{TRUE} and \code{group_as_list = TRUE}, collapses the list of frequency tables into a single data frame with group identifiers. See also [collapse_list()].
#' @param metadata A named list with optional metadata to attach as attributes, e.g. \code{title}, \code{subtitle}, and \code{source_note}.
#' @param convert_factor Logical. If \code{TRUE}, converts labelled variables to factors in the output. See also [convert_factor()].
#'
#' @return A frequency table (\code{tibble}, possibly nested) or a list of such tables. Additional attributes such as labels, metadata, and grouping information may be attached. The returned object is of class \code{"tsg"}.
#'
#' @export
#'
#' @seealso [generate_crosstab()], [generate_output()], [rename_label()], [remove_label()]
#'
#' @examples
#' # Using built-in dataset `person_record`
#'
#'
#' # Basic usage
#' person_record |>
#'  generate_frequency(sex)
#'
#' # Multiple variables
#' person_record |>
#'   generate_frequency(sex, age, marital_status)
#'
#' # Grouping
#' person_record |>
#'   dplyr::group_by(sex) |>
#'   generate_frequency(marital_status)
#'
#' # Output group as list
#' person_record |>
#'   dplyr::group_by(sex) |>
#'   generate_frequency(marital_status, group_as_list = TRUE)
#'
#' # Sorting
#'
#' # default is TRUE
#' person_record |>
#'   generate_frequency(age, sort_value = TRUE)
#'
#' # If FALSE, the output will be sorted by the variable values in ascending order.
#' person_record |>
#'   generate_frequency(age, sort_value = FALSE)
#'
#' # Vignettes for more examples.


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
  as_proportion = FALSE,
  include_na = TRUE,
  recode_na = "auto",
  position_total = c("bottom", "top"),
  calculate_per_group = TRUE,
  group_separator = " - ",
  group_as_list = FALSE,
  group_grand_total = FALSE,
  group_grand_total_label = "All",
  label_as_group_name = TRUE,
  label_stub = NULL,
  label_na = "Not reported",
  label_total = "Total",
  expand_categories = TRUE,
  convert_factor = FALSE,
  collapse_list = FALSE,
  top_n = NULL,
  top_n_only = FALSE,
  metadata = NULL
) {

  is_valid_input_data(data)

  n_args <- rlang::dots_n(...)
  df <- list()

  groups <- dplyr::group_vars(data)

  if(n_args > 0) {
    data <- dplyr::select(data, dplyr::any_of(groups), ...)
  }
  group_attrs <- get_group_attrs(data, groups)

  column_names <- names(data)
  label_stubs <- get_label_stubs(column_names, label_stub)

  if(length(groups) > 0) {
    column_names <- column_names[!(column_names %in% groups)]
  }

  for(column_name in column_names) {

    label <- attributes(data[[column_name]])$label
    if(is.null(label)) { label <- column_name }
    if(length(label) != 1) { label <- column_name }

    list_name <- label
    if(!label_as_group_name) { list_name <- column_name }

    categories <- unique(data[[column_name]])

    if(!is.null(sort_except) & sort_value) {
      sort_value <- !(column_name %in% sort_except)
    } else if(!is.null(sort_except) & !sort_value) {
      sort_value <- column_name %in% sort_except
    }

    data_i <- data |>
      tsg_get_frequency(column_name, include_na) |>
      tsg_sort_col_value(
        sort_value = sort_value,
        sort_desc = sort_desc,
        groups = groups
      )

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
          generate_frequency(
            !!as.name(column_name),
            sort_value = sort_value,
            sort_desc = sort_desc,
            sort_except = sort_except,
            add_total = add_total,
            add_percent = add_percent,
            add_cumulative = add_cumulative,
            add_cumulative_percent = add_cumulative_percent,
            as_proportion = as_proportion,
            include_na = include_na,
            recode_na = recode_na,
            position_total = position_total,
            calculate_per_group = calculate_per_group,
            group_separator = group_separator,
            group_as_list = FALSE,
            label_as_group_name = label_as_group_name,
            label_stub = label_stub,
            label_na = label_na,
            label_total = label_total,
            expand_categories = expand_categories,
            convert_factor = convert_factor,
            collapse_list = collapse_list,
            top_n = top_n,
            top_n_only = top_n_only,
            metadata = metadata
          )
      }

      for(j in seq_along(df_groups$list_group)) {

        list_group_j <- df_groups$list_group[j]

        data_j <- data_i |>
          dplyr::filter(glue::glue(glue_arg) == list_group_j) |>
          expand_category_values(
            categories,
            df_groups[j, groups],
            expand = expand_categories
          ) |>
          add_column_values(
            as_proportion = as_proportion,
            add_percent = add_percent,
            add_cumulative = add_cumulative,
            add_cumulative_percent = add_cumulative_percent
          ) |>
          tsg_add_row_total(
            x = .category,
            add_total = add_total,
            position = position_total[1],
            label_total = label_total,
            groups = groups,
            add_cumulative = add_cumulative,
            add_cumulative_percent = add_cumulative_percent
          ) |>
          tsg_sort_top_n(
            top_n = top_n,
            top_n_only = top_n_only,
            sort_value = sort_value,
            add_total = add_total,
            add_percent = add_percent,
            position_total = position_total[1],
            as_proportion = as_proportion
          ) |>
          set_data_attrs(
            column_name,
            label,
            as_proportion
          ) |>
          set_group_attrs(groups, group_attrs)

        # if with missing .category
        if(include_na & length(data_j$.category[is.na(data_j$.category)]) > 0) {

          data_j$.category <- add_missing_label(
            value = data_j$.category,
            label_na = label_na,
            recode_na = recode_na
          )

        }

        if(convert_factor) {
          data_j <- dplyr::mutate_if(data_j, haven::is.labelled, haven::as_factor)
        }

        data_ij[[list_group_j]] <- dplyr::rename(data_j, category = .category)

      }

      data_i <- data_ij


    } else {

      if(calculate_per_group & length(groups) > 0) {

        data_i <- data_i |>
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
          tidyr::nest(data = -dplyr::all_of(groups)) |>
          dplyr::mutate(data = purrr::map(data, function(x) {
            x |>
              expand_category_values(categories, expand = expand_categories) |>
              add_column_values(
                as_proportion = as_proportion,
                add_percent = add_percent,
                add_cumulative = add_cumulative,
                add_cumulative_percent = add_cumulative_percent
              ) |>
              tsg_add_row_total(
                x = .category,
                add_total = add_total,
                position = position_total[1],
                label_total = label_total,
                add_cumulative = add_cumulative,
                add_cumulative_percent = add_cumulative_percent
              ) |>
              tsg_sort_top_n(
                top_n = top_n,
                top_n_only = top_n_only,
                sort_value = sort_value,
                add_total = add_total,
                add_percent = add_percent,
                position_total = position_total[1],
                as_proportion = as_proportion
              )
          })) |>
          tidyr::unnest(cols = c(data)) |>
          dplyr::ungroup()


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

            if(is.numeric(data_i[[g]])) {
              data_i <- dplyr::mutate(data_i, !!as.name(g) := as.character(!!as.name(g)))
            }

          }

          data_i <- dplyr::bind_rows(
            data_g |>
              dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
              tsg_get_frequency(column_name, include_na) |>
              tsg_sort_col_value(
                sort_value = sort_value,
                sort_desc = sort_desc,
                groups = groups
              ) |>
              tidyr::nest(data = -dplyr::all_of(groups)) |>
              dplyr::mutate(data = purrr::map(data, function(x) {
                x |>
                  expand_category_values(categories, expand = expand_categories) |>
                  add_column_values(
                    as_proportion = as_proportion,
                    add_percent = add_percent,
                    add_cumulative = add_cumulative,
                    add_cumulative_percent = add_cumulative_percent
                  ) |>
                  tsg_add_row_total(
                    x = .category,
                    add_total = add_total,
                    position = position_total[1],
                    label_total = label_total,
                    add_cumulative = add_cumulative,
                    add_cumulative_percent = add_cumulative_percent
                  ) |>
                  tsg_sort_top_n(
                    top_n = top_n,
                    top_n_only = top_n_only,
                    sort_value = sort_value,
                    add_total = add_total,
                    add_percent = add_percent,
                    position_total = position_total[1],
                    as_proportion = as_proportion
                  )
              })) |>
              tidyr::unnest(cols = c(data)) |>
              dplyr::ungroup(),
            data_i
          )


        }


      } else {

        data_i <- data_i |>
          add_column_values(
            as_proportion = as_proportion,
            add_percent = add_percent,
            add_cumulative = add_cumulative,
            add_cumulative_percent = add_cumulative_percent
          ) |>
          tsg_add_row_total(
            x = .category,
            add_total = add_total,
            position = position_total[1],
            label_total = label_total,
            add_cumulative = add_cumulative,
            add_cumulative_percent = add_cumulative_percent
          ) |>
          tsg_sort_top_n(
            top_n = top_n,
            top_n_only = top_n_only,
            sort_value = sort_value,
            add_total = add_total,
            add_percent = add_percent,
            position_total = position_total[1],
            as_proportion = as_proportion
          ) |>
          set_group_attrs(groups, group_attrs)
      }

      data_i <- set_data_attrs(data_i, column_name, label, as_proportion)

      # if with missing .category
      if(include_na & length(data_i$.category[is.na(data_i$.category)]) > 0) {

        data_i$.category <- add_missing_label(
          value = data_i$.category,
          label_na = label_na,
          recode_na = recode_na
        )

      }

      if(convert_factor) {
        data_i <- dplyr::mutate_if(data_i, haven::is.labelled, haven::as_factor)
      }

    }

    if(length(label_stubs) > 0) {
      attr(data_i, "label_xlsx") <- label_stubs[[which(column_names == column_name)]]
    }

    if(group_as_list & inherits(data_i, "list")) {

      df[[list_name]] <- data_i

    } else {

      data_i <- resolve_group_col(data_i, "category" %in% groups)

      if(length(groups) > 0) {
        attr(data_i, "label_total") <- label_total
        attr(data_i, "groups") <- groups
      }

      df[[list_name]] <- dplyr::rename(data_i, category = .category)

    }

  }

  if(length(df) == 1) {
    df <- df[[1]]
  } else if (length(df) > 1 & collapse_list) {
    df <- collapse_list(
      data = df,
      as_proportion = as_proportion
    )
  }

  if(group_as_list & length(groups) > 0) {
    attr(df, "groups") <- groups
    attr(df, "group_attrs") <- group_attrs
  }

  for(meta in names(metadata)) {
    attr(df, meta) <- metadata[[meta]]
  }

  class(df) <- unique(c("tsg", "tsgf", class(df)))

  return(df)

}


