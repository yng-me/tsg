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
  pivot_wider = TRUE,
  name_separator = "_",
  label_separator = "__",
  label_total = "Total",
  label_total_column = NULL,
  label_total_row = NULL,
  label_na = "Not reported",
  label_as_group_name = TRUE,
  group_separator = " - ",
  group_as_list = FALSE,
  calculate_per_group = TRUE,
  expand_categories = TRUE,
  position_total = "bottom",
  metadata = list(
    title = NULL,
    subtitle = NULL,
    source_note = NULL,
    footnotes = NULL
  )
) {

  n_args <- rlang::dots_n(...)
  if(n_args == 0) {
    return(generate_frequency(data, {{x}}))
  }

  data <- dplyr::select(data, dplyr::group_cols(), {{x}}, ...)

  groups <- dplyr::group_vars(data)
  group_attrs <- get_group_attrs(data, groups)

  data_attrs <- get_data_attrs(data)

  column_names <- names(dplyr::select(dplyr::ungroup(data), ...))

  df_list <- list()
  categories <- unique(data[[rlang::as_label(enquo(x))]])

  for(column_name in column_names) {

    list_name <- column_name
    if(label_as_group_name) {
      list_name <- attributes(data[[column_name]])$label
      if(is.null(list_name)) { list_name <- column_name }
    }

    data_i <- tsg_get_crosstab(data, {{x}}, column_name)

    if(group_as_list & length(groups) > 0) {

      glue_arg <- paste0(
        paste0("{haven::as_factor(", groups, ")}"),
        collapse = group_separator
      )

      df_groups <- data_i |>
        dplyr::select(dplyr::any_of(groups)) |>
        dplyr::distinct(.keep_all = T) |>
        dplyr::mutate(list_group = glue::glue(glue_arg))

      data_ij <- list()

      for(j in seq_along(df_groups$list_group)) {

        list_group_j <- df_groups$list_group[j]

        data_ij[[list_group_j]]  <- data_i |>
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
            add_total = add_total,
            add_total_row = add_total_row,
            add_total_column = add_total_column,
            as_proportion = as_proportion,
            percent_by_column = percent_by_column,
            position_total = position_total,
            label_total = label_total_row %||% label_total,
            name_separator = name_separator,
            label_separator = label_separator,
            label_na = label_na
          ) |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything()) |>
          add_total_label(
            label = label_total_column %||% label_total,
            label_separator = label_separator,
            name_separator = name_separator,
            percent_by_column = percent_by_column
          )

      }

      data_i <- data_ij


    } else {

      if(calculate_per_group & length(groups) > 0) {

        multiplier <- get_multiplier(as_proportion)

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
                add_total = add_total,
                add_total_row = add_total_row,
                add_total_column = add_total_column,
                as_proportion = as_proportion,
                percent_by_column = percent_by_column,
                position_total = position_total,
                label_total = label_total_row %||% label_total,
                name_separator = name_separator,
                label_separator = label_separator,
                label_na = label_na
              )
          })) |>
          tidyr::unnest(cols = c(data)) |>
          dplyr::ungroup() |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything())

      } else {

        data_i <- data_i |>
          tsg_pivot_table(
            column_name,
            data_attr = data_attrs[[column_name]],
            add_total = add_total,
            add_total_row = add_total_row,
            add_total_column = add_total_column,
            as_proportion = as_proportion,
            percent_by_column = percent_by_column,
            position_total = position_total,
            label_total = label_total_row %||% label_total,
            name_separator = name_separator,
            label_separator = label_separator,
            label_na = label_na
          ) |>
          dplyr::select(dplyr::any_of(groups), dplyr::everything())
      }

      data_i <- add_total_label(
        data_i,
        label = label_total_column %||% label_total,
        label_separator = label_separator,
        name_separator = name_separator,
        percent_by_column = percent_by_column
      )

    }

    df_list[[list_name]] <- data_i

  }

  if(length(df_list) == 1) { df_list <- df_list[[1]] }

  if(group_as_list & length(groups) > 0) {
    attr(df_list, "groups") <- groups
  }

  for(meta in names(metadata)) {
    attr(df_list, meta) <- metadata[[meta]]
  }

  return(df_list)

}



tsg_pivot_table <- function(
  data,
  column_name,
  add_total,
  add_total_row,
  add_total_column,
  data_attr,
  as_proportion,
  position_total,
  label_total,
  label_separator,
  name_separator,
  label_na,
  percent_by_column = FALSE,
  groups = NULL
) {

  multiplier <- get_multiplier(as_proportion)

  col_prefix <- glue::glue("frequency{name_separator}")
  col_prefix_p <- glue::glue("{multiplier$col}{name_separator}")


  data <- data |>
    tidyr::pivot_wider(
      names_from = !!as.name(column_name),
      values_from = frequency,
      values_fill = 0,
      names_prefix = col_prefix
    ) |>
    dplyr::select(
      dplyr::any_of(groups),
      dplyr::where(~ !all(. == 0))
    )

  total_col <- "total"
  if(percent_by_column) { total_col <- glue::glue("{col_prefix}total") }

  data[[total_col]] <- rowSums(data[ , grepl(glue::glue("^{col_prefix}"), names(data))], na.rm = TRUE)

  if(!percent_by_column) {
    data <- add_row_total(
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

  if(percent_by_column) {
    data <- add_row_total(
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
    ) |>
    add_column_label(
      column_name = column_name,
      data_attr = data_attr,
      multiplier = multiplier$col,
      name_separator = name_separator,
      label_separator = label_separator,
      label_na = label_na
    ) |>
    dplyr::rename(category = .category)

}



add_column_label <- function(
  data,
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













