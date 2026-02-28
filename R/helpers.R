get_na_value <- function(value, recode_na = "auto", pattern = "^8", offset = 1) {

  if(recode_na != "auto") return(recode_na)

  max_value <- as.integer(max(as.integer(value), na.rm = TRUE))

  attr_value <- attributes(value)$labels
  max_value_attr <- as.integer(max(as.integer(attr_value), na.rm = TRUE))

  if(grepl(pattern, max_value) | grepl(pattern, max_value_attr)) {
    as.integer(paste0(rep(9, 1 + nchar(max_value)), collapse = "")) - offset
  } else {
    as.integer(paste0(rep(9, nchar(max_value)), collapse = "")) - offset
  }
}

add_missing_label <- function(value, label_na = "Not reported", recode_na = "auto", pattern = "^8") {

  if(haven::is.labelled(value)) {

    if(inherits(value, 'character')) {
      value_na <- '__NA__'
    } else if (inherits(value, 'integer') | is.numeric(value)) {
      value_na <- get_na_value(value, recode_na, pattern)
    }

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

expand_category_values <- function(data, categories, ..., expand = TRUE) {

  if(!expand) { return(data) }

  tidyr::complete(
    data = data,
    .category = categories,
    ...,
    fill = list(frequency = 0L)
  )

}

add_column_values <- function(
  data,
  as_proportion = FALSE,
  add_percent = TRUE,
  add_cumulative = FALSE,
  add_cumulative_percent = FALSE
) {

  m <- get_multiplier(as_proportion)

  if(add_percent) {
    data[[m$col]] <- m$value * (data$frequency / sum(data$frequency, na.rm = TRUE))
  }

  if(add_cumulative) {
    data$cumulative <- cumsum(data$frequency)
  }

  if(add_cumulative_percent & add_percent) {
    data[[m$cumulative_col]] <- cumsum(data[[m$col]])
  }

  data

}


tsg_add_row_total <- function(
  data,
  x,
  add_total = TRUE,
  position = "bottom",
  label_total = "Total",
  groups = NULL,
  add_cumulative = FALSE,
  add_cumulative_percent = FALSE
) {

  if(!add_total) { return(data) }

  attr(data, "category") <- names(dplyr::select(data, {{x}}))
  attr(data, "groups") <- groups

  data <- add_row_total(
    data = data,
    position = position,
    label_total = label_total
  )

  if("cumulative_percent" %in% names(data)) {
    data$cumulative_percent[[nrow(data)]] <- NA_real_
  }

  if("cumulative_proportion" %in% names(data)) {
    data$cumulative_proportion[[nrow(data)]] <- NA_real_
  }

  if("cumulative" %in% names(data)) {
    data$cumulative[[nrow(data)]] <- NA_integer_
  }

  data

}


coerce_total <- function(data, col, x, label_total = "Total", value = NULL, default_code = 0L) {

  if(!haven::is.labelled(x) & !is.factor(x)) {
    data[[col]] <- label_total
    return(data)
  }


  if(inherits(x, 'character')) {
    .value <- paste0("__", label_total, "__")
  } else {
    .value <- 0L
    if (inherits(x, 'integer') | is.numeric(x)) {
      if(!is.null(value)) {
        .value <- value - default_code
      } else {
        if(min(as.integer(x), na.rm = TRUE) == 0) { .value <- -1L - default_code }
      }
    }
  }

  data[[col]] <- .value

  if(haven::is.labelled(x)) {

    data[[col]] <- haven::labelled(
      data[[col]],
      labels = stats::setNames(.value, label_total)
    )

  } else if(is.factor(x)) {

    data[[col]] <- factor(
      data[[col]],
      levels = .value,
      labels = label_total
    )
  }

  data

}


coerce_category <- function(data, x) {

  col <- names(dplyr::select(data, {{x}}))
  is_labelled <- haven::is.labelled(data[[col]])
  is_factor <- is.factor(data[[col]])
  is_char <- is.character(data[[col]])

  if(!is_labelled & !is_factor & !is_char) {
    data <- dplyr::mutate(data, {{x}} := as.character({{x}}))
  }

  data

}


get_group_attrs <- function(data, groups) {
  stats::setNames(
    lapply(groups, \(x) attributes(data[[x]])),
    groups
  )
}

get_data_attrs <- function(data) {

  data_attrs <- list()

  for(i in names(data)) {

    attr_i <- attributes(data[[i]])
    label <- attr_i$label

    if(is.null(label)) label <- i

    data_attrs[[i]] <- list(
      value = i,
      label = label,
      type = typeof(data[[i]]),
      labels = attr_i$labels
    )
  }

  data_attrs

}


get_multiplier <- function(as_proportion, key = NULL) {

  multiplier <- 100
  multiplier_label <- "Percent"
  if(as_proportion) {
    multiplier <- 1
    multiplier_label <- "Proportion"
  }

  multiplier_col <- tolower(multiplier_label)
  cumulative_col <- glue::glue("cumulative_{multiplier_col}")
  cumulative_label <- glue::glue("Cumulative {multiplier_col}")

  values <- list(
    value = multiplier,
    col = multiplier_col,
    label = multiplier_label,
    cumulative_col = cumulative_col,
    cumulative_label = cumulative_label
  )

  if(!is.null(key)) {
    if(!key %in% names(values)) {
      stop(glue::glue("Key '{key}' not found in values"))
    }
    return(values[[key]])
  } else {
    return(values)
  }
}


get_label_stubs <- function(column_names, label_stub) {
  label_stubs <- NULL
  if(length(label_stub) > 0) {
    stub_eq <- length(column_names) == length(label_stub)
    if(length(label_stub) == 1 | (length(label_stub) > 1 & !stub_eq)) {
      label_stubs <- rep(label_stub, length(column_names))
    } else if (length(label_stub) > 1 & stub_eq) {
      label_stubs <- label_stub
    }
  }

  label_stubs
}


set_data_attrs <- function(data, column_name, label, as_proportion = FALSE) {

  m <- get_multiplier(as_proportion)

  attr(data$frequency, "label") <- "Frequency"

  if(m$col %in% names(data)) {
    attr(data[[m$col]], "label") <- m$label
  }

  if("cumulative" %in% names(data)) {
    attr(data$cumulative, "label") <- "Cumulative frequency"
  }

  if(m$cumulative_col %in% names(data)) {
    attr(data[[m$cumulative_col]], "label") <- m$cumulative_label
  }

  attr(data$.category, "label") <- label

  data

}


set_group_attrs <- function(data, groups, group_attrs, resolve = TRUE) {

  attr_names <- names(group_attrs)

  for(k in seq_along(attr_names)) {

    attr_k <- attr_names[k]
    attr_label <- group_attrs[[attr_k]]$label
    attr_labels <- group_attrs[[attr_k]]$labels

    if(!is.null(attr_labels)) {

      data[[attr_k]] <- haven::labelled(
        x = data[[attr_k]],
        label = group_attrs[[attr_k]]$label,
        labels = group_attrs[[attr_k]]$labels
      )

    } else if (!is.null(attr_label)) {
      attr(data[[attr_k]], "label") <- attr_label
    } else {
      attr(data[[attr_k]], "label") <- attr_k
    }
  }

  attr(data, "groups") <- groups
  attr(data, "group_attrs") <- group_attrs

  if(resolve) {
    data |>
      resolve_group_col() |>
      dplyr::select(dplyr::any_of(groups), dplyr::everything())
  } else {
    dplyr::select(data, dplyr::any_of(groups), dplyr::everything())
  }
}


resolve_group_col <- function(data, condition = TRUE) {

  if("category" %in% names(data) & condition) {
    colnames(data)[which(colnames(data) == "category")] <- "category_group"
    message("WARNING: `category` is a reserved column name and has been renamed to `category_group`")
  }

  data
}

tsg_sort_top_n <- function(
  data,
  top_n = NULL,
  top_n_only = FALSE,
  sort_value = TRUE,
  add_total = TRUE,
  add_percent = TRUE,
  position_total = "bottom",
  as_proportion = FALSE
) {

  multiplier <- get_multiplier(as_proportion)

  if(is.null(top_n) | !sort_value) { return(data) }
  if(nrow(data) <= top_n + 1 + add_total) { return(data) }

  if(position_total == "bottom") {
    total <- utils::tail(data, add_total)
    data_top_n <- dplyr::bind_rows(
      dplyr::slice_head(data, n = top_n),
      utils::tail(data, add_total)
    )
  } else {
    total <- utils::head(data, add_total)
    data_top_n <- dplyr::slice_head(data, n = top_n + add_total)
  }

  if(!top_n_only) {

    if(nrow(total) == 0) {
      total_frequency <- sum(data$frequency, na.rm = TRUE)
    } else {
      total_frequency <- total$frequency
    }

    if(add_percent) {

      data_others <- dplyr::summarise(
        dplyr::anti_join(data, data_top_n, by = ".category"),
        frequency = sum(frequency, na.rm = TRUE),
        !!as.name(multiplier$col) := (frequency / total_frequency) * multiplier$value,
        .category = "Others"
      )

    } else {

      data_others <- dplyr::summarise(
        dplyr::anti_join(data, data_top_n, by = ".category"),
        frequency = sum(frequency, na.rm = TRUE),
        .category = "Others"
      )
    }

    data_others <- coerce_total(
      data_others,
      col = ".category",
      x = data[[".category"]],
      label_total = "Others",
      value = get_na_value(data[[".category"]], pattern = "^9", offset = 0)
    )


    with_cumulative <- "cumulative" %in% names(data)
    with_cumulative_p <- multiplier$cumulative_col %in% names(data)

    if(with_cumulative & with_cumulative_p) {

      data_others <- dplyr::mutate(
        data_others,
        cumulative = total_frequency,
        !!as.name(multiplier$cumulative_col) := 100
      )

    } else if (with_cumulative) {

      data_others <- dplyr::mutate(
        data_others,
        cumulative = total_frequency
      )

    } else if (with_cumulative_p) {

      data_others <- dplyr::mutate(
        data_others,
        !!as.name(multiplier$cumulative_col) := 100
      )

    }

    if(position_total == "bottom") {

      data_top_n <- dplyr::slice_head(data, n = top_n) |>
        dplyr::bind_rows(data_others) |>
        dplyr::bind_rows(utils::tail(data, add_total))

    } else {

      data_top_n <- dplyr::bind_rows(data_top_n, data_others)
    }

  }

  data_top_n

}


tsg_sort_col_value <- function(
  data,
  sort_value,
  sort_desc,
  groups
) {

  if(length(groups) > 0) { return(data) }
  if(!sort_value) {

    data <- suppressWarnings(dplyr::arrange(data, as.integer(.category), .category))
    return(data)

  }

  if(sort_desc) {
    dplyr::arrange(data, dplyr::desc(frequency))
  } else {
    dplyr::arrange(data, frequency)
  }

}


tsg_get_frequency <- function(data, column_name, include_na) {

  if(!include_na) {
    data <- dplyr::filter(data, !is.na(!!as.name(column_name)))
  }

  data |>
    dplyr::rename(.category := !!as.name(column_name)) |>
    dplyr::group_by(.category, .add = TRUE) |>
    dplyr::count(name = "frequency") |>
    dplyr::ungroup() |>
    dplyr::collect()
}


tsg_get_crosstab <- function(data, x, column_name, include_na) {

  if(!include_na) {
    data <- dplyr::filter(data, !is.na(!!as.name(column_name)))
  }

  data |>
    dplyr::group_by({{x}}, !!as.name(column_name), .add = TRUE) |>
    dplyr::count(name = "frequency") |>
    dplyr::ungroup() |>
    dplyr::rename(.category := {{x}}) |>
    dplyr::collect()
}
