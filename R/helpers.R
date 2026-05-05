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

  index_pos <- nrow(data)

  if(position == "top") {
    index_pos <- 1L
  }

  if("cumulative_percent" %in% names(data)) {
    data$cumulative_percent[[index_pos]] <- NA_real_
  }

  if("cumulative_proportion" %in% names(data)) {
    data$cumulative_proportion[[index_pos]] <- NA_real_
  }

  if("cumulative" %in% names(data)) {
    data$cumulative[[index_pos]] <- NA_integer_
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

  col_names <- names(data)

  stats::setNames(
    lapply(col_names, function(i) {
      attr_i <- attributes(data[[i]])
      label  <- attr_i$label %||% i
      list(
        value  = i,
        label  = label,
        type   = typeof(data[[i]]),
        labels = attr_i$labels
      )
    }),
    col_names
  )

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
  groups = NULL
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
    dplyr::select(dplyr::any_of(c(dplyr::group_vars(data), column_name))) |>
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
    dplyr::select(dplyr::any_of(c(dplyr::group_vars(data), rlang::as_label(rlang::enquo(x)), column_name))) |>
    dplyr::group_by({{x}}, !!as.name(column_name), .add = TRUE) |>
    dplyr::count(name = "frequency") |>
    dplyr::ungroup() |>
    dplyr::rename(.category := {{x}}) |>
    dplyr::collect()
}


coerce_groups_total <- function(data, groups, label) {
  data_g <- dplyr::ungroup(data)
  for(g in groups) {
    lbl    <- .resolve_hierarchy_label(label, g)
    data_g <- coerce_total(
      data = data_g,
      col = g,
      x = data_g[[g]],
      label_total = lbl,
      default_code = -1L
    )
  }
  data_g
}


# Assembles pre-computed data frames into hierarchically-ordered output.
#
# Given a grand-total data frame, a list of intermediate-level subtotal data frames, and the
# leaf data frame (all group combinations), returns a single data frame with rows ordered as:
#   grand total → (level-1 subtotal + all descendants) for each top-level group value → ...
#
# @param grand_total_df  Data frame for the overall grand total (all group cols = total label).
# @param subtotals       List of length n_groups-1. subtotals[[k]] is a data frame of
#                        subtotals where groups[1..k] have actual values and groups[k+1..n]
#                        have the total label.
# @param leaf_data       Data frame with all leaf rows (full group combinations).
# @param groups          Character vector of group column names in hierarchy order.
#
# @return A single data frame in hierarchical order with no extra columns added.
assemble_group_hierarchy <- function(grand_total_df, subtotals, leaf_data, groups) {

  n_groups <- length(groups)

  if(n_groups == 0) return(leaf_data)

  # coerce_total() converts plain (non-labelled, non-factor) numeric group columns to character.
  # Ensure leaf_data and subtotals match that type so bind_rows doesn't fail.
  for(g in groups) {
    if(is.character(grand_total_df[[g]])) {
      leaf_data[[g]] <- as.character(leaf_data[[g]])
      for(k in seq_along(subtotals)) {
        subtotals[[k]][[g]] <- as.character(subtotals[[k]][[g]])
      }
    }
  }

  if(n_groups == 1) return(dplyr::bind_rows(grand_total_df, leaf_data))

  # Convert a group column to a character vector for comparison (handles haven-labelled, factor, etc.)
  gval_to_char <- function(x) {
    if(haven::is.labelled(x)) return(as.character(haven::as_factor(x)))
    if(is.factor(x)) return(as.character(x))
    as.character(x)
  }

  # Attach temporary character-key columns to a data frame (for matching)
  add_keys <- function(df) {
    for(g in groups) df[[paste0(".hk_", g)]] <- gval_to_char(df[[g]])
    df
  }
  drop_keys <- function(df) df[, !startsWith(names(df), ".hk_"), drop = FALSE]

  leaf_k       <- add_keys(leaf_data)
  subtotals_k  <- lapply(subtotals, add_keys)
  result_parts <- list(grand_total_df)

  # Recursive: builds ordered parts for `depth` (1-indexed), within the context of
  # `ctx` — a named character vector mapping group names to their current value keys.
  build_level <- function(depth, ctx) {
    g    <- groups[depth]
    hk_g <- paste0(".hk_", g)

    # Filter leaf data to the current context
    leaf_ctx <- leaf_k
    for(cg in names(ctx)) {
      hk_cg      <- paste0(".hk_", cg)
      leaf_ctx   <- leaf_ctx[leaf_ctx[[hk_cg]] == ctx[[cg]], , drop = FALSE]
    }

    # Ordered unique values at this depth within the current context
    unique_vals <- unique(leaf_ctx[[hk_g]])

    for(v in unique_vals) {

      new_ctx  <- c(ctx, setNames(v, g))
      leaf_v   <- leaf_ctx[leaf_ctx[[hk_g]] == v, , drop = FALSE]

      if(depth < n_groups) {
        # Find matching subtotal in subtotals_k[[depth]] (level-k subtotals use groups[1..depth])
        stdf <- subtotals_k[[depth]]
        for(cg in names(new_ctx)) {
          hk_cg <- paste0(".hk_", cg)
          stdf  <- stdf[stdf[[hk_cg]] == new_ctx[[cg]], , drop = FALSE]
        }
        result_parts[[length(result_parts) + 1]] <<- drop_keys(stdf)
        build_level(depth + 1, new_ctx)
      } else {
        result_parts[[length(result_parts) + 1]] <<- drop_keys(leaf_v)
      }
    }
  }

  build_level(1L, character(0))
  dplyr::bind_rows(result_parts)
}


# Convert a raw group column value to its display key string.
#
# For haven-labelled columns, resolves the value to its label string.
# Falls back to as.character() for all other column types.
#
# @param v         A single scalar value from the group column.
# @param col_attrs A list with at least a `labels` element (named integer vector, or NULL).
#
# @return A length-1 character string.
val_to_group_key <- function(v, col_attrs) {
  lbl_vec <- col_attrs$labels
  if (!is.null(lbl_vec)) {
    idx <- match(v, lbl_vec)
    if (!is.na(idx)) return(names(lbl_vec)[idx])
  }
  as.character(v)
}


# Recursively build a nested named list of group tables.
#
# @param raw_data    Raw (ungrouped) data frame, already filtered to parent group values.
# @param groups_rem  Remaining group columns to nest on (character vector, in hierarchy order).
# @param group_attrs Named list of group column attributes from `get_group_attrs()`.
# @param add_totals  If TRUE, insert a total entry at each level using `total_label`.
# @param total_label Label string (or named character vector) for total entries. A single
#   string is used for all levels; a named vector keyed by group column name selects a
#   per-variable label, falling back to the first element when the name is absent.
# @param compute_fn  function(sub_data) -> single formatted table (data.frame).
#
# @return A named list; leaves are data frames, branches are nested named lists.
build_nested_group_list <- function(raw_data, groups_rem, group_attrs, add_totals, total_label, compute_fn) {
  g          <- groups_rem[[1]]
  next_groups <- groups_rem[-1]
  result     <- list()

  if (add_totals) {
    g_label   <- group_attrs[[g]]$label %||% g
    lbl       <- .resolve_hierarchy_label(total_label, g)
    total_key <- paste0(g_label, ": ", lbl)
    coerced   <- raw_data
    for (cg in c(g, next_groups)) {
      cg_lbl  <- .resolve_hierarchy_label(total_label, cg)
      coerced <- coerce_total(coerced, cg, raw_data[[cg]], cg_lbl, default_code = -1L)
    }
    result[[total_key]] <- compute_fn(coerced)
  }

  uniq_vals <- unique(raw_data[[g]])
  uniq_vals <- uniq_vals[!is.na(uniq_vals)]

  for (v in uniq_vals) {
    key      <- val_to_group_key(v, group_attrs[[g]])
    sub_data <- dplyr::filter(raw_data, !!rlang::sym(g) == v)

    if (length(next_groups) == 0) {
      result[[key]] <- compute_fn(sub_data)
    } else {
      result[[key]] <- build_nested_group_list(
        sub_data, next_groups, group_attrs,
        add_totals, total_label, compute_fn
      )
    }
  }

  result
}


# Resolve a hierarchy label for a specific group column.
#
# @param label      A single string (applied to all groups) or a named character vector
#   keyed by group column name.  For a named vector, the label for `group_name` is
#   returned; when the name is absent the first unnamed element is used as the default,
#   and if all elements are named the first element is used as final fallback.
# @param group_name The group column name for which to resolve the label.
# @return A single character string.
.resolve_hierarchy_label <- function(label, group_name) {
  if (is.null(names(label))) {
    return(label[[1L]])
  }
  nm <- names(label)
  if (group_name %in% nm) {
    return(unname(label[[group_name]]))
  }
  # Use the first unnamed (empty-name "") element as the default, if present
  unnamed_idx <- which(nm == "")
  if (length(unnamed_idx) > 0L) {
    return(unname(label[[unnamed_idx[[1L]]]]))
  }
  # Final fallback: first element
  unname(label[[1L]])
}


# Normalise the footnotes value to a canonical list(text, placement, locations).
#
# Accepts either:
#   - NULL                        → returns NULL
#   - a plain character vector    → wraps into the canonical list with "auto" placement
#   - a list with $text, $placement, $locations (from add_footnote() attr)
#
# The returned list always has:
#   $text       character vector of footnote strings
#   $placement  character vector, same length as $text ("auto"/"left"/"right")
#   $locations  list, same length as $text (each element is NULL or a character vector
#               of column names)
.normalize_footnotes <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) {
    n <- length(x)
    return(list(
      text      = x,
      placement = rep("auto", n),
      locations = vector("list", n)
    ))
  }
  # Already a list — ensure all three slots are present and length-consistent
  n <- length(x$text)
  if (n == 0L) return(NULL)
  x$placement <- x$placement %||% rep("auto", n)
  x$locations <- x$locations %||% vector("list", n)
  x
}
