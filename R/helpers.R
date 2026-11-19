get_na_value <- function(value, recode_na = "auto", pattern = "^8", offset = 1) {

  if(recode_na != "auto") return(recode_na)

  max_value <- as.integer(max(as.integer(value), na.rm = TRUE))

  attr_value <- attributes(value)$labels
  max_value_attr <- as.integer(max(as.integer(attr_value), na.rm = TRUE))

  if(grepl(pattern, max_value) | grepl(pattern, max_value_attr)) {
    code <- as.integer(paste0(rep(9, 1 + nchar(max_value)), collapse = "")) - offset
  } else {
    code <- as.integer(paste0(rep(9, nchar(max_value)), collapse = "")) - offset
  }

  # e.g. 8 = "Don't know" is labelled but unused in the data: use 98 instead
  used <- c(as.integer(value), as.integer(attr_value))
  while(code %in% used) {
    code <- as.integer(paste0("9", code))
  }

  code
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
      label = attr(value, "label", exact = TRUE)
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

  data <- row_total(
    data = data,
    position = position,
    label_total = label_total,
    col = x,
    groups = groups
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
    # mutate() also works on a lazy table (whose x is NULL)
    return(dplyr::mutate(data, !!col := label_total))
  }


  if(inherits(x, 'character')) {
    .value <- paste0("__", label_total, "__")
  } else {
    .value <- 0L
    if (inherits(x, 'integer') | is.numeric(x)) {
      if(!is.null(value)) {
        .value <- value - default_code
      } else {
        # a code no category uses, in the data or in its labels (e.g. an unused 0 = "None")
        used <- c(as.integer(x), as.integer(attr(x, "labels", exact = TRUE)))
        while(.value %in% used) .value <- .value - 1L
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


coerce_category <- function(data, col) {

  x <- data[[col]]

  if(!haven::is.labelled(x) & !is.factor(x) & !is.character(x)) {
    data[[col]] <- as.character(x)
  }

  data
}


# Same as dplyr::select(data, any_of(first), everything()), without tidyselect's
# per-call cost; keeps the table's attributes.
move_cols_first <- function(data, first) {
  data[c(intersect(first, names(data)), setdiff(names(data), first))]
}


get_group_attrs <- function(data, groups) {
  stats::setNames(
    lapply(groups, function(x) attributes(local_col(data, x))),
    groups
  )
}

get_data_attrs <- function(data) {

  vars <- col_names(data)

  stats::setNames(
    lapply(vars, function(i) {
      x <- local_col(data, i)
      attr_i <- attributes(x)
      list(
        value = i,
        label = attr_i[["label"]] %||% i, # $label would partial-match $labels
        type = typeof(x),
        labels = attr_i$labels
      )
    }),
    vars
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

  # a per-group table may already have its group columns split off
  attr_names <- intersect(names(group_attrs), names(data))

  for(k in seq_along(attr_names)) {

    attr_k <- attr_names[k]
    attr_label <- group_attrs[[attr_k]][["label"]]
    attr_labels <- group_attrs[[attr_k]]$labels

    if(!is.null(attr_labels)) {

      data[[attr_k]] <- haven::labelled(
        x = data[[attr_k]],
        label = group_attrs[[attr_k]][["label"]],
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
    data <- resolve_group_col(data)
  }

  move_cols_first(data, groups)
}


# `category` is the stub column of every table. A group of that name is renamed before
# anything else, so the column, its header and the groups attribute all agree.
rename_category_group <- function(data) {
  if(!("category" %in% dplyr::group_vars(data))) return(data)
  message("WARNING: `category` is a reserved column name and has been renamed to `category_group`")
  dplyr::rename(data, category_group = "category")
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
        !!as.name(multiplier$cumulative_col) := multiplier$value
      )

    } else if (with_cumulative) {

      data_others <- dplyr::mutate(
        data_others,
        cumulative = total_frequency
      )

    } else if (with_cumulative_p) {

      data_others <- dplyr::mutate(
        data_others,
        !!as.name(multiplier$cumulative_col) := multiplier$value
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
    dplyr::select(dplyr::any_of(c(dplyr::group_vars(data), column_name, precount_col))) |>
    dplyr::rename(.category := !!as.name(column_name)) |>
    dplyr::group_by(.category, .add = TRUE) |>
    dplyr::count(name = "frequency", wt = !!count_weight(data)) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    sort_counts()
}


tsg_get_crosstab <- function(data, x, column_name, include_na) {

  if(!include_na) {
    data <- dplyr::filter(data, !is.na(!!as.name(column_name)))
  }

  data |>
    dplyr::select(dplyr::any_of(c(dplyr::group_vars(data), rlang::as_label(rlang::enquo(x)), column_name, precount_col))) |>
    dplyr::group_by({{x}}, !!as.name(column_name), .add = TRUE) |>
    dplyr::count(name = "frequency", wt = !!count_weight(data)) |>
    dplyr::ungroup() |>
    dplyr::rename(.category := {{x}}) |>
    dplyr::collect() |>
    sort_counts()
}

# A database returns grouped counts in no set order (and maybe as integer64); sort and
# type them as an in-memory count() does.
sort_counts <- function(data) {
  data$frequency <- as.integer(data$frequency)
  dplyr::arrange(data, dplyr::pick(-frequency))
}

# Name of the count column precount_lazy() adds; tsg_get_frequency()/tsg_get_crosstab()
# sum it instead of counting rows.
precount_col <- ".tsg_n"

# Nested group lists count every group separately, which on a lazy table would be one
# query per group. Instead, count a lazy table once in the database by `cols` and
# collect those counts (in precount_col); in-memory data is returned unchanged.
precount_lazy <- function(data, cols) {
  if(is.data.frame(data)) return(data)
  out <- data |>
    dplyr::ungroup() |>
    dplyr::count(dplyr::across(dplyr::all_of(cols)), name = precount_col) |>
    dplyr::collect()
  out[[precount_col]] <- as.integer(out[[precount_col]])
  out
}

# Raw rows weigh 1 each; pre-counted rows (precount_lazy()) weigh their count.
with_row_weight <- function(data) {
  if(precount_col %in% names(data)) return(data)
  data[[precount_col]] <- 1L
  data
}

# Weight for count(): precount_col when the data was pre-counted, else none (NULL).
count_weight <- function(data) {
  if(precount_col %in% col_names(data)) rlang::sym(precount_col)
}


# --- Shared steps of the generators (grouping, nested lists) and the writers ---

# The x columns of a multi-x crosstab pasted into one column, kept alongside them.
# tidyr::unite() has no lazy method; NA becomes "NA" as unite() does.
unite_cols <- function(data, name, cols) {
  if(is.data.frame(data)) {
    return(tidyr::unite(data, !!name, dplyr::all_of(cols), remove = FALSE, sep = "__"))
  }
  parts <- lapply(cols, function(col) rlang::expr(dplyr::coalesce(as.character(!!as.name(col)), "NA")))
  dplyr::mutate(data, !!name := paste(!!!parts, sep = "__"))
}


group_list_glue <- function(groups, separator) {
  paste0(paste0("{haven::as_factor(", groups, ")}"), collapse = separator)
}


coerce_groups_total <- function(data, groups, label) {
  data_g <- dplyr::ungroup(data)
  for(g in groups) {
    lbl <- .resolve_hierarchy_label(label, g)
    data_g <- coerce_total(
      data = data_g,
      col = g,
      x = local_col(data_g, g),
      label_total = lbl,
      default_code = -1L
    )
  }
  data_g
}


# The subtotal tables of a hierarchy (group_as_hierarchy) for 2+ groups: subtotals[[k]]
# has groups[1..k] at their values and the deeper groups set to the hierarchy label,
# computed with `compute` for each combination of the first k groups (NA included).
hierarchy_subtotals <- function(data, groups, label, compute) {
  data <- dplyr::ungroup(data)
  subtotals <- list()
  for (k in seq_len(length(groups) - 1)) {
    parent_groups <- groups[seq_len(k)]
    child_groups <- groups[(k + 1):length(groups)]
    combos <- dplyr::distinct(dplyr::select(data, dplyr::all_of(parent_groups)))
    parts <- vector("list", nrow(combos))
    for (i in seq_len(nrow(combos))) {
      data_k <- data
      for (pg in parent_groups) {
        data_k <- filter_group_value(data_k, pg, combos[[pg]][i])
      }
      for (cg in child_groups) {
        data_k <- coerce_total(data_k, cg, data_k[[cg]], .resolve_hierarchy_label(label, cg), default_code = -1L)
      }
      parts[[i]] <- compute(data_k)
    }
    subtotals[[k]] <- dplyr::bind_rows(parts)
  }
  subtotals
}

# Rows whose group `g` equals `value`, NA matching NA. !!value puts the value in the
# filter; a bare name would be read as a data column of that name.
filter_group_value <- function(data, g, value) {
  if (is.na(value)) return(dplyr::filter(data, is.na(!!rlang::sym(g))))
  dplyr::filter(data, !!rlang::sym(g) == !!value)
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

  # Attach temporary character-key columns to a data frame (for matching). A missing
  # group value gets a key of its own: comparing NA with == would select rows of NAs.
  add_keys <- function(df) {
    for(g in groups) {
      key <- gval_to_char(df[[g]])
      key[is.na(key)] <- "\u0001NA"
      df[[paste0(".hk_", g)]] <- key
    }
    df
  }
  drop_keys <- function(df) df[, !startsWith(names(df), ".hk_"), drop = FALSE]

  leaf_k <- add_keys(leaf_data)
  subtotals_k <- lapply(subtotals, add_keys)
  result_parts <- list(grand_total_df)

  # Recursive: builds ordered parts for `depth` (1-indexed), within the context of
  # `ctx` — a named character vector mapping group names to their current value keys.
  build_level <- function(depth, ctx) {
    g <- groups[depth]
    hk_g <- paste0(".hk_", g)

    # Filter leaf data to the current context
    leaf_ctx <- leaf_k
    for(cg in names(ctx)) {
      hk_cg <- paste0(".hk_", cg)
      leaf_ctx <- leaf_ctx[leaf_ctx[[hk_cg]] == ctx[[cg]], , drop = FALSE]
    }

    # Ordered unique values at this depth within the current context
    unique_vals <- unique(leaf_ctx[[hk_g]])

    for(v in unique_vals) {

      new_ctx <- c(ctx, setNames(v, g))
      leaf_v <- leaf_ctx[leaf_ctx[[hk_g]] == v, , drop = FALSE]

      if(depth < n_groups) {
        # Find matching subtotal in subtotals_k[[depth]] (level-k subtotals use groups[1..depth])
        stdf <- subtotals_k[[depth]]
        for(cg in names(new_ctx)) {
          hk_cg <- paste0(".hk_", cg)
          stdf <- stdf[stdf[[hk_cg]] == new_ctx[[cg]], , drop = FALSE]
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
  g <- groups_rem[[1]]
  next_groups <- groups_rem[-1]
  result <- list()

  if (add_totals) {
    g_label <- group_attrs[[g]][["label"]] %||% g
    lbl <- .resolve_hierarchy_label(total_label, g)
    total_key <- paste0(g_label, ": ", lbl)
    coerced <- raw_data
    for (cg in c(g, next_groups)) {
      cg_lbl <- .resolve_hierarchy_label(total_label, cg)
      coerced <- coerce_total(coerced, cg, local_col(raw_data, cg), cg_lbl, default_code = -1L)
    }
    result[[total_key]] <- compute_fn(coerced)
  }

  # distinct() + pull() rather than unique(raw_data[[g]]): it also works on a lazy table
  uniq_vals <- unique(dplyr::pull(dplyr::distinct(dplyr::select(raw_data, dplyr::all_of(g)))))
  # a database returns them in no set order
  if (!is.data.frame(raw_data)) uniq_vals <- sort(uniq_vals, na.last = TRUE)

  for (v in uniq_vals) {
    # a missing group keeps its rows under "NA", as a one-group list does
    key <- if (is.na(v)) "NA" else val_to_group_key(v, group_attrs[[g]])
    sub_data <- filter_group_value(raw_data, g, v)

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
      text = x,
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


# The tables of a list, nested lists included (group_as_list with 2+ groups), as one
# flat list named by their path: list(Male = list(Yes = t)) -> list("Male - Yes" = t).
# The HTML, PDF and Word writers render one table per list item.
flatten_tables <- function(data, prefix = NULL, sep = " - ") {
  out <- list()
  for (i in seq_along(data)) {
    name <- names(data)[i] %||% as.character(i)
    key <- if (is.null(prefix)) name else paste0(prefix, sep, name)
    if (is.data.frame(data[[i]])) {
      out <- c(out, stats::setNames(list(data[[i]]), key)) # append: repeated names are kept
    } else {
      out <- c(out, flatten_tables(data[[i]], key, sep))
    }
  }
  out
}


# How the numbers of a table are shown, the same in every output format: the columns
# after `category` (the category and grouping columns are labels, not quantities).
# With `decimal_cols` (table.decimalCols), exactly those columns get decimals. Otherwise
# a double column with any fractional value gets decimals; other numbers are counts.
# Pass every row that shares the format, e.g. all groups of a sheet.
number_format_cols <- function(data, decimal_cols = NULL) {
  first <- match("category", names(data), nomatch = 0L) + 1L
  cols <- names(data)[seq_along(data) >= first]
  cols <- cols[vapply(data[cols], function(x) is.numeric(x) && !is.factor(x), logical(1))]

  if (length(decimal_cols) > 0) {
    decimal <- intersect(cols, decimal_cols)
  } else {
    decimal <- cols[vapply(data[cols], function(x) {
      is.double(x) && any(abs(x - round(x)) > 1e-9, na.rm = TRUE)
    }, logical(1))]
  }

  list(integer = setdiff(cols, decimal), decimal = decimal)
}
