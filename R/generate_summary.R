#' Generate a summary table of numeric variables
#'
#' @param data A data frame (typically \code{tibble}), or a lazy table (e.g. \code{dplyr::tbl()} on a database connection), in which case the statistics are computed in the database. Group it with \code{dplyr::group_by()} to summarise per group.
#' @param ... Numeric variables to summarise (tidyselect). If none are given, every numeric column without value labels is used.
#' @param stats Statistics to compute, any of \code{"n"}, \code{"n_missing"}, \code{"mean"}, \code{"sd"}, \code{"median"}, \code{"min"}, \code{"max"}, \code{"p25"} and \code{"p75"}. A database that cannot compute one (e.g. SQLite has no \code{quantile()} for \code{"p25"}) gives an error naming it.
#' @param group_as_list Logical. If \code{TRUE}, returns a list with one table per group.
#' @param group_separator Character. Separator between group values in list names.
#' @param metadata A named list with optional metadata to attach as attributes, e.g. \code{title}, \code{subtitle}, and \code{source_note}.
#'
#' @return A table of class \code{"tsg"} with one row per variable (its label in \code{category}) and one column per statistic, or a list of such tables when \code{group_as_list = TRUE}.
#' @export
#'
#' @examples
#' generate_summary(person_record, age)
#'
#' person_record |>
#'   dplyr::group_by(sex) |>
#'   generate_summary(age, stats = c("n", "mean", "median"))
generate_summary <- function(
  data,
  ...,
  stats = c("n", "mean", "sd", "median", "min", "max"),
  group_as_list = FALSE,
  group_separator = " - ",
  metadata = NULL
) {

  is_valid_input_data(data)
  stats <- match.arg(stats, names(summary_stat_labels), several.ok = TRUE)

  data <- rename_category_group(data)
  groups <- dplyr::group_vars(data)
  group_attrs <- get_group_attrs(data, groups)

  # Column types without pulling rows from a database.
  proto <- if (is.data.frame(data)) data else dplyr::collect(utils::head(dplyr::ungroup(data), 0))
  proto <- dplyr::ungroup(proto)
  if (rlang::dots_n(...) > 0) {
    vars <- setdiff(col_names(dplyr::select(proto, ...)), groups)
  } else {
    vars <- setdiff(col_names(proto), groups)
    vars <- vars[vapply(vars, function(v) is_summary_var(proto[[v]]), logical(1))]
    if (length(vars) == 0) {
      stop("No numeric variables without value labels to summarise.", call. = FALSE)
    }
  }
  categorical <- vars[!vapply(vars, function(v) is_summary_var(proto[[v]]), logical(1))]
  if (length(categorical) > 0) {
    stop(
      "Not numeric: ", paste0("`", categorical, "`", collapse = ", "),
      ". Use generate_frequency() for categorical variables.",
      call. = FALSE
    )
  }

  labels <- vapply(vars, function(v) attr(local_col(data, v), "label", exact = TRUE) %||% v, character(1))

  if (is.data.frame(data)) {
    # value-label classes get in the way of mean(), quantile(), ...
    data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(vars), ~ as.vector(unclass(.x))))
  }

  out <- data |>
    summarise_stats(vars, stats) |>
    tidyr::pivot_longer(
      -dplyr::all_of(groups),
      names_to = c(".var", ".stat"),
      names_pattern = "^\\.(\\d+)_(.+)$"
    ) |>
    tidyr::pivot_wider(names_from = ".stat", values_from = "value") |>
    dplyr::mutate(.var = as.integer(.var)) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c(groups, ".var")))) |>
    dplyr::mutate(category = unname(labels[.var]), .before = 1) |>
    dplyr::select(-".var")

  for (s in stats) {
    if (s %in% c("n", "n_missing")) out[[s]] <- as.integer(out[[s]])
    # an all-missing group gives Inf/-Inf/NaN in R but NULL in a database: report NA for both
    if (is.double(out[[s]])) out[[s]][!is.finite(out[[s]])] <- NA
    attr(out[[s]], "label") <- summary_stat_labels[[s]]
  }
  attr(out$category, "label") <- "Variable"

  out <- set_group_attrs(out, groups, group_attrs, resolve = FALSE)

  if (group_as_list & length(groups) > 0) {
    keys <- glue::glue_data(out, group_list_glue(groups, group_separator))
    out <- split(out, factor(keys, levels = unique(keys)))
    out <- lapply(out, set_group_attrs, groups = groups, group_attrs = group_attrs, resolve = FALSE)
  }

  new_tsg(
    out,
    "tsgs",
    groups = if (group_as_list) groups,
    group_attrs = group_attrs,
    metadata = metadata
  )
}


summary_stat_labels <- c(
  n = "N",
  n_missing = "Missing",
  mean = "Mean",
  sd = "SD",
  median = "Median",
  min = "Min",
  max = "Max",
  p25 = "P25",
  p75 = "P75"
)

# Numeric and not categorical: value labels mark a code list, not a measurement.
is_summary_var <- function(x) {
  is.numeric(x) && !is.factor(x) && length(attr(x, "labels", exact = TRUE)) == 0
}

# Written with functions dbplyr can translate to SQL.
summary_stat_expr <- function(stat, col) {
  x <- as.name(col)
  switch(
    stat,
    n = rlang::expr(sum(ifelse(is.na(!!x), 0L, 1L), na.rm = TRUE)),
    n_missing = rlang::expr(sum(ifelse(is.na(!!x), 1L, 0L), na.rm = TRUE)),
    mean = rlang::expr(mean(!!x, na.rm = TRUE)),
    sd = rlang::expr(stats::sd(!!x, na.rm = TRUE)),
    median = rlang::expr(stats::median(!!x, na.rm = TRUE)),
    min = rlang::expr(min(!!x, na.rm = TRUE)),
    max = rlang::expr(max(!!x, na.rm = TRUE)),
    p25 = rlang::expr(as.numeric(stats::quantile(!!x, 0.25, na.rm = TRUE))),
    p75 = rlang::expr(as.numeric(stats::quantile(!!x, 0.75, na.rm = TRUE)))
  )
}

# One row per group, one column per variable and statistic, named `.<var index>_<stat>`.
summarise_stats <- function(data, vars, stats) {
  exprs <- list()
  for (i in seq_along(vars)) {
    for (s in stats) {
      exprs[[paste0(".", i, "_", s)]] <- summary_stat_expr(s, vars[i])
    }
  }

  tryCatch(
    # min()/max() of an all-missing group warn in R; the result is set to NA afterwards
    suppressWarnings(dplyr::collect(dplyr::summarise(data, !!!exprs, .groups = "drop"))),
    error = function(e) {
      if (is.data.frame(data)) stop(e)
      failing <- Filter(function(s) {
        one <- stats::setNames(list(summary_stat_expr(s, vars[1])), s)
        inherits(try(dplyr::collect(dplyr::summarise(data, !!!one)), silent = TRUE), "try-error")
      }, stats)
      if (length(failing) == 0) stop(e)
      stop(
        "This database cannot compute: ", paste0("`", failing, "`", collapse = ", "),
        ". Remove it from `stats`.",
        call. = FALSE
      )
    }
  )
}
