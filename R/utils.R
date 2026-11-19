`%||%` <- function(a, b) if (!is.null(a)) a else b

is_valid_input_data <- function(x) {
  error_message <- "data input must be a valid data frame or Arrow format."

  if(is.vector(x) | is.character(x)) stop(error_message)
  if(
    !(inherits(x, 'data.frame') |
       inherits(x, 'ArrowObject') |
       inherits(x, 'arrow_dplyr_query') |
       inherits(x, 'rcdf_tbl_db') |
       inherits(x, 'tbl_lazy')
  )
  ) stop(error_message)
}

# Arrow input is collected up front: Arrow cannot group by haven-labelled columns, and
# collecting restores their labels. Database tables (tbl_lazy) stay lazy.
collect_data <- function(data) {
  if(inherits(data, c("ArrowObject", "arrow_dplyr_query"))) dplyr::collect(data) else data
}

# Column names of any input; names() of a lazy table returns its internals.
col_names <- function(data) as.character(dplyr::tbl_vars(data))

# A column of in-memory input; NULL for a lazy table, whose columns carry no attributes.
local_col <- function(data, col) if (is.data.frame(data)) data[[col]]

# Drop count columns that are all zero (categories nobody falls in). Group columns
# are kept even when numeric and zero (e.g. a group age = 0).
deselect_zero_cols <- function(data, keep = NULL) {
  is_zero <- vapply(data, function(x) is.numeric(x) && sum(x, na.rm = TRUE) == 0, logical(1))
  data[!is_zero | names(data) %in% keep]
}

