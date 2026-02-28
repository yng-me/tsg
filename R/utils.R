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

collect_data <- function(data, ...) {

  is_valid_input_data(data)

  if("ArrowObject" %in% class(data) | "arrow_dplyr_query" %in% class(data)) {
    dplyr::collect(data)
  } else {
    data
  }
}


convert_to_nested_list <- function(key_value_pairs) {

  # Helper function to split a key (e.g., style.body.border) and create a nested list
  create_nested_list <- function(key_parts, value) {
    # Base case: if there is only one key part, return the value
    if (length(key_parts) == 1) {
      return(stats::setNames(list(value), key_parts))
    }

    # Recursive case: pass the remaining parts to the next level of the list
    list_name <- key_parts[1]
    remaining_parts <- key_parts[-1]

    # Recursively create the nested structure
    nested_list <- create_nested_list(remaining_parts, value)

    return(stats::setNames(list(nested_list), list_name))
  }

  # Initialize an empty list to hold the final result
  result <- list()

  # Loop through each key-value pair
  for (key in names(key_value_pairs)) {
    value <- key_value_pairs[[key]]
    key_parts <- strsplit(key, "\\.")[[1]]  # Split the key by dot "."
    nested_value <- create_nested_list(key_parts, value)

    # Merge the nested value into the result list
    result <- utils::modifyList(result, nested_value)
  }

  return(result)
}


flatten_nested_list <- function(nested_list, parent_key = NULL) {
  flat_list <- list()

  for (name in names(nested_list)) {
    value <- nested_list[[name]]

    # Build the new key
    new_key <- if (is.null(parent_key)) name else paste(parent_key, name, sep = ".")

    # If the value is a list, recurse deeper
    if (is.list(value) && !is.null(names(value))) {
      flat_list <- c(flat_list, flatten_nested_list(value, new_key))
    } else {
      flat_list[[new_key]] <- value
    }
  }

  return(flat_list)
}


deselect_zero_cols <- function(data) {

  s <- dplyr::summarise_all(dplyr::select(data, dplyr::where(is.numeric)), ~ sum(., na.rm = TRUE))

  zero_cols <- c()
  for(col in names(s)) {

    if(s[[col]][1] == 0) {
      zero_cols <- c(zero_cols, col)
    }

  }

  if(length(zero_cols) > 0) {
    data <- dplyr::select(data, -dplyr::any_of(unlist(zero_cols)))
  }

  data

}

