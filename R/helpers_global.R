# ------------------------------------------------------------------------------
# convert tidyeval argument passed to string
set_as_string <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
}

# ------------------------------------------------------------------------------
# Check if valid input data
check_input_data_validity <- function(x) {
  error_message <- ".data input must be a valid .data frame or Arrow format. Try calling `collect()` first."
  if(is.vector(x) | is.character(x)) stop(error_message)
  if(
    !('data.frame' %in% class(x) |
      'arrow_dplyr_query' %in% class(x) |
      'ArrowObject' %in% class(x)
    )
  ) { stop(error_message) }
}

# ------------------------------------------------------------------------------
# create grouping
create_group <- function(.df, g, ...) {

  if(is.character(g) & length(g) > 0) {
    for(i in 1:length(g)) {
      .df <- .df |> dplyr::group_by(!!as.name(g[i]), ..., .add = T)
    }
    return(.df)
  } else {
    stop('Grouping variable is invalid.')
  }
}

# ------------------------------------------------------------------------------
# Set user configuration
set_config <- function(config_file, cwd = NULL) {

  valid_type_ext <- c('.yml', '.json')
  ext <- stringr::str_extract(basename(config_file), '\\..*$')

  if(!(ext %in% valid_type_ext)) {
    stop("Accepts valid file type only for the config. Use either '.yml' or '.json'.")
  }

  if(is.null(cwd)) {
    file <- clean_path(config_file)
  } else {
    file <- clean_path(paste0(cwd, '/', config_file))
  }

  if(ext == '.yml') {
    config <- yaml::read_yaml(file, readLines.warn = F)
  }

  if(ext == '.json') {
    config <- jsonlite::fromJSON(file)
  }

  return(config)
}


# ------------------------------------------------------------------------------
get_factor <- function(.data, x, y) {

  `:=` <- NULL
  new_variable_name <- NULL
  value <- NULL
  list_name <- NULL
  valueset <- NULL


  if(exists('refs')) {
    if(!exists('selected_input')) {
      selected_input <- 'hp'
    }

    x_v <- refs[[selected_input]]$data_dictionary |>
      dplyr::mutate(value = dplyr::if_else(!is.na(new_variable_name), new_variable_name, value)) |>
      dplyr::select(value, valueset) |>
      dplyr::filter(value == x) |>
      dplyr::pull(valueset)

    if(length(x_v) > 0) {

      x_vs <- refs[[selected_input]]$valueset |>
        dplyr::filter(list_name == x_v[1])

      if(nrow(x_vs) > 0) {

        if(grepl('\\d+', x_vs$value[1])) {
          x_vs <- x_vs |>
            dplyr::mutate(value = as.integer(value))
        }

        .data <- .data |>
          dplyr::mutate(!!as.name(x) := factor(!!as.name(x), x_vs$value, x_vs$label))
      }

    }

    if(y != 'NULL') {

      y_v <- refs[[selected_input]]$data_dictionary |>
        dplyr::mutate(value = dplyr::if_else(!is.na(new_variable_name), new_variable_name, value)) |>
        dplyr::select(value, valueset) |>
        dplyr::filter(value == y) |>
        dplyr::pull(valueset)

      if(length(y_v) > 0) {

        y_vs <- refs[[selected_input]]$valueset |>
          dplyr::filter(list_name == y_v[1])

        if(nrow(y_vs) > 0) {
          if(grepl('\\d+', y_vs$value[1])) {
            y_vs <- y_vs |>
              dplyr::mutate(value = as.integer(value))
          }

        .data <- .data |>
          dplyr::mutate(!!as.name(y) := factor(!!as.name(y), y_vs$value, y_vs$label))
        }

      }
    }

  }

  return(.data)

}


# ------------------------------------------------------------------------------
# Get user configuration
get_config <- function(y) {

  x <- NULL
  new_variable_name <- NULL
  value <- NULL
  label <- NULL

  if(exists('refs')) {

    if(!exists('selected_input')) {
      selected_input <- 'hp'
    }

    x_label <- refs[[selected_input]]$data_dictionary |>
      dplyr::mutate(value = dplyr::if_else(!is.na(new_variable_name), new_variable_name, value)) |>
      dplyr::select(value, label) |>
      dplyr::filter(value == y) |>
      dplyr::pull(label)

    if(length(x_label) > 0) {
      x <- x_label[1]
    }

  }

  return(x)
}

# ------------------------------------------------------------------------------
# Clean path
clean_path <- function(path) {
  stringr::str_replace_all(path, '/\\.?/', '/')
}

# ------------------------------------------------------------------------------
# Batch rename columns based on input dictionary
rename_from_dictionary <- function(data, join_ref_by = 'value') {

  refs <- NULL
  value <- NULL
  label <- NULL
  config <- NULL

  if(exists('tsg_config')) {
    config <- eval(as.name('tsg_config'))
    refs <- config$data_dictionary
  }

  if(is.null(refs)) {
    return(data)
  }

  df_name <- names(data)

  df_name <- dplyr::as_tibble(names(data)) |>
    dplyr::left_join(refs, by = join_ref_by) |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), value, label)) |>
    dplyr::pull(label)

  colnames(data) <- df_name
  return(data)
}

# ------------------------------------------------------------------------------
# Select only columns included in the tabulation
select_only <- function(.data, x_group = NULL, y_group = NULL, ...) {
  .data |> dplyr::select(
    dplyr::matches(paste0('^', x_group, '$')),
    dplyr::matches(paste0('^', y_group, '$')),
    ...
  )
}

