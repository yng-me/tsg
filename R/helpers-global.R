# ------------------------------------------------------------------------------
# convert tidyeval argument passed to string
set_as_string <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
}

# ------------------------------------------------------------------------------
# Check if valid input data
check_input_data_validity <- function(x) {
  error_message <- ".data input must be a valid .data frame or Arrow format."

  if(is.vector(x) | is.character(x)) stop(error_message)
  if(!(inherits(x, 'data.frame') |
      inherits(x, 'ArrowObject') |
      inherits(x, 'arrow_dplyr_query')
    )
  ) stop(error_message)
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
# Get user configuration
get_config <- function(label) {

  x <- NULL

  if(exists('config')) {
    config <- eval(as.name('config'))
    x <- config$tsgx_config[[label]]
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

  if(exists('tsgx_config')) {
    config <- eval(as.name('tsgx_config'))
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
    dplyr::any_of(x_group),
    dplyr::any_of(y_group),
    ...
  )
}

str_dice <- function(s, width = NULL) {

  if(!is.null(width)) {
    L <- nchar(s)
    substring(s, seq(1L, L - width + 1, width), seq(width, L, width))[[1]]
  } else {
    strsplit(s, split = '')[[1]]
  }
}

