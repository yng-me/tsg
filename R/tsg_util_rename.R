ts_util_rename <- function(data, join_ref_by = 'value') {

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
