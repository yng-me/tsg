ts_util_rename <- function(data, by = 'value') {

  refs <- NULL
  value <- NULL
  label <- NULL

  if(exists('tsg_config')) {
    refs <- tsg_config$data_dictionary
  }

  if(!is.null(refs)) {

    df_name <- names(data)

    df_name <- dplyr::as_tibble(names(data)) |>
      dplyr::left_join(refs, by = by) |>
      dplyr::mutate(label = dplyr::if_else(is.na(label), value, label)) |>
      dplyr::pull(label)

    colnames(data) <- df_name

    return(data)
  } else {
    return(data)
  }

}
