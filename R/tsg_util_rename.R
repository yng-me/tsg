ts_util_rename <- function(data, by = 'value') {

  ref <- NULL
  refs <- NULL
  value <- NULL
  label <- NULL

  if(exists('refs')) {
    ref <- refs$data_dictionary
  }

  if(!is.null(ref)) {

    df_name <- names(data)

    df_name <- dplyr::as_tibble(names(data)) |>
      dplyr::left_join(ref, by = by) |>
      dplyr::mutate(label = dplyr::if_else(is.na(label), value, label)) |>
      dplyr::pull(label)

    colnames(data) <- df_name

    return(data)
  } else {
    return(data)
  }

}
