gt_for_tsg <- function(
  .data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  y_group_separator = '>'
) {

  value <- NULL
  key <- NULL
  `:=` <- NULL

  source_note_formatted <- ''
  if(!is.null(source_note)) {
    source_note_formatted <- gt::md(
      paste0('*Source: ', source_note, '*')
    )
  }

  gtx <- .data |>
    gt::gt() |>
    gt::tab_source_note(
      source_note = source_note_formatted
    )

  gtx_cols <- dplyr::as_tibble(names(.data)) |>
    tidyr::separate(value, into = c('key', 'value')) |>
    dplyr::filter(!is.na(value))
    # dplyr::mutate(
    #   original_value = value,
    #   n = nchar(stringr::str_extract_all(value, y_group_separator, simplify = T)),
    #   value = stringr::str_split(value, y_group_separator),
    #   col_from = 1:n()
    # ) |>
    # dplyr::filter(n > 0) |>
    # tidyr::unnest(value)

  print(gtx_cols)
  spanner <- unique(gtx_cols$key)

  for(i in 1:length(spanner)) {

    col_spanner_items <- gtx_cols |> dplyr::filter(key == spanner[i])
    cols <- paste0(spanner[i], y_group_separator, col_spanner_items$value)

    gtx <- gtx |>
      gt::tab_spanner(
        label = spanner[i],
        columns = cols
      )

    for(j in 1:length(cols)) {

      gtx <- gtx |>
        gt::cols_label(
          (!!as.name(cols[j])) := stringr::str_remove(cols[j], paste0('^', spanner[i], y_group_separator))
        )
    }
  }


  return(gtx)


}
