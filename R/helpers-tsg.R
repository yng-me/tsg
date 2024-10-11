frequency_inclusion <- function(
  .data_piped,
  excluded_cols,
  include_total,
  include_cumulative,
  include_zero_value
) {

  Frequency <- NULL
  Percent <- NULL

  df <- .data_piped |> dplyr::tibble()

  if(include_cumulative == T) {
    cumulative <- df |>
      dplyr::select(-dplyr::any_of(excluded_cols)) |>
      cumsum() |>
      dplyr::rename(
        'Cumulative Total' = Frequency,
        'Cumulative Percent' = Percent
      )
    df <- df |> dplyr::bind_cols(cumulative)
  }

  if(include_total == T) {
    df <- df |>
      janitor::adorn_totals(
        where = 'row',
        fill = '-',
        na.rm = T,
        name = 'Total',
        -dplyr::any_of(excluded_cols),
        -dplyr::contains('Cumulative')
      ) |>
      convert_to_na('-') |>
      dplyr::mutate_at(dplyr::vars(dplyr::contains('Cumulative')), as.numeric)
  }

  if(include_zero_value == T) {
    df <- df |> dplyr::filter(Frequency > 0)
  }

  return(df)

}

convert_to_na <- function(.data, to = '') {
  .data |>
    dplyr::mutate_if(
      is.character,
      ~ dplyr::if_else(
        stringr::str_trim(.) == to,
        NA_character_,
        .
      )
    )
}


factor_col <- function(.data, .col, .keep_cols = TRUE, .rename_cols = FALSE) {

  attr_i <- attributes(.data[[.col]])

  if (!is.null(attr_i$valueset)) {
    col_fct <- .col
    if (.keep_cols) col_fct <- paste0(.col, "_fct")

    if(is.numeric(as.integer(attr_i$valueset$value[1]))) {

      .data <- .data |>
        dplyr::mutate(
          !!as.name(col_fct) := factor(
            as.integer(!!as.name(.col)),
            as.integer(attr_i$valueset$value),
            attr_i$valueset$label
          )
        )

    } else {
      .data <- .data |>
        dplyr::mutate(
          !!as.name(col_fct) := factor(
            !!as.name(.col),
            attr_i$valueset$value,
            attr_i$valueset$label
          )
        )
    }
  }

  if(!is.null(attr_i$label) & .rename_cols & !.keep_cols) {
    .data <- .data |>
      dplyr::rename(!!as.name(attr_i$label) := !!as.name(col_fct))
  }
  .data
}
