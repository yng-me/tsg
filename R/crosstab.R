generate_crosstab <- function(
  data,
  ...,
  by = NULL,
  as_proportion = FALSE,
  names_separator = "__",
  pivot_wide = TRUE,
  include_na = TRUE,
  calculate_per_group = TRUE,
  group_separator = " - ",
  group_as_list = FALSE,
  label_total_row = "Total",
  label_total_column = "Total",
  metadata = list(
    title = NULL,
    subtitle = NULL,
    source_note = NULL,
    footnotes = NULL
  )
) {

  if(is.null(by)) {
    return(generate_frequency(data, ...))
  }

  groups <- dplyr::group_vars(data)
  group_attrs <- stats::setNames(lapply(groups, \(x) attributes(data[[x]])), groups)

  multiplier <- 100
  multiplier_label <- "Percent"
  if(as_proportion) {
    multiplier <- 1
    multiplier_label <- "Proportion"
  }

  multiplier_col <- tolower(multiplier_label)

  data <- dplyr::select(data, dplyr::any_of(groups), ..., dplyr::any_of(by))

  column_names <- names(data)

  print(column_names)
  if(length(groups) > 0) {
    column_names <- column_names[!(column_names %in% groups)]
  }

  df <- list()

  for(i in seq_along(column_names)) {

    column_name <- column_names[i]
    label <- attributes(data[[column_name]])$label

    if(is.null(label)) {
      label <- column_name
    }

    if(!include_na) {
      data <- dplyr::filter(data, !is.na(!!as.name(column_name)))
    }

    df_i <- data |>
      dplyr::rename(category := !!as.name(column_name)) |>
      dplyr::group_by(category, .add = TRUE) |>
      dplyr::count(name = "frequency") |>
      dplyr::ungroup()

  }

  return(df)

}


