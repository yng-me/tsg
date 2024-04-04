#' @title Generate summary tables as a list based on defined grouping or aggregation
#' @description This function allows you to generate summary tables based on a defined grouping or aggregation.
#' You can optionally return table values in either frequency or proportion/percentage or both.
#' It uses any \code{tsgx_*} valid functions as calculator to generate desired tabulation structure.
#'
#' @param .data \strong{Required}. A .data frame, .data frame extension (e.g. a tibble), a lazy .data frame (e.g. from dbplyr or dtplyr), or Arrow .data format.
#' @param list_group \strong{Required}. A factor or categorical variable from \code{.data} to be used grouping for the list generated.
#' @param indicator \strong{Required}. column name of the variable to be used as categories.
#' @param ... Accepts valid arguments of the selected function in \code{fn}.
#' @param fn Accepts \code{generate_frequency} | \code{generate_crosstab}. The default value is \code{generate_frequency}.
#' @param list_name_overall Accepts a string that will be used as name/label for the first list. The default value is \code{All}.
#' @param exclude_overall Whether to exclude the overall (aggregate) table (first table) in the list.
#' @param collapse_overall Whether to conform the structure of the first table with the rest in the list.
#' @param save_as_excel \code{Bolean}. Whether to save the output in Excel. Default is \code{FALSE}.
#' @param title Title of table to be applied for each sheet.
#' @param description Table description.
#' @param footnote Table footnote.
#' @param source_note Table footnote.
#' @param formatted Whether to apply formatting for the Excel output. Default is \code{FALSE}.
#' @param filename Valid filename with \code{.xlsx} extension. If not specified, it will use \code{tsgx_list.xlsx} as a filename and will be saved in the current working directory.
#' @param names_separator Column separator that defines the table hierarchy.
#' @param distinct_stub_head Whether to use \code{indicator} variable as x (if \code{FALSE}) or y (if \code{TRUE})
#'
#' @return Returns a list of tables aggregated based on values defined in \code{list_group}.
#' @export
#'
#' @examples

generate_as_list <- function(
  .data,
  list_group,
  indicator,
  ...,
  fn = 'generate_crosstab',
  list_name_overall = 'ALL',
  exclude_overall = FALSE,
  collapse_overall = TRUE,
  save_as_excel = FALSE,
  title = NULL,
  description = NULL,
  footnote = NULL,
  source_note = NULL,
  formatted = TRUE,
  filename = NULL,
  names_separator = '>',
  distinct_stub_head = F
) {

  value <- NULL
  valid_fn <- c('generate_crosstab', 'generate_frequency', 'generate_multiple_response')

  if(!(fn %in% valid_fn)) {
    fn <- 'generate_frequency'
    warning(
      "You have entered invalid agrument for 'func' parameter.
      It only accepts: 'generate_crosstab' | 'generate_frequency' | 'generate_multiple_response'"
    )
  }

  func <- eval(as.name(fn))

  if(distinct_stub_head == T) {

    list_names <- list_group
    exclude_overall <- TRUE
    collapse_overall <- TRUE

  } else {

    list_names <- .data |>
      dplyr::distinct({{list_group}}) |>
      dplyr::collect() |>
      dplyr::pull({{list_group}})

    list_names <- as.character(list_names)

  }

  df <- list()

  if(exclude_overall == F) {

    if(collapse_overall == T) {
      df_all <- .data |>
        func({{indicator}}, ...)

    } else {
      df_all <- .data |>
        dplyr::select(-{{indicator}}) |>
        func({{list_group}}, ...)
    }

    df[[list_name_overall]] <- df_all
  }

  if(distinct_stub_head == T) {

    for(i in 1:length(list_names[[1]])) {

      x <- list_names[[2]][i]
      label_stub <- list_names[[1]][i]

      df[[label_stub]]  <- .data |>
        func(!!as.name(x), {{indicator}}, ..., label_stub = label_stub)

    }

  } else {

    for(i in 1:length(list_names)) {

      df_d <- .data |>
        dplyr::filter({{list_group}} == list_names[i]) |>
        dplyr::select(-{{list_group}})

      if(nrow(dplyr::compute(df_d)) > 0) {

        df[[list_names[i]]] <- df_d |>
          func({{indicator}}, ...)
      }
    }
  }

  df <- Filter(Negate(is.null), df)

  if(save_as_excel == T) {
    df |>
      save_as_excel(
        formatted = formatted,
        filename = filename,
        title = title,
        description = description,
        footnote = footnote,
        source_note = source_note,
        names_separator = names_separator
      )
  }

  return(df)
}
