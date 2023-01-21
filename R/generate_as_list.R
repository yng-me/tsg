#' @title Generate summary tables as a list based on defined grouping or aggregation
#' @description This function allows you to generate summary tables based on a defined grouping or aggregation.
#' You can optionally return table values in either frequency or proportion/percentage or both.
#' It uses any \code{tsg_*} valid functions as calculator to generate desired tabulation structure.
#'
#' @param .data \strong{Required}. A .data frame, .data frame extension (e.g. a tibble), a lazy .data frame (e.g. from dbplyr or dtplyr), or Arrow .data format.
#' @param list_group \strong{Required}. A factor or categorical variable from \code{.data} to be used grouping for the list generated.
#' @param x \strong{Required}. column name of the variable to be used as categories.
#' @param ... Accepts valid arguments of the selected function in \code{fn}.
#' @param fn Accepts \code{generate_frequency} | \code{generate_crosstab}. The default value is \code{generate_frequency}.
#' @param list_name_overall Accepts a string that will be used as name/label for the first list. The default value is \code{All}.
#' @param exclude_overall Whether to exclude the overall (aggregate) table (first table) in the list.
#' @param collapse_overall Whether to conform the structure of the first table with the rest in the list.
#' @param save_as_excel \code{Bolean}. Whether to save the output in Excel. Default is \code{FALSE}.
#' @param formatted Whether to apply formatting for the Excel output. Default is \code{FALSE}.
#' @param filename Valid filename with \code{.xlsx} extension. If not specified, it will use \code{tsg_list.xlsx} as a filename and will be saved in the current working directory.
#'
#' @return Returns a list of tables aggregated based on values defined in \code{list_group}.
#' @export
#'
#' @examples
#' mtcars_by_cyl_freq <- mtcars |>
#'   generate_as_list(list_group = cyl, x = am)
#'
#' mtcars_by_cyl_freq
#'
#' mtcars_by_cyl_prop <- mtcars |>
#'   generate_as_list(list_group = cyl, x = am, gear, fn = 'generate_crosstab')
#'
#' mtcars_by_cyl_prop


generate_as_list <- function(
  .data,
  list_group,
  x,
  ...,
  fn = 'generate_crosstab',
  list_name_overall = 'ALL',
  exclude_overall = FALSE,
  collapse_overall = TRUE,
  save_as_excel = FALSE,
  formatted = TRUE,
  filename = NULL
) {

  value <- NULL
  valid_fn <- c('generate_crosstab', 'generate_frequency', 'generate_multiple_response')

  if(!(fn %in% valid_fn)) {
    fn <- 'generate_frequency'
    warning("You have entered invalid agrument for 'func' parameter. It only accepts: 'generate_crosstab' | 'generate_frequency' | 'generate_multiple_response'")
  }

  f <- eval(as.name(fn))

  list_names <- dplyr::distinct(.data, {{list_group}}) |>
    dplyr::collect() |>
    dplyr::pull({{list_group}})

  df <- list()

  if(exclude_overall == F) {

    if(collapse_overall == T) {
      df_all <- .data |>
        f({{x}}, x_as_group = !collapse_overall, ...)

    } else {
      df_all <- .data |>
        dplyr::select(-{{x}}) |>
        f({{list_group}}, x_as_group = !collapse_overall, ...)
    }

    df[[list_name_overall]] <- df_all
  }

  for(i in 1:length(list_names)) {
    df_d <- .data |>
      dplyr::filter({{list_group}} == list_names[i]) |>
      dplyr::select(-{{list_group}})

    if(nrow(df_d) > 0) {
      df[[list_names[i]]] <- df_d |> f({{x}}, ...)
    }
  }

  df <- Filter(Negate(is.null), df)

  if(save_as_excel == T) {
    df |> save_as_excel(
      formatted = formatted,
      filename = filename
    )
  }

  return(df)
}
