#' Save tables into Excel
#'
#' @param .list list of data frames to be exported. Each element will be written in its own sheet/tab.
#' @param filename Filename with .xlsx extension; e.g., 'my_workbook.xlsx'
#' @param formatted Whether to apply default formatting/styling.
#' @param overwrite Whether to overwrite the existing file.
#' @param export_settings Use exporting settings to apply custom rendering.
#' @param include_table_list Whether to include a summary of list of tables generated
#' @param tab_variable_name Column name to be used as tab/sheet name.
#' @param ... Additional arguments
#'
#' @return
#' @export
#'
#' @examples

tse_save_excel <- function(
  .list,
  filename = NULL,
  formatted = TRUE,
  overwrite = TRUE,
  export_settings = NULL,
  include_table_list = TRUE,
  tab_variable_name = 'tab_name',
  ...
) {

  wb <- openxlsx::createWorkbook()
  print_file_location <- NULL
  value <- NULL

  if(is.null(filename)) {
    filename <- 'tsg_list.xlsx'
    wd <- getwd()
    print_file_location <- paste0('File location: ', wd, '/', filename)
  }

  df_names <- dplyr::as_tibble(names(.list))

  if(!is.null(export_settings)) {
    df_names <- export_settings |>
      dplyr::filter(!!as.name(tab_variable_name) %in% names(.list))
  }

  df_sheet_names <- df_names |>
    dplyr::mutate(
      value = dplyr::if_else(
        nchar(value) > 31,
        stringr::str_sub(value, 1, 31),
        value
      )
    ) |>
    dplyr::pull(value)

  names(.list) <- df_sheet_names

  if(formatted == T) {

    for(i in seq_along(df_sheet_names)) {
      tse_write_excel(
        .list[[df_sheet_names[i]]],
        wb = wb,
        sheet = df_sheet_names[i],
        title = df_sheet_names[i]
      )
    }
    openxlsx::saveWorkbook(wb, file = filename, overwrite = overwrite)

  } else {
    openxlsx::write.xlsx(.list, file = filename, overwrite = overwrite)
  }

  if(!is.null(print_file_location)) {
    print(print_file_location)
  }

}
