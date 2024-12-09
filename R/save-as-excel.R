#' Save tables into Excel
#'
#' @param .list list of data frames to be exported. Each element will be written in its own sheet/tab.
#' @param filename File name with \code{.xlsx} extension; e.g., \code{'my_workbook.xlsx'}
#' @param formatted Whether to apply default formatting/styling.
#' @param overwrite Whether to overwrite the existing file.
#' @param export_settings Use exporting settings to apply custom rendering.
#' @param title Title of the table.
#' @param include_table_list (IGNORED) Whether to include a summary of list of tables generated
#' @param tab_variable_name Column name to be used as tab/sheet name.
#' @param ... Additional arguments
#' @param options
#'
#' @return Workbook
#' @export
#'
#' @examples

save_as_excel <- function(
  .list,
  filename = NULL,
  formatted = TRUE,
  overwrite = TRUE,
  title = NULL,
  export_settings = NULL,
  include_table_list = TRUE,
  tab_variable_name = 'tab_name',
  options = list(),
  table_title = NULL,
  ...
) {

  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontName = 'Arial', fontSize = 12)

  print_file_location <- NULL
  value <- NULL

  if(is.null(filename)) {
    filename <- 'Book 1.xlsx'
    wd <- getwd()
    print_file_location <- paste0('File location: ', wd, '/', filename)
  }

  if(formatted == T) {

    if('list' %in% class(.list)) {

      df_names <- dplyr::as_tibble(names(.list)) |>
        dplyr::mutate(
          title = value,
          subtitle = NA_character_,
          description = NA_character_
        )

      if(!is.null(export_settings)) {
        df_sheet_names <- export_settings |>
          dplyr::filter(!!as.name(tab_variable_name) %in% names(.list))
      } else {

        df_sheet_names <- df_names |>
          dplyr::mutate(
            value = dplyr::if_else(
              nchar(value) > 31,
              stringr::str_sub(value, 1, 31),
              value
            )
          )

        names(.list) <- df_sheet_names$value
      }

      for(i in seq_along(df_sheet_names$value)) {

        description <- df_sheet_names$description[i]
        if(is.na(description)) description <- NULL

        write_as_excel(
          .list[[df_sheet_names$value[i]]],
          wb = wb,
          sheet = df_sheet_names$value[i],
          title = df_sheet_names$title[i],
          description = description,
          ...
        )
      }
    } else if ('data.frame' %in% class(.list)) {

      write_as_excel(
        .list,
        wb = wb,
        sheet = 'Sheet 1',
        title = title,
        ...
      )

      openxlsx::writeData(
        wb,
        x = table_title,
        sheet = 'Sheet 1',
        startRow = 2,
        startCol = 2
      )
    }

    if(length(options) > 0) {

      if(!is.null(options$width)) {
        openxlsx::setColWidths(
          wb,
          sheet = 'Sheet 1',
          cols = options$width$cols,
          widths = options$width$values
        )
      }

      if(!is.null(options$height)) {
        openxlsx::setRowHeights(
          wb,
          sheet = 'Sheet 1',
          rows = options$height$rows,
          heights = options$height$values
        )
      }

    }

    openxlsx::saveWorkbook(wb, file = filename, overwrite = overwrite)

  } else {

    openxlsx::write.xlsx(.list, file = filename, overwrite = overwrite)

  }

  if(!is.null(print_file_location)) {
    cat(print_file_location)
  }

}
