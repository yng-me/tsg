#' Generate a template
#'
#' @param path A character string specifying the path where the template should be saved. If a directory is provided, the template will be saved with a default name based on the template type.
#' @param template A character string specifying the type of template to generate. Options are "facade" for a YAML facade template or "table-list" for an Excel table list template.
#'
#' @returns Void. A file path where the template has been saved.
#' @export
#'
#' @examples
#'
#' template_path_facade <- tempfile(fileext = ".yaml")
#' generate_template(template_path_facade, template = "facade")
#'
#' template_path_table_list <- tempfile(fileext = ".xlsx")
#' generate_template(template_path_table_list, template = "table-list")
#'
#' unlink(template_path_facade)
#' unlink(template_path_table_list)


generate_template <- function(
  path,
  template = c("facade", "table-list")
) {

  match.arg(template[1], c("facade", "table-list"), several.ok = FALSE)
  template <- template[1]

  ext <- ifelse(template == "facade", "yaml", "xlsx")

  if(fs::is_dir(path)) {
    path <- file.path(path, paste0(template, "-template.", ext))
  }

  if(!grepl("\\.(yml|yaml|xlsx)$", path)) {
    path <- paste0(path, ".", ext)
  }

  if (template == "facade") {
    template_from <- system.file("extdata", "facade", "xlsx", "default.yaml", package = "tsg")

    fs::file_copy(template_from, path, overwrite = TRUE)

  } else if (template == "table-list") {

    data <- dplyr::tibble(
      table_number = 1:3,
      table_name = paste0("Sample Table ", 1:3),
      title = rep("Table title here...", 3),
      subtitle = rep("Table subtitle here...", 3),
      footnotes = rep("Footnote for table ", 3),
      source_note = rep(NA_character_, 3),
      facade = jsonlite::toJSON(
        list(
          fontSize = 11,
          border = "none",
          borderColour = "#000000",
          borderStyle = "thin",
          bgFill = "#FFFFFF",
          halign = "left",
          textDecoration = "none",
          wrapText = FALSE
        ),
        auto_unbox = TRUE
      )
    )

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sheet1")
    openxlsx::writeDataTable(wb, x = data, sheet = "Sheet1")

    openxlsx::setColWidths(
      wb = wb,
      sheet = "Sheet1",
      cols = 1:ncol(data),
      widths = c(15, 30, 40, 60, 50, 50, 150)
    )

    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }
}
