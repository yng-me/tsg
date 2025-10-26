#' Generate a template
#'
#' @param path A character string specifying the path where the template should be saved. If a directory is provided, the template will be saved with a default name based on the template type.
#' @param template A character string specifying the type of template to generate. Options are "facade" for a YAML facade template or "table-list" for an Excel table list template.
#' @param which A character string specifying the format of the template to generate. Options are "xlsx", "pdf", or "html". Defaults to "xlsx".
#'
#' @returns
#' @export
#'
#' @examples
#'

generate_template <- function(
  path,
  template = c("facade", "table-list"),
  which = c("xlsx", "pdf", "html")
) {

  match.arg(template[1], c("facade", "table-list"), several.ok = FALSE)
  ext <- ifelse(template[1] == "facade", "yaml", "xlsx")
  match.arg(which[1], c("xlsx", "pdf", "html"), several.ok = FALSE)

  if(fs::is_dir(path)) {
    path <- file.path(path, paste0(template[1], "-template.", ext))
  }

  print(path)

  if(!grepl("\\.(yml|yaml|xlsx)$", path)) {
    path <- paste0(path, ".", ext)
  }

  if (template[1] == "facade") {
    template_from <- system.file("extdata", "facade", "xlsx", "default.yaml", package = "tsg")

    fs::file_copy(template_from, path, overwrite = TRUE)

  } else if (template[1] == "table-list") {

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

    # openxlsx::writeComment(
    #   wb,
    #   sheet = "Sheet1",
    #   col = 1,
    #   row = 1,
    #   comment = openxlsx::createComment(
    #     "Add table number here (e.g., 1, 2, 3, ...). For sub-table, use 1.1, 1.2, etc.",
    #     author = "TSG"
    #   )
    # )
    #
    # openxlsx::writeComment(
    #   wb,
    #   sheet = "Sheet1",
    #   col = 2,
    #   row = 1,
    #   comment = openxlsx::createComment(
    #     "Add table name here (e.g., 'Star Wars Characters'). This will be used in the table list.",
    #     author = "TSG"
    #   )
    # )
    #
    # openxlsx::writeComment(
    #   wb,
    #   sheet = "Sheet1",
    #   col = 3,
    #   row = 1,
    #   comment = openxlsx::createComment(
    #     "Add table title here (e.g., 'Frequency of Star Wars Characters'). This will be used in the table list.",
    #     author = "TSG"
    #   )
    # )

    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }
}
