add_facade <- function(
  data,
  table.offsetRow = 0,
  table.offsetCol = 0,
  table.gridLines = NULL,
  table.tabColour = NULL,
  table.fontName = NULL,
  table.fontSize = NULL,
  table.fontColour = NULL,
  table.bgFill = NULL,
  table.fgFill = NULL,
  table.halign = NULL,
  table.valign = NULL,
  table.wrapText = FALSE,
  table.indent = NULL,
  table.locked = NULL,
  table.hidden = NULL,
  table.decimalPrecision = NULL,
  table.decimalCols = NULL,
  table.lastRowBold = NULL,
  table.widths = NULL,
  table.widthOffset = NULL,
  title.fontName = NULL,
  title.fontSize = NULL,
  title.fontColour = NULL,
  title.numFmt = NULL,
  title.border = NULL,
  title.borderColour = NULL,
  title.borderStyle = NULL,
  title.bgFill = NULL,
  title.fgFill = NULL,
  title.halign = NULL,
  title.valign = NULL,
  title.textDecoration = NULL,
  title.wrapText = NULL,
  title.textRotation = NULL,
  title.indent = NULL,
  title.heights = NULL,
  subtitle.fontName = NULL,
  subtitle.fontSize = NULL,
  subtitle.fontColour = NULL,
  subtitle.numFmt = NULL,
  subtitle.border = NULL,
  subtitle.borderColour = NULL,
  subtitle.borderStyle = NULL,
  subtitle.bgFill = NULL,
  subtitle.fgFill = NULL,
  subtitle.halign = NULL,
  subtitle.valign = NULL,
  subtitle.textDecoration = NULL,
  subtitle.wrapText = NULL,
  subtitle.textRotation = NULL,
  subtitle.indent = NULL,
  subtitle.heights = NULL,
  header.fontName = NULL,
  header.fontSize = NULL,
  header.fontColour = NULL,
  header.numFmt = NULL,
  header.border = NULL,
  header.borderColour = NULL,
  header.borderStyle = NULL,
  header.bgFill = NULL,
  header.fgFill = NULL,
  header.halign = NULL,
  header.valign = NULL,
  header.textDecoration = NULL,
  header.wrapText = NULL,
  header.textRotation = NULL,
  header.indent = NULL,
  header.heights = NULL,
  spanner.fontName = NULL,
  spanner.fontSize = NULL,
  spanner.fontColour = NULL,
  spanner.numFmt = NULL,
  spanner.border = NULL,
  spanner.borderColour = NULL,
  spanner.borderStyle = NULL,
  spanner.bgFill = NULL,
  spanner.fgFill = NULL,
  spanner.halign = NULL,
  spanner.valign = NULL,
  spanner.textDecoration = NULL,
  spanner.wrapText = NULL,
  spanner.textRotation = NULL,
  spanner.indent = NULL,
  spanner.heights = NULL,
  body.fontName = NULL,
  body.fontSize = NULL,
  body.fontColour = NULL,
  body.numFmt = NULL,
  body.border = NULL,
  body.borderColour = NULL,
  body.borderStyle = NULL,
  body.bgFill = NULL,
  body.fgFill = NULL,
  body.halign = NULL,
  body.valign = NULL,
  body.textDecoration = NULL,
  body.wrapText = NULL,
  body.textRotation = NULL,
  body.indent = NULL,
  body.heights = NULL,
  col_first.fontName = NULL,
  col_first.fontSize = NULL,
  col_first.fontColour = NULL,
  col_first.numFmt = NULL,
  col_first.border = NULL,
  col_first.borderColour = NULL,
  col_first.borderStyle = NULL,
  col_first.bgFill = NULL,
  col_first.fgFill = NULL,
  col_first.halign = NULL,
  col_first.valign = NULL,
  col_first.textDecoration = NULL,
  col_first.wrapText = NULL,
  col_first.textRotation = NULL,
  col_first.indent = NULL,
  col_first.widths = NULL,
  col_last.fontName = NULL,
  col_last.fontSize = NULL,
  col_last.fontColour = NULL,
  col_last.numFmt = NULL,
  col_last.border = NULL,
  col_last.borderColour = NULL,
  col_last.borderStyle = NULL,
  col_last.bgFill = NULL,
  col_last.fgFill = NULL,
  col_last.halign = NULL,
  col_last.valign = NULL,
  col_last.textDecoration = NULL,
  col_last.wrapText = NULL,
  col_last.textRotation = NULL,
  col_last.indent = NULL,
  col_last.widths = NULL,
  row_group.fontName = NULL,
  row_group.fontSize = NULL,
  row_group.fontColour = NULL,
  row_group.numFmt = NULL,
  row_group.border = NULL,
  row_group.borderColour = NULL,
  row_group.borderStyle = NULL,
  row_group.bgFill = NULL,
  row_group.fgFill = NULL,
  row_group.halign = NULL,
  row_group.valign = NULL,
  row_group.textDecoration = NULL,
  row_group.wrapText = NULL,
  row_group.textRotation = NULL,
  row_group.indent = NULL,
  row_group.widths = NULL,
  source_note.fontName = NULL,
  source_note.fontSize = NULL,
  source_note.fontColour = NULL,
  source_note.numFmt = NULL,
  source_note.border = NULL,
  source_note.borderColour = NULL,
  source_note.borderStyle = NULL,
  source_note.bgFill = NULL,
  source_note.fgFill = NULL,
  source_note.halign = NULL,
  source_note.valign = NULL,
  source_note.textDecoration = NULL,
  source_note.wrapText = NULL,
  source_note.textRotation = NULL,
  source_note.indent = NULL,
  source_note.heights = NULL,
  footnotes.fontName = NULL,
  footnotes.fontSize = NULL,
  footnotes.fontColour = NULL,
  footnotes.numFmt = NULL,
  footnotes.border = NULL,
  footnotes.borderColour = NULL,
  footnotes.borderStyle = NULL,
  footnotes.bgFill = NULL,
  footnotes.fgFill = NULL,
  footnotes.halign = NULL,
  footnotes.valign = NULL,
  footnotes.textDecoration = NULL,
  footnotes.wrapText = NULL,
  footnotes.textRotation = NULL,
  footnotes.indent = NULL,
  footnotes.heights = NULL
) {

  args <- as.list(match.call())[-1]
  args$data <- NULL
  args <- purrr::discard(args, is.null)

  existing_facade <- attributes(data)$facade

  if (is.null(existing_facade)) {
    attr(data, "facade") <- args
  } else {
    attr(data, "facade") <- modifyList(existing_facade, args)
  }

  return(data)
}


#' Title
#'
#' @param facade A character string specifying the name of the facade to retrieve. Defaults to "default". The facade is a YAML file that defines the styling and layout of the table
#' @param which A character string specifying the format of the facade to retrieve. Options are "xlsx", "pdf", or "html". Defaults to "xlsx".
#'
#' @returns
#' @export
#'
#' @examples
#'

get_tsg_facade <- function(facade = "default", which = c("xlsx", "pdf", "html")) {

  if(file.exists(facade)) {

    if(grepl("\\.(yaml|yml)$", facade)) {
      return(yaml::read_yaml(facade_path))
    } else if (grepl("\\.json$", facade)) {
      return(jsonlite::read_json(facade_path, simplifyVector = TRUE))
    }
  }

  match.arg(which, several.ok = FALSE)
  facade_path <- system.file("extdata", file.path("facade", which[1], paste0(facade, ".yaml")), package = "tsg")

  if(!file.exists(facade_path)) {
    facade_path <- system.file("extdata", file.path("facade", which[1], "default.yaml"), package = "tsg")
  }

  facade <- yaml::read_yaml(facade_path)
  attr(facade, "source") <- "built-in"
  facade

}


extract_facade <- function(facade, key, which = NULL) {

  facade <- facade |>
    purrr::discard(is.null) |>
    convert_to_nested_list() |>
    purrr::pluck(key)

  if(is.null(which)) {

    facade[
      names(facade) %in% c(
        "fontName",
        "fontSize",
        "fontColour",
        "numFmt",
        "border",
        "borderColour",
        "borderStyle",
        "bgFill",
        "fgFill",
        "halign",
        "valign",
        "textDecoration",
        "wrapText",
        "textRotation",
        "indent",
        "locked",
        "hidden"
      )
    ]
  } else {
    return(facade[[which]])
  }

}


resolve_facade <- function(facade, attrs) {

  facade_source <- attributes(facade)$source

  if(!is.null(attrs)) {
    if(is.null(facade_source)) {
      facade <- modifyList(attrs, facade)
    } else if (facade_source == "built-in") {
      facade <- modifyList(facade, attrs)
    } else {
      facade <- modifyList(get_tsg_facade(), facade)
    }
  }

  facade

}





