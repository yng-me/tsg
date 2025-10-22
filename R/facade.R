get_tsg_facade <- function(facade = "default", which = c("xlsx", "pdf", "html")) {

  match.arg(which, several.ok = FALSE)
  facade_path <- system.file("extdata", file.path("facade", which, paste0(facade, ".yaml")), package = "tsg")

  if(!file.exists(facade_path)) {
    facade_path <- system.file("extdata", file.path("facade", which, "default.yaml"), package = "tsg")
  }

  yaml::read_yaml(facade_path)

}

# TODO
add_facade <- function(
  data,
  gridines = NULL,
  lastRowBold = NULL,
  decimal.precision = NULL,
  height.title = NULL,
  height.subtitle = NULL,
  height.header = NULL,
  height.bottomHeader = NULL,
  height.body = NULL,
  height.sourceNote = NULL,
  height.group = NULL,
  width.offset = NULL,
  width.first = NULL,
  width.last = NULL,
  width.all = NULL,
  style.indent = NULL,
  style.valign = NULL,
  style.border = NULL,
  style.numFmt = NULL,
  style.borderStyle = NULL,
  style.borderColour = NULL,
  style.title.fontSize = NULL,
  style.title.textDecoration = NULL,
  style.subtitle.fontSize = NULL,
  style.subtitle.textDecoration = NULL,
  style.subtitle.valign = NULL,
  style.footnote.fontSize = NULL,
  style.footnote.textDecoration = NULL,
  style.source_note.fontSize = NULL,
  style.source_note.textDecoration = NULL,
  style.source_note.valign = NULL,
  style.header.wrapText = NULL,
  style.header.fgFill = NULL,
  style.header.border = NULL,
  style.header.borderStyle = NULL,
  style.header.borderColour = NULL,
  style.indent.indent = NULL,
  style.indent.valign = NULL,
  style.border_outer.borderColour = NULL,
  style.border_header.border = NULL,
  style.border_header.borderColour = NULL,
  style.border_header.borderStyle = NULL
) {

  args <- as.list(match.call())[-1]

}
