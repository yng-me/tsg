#' Add a facade to a tsg table
#'
#' @description This function adds a facade to a \code{tsg} table object. A facade is a set of styling options that can be applied to the table to customize its appearance. For Excel output, see [openxlsx::createStyle()] for all valid values.
#'
#' @param data A tsg table object to which the facade will be added. This is typically a data frame or tibble that has been processed using tsg functions.
#' @param ... Facade settings as \code{part.property = value} pairs, e.g. \code{header.fontSize = 12}. Valid keys are those of the default facade (see Details); an unknown key is an error. \code{NULL} values are ignored (they do not turn a setting off; use a facade file for that).
#'
#' @details
#' Keys are \code{part.property}. The parts are \code{table}, \code{title}, \code{subtitle}, \code{header}, \code{spanner} (upper rows of a multi-level header), \code{body}, \code{col_first}, \code{col_last}, \code{row_group}, \code{sub_group}, \code{source_note}, \code{footnotes}, \code{border_header}, \code{border_outer}, \code{border_bottom} and \code{label}.
#'
#' Style properties follow [openxlsx::createStyle()]: \code{fontName}, \code{fontSize}, \code{fontColour}, \code{numFmt}, \code{border}, \code{borderColour}, \code{borderStyle}, \code{bgFill}, \code{fgFill}, \code{halign}, \code{valign}, \code{textDecoration}, \code{wrapText} and \code{indent}. \code{fgFill} is the fill colour of a cell; \code{bgFill} is the background of an Excel fill pattern and shows only behind one (HTML and Word use it when \code{fgFill} is unset). Colours are hexadecimal codes (e.g. \code{"#FF0000"}) or colour names. \code{border} is any of \code{"top"}, \code{"bottom"}, \code{"left"} and \code{"right"}; \code{borderStyle} uses the Excel names (\code{"thin"}, \code{"medium"}, \code{"thick"}, \code{"dashed"}, \code{"dotted"}, \code{"double"}, \code{"hair"}, \code{"mediumDashed"}, \code{"dashDot"}, ...) in every output format; HTML, PDF and Word translate them to their nearest line.
#'
#' The same keys style every output format. Settings that only mean something in a spreadsheet (\code{height}, \code{width}, \code{table.offsetRow}, \code{table.offsetCol}, \code{table.gridLines}, \code{table.tabColour}, \code{table.locked}, \code{table.hidden}, \code{table.widthOffset}, \code{indent}, \code{wrapText}) are ignored by HTML, PDF and Word.
#'
#' Layout properties:
#' \itemize{
#'   \item \code{height}: row height in points (\code{border_bottom.height} is the last header row of a multi-level header).
#'   \item \code{width}: column width (\code{table.width} for all columns, \code{col_first.width} for the category column, \code{row_group.width} for grouping columns before it, \code{col_last.width} for the last). An unset (\code{~}) \code{col_first.width} or \code{row_group.width} fits the column's content.
#'   \item \code{table.offsetRow}, \code{table.offsetCol}: rows and columns left empty before the table.
#'   \item \code{table.gridLines}, \code{table.tabColour}, \code{table.hidden}: worksheet grid lines, tab colour and visibility.
#'   \item \code{table.decimalPrecision}: decimal places for numeric columns with fractional values; other numbers are shown as whole numbers with thousands separators. The category and grouping columns are not formatted.
#'   \item \code{table.decimalCols}: names of the columns to show with decimals (even when their values are whole); all other numeric columns are shown as whole numbers. The same in Excel, HTML, PDF and Word.
#'   \item \code{table.lastRowBold}: bold last row.
#'   \item \code{border_outer.border}: which outer sides of the table get a border, any of \code{"top"}, \code{"bottom"}, \code{"left"} and \code{"right"}.
#'   \item \code{body.bandFill}: fill colour of every other body row (banded rows); off by default.
#'   \item \code{table.widthOffset}: width of the offset columns.
#'   \item \code{label.titleTableList}, \code{label.backHyperlink}: name of the table-list sheet and text of the link back to it.
#' }
#'
#' In a facade file (or a \code{facade} list passed to a writer), a key set to \code{~} (\code{NULL}) turns that setting off instead of falling back to the default. In \code{add_facade()} a \code{NULL} value is ignored, so it cannot turn a setting off for one table.
#'
#' Use \code{generate_template(path, "facade")} to get a YAML file listing every key with its default.
#'
#' @returns A \code{tsg} object with the specified facade settings applied as attributes.
#' @export
#'
#' @examples
#' person_record |>
#'  generate_frequency(sex) |>
#'  add_facade(table.offsetRow = 2, table.offsetCol = 1)

add_facade <- function(data, ...) {

  args <- list(...)
  keys <- names(args)

  if(length(args) > 0 & (is.null(keys) | any(keys == ""))) {
    stop("All facade settings must be named, e.g. `header.fontSize = 12`.")
  }

  unknown <- setdiff(keys, names(get_tsg_facade()))
  if(length(unknown) > 0) {
    stop("Unknown facade setting(s): ", paste0("`", unknown, "`", collapse = ", "))
  }

  merge_facade_attr(data, args)
}


#' Get a facade from the package or a file
#'
#' @param facade A character string specifying the name of the facade to retrieve. Defaults to "default". Built-in facades: \code{"default"}, \code{"yolo"}, \code{"apa"} (horizontal rules only, Times New Roman), \code{"booktabs"} (thick top and bottom rules, thin rule under the header), \code{"striped"} (coloured header, banded rows) \code{"gridded"} (thin lines around every cell), \code{"minimal"} (no borders or fills), \code{"financial"} (dark blue header, grid, bold last row), \code{"nyt"} (editorial, banded), \code{"jama"} (medical journal, horizontal rules), \code{"dark"} (light text on dark), \code{"pastel"} (soft blue accents) and \code{"census"} (grey header, small italic notes). An unknown name is an error. Or the path to a \code{.yaml} or \code{.json} facade file.
#'
#' Every built-in facade is one file shared by all output formats, so a table looks the same in Excel, HTML, PDF and Word. Only \code{"default"} differs per format; the other facades are laid over that format's default. Spreadsheet-only settings (row heights, column widths, grid lines, tab colour, sheet offsets, cell locking) are ignored outside Excel.
#' @param which The output format whose default facade to use: \code{"xlsx"} (default), \code{"html"}, \code{"pdf"} (same as \code{"html"}) or \code{"docx"}. It only changes the result for \code{"default"}.
#'
#' @returns A list containing the facade settings for the specified format. The facade includes styling attributes such as font size, color, border styles, and background fills for different parts of the table.
#' @export
#'
#' @examples
#'
#' # Default facade
#' get_tsg_facade()
#'
#' # Other built-in facades
#' get_tsg_facade("yolo")
#' get_tsg_facade("booktabs")

get_tsg_facade <- function(facade = "default", which = c("xlsx", "html", "docx", "pdf")) {

  if(fs::is_file(facade) && file.exists(facade)) {
    if(grepl("\\.(yaml|yml)$", facade)) {
      return(yaml::read_yaml(facade))
    } else if (grepl("\\.json$", facade)) {
      return(jsonlite::read_json(facade, simplifyVector = TRUE))
    }
  }

  which <- match.arg(which)
  # PDF is rendered from the gt (html) table, so it shares the html default facade.
  if(which == "pdf") which <- "html"

  # One file per built-in facade serves every format (adr/0002); only the default differs.
  facade_dir <- system.file("extdata", "facade", package = "tsg")
  facade_path <- if(identical(facade, "default")) {
    file.path(facade_dir, "default", paste0(which, ".yaml"))
  } else {
    file.path(facade_dir, paste0(facade, ".yaml"))
  }

  if(!file.exists(facade_path)) {
    names <- c("default", sub("\\.yaml$", "", list.files(facade_dir, pattern = "\\.yaml$")))
    stop(
      "Unknown facade `", facade, "`. Built-in facades: ", paste(names, collapse = ", "),
      ". Or give the path to a .yaml or .json facade file.",
      call. = FALSE
    )
  }

  # Built-in facades never change in a session; write_xlsx() resolves one per table.
  facade_cache[[facade_path]] <- facade_cache[[facade_path]] %||% yaml::read_yaml(facade_path)
  facade_cache[[facade_path]]
}

facade_cache <- new.env(parent = emptyenv())


#' Add a facade to a tsg table (alternative way)
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' [add_facade()] now evaluates its arguments, so this alternative is no longer needed.
#'
#' @param data a \code{tsg} data frame
#' @param ... List of supported facade items (see [tsg::add_facade()])
#'
#' @returns A \code{tsg} object with the specified facade settings applied as attributes.
#' @export
#'
#' @examples
#' person_record |>
#'  generate_frequency(sex) |>
#'  add_facade_alt(table.offsetRow = 2, table.offsetCol = 1)

add_facade_alt <- function(data, ...) {
  lifecycle::deprecate_soft("0.2.0", "add_facade_alt()", "add_facade()")
  add_facade(data, ...)
}


merge_facade_attr <- function(data, args) {
  args <- purrr::discard(args, is.null)
  attr(data, "facade") <- utils::modifyList(attr(data, "facade") %||% list(), args)
  data
}


extract_facade <- function(facade, key, which = NULL) {

  prefix <- paste0(key, ".")
  keys <- names(facade)[startsWith(names(facade), prefix)]
  props <- purrr::discard(stats::setNames(facade[keys], substring(keys, nchar(prefix) + 1)), is.null)

  if(!is.null(which)) {
    return(props[[which]])
  }

  props[
    names(props) %in% c(
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
}


# Precedence, least to most specific: built-in default < facade given to the writer
# < add_facade() on the list < add_facade() on the table.
# A key set to ~ (NULL) turns that setting off; it is kept as NULL rather than
# dropped (as modifyList() would), so resolving again cannot bring the default back.
resolve_facade <- function(facade, attrs = NULL, which = "xlsx") {
  out <- get_tsg_facade(which = which)
  # A built-in facade name or a .yaml/.json path, as well as a list.
  if(is.character(facade)) facade <- get_tsg_facade(facade, which = which)
  for(layer in list(facade, attrs)) {
    out[names(layer)] <- layer
  }
  out
}


# Excel border style names (the facade's vocabulary, adr/0002) as a CSS line style and
# a width in px; the docx writer converts px to pt. CSS has no dash-dot, so those dash.
border_lines <- list(
  thin = c("solid", 1), hair = c("solid", 1), medium = c("solid", 2), thick = c("solid", 3),
  dashed = c("dashed", 1), mediumDashed = c("dashed", 2), dotted = c("dotted", 1),
  double = c("double", 3), dashDot = c("dashed", 1), mediumDashDot = c("dashed", 2),
  dashDotDot = c("dashed", 1), mediumDashDotDot = c("dashed", 2), slantDashDot = c("dashed", 2)
)

# The borders `part.border` asks for, as list(<side> = list(style, width, color)), for
# writers other than xlsx. Sides may be a vector or openxlsx's joined form ("TopBottom");
# borderStyle and borderColour pair with the sides in order, as in openxlsx::createStyle().
facade_borders <- function(facade, part) {
  sides <- facade[[paste0(part, ".border")]]
  if (length(sides) == 0) return(list())

  sides <- tolower(unlist(strsplit(gsub("([a-z])([A-Z])", "\\1 \\2", sides), " ")))
  sides <- intersect(sides, c("top", "bottom", "left", "right"))
  styles <- rep_len(facade[[paste0(part, ".borderStyle")]] %||% "thin", length(sides))
  colours <- rep_len(facade[[paste0(part, ".borderColour")]] %||% "black", length(sides))

  unknown <- setdiff(styles, c("none", names(border_lines)))
  if (length(unknown) > 0) {
    stop(
      "Unknown `", part, ".borderStyle`: ", paste0("\"", unknown, "\"", collapse = ", "),
      ". Use one of: ", paste0("\"", names(border_lines), "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }

  keep <- styles != "none"
  stats::setNames(
    purrr::map2(styles[keep], colours[keep], function(style, colour) {
      line <- border_lines[[style]]
      list(style = line[[1]], width = as.numeric(line[[2]]), color = colour)
    }),
    sides[keep]
  )
}
