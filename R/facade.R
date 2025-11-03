#' Add a facade to a tsg table
#'
#' @param data A tsg table object to which the facade will be added. This is typically a data frame or tibble that has been processed using tsg functions.
#' @param table.offsetRow Row offset of the table
#' @param table.offsetCol Column offset of the table
#' @param table.gridLines Boolean indicating whether to show grid lines in the table
#' @param table.tabColour Color of the table tab in Excel. Hexadecimal color code (e.g., "#FF0000" for red) or a named color (e.g., "red"). Defaults to NULL, which means no tab colour.
#' @param table.fontName Font name for the table text. Defaults to NULL, which means the default font will be used.
#' @param table.fontSize Font size for the table text. Defaults to NULL, which means the default font size will be used.
#' @param table.fontColour Font colour for the table text. Hexadecimal color code (e.g., "#FFFFFF" for white) or a named color (e.g., "white"). Defaults to NULL, which means no specific font colour will be applied.
#' @param table.bgFill Background fill for the table cells. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param table.fgFill Foreground fill for the table cells. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param table.halign Horizontal alignment of the table text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param table.valign Vertical alignment of the table text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param table.wrapText Boolean indicating whether to wrap text in the table cells. Defaults to FALSE, which means text will not be wrapped.
#' @param table.indent Indentation for the table text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param table.locked Boolean indicating whether the table cells should be locked for editing. Defaults to NULL, which means the default lock setting will be used.
#' @param table.hidden Boolean indicating whether the table cells should be hidden. Defaults to NULL, which means the default visibility setting will be used.
#' @param table.decimalPrecision Number of decimal places to display for numeric columns in the table. Defaults to NULL, which means no specific precision will be applied.
#' @param table.decimalCols A character vector specifying which columns should have decimal precision applied. Defaults to NULL, which means no specific columns will be targeted.
#' @param table.lastRowBold Boolean indicating whether the last row of the table should be bold. Defaults to NULL, which means the default setting will be used.
#' @param table.widths A numeric vector specifying the widths of the columns in the table. Defaults to NULL, which means the default column widths will be used.
#' @param table.widthOffset Numeric value indicating the offset to apply to the column widths. Defaults to NULL, which means no offset will be applied.
#' @param title.fontName Font name for the table title. Defaults to NULL, which means the default font will be used.
#' @param title.fontSize Font size for the table title. Defaults to NULL, which means the default font size will be used.
#' @param title.fontColour Font colour for the table title. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param title.border Boolean indicating whether to apply a border to the table title. Defaults to NULL, which means no border will be applied.
#' @param title.borderColour Border colour for the table title. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param title.borderStyle Border style for the table title. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param title.bgFill Background fill for the table title. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param title.fgFill Foreground fill for the table title. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param title.halign Horizontal alignment of the table title text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param title.valign Vertical alignment of the table title text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param title.textDecoration Text decoration for the table title. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param title.wrapText Boolean indicating whether to wrap text in the table title. Defaults to NULL, which means the default setting will be used.
#' @param title.textRotation Text rotation for the table title. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param title.indent Indentation for the table title text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param title.heights A numeric vector specifying the heights of the rows in the table title. Defaults to NULL, which means the default row heights will be used.
#' @param subtitle.fontName Font name for the table subtitle. Defaults to NULL, which means the default font will be used.
#' @param subtitle.fontSize Font size for the table subtitle. Defaults to NULL, which means the default font size will be used.
#' @param subtitle.fontColour Font colour for the table subtitle. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param subtitle.border Boolean indicating whether to apply a border to the table subtitle. Defaults to NULL, which means no border will be applied.
#' @param subtitle.borderColour Border colour for the table subtitle. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param subtitle.borderStyle Border style for the table subtitle. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param subtitle.bgFill Background fill for the table subtitle. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param subtitle.fgFill Foreground fill for the table subtitle. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param subtitle.halign Horizontal alignment of the table subtitle text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param subtitle.valign Vertical alignment of the table subtitle text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param subtitle.textDecoration Text decoration for the table subtitle. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param subtitle.wrapText Boolean indicating whether to wrap text in the table subtitle. Defaults to NULL, which means the default setting will be used.
#' @param subtitle.textRotation Text rotation for the table subtitle. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param subtitle.indent Indentation for the table subtitle text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param subtitle.heights A numeric vector specifying the heights of the rows in the table subtitle. Defaults to NULL, which means the default row heights will be used.
#' @param header.fontName Font name for the table header. Defaults to NULL, which means the default font will be used.
#' @param header.fontSize Font size for the table header. Defaults to NULL, which means the default font size will be used.
#' @param header.fontColour Font colour for the table header. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param header.border Boolean indicating whether to apply a border to the table header. Defaults to NULL, which means no border will be applied.
#' @param header.borderColour Border colour for the table header. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param header.borderStyle Border style for the table header. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param header.bgFill Background fill for the table header. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param header.fgFill Foreground fill for the table header. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param header.halign Horizontal alignment of the table header text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param header.valign Vertical alignment of the table header text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param header.textDecoration Text decoration for the table header. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param header.wrapText Boolean indicating whether to wrap text in the table header. Defaults to NULL, which means the default setting will be used.
#' @param header.textRotation Text rotation for the table header. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param header.indent Indentation for the table header text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param header.heights A numeric vector specifying the heights of the rows in the table header. Defaults to NULL, which means the default row heights will be used.
#' @param spanner.fontName Font name for the table spanner (a spanning header). Defaults to NULL, which means the default font will be used.
#' @param spanner.fontSize Font size for the table spanner. Defaults to NULL, which means the default font size will be used.
#' @param spanner.fontColour Font colour for the table spanner. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param spanner.border Boolean indicating whether to apply a border to the table spanner. Defaults to NULL, which means no border will be applied.
#' @param spanner.borderColour Border colour for the table spanner. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param spanner.borderStyle Border style for the table spanner. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param spanner.bgFill Background fill for the table spanner. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param spanner.fgFill Foreground fill for the table spanner. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param spanner.halign Horizontal alignment of the table spanner text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param spanner.valign Vertical alignment of the table spanner text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param spanner.textDecoration Text decoration for the table spanner. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param spanner.wrapText Boolean indicating whether to wrap text in the table spanner. Defaults to NULL, which means the default setting will be used.
#' @param spanner.textRotation Text rotation for the table spanner. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param spanner.indent Indentation for the table spanner text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param spanner.heights A numeric vector specifying the heights of the rows in the table spanner. Defaults to NULL, which means the default row heights will be used.
#' @param body.fontName Font name for the table body. Defaults to NULL, which means the default font will be used.
#' @param body.fontSize Font size for the table body. Defaults to NULL, which means the default font size will be used.
#' @param body.fontColour Font colour for the table body. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param body.numFmt Number format for the table body. This can be a string representing a number format (e.g., "0.00" for two decimal places). Defaults to NULL, which means no specific number format will be applied.
#' @param body.border Boolean indicating whether to apply a border to the table body. Defaults to NULL, which means no border will be applied.
#' @param body.borderColour Border colour for the table body. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param body.borderStyle Border style for the table body. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param body.bgFill Background fill for the table body. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param body.fgFill Foreground fill for the table body. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param body.halign Horizontal alignment of the table body text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param body.valign Vertical alignment of the table body text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param body.textDecoration Text decoration for the table body. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param body.wrapText Boolean indicating whether to wrap text in the table body. Defaults to NULL, which means the default setting will be used.
#' @param body.textRotation Text rotation for the table body. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param body.indent Indentation for the table body text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param body.heights A numeric vector specifying the heights of the rows in the table body. Defaults to NULL, which means the default row heights will be used.
#' @param col_first.fontName Font name for the first column of the table. Defaults to NULL, which means the default font will be used.
#' @param col_first.fontSize Font size for the first column of the table. Defaults to NULL, which means the default font size will be used.
#' @param col_first.fontColour Font colour for the first column of the table. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param col_first.numFmt Number format for the first column of the table. This can be a string representing a number format (e.g., "0.00" for two decimal places). Defaults to NULL, which means no specific number format will be applied.
#' @param col_first.border Boolean indicating whether to apply a border to the first column of the table. Defaults to NULL, which means no border will be applied.
#' @param col_first.borderColour Border colour for the first column of the table. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param col_first.borderStyle Border style for the first column of the table. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param col_first.bgFill Background fill for the first column of the table. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param col_first.fgFill Foreground fill for the first column of the table. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param col_first.halign Horizontal alignment of the first column text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param col_first.valign Vertical alignment of the first column text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param col_first.textDecoration Text decoration for the first column. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param col_first.wrapText Boolean indicating whether to wrap text in the first column. Defaults to NULL, which means the default setting will be used.
#' @param col_first.textRotation Text rotation for the first column. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param col_first.indent Indentation for the first column text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param col_first.widths A numeric vector specifying the widths of the first column. Defaults to NULL, which means the default column width will be used.
#' @param col_last.fontName Font name for the last column of the table. Defaults to NULL, which means the default font will be used.
#' @param col_last.fontSize Font size for the last column of the table. Defaults to NULL, which means the default font size will be used.
#' @param col_last.fontColour Font colour for the last column of the table. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param col_last.numFmt Number format for the last column of the table. This can be a string representing a number format (e.g., "0.00" for two decimal places). Defaults to NULL, which means no specific number format will be applied.
#' @param col_last.border Boolean indicating whether to apply a border to the last column of the table. Defaults to NULL, which means no border will be applied.
#' @param col_last.borderColour Border colour for the last column of the table. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param col_last.borderStyle Border style for the last column of the table. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param col_last.bgFill Background fill for the last column of the table. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param col_last.fgFill Foreground fill for the last column of the table. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param col_last.halign Horizontal alignment of the last column text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param col_last.valign Vertical alignment of the last column text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param col_last.textDecoration Text decoration for the last column. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param col_last.wrapText Boolean indicating whether to wrap text in the last column. Defaults to NULL, which means the default setting will be used.
#' @param col_last.textRotation Text rotation for the last column. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param col_last.indent Indentation for the last column text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param col_last.widths A numeric vector specifying the widths of the last column. Defaults to NULL, which means the default column width will be used.
#' @param row_group.fontName Font name for the row group header. Defaults to NULL, which means the default font will be used.
#' @param row_group.fontSize Font size for the row group header. Defaults to NULL, which means the default font size will be used.
#' @param row_group.fontColour Font colour for the row group header. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param row_group.border Boolean indicating whether to apply a border to the row group header. Defaults to NULL, which means no border will be applied.
#' @param row_group.borderColour Border colour for the row group header. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param row_group.borderStyle Border style for the row group header. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param row_group.bgFill Background fill for the row group header. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param row_group.fgFill Foreground fill for the row group header. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param row_group.halign Horizontal alignment of the row group header text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param row_group.valign Vertical alignment of the row group header text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param row_group.textDecoration Text decoration for the row group header. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param row_group.wrapText Boolean indicating whether to wrap text in the row group header. Defaults to NULL, which means the default setting will be used.
#' @param row_group.textRotation Text rotation for the row group header. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param row_group.indent Indentation for the row group header text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param row_group.widths A numeric vector specifying the widths of the columns in the row group header. Defaults to NULL, which means the default column widths will be used.
#' @param source_note.fontName Font name for the source note. Defaults to NULL, which means the default font will be used.
#' @param source_note.fontSize Font size for the source note. Defaults to NULL, which means the default font size will be used.
#' @param source_note.fontColour Font colour for the source note. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param source_note.border Boolean indicating whether to apply a border to the source note. Defaults to NULL, which means no border will be applied.
#' @param source_note.borderColour Border colour for the source note. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param source_note.borderStyle Border style for the source note. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param source_note.bgFill Background fill for the source note. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param source_note.fgFill Foreground fill for the source note. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param source_note.halign Horizontal alignment of the source note text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param source_note.valign Vertical alignment of the source note text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param source_note.textDecoration Text decoration for the source note. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param source_note.wrapText Boolean indicating whether to wrap text in the source note. Defaults to NULL, which means the default setting will be used.
#' @param source_note.textRotation Text rotation for the source note. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param source_note.indent Indentation for the source note text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param source_note.heights A numeric vector specifying the heights of the rows in the source note. Defaults to NULL, which means the default row heights will be used.
#' @param footnotes.fontName Font name for the footnotes. Defaults to NULL, which means the default font will be used.
#' @param footnotes.fontSize Font size for the footnotes. Defaults to NULL, which means the default font size will be used.
#' @param footnotes.fontColour Font colour for the footnotes. Hexadecimal color code. Defaults to NULL, which means no specific font colour will be applied.
#' @param footnotes.border Boolean indicating whether to apply a border to the footnotes. Defaults to NULL, which means no border will be applied.
#' @param footnotes.borderColour Border colour for the footnotes. Hexadecimal color code. Defaults to NULL, which means no specific border colour will be applied.
#' @param footnotes.borderStyle Border style for the footnotes. Options are "thin", "medium", "thick", etc. Defaults to NULL, which means no specific border style will be applied.
#' @param footnotes.bgFill Background fill for the footnotes. Hexadecimal color code. Defaults to NULL, which means no specific background fill will be applied.
#' @param footnotes.fgFill Foreground fill for the footnotes. Hexadecimal color code. Defaults to NULL, which means no specific foreground fill will be applied.
#' @param footnotes.halign Horizontal alignment of the footnotes text. Options are "left", "center", or "right". Defaults to NULL, which means the default alignment will be used.
#' @param footnotes.valign Vertical alignment of the footnotes text. Options are "top", "middle", or "bottom". Defaults to NULL, which means the default alignment will be used.
#' @param footnotes.textDecoration Text decoration for the footnotes. Options are "none", "bold", "italic", etc. Defaults to NULL, which means no specific text decoration will be applied.
#' @param footnotes.wrapText Boolean indicating whether to wrap text in the footnotes. Defaults to NULL, which means the default setting will be used.
#' @param footnotes.textRotation Text rotation for the footnotes. Numeric value indicating the angle of rotation in degrees. Defaults to NULL, which means no rotation will be applied.
#' @param footnotes.indent Indentation for the footnotes text. Numeric value indicating the number of spaces to indent. Defaults to NULL, which means no indentation will be applied.
#' @param footnotes.heights A numeric vector specifying the heights of the rows in the footnotes. Defaults to NULL, which means the default row heights will be used.
#'
#' @returns A tsg object with the specified facade settings applied as attributes.
#' @export
#'
#' @examples
#' person_record |>
#'  generate_frequency(sex) |>
#'  add_facade(table.offsetRow = 2, table.offsetCol = 1)

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
    attr(data, "facade") <- utils::modifyList(existing_facade, args)
  }

  return(data)
}


#' Get a facade from the package or a file
#'
#' @param facade A character string specifying the name of the facade to retrieve. Defaults to "default". The facade is a YAML file that defines the styling and layout of the table
#' @param which A character string specifying the format of the facade to retrieve. Options are "xlsx", "pdf", or "html". Defaults to "xlsx".
#'
#' @returns A list containing the facade settings for the specified format. The facade includes styling attributes such as font size, color, border styles, and background fills for different parts of the table.
#' @export
#'
#' @examples
#'
#' # Default facade
#' get_tsg_facade()
#'
#' # Other built-in facade
#' get_tsg_facade("yolo")

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
      facade <- utils::modifyList(attrs, facade)
    } else if (facade_source == "built-in") {
      facade <- utils::modifyList(facade, attrs)
    } else {
      facade <- utils::modifyList(get_tsg_facade(), facade)
    }
  }

  facade

}





