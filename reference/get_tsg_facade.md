# Get a facade from the package or a file

Get a facade from the package or a file

## Usage

``` r
get_tsg_facade(facade = "default", which = c("xlsx", "pdf", "html"))
```

## Arguments

- facade:

  A character string specifying the name of the facade to retrieve.
  Defaults to "default". The facade is a YAML file that defines the
  styling and layout of the table

- which:

  A character string specifying the format of the facade to retrieve.
  Options are "xlsx", "pdf", or "html". Defaults to "xlsx".

## Value

A list containing the facade settings for the specified format. The
facade includes styling attributes such as font size, color, border
styles, and background fills for different parts of the table.

## Examples

``` r
# Default facade
get_tsg_facade()
#> $table.offsetRow
#> [1] 0
#> 
#> $table.offsetCol
#> [1] 0
#> 
#> $table.gridLines
#> [1] FALSE
#> 
#> $table.tabColour
#> NULL
#> 
#> $table.fontName
#> [1] "Arial"
#> 
#> $table.fontSize
#> [1] 12
#> 
#> $table.fontColour
#> NULL
#> 
#> $table.bgFill
#> NULL
#> 
#> $table.fgFill
#> NULL
#> 
#> $table.halign
#> NULL
#> 
#> $table.valign
#> [1] "center"
#> 
#> $table.wrapText
#> [1] FALSE
#> 
#> $table.indent
#> NULL
#> 
#> $table.locked
#> NULL
#> 
#> $table.hidden
#> NULL
#> 
#> $table.decimalPrecision
#> [1] 2
#> 
#> $table.decimalCols
#> NULL
#> 
#> $table.lastRowBold
#> [1] FALSE
#> 
#> $table.width
#> [1] 14
#> 
#> $table.widthOffset
#> [1] 2
#> 
#> $border_outer.borderColour
#> [1] "#8f8f8f"
#> 
#> $border_header.border
#> [1] "bottom"
#> 
#> $border_header.borderColour
#> [1] "#9f9e9e"
#> 
#> $border_header.borderStyle
#> [1] "medium"
#> 
#> $border_bottom.height
#> [1] 42
#> 
#> $body.fontName
#> NULL
#> 
#> $body.fontSize
#> NULL
#> 
#> $body.fontColour
#> NULL
#> 
#> $body.numFmt
#> [1] "###0"
#> 
#> $body.border
#> [1] "left"  "right"
#> 
#> $body.borderColour
#> [1] "#cfcfcf"
#> 
#> $body.borderStyle
#> [1] "dashed"
#> 
#> $body.bgFill
#> NULL
#> 
#> $body.fgFill
#> NULL
#> 
#> $body.halign
#> NULL
#> 
#> $body.valign
#> [1] "center"
#> 
#> $body.textDecoration
#> NULL
#> 
#> $body.wrapText
#> NULL
#> 
#> $body.indent
#> [1] 1
#> 
#> $body.height
#> [1] 20
#> 
#> $title.fontName
#> NULL
#> 
#> $title.fontSize
#> [1] 13
#> 
#> $title.fontColour
#> NULL
#> 
#> $title.numFmt
#> NULL
#> 
#> $title.border
#> NULL
#> 
#> $title.borderColour
#> NULL
#> 
#> $title.borderStyle
#> NULL
#> 
#> $title.bgFill
#> NULL
#> 
#> $title.fgFill
#> NULL
#> 
#> $title.halign
#> NULL
#> 
#> $title.valign
#> [1] "center"
#> 
#> $title.textDecoration
#> [1] "bold"
#> 
#> $title.wrapText
#> NULL
#> 
#> $title.indent
#> NULL
#> 
#> $title.height
#> [1] 24
#> 
#> $subtitle.fontName
#> NULL
#> 
#> $subtitle.fontSize
#> [1] 12
#> 
#> $subtitle.fontColour
#> NULL
#> 
#> $subtitle.numFmt
#> NULL
#> 
#> $subtitle.border
#> NULL
#> 
#> $subtitle.borderColour
#> NULL
#> 
#> $subtitle.borderStyle
#> NULL
#> 
#> $subtitle.bgFill
#> NULL
#> 
#> $subtitle.fgFill
#> NULL
#> 
#> $subtitle.halign
#> NULL
#> 
#> $subtitle.valign
#> [1] "center"
#> 
#> $subtitle.textDecoration
#> NULL
#> 
#> $subtitle.wrapText
#> NULL
#> 
#> $subtitle.indent
#> NULL
#> 
#> $subtitle.height
#> [1] 22
#> 
#> $header.fontName
#> NULL
#> 
#> $header.fontSize
#> NULL
#> 
#> $header.fontColour
#> NULL
#> 
#> $header.numFmt
#> NULL
#> 
#> $header.border
#> [1] "top"    "bottom" "left"   "right" 
#> 
#> $header.borderColour
#> [1] "#cfcfcf"
#> 
#> $header.borderStyle
#> [1] "dashed"
#> 
#> $header.bgFill
#> NULL
#> 
#> $header.fgFill
#> [1] "#f5f5f5"
#> 
#> $header.halign
#> NULL
#> 
#> $header.valign
#> NULL
#> 
#> $header.textDecoration
#> NULL
#> 
#> $header.wrapText
#> [1] TRUE
#> 
#> $header.indent
#> NULL
#> 
#> $header.height
#> [1] 24
#> 
#> $spanner.fontName
#> NULL
#> 
#> $spanner.fontSize
#> NULL
#> 
#> $spanner.fontColour
#> NULL
#> 
#> $spanner.numFmt
#> NULL
#> 
#> $spanner.border
#> NULL
#> 
#> $spanner.borderColour
#> NULL
#> 
#> $spanner.borderStyle
#> NULL
#> 
#> $spanner.bgFill
#> NULL
#> 
#> $spanner.fgFill
#> NULL
#> 
#> $spanner.halign
#> NULL
#> 
#> $spanner.valign
#> NULL
#> 
#> $spanner.textDecoration
#> NULL
#> 
#> $spanner.wrapText
#> NULL
#> 
#> $spanner.indent
#> NULL
#> 
#> $spanner.height
#> NULL
#> 
#> $col_first.fontName
#> NULL
#> 
#> $col_first.fontSize
#> NULL
#> 
#> $col_first.fontColour
#> NULL
#> 
#> $col_first.numFmt
#> NULL
#> 
#> $col_first.border
#> NULL
#> 
#> $col_first.borderColour
#> NULL
#> 
#> $col_first.borderStyle
#> NULL
#> 
#> $col_first.bgFill
#> NULL
#> 
#> $col_first.fgFill
#> NULL
#> 
#> $col_first.halign
#> NULL
#> 
#> $col_first.valign
#> NULL
#> 
#> $col_first.textDecoration
#> NULL
#> 
#> $col_first.wrapText
#> NULL
#> 
#> $col_first.indent
#> NULL
#> 
#> $col_first.width
#> [1] 24
#> 
#> $col_last.fontName
#> NULL
#> 
#> $col_last.fontSize
#> NULL
#> 
#> $col_last.fontColour
#> NULL
#> 
#> $col_last.numFmt
#> NULL
#> 
#> $col_last.border
#> NULL
#> 
#> $col_last.borderColour
#> NULL
#> 
#> $col_last.borderStyle
#> NULL
#> 
#> $col_last.bgFill
#> NULL
#> 
#> $col_last.fgFill
#> NULL
#> 
#> $col_last.halign
#> NULL
#> 
#> $col_last.valign
#> NULL
#> 
#> $col_last.textDecoration
#> NULL
#> 
#> $col_last.wrapText
#> NULL
#> 
#> $col_last.indent
#> NULL
#> 
#> $col_last.width
#> [1] 14
#> 
#> $row_group.fontName
#> NULL
#> 
#> $row_group.fontSize
#> NULL
#> 
#> $row_group.fontColour
#> NULL
#> 
#> $row_group.numFmt
#> NULL
#> 
#> $row_group.border
#> NULL
#> 
#> $row_group.borderColour
#> NULL
#> 
#> $row_group.borderStyle
#> NULL
#> 
#> $row_group.bgFill
#> NULL
#> 
#> $row_group.fgFill
#> NULL
#> 
#> $row_group.halign
#> NULL
#> 
#> $row_group.valign
#> [1] "center"
#> 
#> $row_group.textDecoration
#> NULL
#> 
#> $row_group.wrapText
#> NULL
#> 
#> $row_group.indent
#> NULL
#> 
#> $row_group.width
#> NULL
#> 
#> $row_group.height
#> NULL
#> 
#> $source_note.fontName
#> [1] 10
#> 
#> $source_note.fontSize
#> NULL
#> 
#> $source_note.fontColour
#> NULL
#> 
#> $source_note.numFmt
#> NULL
#> 
#> $source_note.border
#> NULL
#> 
#> $source_note.borderColour
#> NULL
#> 
#> $source_note.borderStyle
#> NULL
#> 
#> $source_note.bgFill
#> NULL
#> 
#> $source_note.fgFill
#> NULL
#> 
#> $source_note.halign
#> NULL
#> 
#> $source_note.valign
#> [1] "center"
#> 
#> $source_note.textDecoration
#> [1] "italic"
#> 
#> $source_note.wrapText
#> NULL
#> 
#> $source_note.indent
#> NULL
#> 
#> $source_note.height
#> [1] 20
#> 
#> $footnotes.fontName
#> [1] 10
#> 
#> $footnotes.fontSize
#> NULL
#> 
#> $footnotes.fontColour
#> NULL
#> 
#> $footnotes.numFmt
#> NULL
#> 
#> $footnotes.border
#> NULL
#> 
#> $footnotes.borderColour
#> NULL
#> 
#> $footnotes.borderStyle
#> NULL
#> 
#> $footnotes.bgFill
#> NULL
#> 
#> $footnotes.fgFill
#> NULL
#> 
#> $footnotes.halign
#> NULL
#> 
#> $footnotes.valign
#> [1] "center"
#> 
#> $footnotes.textDecoration
#> [1] "italic"
#> 
#> $footnotes.wrapText
#> NULL
#> 
#> $footnotes.indent
#> NULL
#> 
#> $footnotes.height
#> [1] 20
#> 
#> attr(,"source")
#> [1] "built-in"

# Other built-in facade
get_tsg_facade("yolo")
#> $table.offsetRow
#> [1] 1
#> 
#> $table.offsetCol
#> [1] 1
#> 
#> $table.gridLines
#> [1] FALSE
#> 
#> $table.tabColour
#> NULL
#> 
#> $table.fontName
#> [1] "Arial"
#> 
#> $table.fontSize
#> [1] 12
#> 
#> $table.fontColour
#> NULL
#> 
#> $table.bgFill
#> NULL
#> 
#> $table.fgFill
#> NULL
#> 
#> $table.halign
#> NULL
#> 
#> $table.valign
#> [1] "center"
#> 
#> $table.wrapText
#> [1] FALSE
#> 
#> $table.indent
#> NULL
#> 
#> $table.locked
#> NULL
#> 
#> $table.hidden
#> NULL
#> 
#> $table.decimalPrecision
#> [1] 2
#> 
#> $table.decimalCols
#> NULL
#> 
#> $table.lastRowBold
#> [1] FALSE
#> 
#> $table.width
#> [1] 14
#> 
#> $table.widthOffset
#> [1] 2
#> 
#> $border_outer.borderColour
#> [1] "#8f8f8f"
#> 
#> $border_header.border
#> [1] "bottom"
#> 
#> $border_header.borderColour
#> [1] "#9f9e9e"
#> 
#> $border_header.borderStyle
#> [1] "medium"
#> 
#> $border_bottom.height
#> [1] 42
#> 
#> $body.fontName
#> NULL
#> 
#> $body.fontSize
#> NULL
#> 
#> $body.fontColour
#> NULL
#> 
#> $body.numFmt
#> [1] "###0"
#> 
#> $body.border
#> [1] "left"  "right"
#> 
#> $body.borderColour
#> [1] "#cfcfcf"
#> 
#> $body.borderStyle
#> [1] "dashed"
#> 
#> $body.bgFill
#> NULL
#> 
#> $body.fgFill
#> NULL
#> 
#> $body.halign
#> NULL
#> 
#> $body.valign
#> [1] "center"
#> 
#> $body.textDecoration
#> NULL
#> 
#> $body.wrapText
#> NULL
#> 
#> $body.indent
#> [1] 1
#> 
#> $body.height
#> [1] 20
#> 
#> $title.fontName
#> NULL
#> 
#> $title.fontSize
#> [1] 14
#> 
#> $title.fontColour
#> NULL
#> 
#> $title.numFmt
#> NULL
#> 
#> $title.border
#> NULL
#> 
#> $title.borderColour
#> NULL
#> 
#> $title.borderStyle
#> NULL
#> 
#> $title.bgFill
#> NULL
#> 
#> $title.fgFill
#> NULL
#> 
#> $title.halign
#> NULL
#> 
#> $title.valign
#> [1] "center"
#> 
#> $title.textDecoration
#> [1] "bold"
#> 
#> $title.wrapText
#> NULL
#> 
#> $title.indent
#> NULL
#> 
#> $title.height
#> [1] 24
#> 
#> $subtitle.fontName
#> NULL
#> 
#> $subtitle.fontSize
#> [1] 12
#> 
#> $subtitle.fontColour
#> NULL
#> 
#> $subtitle.numFmt
#> NULL
#> 
#> $subtitle.border
#> NULL
#> 
#> $subtitle.borderColour
#> NULL
#> 
#> $subtitle.borderStyle
#> NULL
#> 
#> $subtitle.bgFill
#> NULL
#> 
#> $subtitle.fgFill
#> NULL
#> 
#> $subtitle.halign
#> NULL
#> 
#> $subtitle.valign
#> [1] "center"
#> 
#> $subtitle.textDecoration
#> NULL
#> 
#> $subtitle.wrapText
#> NULL
#> 
#> $subtitle.indent
#> NULL
#> 
#> $subtitle.height
#> [1] 22
#> 
#> $header.fontName
#> NULL
#> 
#> $header.fontSize
#> NULL
#> 
#> $header.fontColour
#> [1] "white"
#> 
#> $header.numFmt
#> NULL
#> 
#> $header.border
#> [1] "top"    "bottom" "left"   "right" 
#> 
#> $header.borderColour
#> [1] "#cfcfcf"
#> 
#> $header.borderStyle
#> NULL
#> 
#> $header.bgFill
#> NULL
#> 
#> $header.fgFill
#> [1] "#000000"
#> 
#> $header.halign
#> [1] "center"
#> 
#> $header.valign
#> NULL
#> 
#> $header.textDecoration
#> NULL
#> 
#> $header.wrapText
#> [1] TRUE
#> 
#> $header.indent
#> NULL
#> 
#> $header.height
#> [1] 24
#> 
#> $spanner.fontName
#> NULL
#> 
#> $spanner.fontSize
#> NULL
#> 
#> $spanner.fontColour
#> NULL
#> 
#> $spanner.numFmt
#> NULL
#> 
#> $spanner.border
#> NULL
#> 
#> $spanner.borderColour
#> NULL
#> 
#> $spanner.borderStyle
#> NULL
#> 
#> $spanner.bgFill
#> NULL
#> 
#> $spanner.fgFill
#> NULL
#> 
#> $spanner.halign
#> NULL
#> 
#> $spanner.valign
#> NULL
#> 
#> $spanner.textDecoration
#> NULL
#> 
#> $spanner.wrapText
#> NULL
#> 
#> $spanner.indent
#> NULL
#> 
#> $spanner.height
#> NULL
#> 
#> $col_first.fontName
#> NULL
#> 
#> $col_first.fontSize
#> NULL
#> 
#> $col_first.fontColour
#> NULL
#> 
#> $col_first.numFmt
#> NULL
#> 
#> $col_first.border
#> NULL
#> 
#> $col_first.borderColour
#> NULL
#> 
#> $col_first.borderStyle
#> NULL
#> 
#> $col_first.bgFill
#> NULL
#> 
#> $col_first.fgFill
#> NULL
#> 
#> $col_first.halign
#> [1] "left"
#> 
#> $col_first.valign
#> NULL
#> 
#> $col_first.textDecoration
#> NULL
#> 
#> $col_first.wrapText
#> NULL
#> 
#> $col_first.indent
#> NULL
#> 
#> $col_first.width
#> [1] 24
#> 
#> $col_last.fontName
#> NULL
#> 
#> $col_last.fontSize
#> NULL
#> 
#> $col_last.fontColour
#> NULL
#> 
#> $col_last.numFmt
#> NULL
#> 
#> $col_last.border
#> NULL
#> 
#> $col_last.borderColour
#> NULL
#> 
#> $col_last.borderStyle
#> NULL
#> 
#> $col_last.bgFill
#> NULL
#> 
#> $col_last.fgFill
#> NULL
#> 
#> $col_last.halign
#> NULL
#> 
#> $col_last.valign
#> NULL
#> 
#> $col_last.textDecoration
#> NULL
#> 
#> $col_last.wrapText
#> NULL
#> 
#> $col_last.indent
#> NULL
#> 
#> $col_last.width
#> [1] 14
#> 
#> $row_group.fontName
#> NULL
#> 
#> $row_group.fontSize
#> NULL
#> 
#> $row_group.fontColour
#> NULL
#> 
#> $row_group.numFmt
#> NULL
#> 
#> $row_group.border
#> NULL
#> 
#> $row_group.borderColour
#> NULL
#> 
#> $row_group.borderStyle
#> NULL
#> 
#> $row_group.bgFill
#> NULL
#> 
#> $row_group.fgFill
#> NULL
#> 
#> $row_group.halign
#> NULL
#> 
#> $row_group.valign
#> [1] "center"
#> 
#> $row_group.textDecoration
#> NULL
#> 
#> $row_group.wrapText
#> NULL
#> 
#> $row_group.indent
#> NULL
#> 
#> $row_group.width
#> NULL
#> 
#> $row_group.height
#> NULL
#> 
#> $source_note.fontName
#> [1] 10
#> 
#> $source_note.fontSize
#> NULL
#> 
#> $source_note.fontColour
#> NULL
#> 
#> $source_note.numFmt
#> NULL
#> 
#> $source_note.border
#> NULL
#> 
#> $source_note.borderColour
#> NULL
#> 
#> $source_note.borderStyle
#> NULL
#> 
#> $source_note.bgFill
#> NULL
#> 
#> $source_note.fgFill
#> NULL
#> 
#> $source_note.halign
#> NULL
#> 
#> $source_note.valign
#> [1] "center"
#> 
#> $source_note.textDecoration
#> [1] "italic"
#> 
#> $source_note.wrapText
#> NULL
#> 
#> $source_note.indent
#> NULL
#> 
#> $source_note.height
#> [1] 20
#> 
#> $footnotes.fontName
#> [1] 10
#> 
#> $footnotes.fontSize
#> NULL
#> 
#> $footnotes.fontColour
#> NULL
#> 
#> $footnotes.numFmt
#> NULL
#> 
#> $footnotes.border
#> NULL
#> 
#> $footnotes.borderColour
#> NULL
#> 
#> $footnotes.borderStyle
#> NULL
#> 
#> $footnotes.bgFill
#> NULL
#> 
#> $footnotes.fgFill
#> NULL
#> 
#> $footnotes.halign
#> NULL
#> 
#> $footnotes.valign
#> [1] "center"
#> 
#> $footnotes.textDecoration
#> [1] "italic"
#> 
#> $footnotes.wrapText
#> NULL
#> 
#> $footnotes.indent
#> NULL
#> 
#> $footnotes.height
#> [1] 20
#> 
#> attr(,"source")
#> [1] "built-in"
```
