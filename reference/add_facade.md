# Add a facade to a tsg table

This function adds a facade to a `tsg` table object. A facade is a set
of styling options that can be applied to the table to customize its
appearance. For Excel output, see
[`openxlsx::createStyle()`](https://rdrr.io/pkg/openxlsx/man/createStyle.html)
for all valid values.

## Usage

``` r
add_facade(data, ...)
```

## Arguments

- data:

  A tsg table object to which the facade will be added. This is
  typically a data frame or tibble that has been processed using tsg
  functions.

- ...:

  Facade settings as `part.property = value` pairs, e.g.
  `header.fontSize = 12`. Valid keys are those of the default facade
  (see Details); an unknown key is an error. `NULL` values are ignored
  (they do not turn a setting off; use a facade file for that).

## Value

A `tsg` object with the specified facade settings applied as attributes.

## Details

Keys are `part.property`. The parts are `table`, `title`, `subtitle`,
`header`, `spanner` (upper rows of a multi-level header), `body`,
`col_first`, `col_last`, `row_group`, `sub_group`, `source_note`,
`footnotes`, `border_header`, `border_outer`, `border_bottom` and
`label`.

Style properties follow
[`openxlsx::createStyle()`](https://rdrr.io/pkg/openxlsx/man/createStyle.html):
`fontName`, `fontSize`, `fontColour`, `numFmt`, `border`,
`borderColour`, `borderStyle`, `bgFill`, `fgFill`, `halign`, `valign`,
`textDecoration`, `wrapText` and `indent`. `fgFill` is the fill colour
of a cell; `bgFill` is the background of an Excel fill pattern and shows
only behind one (HTML and Word use it when `fgFill` is unset). Colours
are hexadecimal codes (e.g. `"#FF0000"`) or colour names. `border` is
any of `"top"`, `"bottom"`, `"left"` and `"right"`; `borderStyle` uses
the Excel names (`"thin"`, `"medium"`, `"thick"`, `"dashed"`,
`"dotted"`, `"double"`, `"hair"`, `"mediumDashed"`, `"dashDot"`, ...) in
every output format; HTML, PDF and Word translate them to their nearest
line.

The same keys style every output format. Settings that only mean
something in a spreadsheet (`height`, `width`, `table.offsetRow`,
`table.offsetCol`, `table.gridLines`, `table.tabColour`, `table.locked`,
`table.hidden`, `table.widthOffset`, `indent`, `wrapText`) are ignored
by HTML, PDF and Word.

Layout properties:

- `height`: row height in points (`border_bottom.height` is the last
  header row of a multi-level header).

- `width`: column width (`table.width` for all columns,
  `col_first.width` for the category column, `row_group.width` for
  grouping columns before it, `col_last.width` for the last). An unset
  (`~`) `col_first.width` or `row_group.width` fits the column's
  content.

- `table.offsetRow`, `table.offsetCol`: rows and columns left empty
  before the table.

- `table.gridLines`, `table.tabColour`, `table.hidden`: worksheet grid
  lines, tab colour and visibility.

- `table.decimalPrecision`: decimal places for numeric columns with
  fractional values; other numbers are shown as whole numbers with
  thousands separators. The category and grouping columns are not
  formatted.

- `table.decimalCols`: names of the columns to show with decimals (even
  when their values are whole); all other numeric columns are shown as
  whole numbers. The same in Excel, HTML, PDF and Word.

- `table.lastRowBold`: bold last row.

- `border_outer.border`: which outer sides of the table get a border,
  any of `"top"`, `"bottom"`, `"left"` and `"right"`.

- `body.bandFill`: fill colour of every other body row (banded rows);
  off by default.

- `table.widthOffset`: width of the offset columns.

- `label.titleTableList`, `label.backHyperlink`: name of the table-list
  sheet and text of the link back to it.

In a facade file (or a `facade` list passed to a writer), a key set to
`~` (`NULL`) turns that setting off instead of falling back to the
default. In `add_facade()` a `NULL` value is ignored, so it cannot turn
a setting off for one table.

Use `generate_template(path, "facade")` to get a YAML file listing every
key with its default.

## Examples

``` r
person_record |>
 generate_frequency(sex) |>
 add_facade(table.offsetRow = 2, table.offsetCol = 1)
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  
```
