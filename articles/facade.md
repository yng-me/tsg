# Customizing How Your Tables Look

``` r

library(tsg)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

`tsg` uses a **style** object (called a *facade*) to control how your
tables look when exported — fonts, colours, borders, cell sizes, and
more. You can:

- Use a **built-in style** out of the box,
- **Tweak a few settings** with
  [`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md),
  or
- **Design your own style** by editing a YAML template.

The same style applies consistently to all output formats (Excel, HTML,
PDF, Word).

------------------------------------------------------------------------

## Using a built-in style

The package ships with two named styles for Excel output. Pass one to
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
via the `facade` argument.

| Style name | Description |
|----|----|
| `"default"` | Clean, neutral — used automatically when no style is specified |
| `"yolo"` | Bolder colours and heavier formatting |

``` r

# Bold "yolo" style
person_record |>
  generate_frequency(sex) |>
  write_xlsx(path = "yolo.xlsx", facade = get_tsg_facade("yolo"))

# Explicitly use the default style (same as omitting the facade argument)
person_record |>
  generate_frequency(sex) |>
  write_xlsx(path = "default.xlsx", facade = get_tsg_facade("default"))
```

------------------------------------------------------------------------

## Tweaking a few settings with `add_facade()`

[`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
lets you override individual style settings without replacing the whole
style. Call it in your pipeline before `write_*()`.

Style settings use a two-part name: **which part of the table** `.`
**what to change**. For example:

- `header.bgFill` — background colour of the column header
- `body.fontSize` — font size of the data cells
- `table.offsetRow` — blank rows above the table (Excel only)

``` r

person_record |>
  generate_frequency(sex) |>
  add_facade(
    table.offsetRow = 2,
    table.offsetCol = 1
  )
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100
```

### Common tweaks

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_facade(
    # Move the table down 2 rows and right 1 column (Excel only)
    table.offsetRow        = 2,
    table.offsetCol        = 1,
    # Round numbers to 1 decimal place
    table.decimalPrecision = 1,
    # Make the last row (usually Total) bold
    table.lastRowBold      = TRUE,
    # Header background colour and white text
    header.bgFill          = "#003366",
    header.fontColour      = "#FFFFFF",
    header.textDecoration  = "bold",
    # Data cell font size
    body.fontSize          = 11
  ) |>
  write_xlsx(path = "styled-table.xlsx")
```

> **Good to know:** A few settings only apply to Excel output (such as
> `table.offsetRow`, `table.gridLines`, and `table.tabColour`). They are
> harmlessly ignored when exporting to HTML, Word, or PDF — so you can
> attach them once and export to any format without changing anything.

### Applying the same tweaks across formats

Once you attach style overrides with
[`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md),
those overrides are carried with the table object and applied to
whichever format you export to. You only need to style the table once.

``` r

styled_tbl <- person_record |>
  generate_frequency(sex) |>
  add_table_title("Distribution by Sex") |>
  add_source_note("Source: Survey 2024") |>
  add_facade(
    header.bgFill         = "#003366",
    header.fontColour     = "#FFFFFF",
    header.textDecoration = "bold",
    body.fontSize         = 11,
    table.lastRowBold     = TRUE
  )

write_xlsx(styled_tbl, path = "table.xlsx")
write_html(styled_tbl, path = "table.html")
write_pdf(styled_tbl,  path = "table.pdf")
write_docx(styled_tbl, path = "table.docx")
```

------------------------------------------------------------------------

## Making your own style file

For full control, generate a starter YAML file, edit it to your taste,
and load it back in.

### Step 1 — generate the template

``` r

generate_template("my-style.yaml", template = "facade")
```

### Step 2 — edit the file

Open `my-style.yaml` in any text editor. It looks like this:

``` yaml
table.fontName: 'Arial'
table.fontSize: 12
table.offsetRow: 0
table.offsetCol: 0
table.decimalPrecision: 2
table.lastRowBold: false
table.width: 14

header.bgFill: ~          # ~ means "use the default"
header.fontColour: ~
header.textDecoration: ~
header.height: 28

body.fontSize: ~
body.bgFill: ~
body.numFmt: "###0"
body.height: 20

title.fontSize: 13
title.textDecoration: "bold"
title.height: 24

source_note.fontSize: 10
source_note.textDecoration: "italic"
```

Change any value you like. Use `~` to keep the built-in default for a
setting.

> **Tip:**
> [`get_tsg_facade()`](https://yng-me.github.io/tsg/reference/get_tsg_facade.md)
> also accepts `.json` file paths if you prefer JSON over YAML.

### Step 3 — use your style

``` r

my_style <- get_tsg_facade("my-style.yaml")

person_record |>
  generate_frequency(sex) |>
  write_xlsx(path = "custom-styled.xlsx", facade = my_style)
```

------------------------------------------------------------------------

## Programmatic style overrides

If you need to build style settings dynamically (for example, in a loop
or from a config), use
[`add_facade_alt()`](https://yng-me.github.io/tsg/reference/add_facade_alt.md).
It accepts a named list instead of individual named arguments.

``` r

overrides <- list(
  table.offsetRow   = 3,
  header.bgFill     = "#2E4057",
  header.fontColour = "#FFFFFF",
  body.fontSize     = 11
)

do.call(
  add_facade_alt,
  c(list(data = person_record |> generate_frequency(sex)), overrides)
)
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100
```

------------------------------------------------------------------------

## Style property reference

The table below lists every setting supported by each section of the
table.

| Section | Properties |
|:---|:---|
| table | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, width |
| title | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |
| subtitle | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |
| header | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |
| spanner | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |
| body | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, numFmt, height |
| col_first | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, numFmt, height, width |
| col_last | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, numFmt, height, width |
| row_group | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height, width |
| sub_group | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |
| source_note | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |
| footnotes | fontName, fontSize, fontColour, bgFill, fgFill, halign, valign, textDecoration, wrapText, indent, border, borderColour, borderStyle, height |

### Table-level settings (full list)

| Setting | What it does | Excel | HTML | Word |
|----|----|:--:|:--:|:--:|
| `table.fontName` | Default font family | ✓ | ✓ | ✓ |
| `table.fontSize` | Default font size | ✓ | ✓ | ✓ |
| `table.fontColour` | Default font colour | ✓ | ✓ | ✓ |
| `table.bgFill` | Table background colour | ✓ | ✓ | ✓ |
| `table.decimalPrecision` | Decimal places for numbers | ✓ | ✓ | ✓ |
| `table.decimalCols` | Which columns get decimal formatting | ✓ | ✓ | ✓ |
| `table.lastRowBold` | Make the last row bold | ✓ | ✓ | ✓ |
| `table.offsetRow` | Blank rows above the table | ✓ |  |  |
| `table.offsetCol` | Blank columns to the left | ✓ |  |  |
| `table.gridLines` | Show worksheet grid lines | ✓ |  |  |
| `table.tabColour` | Excel worksheet tab colour | ✓ |  |  |
| `table.locked` | Lock the worksheet | ✓ |  |  |
| `table.hidden` | Hide the worksheet | ✓ |  |  |
| `table.width` | Column width | ✓ |  |  |
| `table.widthOffset` | Extra width added to auto-sized columns | ✓ |  |  |

### Colour values

All `fontColour`, `bgFill`, and `fgFill` settings accept:

- Hex colour codes: `"#003366"`
- Named R colours: `"navy"`
- `~` (in YAML) or `NULL` (in R) to use the inherited default

### Section names

| Section         | What it styles                           |
|-----------------|------------------------------------------|
| `table`         | Global table settings                    |
| `title`         | Title row above the table                |
| `subtitle`      | Subtitle row                             |
| `header`        | Column header row(s)                     |
| `spanner`       | Grouped header rows in cross-tabulations |
| `body`          | Data rows                                |
| `col_first`     | First (category) column                  |
| `col_last`      | Last column                              |
| `row_group`     | Group header rows                        |
| `sub_group`     | Sub-group rows                           |
| `source_note`   | Source note row below the table          |
| `footnotes`     | Footnote rows                            |
| `border_header` | Border under the header                  |
| `border_outer`  | Outer border around the table            |
| `border_bottom` | Bottom border row height                 |
