# Write Data to Excel with Titles, Notes, and Styling

Exports a data frame or a list of data frames to one or multiple Excel
files, with support for titles, subtitles, source notes, footnotes,
grouping, and custom styles. It leverages the `openxlsx` package to
create styled Excel reports suitable for presentation.

## Usage

``` r
write_xlsx(
  data,
  path,
  ...,
  sheet_name = NULL,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = FALSE,
  collapse_list = FALSE,
  row_group_as_column = FALSE,
  names_separator = "__",
  include_table_list = FALSE,
  table_list_reference = NULL,
  facade = get_tsg_facade()
)
```

## Arguments

- data:

  A `data.frame`, tibble, or a named `list` of them. When a list is
  provided:

  - If `separate_files = FALSE`, each element is written to a separate
    sheet in one Excel file.

  - If `separate_files = TRUE`, each element is written to its own Excel
    file.

- path:

  A file path (if `separate_files = FALSE`) or directory path (if
  `separate_files = TRUE`) where the Excel file(s) will be saved. File
  extension `.xlsx` is automatically added if missing.

- ...:

  Additional arguments passed to
  [`openxlsx::createWorkbook()`](https://rdrr.io/pkg/openxlsx/man/createWorkbook.html)
  and
  [`openxlsx::addWorksheet()`](https://rdrr.io/pkg/openxlsx/man/addWorksheet.html).

- sheet_name:

  Optional name for the Excel sheet. Ignored if `data` is a list and
  `separate_files = FALSE`.

- title:

  Optional title displayed above the data in each sheet or file.

- subtitle:

  Optional subtitle displayed under the title.

- source_note:

  Optional source note displayed below the data.

- footnotes:

  Optional character vector of footnotes to display below the source
  note.

- separate_files:

  Logical. If `TRUE`, each list item in `data` is saved as a separate
  Excel file.

- collapse_list:

  Logical. If `TRUE`, a list of data frames will be merged into one
  sheet (if applicable).

- row_group_as_column:

  Logical. If `TRUE`, row groupings are included as columns instead of
  grouped titles.

- names_separator:

  Character used to separate column names when dealing with nested or
  grouped headers.

- include_table_list:

  Logical. If `TRUE`, a table list reference is included in the Excel
  file.

- table_list_reference:

  A data frame containing the table list reference. If `NULL`, it will
  be generated from `data`.

- facade:

  A list of styling options (colors, fonts, sizes, border styles, etc.).
  Defaults to the global option `tsg.options.facade`.

## Value

Invisibly returns `NULL`. The function is called for its side-effect of
writing Excel file(s).

## Details

This function supports advanced Excel formatting including:

- Grouped headers

- Dynamic column widths

- Styled titles, subtitles, source notes, and footnotes

- Border styling (inner, outer, header)

The function is designed to handle export needs in professional and
reporting contexts.

## Examples

``` r
data <- tsg::generate_frequency(dplyr::starwars, sex)

dir_to <- tempfile()
write_xlsx(
  data,
  file.path(dir_to, "starwars_frequency.xlsx")
 )
#> Warning: cannot create file '/tmp/RtmpD3SfWK/file18ba5da1e381/starwars_frequency.xlsx', reason 'No such file or directory'
```
