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
  trim_sheet_names = FALSE,
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
  (e.g. `creator`) or
  [`openxlsx::addWorksheet()`](https://rdrr.io/pkg/openxlsx/man/addWorksheet.html)
  (e.g. `zoom`), matched by name.

- sheet_name:

  Optional name for the Excel sheet. Ignored if `data` is a list and
  `separate_files = FALSE`.

- trim_sheet_names:

  Logical. Excel limits sheet names to 31 characters. If `FALSE`
  (default), a longer sheet name (from `sheet_name`, a list name or the
  `table_name` of `table_list_reference`) is an error. If `TRUE`, it is
  cut to 31 characters, and names that then repeat are numbered
  (`"... (2)"`). File names (`separate_files = TRUE`) are never cut.

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
  grouped titles. Applies to a grouped list written on one sheet
  (`collapse_list = TRUE`) and to the sheets of a list nested two or
  more groups deep.

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

  A built-in facade name (e.g. `"booktabs"`; see
  [`get_tsg_facade()`](https://yng-me.github.io/tsg/reference/get_tsg_facade.md)),
  the path to a `.yaml` or `.json` facade file, or a facade list. A
  built-in facade looks the same in every output format. Keys missing
  from it fall back to this format's default facade; settings added with
  [`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
  take precedence over it.

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
#> Warning: cannot create file '/tmp/Rtmp8y5fep/file19624537f8a1/starwars_frequency.xlsx', reason 'No such file or directory'
```
