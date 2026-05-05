# Write a tsg table (or list of tables) to an HTML file

Saves one or more `tsg` tables as an HTML file using the gt package
(which is already a hard dependency of tsg).

## Usage

``` r
write_html(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = FALSE,
  include_table_list = FALSE,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
)
```

## Arguments

- data:

  A `tsg` or data frame, or a named list of them.

- path:

  File path for the HTML output. A `.html` extension is added if
  missing. When `separate_files = TRUE` the path (minus extension) is
  used as the directory.

- ...:

  Additional arguments passed to
  [`tsg_to_gt()`](https://yng-me.github.io/tsg/reference/tsg_to_gt.md).

- title:

  Optional title string (overrides data attribute).

- subtitle:

  Optional subtitle string.

- source_note:

  Optional source note string.

- footnotes:

  Optional character vector of footnotes.

- separate_files:

  Logical. When `TRUE` and `data` is a list, each table is saved to its
  own HTML file inside a subdirectory derived from `path`.

- include_table_list:

  Logical. When `TRUE` and `data` is a named list with
  `separate_files = FALSE`, prepends a clickable table of contents.

- names_separator:

  Column name separator for spanners. Default `"__"`.

- facade:

  Styling options. Defaults to the global tsg facade.

## Value

Invisibly returns `NULL`.

## Details

When `data` is a named list and `separate_files = FALSE` (default), all
tables are written into a single self-contained HTML document. Set
`include_table_list = TRUE` to prepend a clickable table-of-contents.
Set `separate_files = TRUE` to write one HTML file per table into a
subdirectory.
