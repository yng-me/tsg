# Write a tsg table (or list of tables) to a PDF file

Saves tables as PDF using
[`gt::gtsave()`](https://gt.rstudio.com/reference/gtsave.html), which
requires the webshot2 package (and a Chromium installation reachable by
chromote).

## Usage

``` r
write_pdf(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = TRUE,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
)
```

## Arguments

- data:

  A `tsg` or data frame, or a named list of them.

- path:

  File path for the PDF output. A `.pdf` extension is added if missing.
  When `data` is a list and `separate_files = TRUE`, this is used as a
  directory.

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

  Logical. When `data` is a list, `TRUE` (default) saves each table as a
  separate PDF file inside a subdirectory; `FALSE` merges them into a
  single PDF (requires qpdf).

- names_separator:

  Column name separator for spanners. Default `"__"`.

- facade:

  Styling options. Defaults to the global tsg facade.

## Value

Invisibly returns `NULL`.

## Details

When `data` is a named list, the default (`separate_files = TRUE`)
writes each table to its own `.pdf` file inside a directory. Set
`separate_files = FALSE` to merge all tables into a single PDF (requires
the qpdf package).
