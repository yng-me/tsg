# Write a tsg table (or list of tables) to a Word (.docx) file

Saves one or more `tsg` tables as a Word document using the officer and
flextable packages (both must be installed).

## Usage

``` r
write_docx(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = FALSE,
  names_separator = "__",
  facade = get_tsg_facade(which = "docx")
)
```

## Arguments

- data:

  A `tsg` or data frame, or a named list of them.

- path:

  File path for the Word output. A `.docx` extension is added if
  missing. When `separate_files = TRUE` the path (minus extension) is
  used as the directory.

- ...:

  Currently unused; reserved for future arguments.

- title:

  Optional title string (overrides data attribute).

- subtitle:

  Optional subtitle string.

- source_note:

  Optional source note string.

- footnotes:

  Optional character vector of footnotes.

- separate_files:

  Logical. When `data` is a list, `FALSE` (default) writes all tables
  into a single combined `.docx`; `TRUE` writes one `.docx` per table
  inside a subdirectory.

- names_separator:

  Column name separator for detecting cross-tab spanners. Default
  `"__"`.

- facade:

  A built-in facade name (e.g. `"booktabs"`; see
  [`get_tsg_facade()`](https://yng-me.github.io/tsg/reference/get_tsg_facade.md)),
  the path to a `.yaml` or `.json` facade file, or a facade list. A
  built-in facade looks the same in every output format. Keys missing
  from it fall back to this format's default facade; settings added with
  [`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
  take precedence over it.

## Value

Invisibly returns `NULL`.

## Details

When `data` is a named list and `separate_files = FALSE` (default), all
tables are written into a single `.docx` document, one per page. Set
`separate_files = TRUE` to write one `.docx` file per table inside a
subdirectory derived from `path`.
