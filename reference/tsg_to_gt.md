# Convert a tsg table to a gt object

Convert a tsg table to a gt object

## Usage

``` r
tsg_to_gt(
  data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
)
```

## Arguments

- data:

  A `tsg` or data frame object.

- title:

  Optional title string.

- subtitle:

  Optional subtitle string.

- source_note:

  Optional source note string.

- footnotes:

  Optional character vector of footnotes.

- names_separator:

  Column name separator for spanners. Default `"__"`.

- facade:

  A facade list for styling. Defaults to the global tsg facade.

## Value

A `gt_tbl` object.
