# Convert a tsg table to a flextable object

Internal helper. Builds a `flextable` from a `tsg` data frame, detects
cross-tabulation spanner headers (columns whose display label contains
`names_separator`), and applies basic facade styling (font, header
background, column alignment, decimal precision).

## Usage

``` r
tsg_to_flextable(
  data,
  names_separator = "__",
  facade = get_tsg_facade(which = "docx")
)
```

## Arguments

- data:

  A `tsg` or data frame.

- names_separator:

  Separator string for spanner detection. Default `"__"`.

- facade:

  A facade list. Defaults to the global tsg facade.

## Value

A `flextable` object.
