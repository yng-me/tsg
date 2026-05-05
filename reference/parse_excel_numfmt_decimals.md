# Parse decimal places from an Excel number format string

Returns the number of decimal places encoded in the format string, or
`NA_integer_` if the string is not a recognised numeric format.

## Usage

``` r
parse_excel_numfmt_decimals(numfmt)
```

## Arguments

- numfmt:

  A single character string (e.g. `"#,##0.00"`).

## Value

An `integer` scalar, or `NA_integer_`.
