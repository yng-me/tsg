# Add a footnote attribute to a table

Add a footnote attribute to a table

## Usage

``` r
add_footnote(
  data,
  footnote,
  locations = NULL,
  placement = c("auto", "right", "left")
)
```

## Arguments

- data:

  A data frame, tibble, or `tsg` object to which a footnote attribute
  will be added.

- footnote:

  The footnote text to be added (a single character string).

- locations:

  Optional character vector of **column names** to anchor the footnote
  marker. When supplied, a footnote reference symbol is placed in those
  column headers. Column-level anchoring is supported in HTML and PDF
  output (via gt); XLSX and Word output include the text without
  cell-level markers. Default `NULL` renders the footnote as a
  table-level footer with no marker.

- placement:

  Horizontal alignment of the footnote in the output footer. One of
  `"auto"` (default, equivalent to `"left"`), `"right"`, or `"left"`.
  Respected by all three output formats (HTML/PDF, XLSX, Word).

## Value

The input data frame with an updated `footnotes` attribute (a list with
elements `$text`, `$placement`, and `$locations`).

## Examples

``` r
tbl <- person_record |> generate_frequency(sex)

# Whole-table footer, left-aligned (default)
tbl |> add_footnote("Source: National Survey 2023.")
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  

# Right-aligned footer note
tbl |> add_footnote("Weighted estimates.", placement = "right")
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  

# Footnote anchored to a specific column header (HTML/PDF)
tbl |> add_footnote("Unweighted count.", locations = "frequency")
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  
```
