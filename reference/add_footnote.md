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

  The footnote text to be added.

- locations:

  Locations where the footnote should be applied. Default is NULL
  (applies to entire table).

- placement:

  Placement of the footnote. One of "auto" (default), "right", or
  "left".

## Value

The input data frame with an added footnote attribute.

## Examples

``` r
add_footnote(
  dplyr::starwars,
  footnote = "This is a footnote.",
  locations = c("A1", "B2"),
  placement = "right"
)
#> # A tibble: 87 × 14
#>    name     height  mass hair_color skin_color eye_color birth_year sex   gender
#>    <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#>  1 Luke Sk…    172    77 blond      fair       blue            19   male  mascu…
#>  2 C-3PO       167    75 NA         gold       yellow         112   none  mascu…
#>  3 R2-D2        96    32 NA         white, bl… red             33   none  mascu…
#>  4 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
#>  5 Leia Or…    150    49 brown      light      brown           19   fema… femin…
#>  6 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
#>  7 Beru Wh…    165    75 brown      light      blue            47   fema… femin…
#>  8 R5-D4        97    32 NA         white, red red             NA   none  mascu…
#>  9 Biggs D…    183    84 black      light      brown           24   male  mascu…
#> 10 Obi-Wan…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
#> # ℹ 77 more rows
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>
```
