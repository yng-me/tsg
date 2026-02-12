# Add a facade to a tsg table (alternative way)

This function adds a facade to a `tsg`, an alternative way to allow
dynamic values and programmatic evaluation.

## Usage

``` r
add_facade_alt(data, ...)
```

## Arguments

- data:

  a `tsg` data frame

- ...:

  List of supported facade items (see
  [`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md))

## Value

A `tsg` object with the specified facade settings applied as attributes.

## Examples

``` r
person_record |>
 generate_frequency(sex) |>
 add_facade_alt(table.offsetRow = 2, table.offsetCol = 1)
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  
```
