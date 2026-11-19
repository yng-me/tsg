# Add a facade to a tsg table (alternative way)

**\[deprecated\]**

[`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
now evaluates its arguments, so this alternative is no longer needed.

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
#> Warning: `add_facade_alt()` was deprecated in tsg 0.2.0.
#> ℹ Please use `add_facade()` instead.
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  
```
