# Generate a summary table of numeric variables

Generate a summary table of numeric variables

## Usage

``` r
generate_summary(
  data,
  ...,
  stats = c("n", "mean", "sd", "median", "min", "max"),
  group_as_list = FALSE,
  group_separator = " - ",
  metadata = NULL
)
```

## Arguments

- data:

  A data frame (typically `tibble`), or a lazy table (e.g.
  [`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) on a
  database connection), in which case the statistics are computed in the
  database. Group it with
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  to summarise per group.

- ...:

  Numeric variables to summarise (tidyselect). If none are given, every
  numeric column without value labels is used.

- stats:

  Statistics to compute, any of `"n"`, `"n_missing"`, `"mean"`, `"sd"`,
  `"median"`, `"min"`, `"max"`, `"p25"` and `"p75"`. A database that
  cannot compute one (e.g. SQLite has no
  [`quantile()`](https://rdrr.io/r/stats/quantile.html) for `"p25"`)
  gives an error naming it.

- group_as_list:

  Logical. If `TRUE`, returns a list with one table per group.

- group_separator:

  Character. Separator between group values in list names.

- metadata:

  A named list with optional metadata to attach as attributes, e.g.
  `title`, `subtitle`, and `source_note`.

## Value

A table of class `"tsg"` with one row per variable (its label in
`category`) and one column per statistic, or a list of such tables when
`group_as_list = TRUE`.

## Examples

``` r
generate_summary(person_record, age)
#> # A tibble: 1 × 7
#>   category     n  mean    sd median   min   max
#>   <chr>    <int> <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 Age       2918  30.7  21.0     26     0    96

person_record |>
  dplyr::group_by(sex) |>
  generate_summary(age, stats = c("n", "mean", "median"))
#> # A tibble: 2 × 5
#>   sex        category     n  mean median
#>   <int+lbl>  <chr>    <int> <dbl>  <dbl>
#> 1 1 [Male]   Age       1516  30.0     25
#> 2 2 [Female] Age       1402  31.5     27
```
