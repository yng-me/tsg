# Remove all labels

Remove all labels

## Usage

``` r
remove_labels(data, ...)
```

## Arguments

- data:

  A data frame or tibble from which to remove all labels.

- ...:

  A character vector of column names from which to remove labels. If no
  columns are specified, all labels will be removed.

## Value

A data frame or tibble with all labels removed. If no columns are
specified, all labels will be removed.

## Examples

``` r
person_record |>
  generate_frequency(
    seeing,
    hearing,
    walking,
    remembering,
    self_caring,
    communicating
  ) |>
  collapse_list() |>
  remove_labels()
#> # A tibble: 6 × 13
#>   category           frequency_0 frequency_1 frequency_2 frequency_3 frequency_4
#>   <chr>                    <int>       <int>       <int>       <int>       <int>
#> 1 Seeing, even if w…        2918        2623          82           9           1
#> 2 Hearing, even if …        2918        2666          43           5           1
#> 3 Walking or climbi…        2918        2658          48           7           2
#> 4 Remembering or co…        2918        2680          28           7           0
#> 5 Self-caring (such…        2918        2671          37           5           2
#> 6 Communicating usi…        2918        2683          25           5           2
#> # ℹ 7 more variables: frequency_9 <int>, percent_0 <dbl>, percent_1 <dbl>,
#> #   percent_2 <dbl>, percent_3 <dbl>, percent_4 <dbl>, percent_9 <dbl>
```
