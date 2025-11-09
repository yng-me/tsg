# Collapse a list of data frames or tibbles into a single data frame

Collapse a list of data frames or tibbles into a single data frame

## Usage

``` r
collapse_list(
  data,
  ...,
  col_id = "category",
  label = NULL,
  pluck = NULL,
  as_proportion = FALSE,
  name_separator = "_",
  label_separator = "__"
)
```

## Arguments

- data:

  A list of data frames or tibbles to be collapsed.

- ...:

  Additional arguments passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

- col_id:

  The name of the column to be created for the category.

- label:

  A label for the category column. If `NULL`, defaults to "Category".

- pluck:

  A character vector of column names to pluck from the data frames. If
  `NULL`, all columns are retained.

- as_proportion:

  If `TRUE`, the frequency values will be converted to proportions.
  Default is `FALSE`.

- name_separator:

  A string to separate the names of the columns in the output data
  frame. Default is "\_".

- label_separator:

  A string to separate the labels of the columns in the output data
  frame. Default is "\_\_".

## Value

A data frame with the specified category column and the frequency and
percent columns for each category, along with any additional columns
specified in `pluck`.

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
  collapse_list()
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
