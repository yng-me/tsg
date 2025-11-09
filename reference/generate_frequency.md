# Generate frequency table

Creates frequency tables for one or more categorical variables,
optionally grouped by other variables. The function supports various
enhancements such as sorting, totals, percentages, cumulative
statistics, handling of missing values, and label customization. It
returns a single table or a list of frequency tables.

## Usage

``` r
generate_frequency(
  data,
  ...,
  sort_value = TRUE,
  sort_desc = TRUE,
  sort_except = NULL,
  add_total = TRUE,
  add_percent = TRUE,
  add_cumulative = FALSE,
  add_cumulative_percent = FALSE,
  as_proportion = FALSE,
  include_na = TRUE,
  recode_na = "auto",
  position_total = c("bottom", "top"),
  calculate_per_group = TRUE,
  group_separator = " - ",
  group_as_list = FALSE,
  label_as_group_name = TRUE,
  label_stub = NULL,
  label_na = "Not reported",
  label_total = "Total",
  expand_categories = TRUE,
  convert_factor = FALSE,
  collapse_list = FALSE,
  top_n = NULL,
  top_n_only = FALSE,
  metadata = NULL
)
```

## Arguments

- data:

  A data frame (typically `tibble`) containing the variables to
  summarize.

- ...:

  One or more unquoted variable names (passed via tidy evaluation) for
  which to compute frequency tables.

- sort_value:

  Logical. If `TRUE`, frequency values will be sorted.

- sort_desc:

  Logical. If `TRUE`, sorts in descending order of frequency. If
  `sort_value = FALSE`, the category is sorted in ascending order.

- sort_except:

  Optional character vector. Variables to exclude from sorting.

- add_total:

  Logical. If `TRUE`, adds a total row or value to the frequency table.

- add_percent:

  Logical. If `TRUE`, adds percent or proportion values to the table.

- add_cumulative:

  Logical. If `TRUE`, adds cumulative frequency counts.

- add_cumulative_percent:

  Logical. If `TRUE`, adds cumulative percentages (or proportions if
  `as_proportion = TRUE`).

- as_proportion:

  Logical. If `TRUE`, displays proportions instead of percentages (range
  0–1).

- include_na:

  Logical. If `TRUE`, includes missing values in the frequency table.

- recode_na:

  Character or `NULL`. Value used to replace missing values in labelled
  vectors; `"auto"` will determine a code automatically.

- position_total:

  Character. Where to place the total row: `"top"` or `"bottom"`.

- calculate_per_group:

  Logical. If `TRUE`, calculates frequencies within groups defined in
  `data` (from `group_by()` or existing grouping).

- group_separator:

  Character. Separator used when concatenating group values in list
  output (if `group_as_list = TRUE`).

- group_as_list:

  Logical. If `TRUE`, output is a list of frequency tables for each
  group combination.

- label_as_group_name:

  Logical. If `TRUE`, uses variable labels as names in the output list;
  otherwise, uses variable names.

- label_stub:

  Optional character vector used for labeling output tables (e.g., for
  export or display).

- label_na:

  Character. Label to use for missing (`NA`) values.

- label_total:

  Character. Label used for the total row/category.

- expand_categories:

  Logical. If `TRUE`, ensures all categories (including those with zero
  counts) are included in the output.

- convert_factor:

  Logical. If `TRUE`, converts labelled variables to factors in the
  output. See also
  [`convert_factor()`](https://yng-me.github.io/tsg/reference/convert_factor.md).

- collapse_list:

  Logical. If `TRUE` and `group_as_list = TRUE`, collapses the list of
  frequency tables into a single data frame with group identifiers. See
  also
  [`collapse_list()`](https://yng-me.github.io/tsg/reference/collapse_list.md).

- top_n:

  Integer or `NULL`. If specified, limits the output to the top `n`
  categories by frequency.

- top_n_only:

  Logical. If `TRUE` and `top_n` is specified, only the top `n`
  categories are included, excluding others.

- metadata:

  A named list with optional metadata to attach as attributes, e.g.
  `title`, `subtitle`, and `source_note`.

## Value

A frequency table (`tibble`, possibly nested) or a list of such tables.
Additional attributes such as labels, metadata, and grouping information
may be attached. The returned object is of class `"tsg"`.

## See also

[`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md),
[`generate_output()`](https://yng-me.github.io/tsg/reference/generate_output.md),
[`rename_label()`](https://yng-me.github.io/tsg/reference/rename_label.md),
[`remove_label()`](https://yng-me.github.io/tsg/reference/remove_label.md)

## Examples

``` r
# Using built-in dataset `person_record`


# Basic usage
person_record |>
 generate_frequency(sex)
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  

# Multiple variables
person_record |>
  generate_frequency(sex, age, marital_status)
#> $Sex
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100  
#> 
#> $Age
#> # A tibble: 96 × 3
#>    category frequency percent
#>    <chr>        <int>   <dbl>
#>  1 16              82    2.81
#>  2 15              75    2.57
#>  3 12              74    2.54
#>  4 13              70    2.40
#>  5 20              68    2.33
#>  6 14              66    2.26
#>  7 19              66    2.26
#>  8 11              61    2.09
#>  9 24              61    2.09
#> 10 18              59    2.02
#> # ℹ 86 more rows
#> 
#> $`Marital status`
#> # A tibble: 6 × 3
#>   category                 frequency percent
#>   <int+lbl>                    <int>   <dbl>
#> 1 1 [Single/never married]      1544   52.9 
#> 2 2 [Married]                    769   26.4 
#> 3 3 [Common law/live-in]         424   14.5 
#> 4 4 [Widowed]                    138    4.73
#> 5 6 [Separated]                   43    1.47
#> 6 0 [Total]                     2918  100   
#> 
#> attr(,"class")
#> [1] "tsg"  "tsgf" "list"

# Grouping
person_record |>
  dplyr::group_by(sex) |>
  generate_frequency(marital_status)
#> # A tibble: 12 × 4
#>    sex        category                 frequency percent
#>    <int+lbl>  <int+lbl>                    <int>   <dbl>
#>  1 1 [Male]   1 [Single/never married]       859   56.7 
#>  2 1 [Male]   2 [Married]                    387   25.5 
#>  3 1 [Male]   3 [Common law/live-in]         211   13.9 
#>  4 1 [Male]   4 [Widowed]                     40    2.64
#>  5 1 [Male]   6 [Separated]                   19    1.25
#>  6 1 [Male]   0 [Total]                     1516  100   
#>  7 2 [Female] 1 [Single/never married]       685   48.9 
#>  8 2 [Female] 2 [Married]                    382   27.2 
#>  9 2 [Female] 3 [Common law/live-in]         213   15.2 
#> 10 2 [Female] 4 [Widowed]                     98    6.99
#> 11 2 [Female] 6 [Separated]                   24    1.71
#> 12 2 [Female] 0 [Total]                     1402  100   

# Output group as list
person_record |>
  dplyr::group_by(sex) |>
  generate_frequency(marital_status, group_as_list = TRUE)
#> $Male
#> # A tibble: 6 × 4
#>   sex       category                 frequency percent
#>   <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  1 [Single/never married]       859   56.7 
#> 2 1 [Male]  2 [Married]                    387   25.5 
#> 3 1 [Male]  3 [Common law/live-in]         211   13.9 
#> 4 1 [Male]  4 [Widowed]                     40    2.64
#> 5 1 [Male]  6 [Separated]                   19    1.25
#> 6 1 [Male]  0 [Total]                     1516  100   
#> 
#> $Female
#> # A tibble: 6 × 4
#>   sex        category                 frequency percent
#>   <int+lbl>  <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 1 [Single/never married]       685   48.9 
#> 2 2 [Female] 2 [Married]                    382   27.2 
#> 3 2 [Female] 3 [Common law/live-in]         213   15.2 
#> 4 2 [Female] 4 [Widowed]                     98    6.99
#> 5 2 [Female] 6 [Separated]                   24    1.71
#> 6 2 [Female] 0 [Total]                     1402  100   
#> 
#> attr(,"groups")
#> [1] "sex"
#> attr(,"group_attrs")
#> attr(,"group_attrs")$sex
#> attr(,"group_attrs")$sex$labels
#>   Male Female 
#>      1      2 
#> 
#> attr(,"group_attrs")$sex$label
#> [1] "Sex"
#> 
#> attr(,"group_attrs")$sex$class
#> [1] "haven_labelled" "vctrs_vctr"     "integer"       
#> 
#> 
#> attr(,"class")
#> [1] "tsg"  "tsgf" "list"

# Sorting

# default is TRUE
person_record |>
  generate_frequency(age, sort_value = TRUE)
#> # A tibble: 96 × 3
#>    category frequency percent
#>    <chr>        <int>   <dbl>
#>  1 16              82    2.81
#>  2 15              75    2.57
#>  3 12              74    2.54
#>  4 13              70    2.40
#>  5 20              68    2.33
#>  6 14              66    2.26
#>  7 19              66    2.26
#>  8 11              61    2.09
#>  9 24              61    2.09
#> 10 18              59    2.02
#> # ℹ 86 more rows

# If FALSE, the output will be sorted by the variable values in ascending order.
person_record |>
  generate_frequency(age, sort_value = FALSE)
#> # A tibble: 96 × 3
#>    category frequency percent
#>    <chr>        <int>   <dbl>
#>  1 0               32    1.10
#>  2 1               42    1.44
#>  3 2               44    1.51
#>  4 3               41    1.41
#>  5 4               44    1.51
#>  6 5               54    1.85
#>  7 6               44    1.51
#>  8 7               47    1.61
#>  9 8               56    1.92
#> 10 9               48    1.64
#> # ℹ 86 more rows

# Vignettes for more examples.
```
