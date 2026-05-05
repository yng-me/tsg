# Grouped Tables and Side-by-Side Comparisons

``` r

library(tsg)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

This vignette covers three common scenarios that go beyond a single
basic table:

1.  **Get a separate table for each group** — when you need one table
    per region, sex, or other category.
2.  **Compare several indicators side by side** — when you have multiple
    related columns and want them all in one compact table.
3.  **Export many tables at once** — when your report has dozens of
    tables and you want to manage them efficiently.

We will use the `person_record` sample dataset throughout.

------------------------------------------------------------------------

## Get a separate table for each group

By default, grouping with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
produces a single merged table with the group labels in the category
column. If you want **one independent table per group instead**, add
`group_as_list = TRUE`.

``` r

person_record |>
  group_by(sex) |>
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
```

With **two grouping variables**, the result is automatically nested —
you get a list of lists:

``` r

person_record |>
  filter(age >= 15) |> 
  group_by(sex, employed) |>
  generate_frequency(marital_status, group_as_list = TRUE)
#> $Male
#> $Male$Yes
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  1 [Yes]   1 [Single/never married]       127   21.2 
#> 2 1 [Male]  1 [Yes]   2 [Married]                    274   45.7 
#> 3 1 [Male]  1 [Yes]   3 [Common law/live-in]         169   28.2 
#> 4 1 [Male]  1 [Yes]   4 [Widowed]                     18    3.01
#> 5 1 [Male]  1 [Yes]   6 [Separated]                   11    1.84
#> 6 1 [Male]  1 [Yes]   0 [Total]                      599  100   
#> 
#> $Male$No
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  2 [No]    1 [Single/never married]       330   65.0 
#> 2 1 [Male]  2 [No]    2 [Married]                    108   21.3 
#> 3 1 [Male]  2 [No]    3 [Common law/live-in]          41    8.07
#> 4 1 [Male]  2 [No]    4 [Widowed]                     21    4.13
#> 5 1 [Male]  2 [No]    6 [Separated]                    8    1.57
#> 6 1 [Male]  2 [No]    0 [Total]                      508  100   
#> 
#> 
#> $Female
#> $Female$Yes
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 1 [Yes]   1 [Single/never married]        63   19.5 
#> 2 2 [Female] 1 [Yes]   2 [Married]                    155   48.0 
#> 3 2 [Female] 1 [Yes]   3 [Common law/live-in]          67   20.7 
#> 4 2 [Female] 1 [Yes]   4 [Widowed]                     26    8.05
#> 5 2 [Female] 1 [Yes]   6 [Separated]                   12    3.72
#> 6 2 [Female] 1 [Yes]   0 [Total]                      323  100   
#> 
#> $Female$No
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 2 [No]    1 [Single/never married]       234   34.5 
#> 2 2 [Female] 2 [No]    2 [Married]                    218   32.2 
#> 3 2 [Female] 2 [No]    3 [Common law/live-in]         142   20.9 
#> 4 2 [Female] 2 [No]    4 [Widowed]                     72   10.6 
#> 5 2 [Female] 2 [No]    6 [Separated]                   12    1.77
#> 6 2 [Female] 2 [No]    0 [Total]                      678  100   
#> 
#> 
#> attr(,"groups")
#> [1] "sex"      "employed"
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
#> attr(,"group_attrs")$employed
#> attr(,"group_attrs")$employed$labels
#> Yes  No 
#>   1   2 
#> 
#> attr(,"group_attrs")$employed$label
#> [1] "Employment status"
#> 
#> attr(,"group_attrs")$employed$class
#> [1] "haven_labelled" "vctrs_vctr"     "integer"       
#> 
#> 
#> attr(,"class")
#> [1] "tsg"  "tsgf" "list"
```

The same works with
[`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md):

``` r

person_record |>
  filter(age >= 15) |> 
  group_by(sex) |>
  generate_crosstab(marital_status, employed, group_as_list = TRUE)
#> $Male
#> # A tibble: 6 × 9
#>   sex       category        total frequency_1 frequency_2 frequency_NA percent_1
#>   <int+lbl> <int+lbl>       <int>       <int>       <int>        <int>     <dbl>
#> 1 1 [Male]  1 [Single/neve…   463         127         330            6      27.4
#> 2 1 [Male]  2 [Married]       386         274         108            4      71.0
#> 3 1 [Male]  3 [Common law/…   211         169          41            1      80.1
#> 4 1 [Male]  4 [Widowed]        40          18          21            1      45  
#> 5 1 [Male]  6 [Separated]      19          11           8            0      57.9
#> 6 1 [Male]  0 [Total]        1119         599         508           12      53.5
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
#> 
#> $Female
#> # A tibble: 6 × 9
#>   sex        category       total frequency_1 frequency_2 frequency_NA percent_1
#>   <int+lbl>  <int+lbl>      <int>       <int>       <int>        <int>     <dbl>
#> 1 2 [Female] 1 [Single/nev…   304          63         234            7      20.7
#> 2 2 [Female] 2 [Married]      382         155         218            9      40.6
#> 3 2 [Female] 3 [Common law…   213          67         142            4      31.5
#> 4 2 [Female] 4 [Widowed]       98          26          72            0      26.5
#> 5 2 [Female] 6 [Separated]     24          12          12            0      50  
#> 6 2 [Female] 0 [Total]       1021         323         678           20      31.6
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
#> 
#> attr(,"groups")
#> [1] "sex"
#> attr(,"label_separator")
#> [1] "__"
#> attr(,"class")
#> [1] "tsg"  "tsgc" "list"
```

------------------------------------------------------------------------

## Add a grand total to grouped tables

When you want to include an “All groups combined” summary alongside the
per-group breakdowns, use `group_as_hierarchy = TRUE`.

### Flat table with total rows inserted

Without `group_as_list`, `group_as_hierarchy = TRUE` inserts a
grand-total row at each group boundary in the flat output:

``` r

person_record |>
  group_by(sex) |>
  generate_frequency(marital_status, group_as_hierarchy = TRUE)
#> # A tibble: 18 × 4
#>    sex        category                 frequency percent
#>    <int+lbl>  <int+lbl>                    <int>   <dbl>
#>  1 0 [All]    1 [Single/never married]      1544   52.9 
#>  2 0 [All]    2 [Married]                    769   26.4 
#>  3 0 [All]    3 [Common law/live-in]         424   14.5 
#>  4 0 [All]    4 [Widowed]                    138    4.73
#>  5 0 [All]    6 [Separated]                   43    1.47
#>  6 0 [All]    0 [Total]                     2918  100   
#>  7 1 [Male]   1 [Single/never married]       859   56.7 
#>  8 1 [Male]   2 [Married]                    387   25.5 
#>  9 1 [Male]   3 [Common law/live-in]         211   13.9 
#> 10 1 [Male]   4 [Widowed]                     40    2.64
#> 11 1 [Male]   6 [Separated]                   19    1.25
#> 12 1 [Male]   0 [Total]                     1516  100   
#> 13 2 [Female] 1 [Single/never married]       685   48.9 
#> 14 2 [Female] 2 [Married]                    382   27.2 
#> 15 2 [Female] 3 [Common law/live-in]         213   15.2 
#> 16 2 [Female] 4 [Widowed]                     98    6.99
#> 17 2 [Female] 6 [Separated]                   24    1.71
#> 18 2 [Female] 0 [Total]                     1402  100
```

### Separate tables with a total entry per level

Combine `group_as_list = TRUE` and `group_as_hierarchy = TRUE` to get a
nested list where each level includes a special total entry. The total
key is labelled with the variable name and the `label_group_hierarchy`
setting (default: `"All"`).

``` r

person_record |>
  group_by(sex) |>
  generate_frequency(
    marital_status,
    group_as_list      = TRUE,
    group_as_hierarchy = TRUE
  )
#> $`Sex: All`
#> # A tibble: 6 × 4
#>   sex       category                 frequency percent
#>   <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 0         1 [Single/never married]      1544   52.9 
#> 2 0         2 [Married]                    769   26.4 
#> 3 0         3 [Common law/live-in]         424   14.5 
#> 4 0         4 [Widowed]                    138    4.73
#> 5 0         6 [Separated]                   43    1.47
#> 6 0         0 [Total]                     2918  100   
#> 
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
```

This scales to two grouping variables for a fully nested hierarchy:

``` r

person_record |>
  filter(age >= 15) |> 
  group_by(sex, employed) |>
  generate_frequency(
    marital_status,
    group_as_list      = TRUE,
    group_as_hierarchy = TRUE
  )
#> $`Sex: All`
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 0         0         1 [Single/never married]       767   35.8 
#> 2 0         0         2 [Married]                    768   35.9 
#> 3 0         0         3 [Common law/live-in]         424   19.8 
#> 4 0         0         4 [Widowed]                    138    6.45
#> 5 0         0         6 [Separated]                   43    2.01
#> 6 0         0         0 [Total]                     2140  100   
#> 
#> $Male
#> $Male$`Employment status: All`
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  0         1 [Single/never married]       463   41.4 
#> 2 1 [Male]  0         2 [Married]                    386   34.5 
#> 3 1 [Male]  0         3 [Common law/live-in]         211   18.9 
#> 4 1 [Male]  0         4 [Widowed]                     40    3.57
#> 5 1 [Male]  0         6 [Separated]                   19    1.70
#> 6 1 [Male]  0         0 [Total]                     1119  100   
#> 
#> $Male$Yes
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  1 [Yes]   1 [Single/never married]       127   21.2 
#> 2 1 [Male]  1 [Yes]   2 [Married]                    274   45.7 
#> 3 1 [Male]  1 [Yes]   3 [Common law/live-in]         169   28.2 
#> 4 1 [Male]  1 [Yes]   4 [Widowed]                     18    3.01
#> 5 1 [Male]  1 [Yes]   6 [Separated]                   11    1.84
#> 6 1 [Male]  1 [Yes]   0 [Total]                      599  100   
#> 
#> $Male$No
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  2 [No]    1 [Single/never married]       330   65.0 
#> 2 1 [Male]  2 [No]    2 [Married]                    108   21.3 
#> 3 1 [Male]  2 [No]    3 [Common law/live-in]          41    8.07
#> 4 1 [Male]  2 [No]    4 [Widowed]                     21    4.13
#> 5 1 [Male]  2 [No]    6 [Separated]                    8    1.57
#> 6 1 [Male]  2 [No]    0 [Total]                      508  100   
#> 
#> 
#> $Female
#> $Female$`Employment status: All`
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 0         1 [Single/never married]       304   29.8 
#> 2 2 [Female] 0         2 [Married]                    382   37.4 
#> 3 2 [Female] 0         3 [Common law/live-in]         213   20.9 
#> 4 2 [Female] 0         4 [Widowed]                     98    9.60
#> 5 2 [Female] 0         6 [Separated]                   24    2.35
#> 6 2 [Female] 0         0 [Total]                     1021  100   
#> 
#> $Female$Yes
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 1 [Yes]   1 [Single/never married]        63   19.5 
#> 2 2 [Female] 1 [Yes]   2 [Married]                    155   48.0 
#> 3 2 [Female] 1 [Yes]   3 [Common law/live-in]          67   20.7 
#> 4 2 [Female] 1 [Yes]   4 [Widowed]                     26    8.05
#> 5 2 [Female] 1 [Yes]   6 [Separated]                   12    3.72
#> 6 2 [Female] 1 [Yes]   0 [Total]                      323  100   
#> 
#> $Female$No
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 2 [No]    1 [Single/never married]       234   34.5 
#> 2 2 [Female] 2 [No]    2 [Married]                    218   32.2 
#> 3 2 [Female] 2 [No]    3 [Common law/live-in]         142   20.9 
#> 4 2 [Female] 2 [No]    4 [Widowed]                     72   10.6 
#> 5 2 [Female] 2 [No]    6 [Separated]                   12    1.77
#> 6 2 [Female] 2 [No]    0 [Total]                      678  100   
#> 
#> 
#> attr(,"groups")
#> [1] "sex"      "employed"
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
#> attr(,"group_attrs")$employed
#> attr(,"group_attrs")$employed$labels
#> Yes  No 
#>   1   2 
#> 
#> attr(,"group_attrs")$employed$label
#> [1] "Employment status"
#> 
#> attr(,"group_attrs")$employed$class
#> [1] "haven_labelled" "vctrs_vctr"     "integer"       
#> 
#> 
#> attr(,"class")
#> [1] "tsg"  "tsgf" "list"
```

### Change the total label

Use `label_group_hierarchy` to rename the `"All"` label. Pass a single
string to use the same label everywhere, or a **named vector** to set a
different label per grouping variable:

``` r

person_record |>
  group_by(sex) |>
  generate_frequency(
    marital_status,
    group_as_hierarchy    = TRUE,
    label_group_hierarchy = "Grand Total"
  )
#> # A tibble: 18 × 4
#>    sex             category                 frequency percent
#>    <int+lbl>       <int+lbl>                    <int>   <dbl>
#>  1 0 [Grand Total] 1 [Single/never married]      1544   52.9 
#>  2 0 [Grand Total] 2 [Married]                    769   26.4 
#>  3 0 [Grand Total] 3 [Common law/live-in]         424   14.5 
#>  4 0 [Grand Total] 4 [Widowed]                    138    4.73
#>  5 0 [Grand Total] 6 [Separated]                   43    1.47
#>  6 0 [Grand Total] 0 [Total]                     2918  100   
#>  7 1 [Male]        1 [Single/never married]       859   56.7 
#>  8 1 [Male]        2 [Married]                    387   25.5 
#>  9 1 [Male]        3 [Common law/live-in]         211   13.9 
#> 10 1 [Male]        4 [Widowed]                     40    2.64
#> 11 1 [Male]        6 [Separated]                   19    1.25
#> 12 1 [Male]        0 [Total]                     1516  100   
#> 13 2 [Female]      1 [Single/never married]       685   48.9 
#> 14 2 [Female]      2 [Married]                    382   27.2 
#> 15 2 [Female]      3 [Common law/live-in]         213   15.2 
#> 16 2 [Female]      4 [Widowed]                     98    6.99
#> 17 2 [Female]      6 [Separated]                   24    1.71
#> 18 2 [Female]      0 [Total]                     1402  100
```

``` r

person_record |>
  filter(age >= 15) |>
  group_by(sex, employed) |>
  generate_frequency(
    marital_status,
    group_as_list         = TRUE,
    group_as_hierarchy    = TRUE,
    label_group_hierarchy = c(sex = "All sexes", employed = "All workers")
  )
#> $`Sex: All sexes`
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 0         0         1 [Single/never married]       767   35.8 
#> 2 0         0         2 [Married]                    768   35.9 
#> 3 0         0         3 [Common law/live-in]         424   19.8 
#> 4 0         0         4 [Widowed]                    138    6.45
#> 5 0         0         6 [Separated]                   43    2.01
#> 6 0         0         0 [Total]                     2140  100   
#> 
#> $Male
#> $Male$`Employment status: All workers`
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  0         1 [Single/never married]       463   41.4 
#> 2 1 [Male]  0         2 [Married]                    386   34.5 
#> 3 1 [Male]  0         3 [Common law/live-in]         211   18.9 
#> 4 1 [Male]  0         4 [Widowed]                     40    3.57
#> 5 1 [Male]  0         6 [Separated]                   19    1.70
#> 6 1 [Male]  0         0 [Total]                     1119  100   
#> 
#> $Male$Yes
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  1 [Yes]   1 [Single/never married]       127   21.2 
#> 2 1 [Male]  1 [Yes]   2 [Married]                    274   45.7 
#> 3 1 [Male]  1 [Yes]   3 [Common law/live-in]         169   28.2 
#> 4 1 [Male]  1 [Yes]   4 [Widowed]                     18    3.01
#> 5 1 [Male]  1 [Yes]   6 [Separated]                   11    1.84
#> 6 1 [Male]  1 [Yes]   0 [Total]                      599  100   
#> 
#> $Male$No
#> # A tibble: 6 × 5
#>   sex       employed  category                 frequency percent
#>   <int+lbl> <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 1 [Male]  2 [No]    1 [Single/never married]       330   65.0 
#> 2 1 [Male]  2 [No]    2 [Married]                    108   21.3 
#> 3 1 [Male]  2 [No]    3 [Common law/live-in]          41    8.07
#> 4 1 [Male]  2 [No]    4 [Widowed]                     21    4.13
#> 5 1 [Male]  2 [No]    6 [Separated]                    8    1.57
#> 6 1 [Male]  2 [No]    0 [Total]                      508  100   
#> 
#> 
#> $Female
#> $Female$`Employment status: All workers`
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 0         1 [Single/never married]       304   29.8 
#> 2 2 [Female] 0         2 [Married]                    382   37.4 
#> 3 2 [Female] 0         3 [Common law/live-in]         213   20.9 
#> 4 2 [Female] 0         4 [Widowed]                     98    9.60
#> 5 2 [Female] 0         6 [Separated]                   24    2.35
#> 6 2 [Female] 0         0 [Total]                     1021  100   
#> 
#> $Female$Yes
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 1 [Yes]   1 [Single/never married]        63   19.5 
#> 2 2 [Female] 1 [Yes]   2 [Married]                    155   48.0 
#> 3 2 [Female] 1 [Yes]   3 [Common law/live-in]          67   20.7 
#> 4 2 [Female] 1 [Yes]   4 [Widowed]                     26    8.05
#> 5 2 [Female] 1 [Yes]   6 [Separated]                   12    3.72
#> 6 2 [Female] 1 [Yes]   0 [Total]                      323  100   
#> 
#> $Female$No
#> # A tibble: 6 × 5
#>   sex        employed  category                 frequency percent
#>   <int+lbl>  <int+lbl> <int+lbl>                    <int>   <dbl>
#> 1 2 [Female] 2 [No]    1 [Single/never married]       234   34.5 
#> 2 2 [Female] 2 [No]    2 [Married]                    218   32.2 
#> 3 2 [Female] 2 [No]    3 [Common law/live-in]         142   20.9 
#> 4 2 [Female] 2 [No]    4 [Widowed]                     72   10.6 
#> 5 2 [Female] 2 [No]    6 [Separated]                   12    1.77
#> 6 2 [Female] 2 [No]    0 [Total]                      678  100   
#> 
#> 
#> attr(,"groups")
#> [1] "sex"      "employed"
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
#> attr(,"group_attrs")$employed
#> attr(,"group_attrs")$employed$labels
#> Yes  No 
#>   1   2 
#> 
#> attr(,"group_attrs")$employed$label
#> [1] "Employment status"
#> 
#> attr(,"group_attrs")$employed$class
#> [1] "haven_labelled" "vctrs_vctr"     "integer"       
#> 
#> 
#> attr(,"class")
#> [1] "tsg"  "tsgf" "list"
```

The same arguments work with
[`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md):

``` r

person_record |>
  filter(age >= 15) |> 
  group_by(sex) |>
  generate_crosstab(
    marital_status,
    employed,
    group_as_list      = TRUE,
    group_as_hierarchy = TRUE
  )
#> $`Sex: All`
#> # A tibble: 6 × 9
#>   sex    category total frequency_1 frequency_2 frequency_NA percent_1 percent_2
#>   <int+> <int+lb> <int>       <int>       <int>        <int>     <dbl>     <dbl>
#> 1 0      1 [Sing…   767         190         564           13      24.8      73.5
#> 2 0      2 [Marr…   768         429         326           13      55.9      42.4
#> 3 0      3 [Comm…   424         236         183            5      55.7      43.2
#> 4 0      4 [Wido…   138          44          93            1      31.9      67.4
#> 5 0      6 [Sepa…    43          23          20            0      53.5      46.5
#> 6 0      0 [Tota…  2140         922        1186           32      43.1      55.4
#> # ℹ 1 more variable: percent_NA <dbl>
#> 
#> $Male
#> # A tibble: 6 × 9
#>   sex       category        total frequency_1 frequency_2 frequency_NA percent_1
#>   <int+lbl> <int+lbl>       <int>       <int>       <int>        <int>     <dbl>
#> 1 1 [Male]  1 [Single/neve…   463         127         330            6      27.4
#> 2 1 [Male]  2 [Married]       386         274         108            4      71.0
#> 3 1 [Male]  3 [Common law/…   211         169          41            1      80.1
#> 4 1 [Male]  4 [Widowed]        40          18          21            1      45  
#> 5 1 [Male]  6 [Separated]      19          11           8            0      57.9
#> 6 1 [Male]  0 [Total]        1119         599         508           12      53.5
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
#> 
#> $Female
#> # A tibble: 6 × 9
#>   sex        category       total frequency_1 frequency_2 frequency_NA percent_1
#>   <int+lbl>  <int+lbl>      <int>       <int>       <int>        <int>     <dbl>
#> 1 2 [Female] 1 [Single/nev…   304          63         234            7      20.7
#> 2 2 [Female] 2 [Married]      382         155         218            9      40.6
#> 3 2 [Female] 3 [Common law…   213          67         142            4      31.5
#> 4 2 [Female] 4 [Widowed]       98          26          72            0      26.5
#> 5 2 [Female] 6 [Separated]     24          12          12            0      50  
#> 6 2 [Female] 0 [Total]       1021         323         678           20      31.6
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
#> 
#> attr(,"groups")
#> [1] "sex"
#> attr(,"label_separator")
#> [1] "__"
#> attr(,"class")
#> [1] "tsg"  "tsgc" "list"
```

------------------------------------------------------------------------

## Compare several Yes/No indicators side by side

`multiple_columns = TRUE` lets you cross-tabulate a row variable against
**multiple indicator columns at once**. Instead of a separate table for
each indicator, all results appear in a single wide table — each
indicator becomes its own column group.

This is particularly useful for survey modules where several questions
share the same response scale. In `person_record`, the functional
difficulty columns (`seeing`, `hearing`, `walking`, etc.) use a scale
where **1 = No difficulty**, **2 = Some difficulty**, **3 = A lot of
difficulty**, and **4 = Cannot do it at all**. The
`multiple_columns_filter` argument controls which response value to
count (default: `1L`).

### Basic usage

The example below counts respondents who reported **“Some difficulty”**
(value `2`) in each domain, broken down by `sex`:

``` r

person_record |>
  generate_crosstab(
    sex,
    seeing,
    hearing,
    walking,
    remembering,
    self_caring,
    communicating,
    multiple_columns        = TRUE,
    multiple_columns_filter = 2L
  )
#> # A tibble: 3 × 14
#>   category   total frequency_seeing frequency_hearing frequency_walking
#>   <int+lbl>  <int>            <int>             <int>             <int>
#> 1 1 [Male]    1516               40                23                27
#> 2 2 [Female]  1402               42                20                21
#> 3 0 [Total]   2918               82                43                48
#> # ℹ 9 more variables: frequency_remembering <int>, frequency_self_caring <int>,
#> #   frequency_communicating <int>, percent_seeing <dbl>, percent_hearing <dbl>,
#> #   percent_walking <dbl>, percent_remembering <dbl>,
#> #   percent_self_caring <dbl>, percent_communicating <dbl>
```

### Count a different response level

Change `multiple_columns_filter` to target any response level:

``` r

person_record |>
  generate_crosstab(
    sex,
    seeing,
    hearing,
    multiple_columns        = TRUE,
    multiple_columns_filter = 3L   # "A lot of difficulty"
  )
```

### Combining with grouping

All grouping options work with `multiple_columns`. Use
`calculate_per_group = TRUE` to compute percentages independently within
each group:

``` r

person_record |>
  group_by(marital_status) |>
  generate_crosstab(
    sex,
    seeing,
    hearing,
    walking,
    multiple_columns        = TRUE,
    multiple_columns_filter = 2L,
    calculate_per_group     = TRUE
  )
#> # A tibble: 15 × 9
#>    marital_status           category   total frequency_seeing frequency_hearing
#>    <int+lbl>                <int+lbl>  <int>            <int>             <int>
#>  1 2 [Married]              1 [Male]     387               25                11
#>  2 2 [Married]              2 [Female]   382               19                 7
#>  3 2 [Married]              0 [Total]    769               44                18
#>  4 1 [Single/never married] 1 [Male]     859                6                 7
#>  5 1 [Single/never married] 2 [Female]   685                6                 5
#>  6 1 [Single/never married] 0 [Total]   1544               12                12
#>  7 3 [Common law/live-in]   1 [Male]     211                2                 0
#>  8 3 [Common law/live-in]   2 [Female]   213                4                 1
#>  9 3 [Common law/live-in]   0 [Total]    424                6                 1
#> 10 4 [Widowed]              1 [Male]      40                6                 5
#> 11 4 [Widowed]              2 [Female]    98               13                 7
#> 12 4 [Widowed]              0 [Total]    138               19                12
#> 13 6 [Separated]            1 [Male]      19                1                 0
#> 14 6 [Separated]            2 [Female]    24                0                 0
#> 15 6 [Separated]            0 [Total]     43                1                 0
#> # ℹ 4 more variables: frequency_walking <int>, percent_seeing <dbl>,
#> #   percent_hearing <dbl>, percent_walking <dbl>
```

Use `group_as_list = TRUE` to get a separate table per group:

``` r

person_record |>
  group_by(marital_status) |>
  generate_crosstab(
    sex,
    seeing,
    hearing,
    walking,
    multiple_columns        = TRUE,
    multiple_columns_filter = 2L,
    group_as_list           = TRUE
  )
#> $Married
#> # A tibble: 3 × 9
#>   marital_status category   total frequency_seeing frequency_hearing
#>   <int+lbl>      <int+lbl>  <int>            <int>             <int>
#> 1 2 [Married]    1 [Male]     387               25                11
#> 2 2 [Married]    2 [Female]   382               19                 7
#> 3 2 [Married]    0 [Total]    769               44                18
#> # ℹ 4 more variables: frequency_walking <int>, percent_seeing <dbl>,
#> #   percent_hearing <dbl>, percent_walking <dbl>
#> 
#> $`Single/never married`
#> # A tibble: 3 × 9
#>   marital_status           category   total frequency_seeing frequency_hearing
#>   <int+lbl>                <int+lbl>  <int>            <int>             <int>
#> 1 1 [Single/never married] 1 [Male]     859                6                 7
#> 2 1 [Single/never married] 2 [Female]   685                6                 5
#> 3 1 [Single/never married] 0 [Total]   1544               12                12
#> # ℹ 4 more variables: frequency_walking <int>, percent_seeing <dbl>,
#> #   percent_hearing <dbl>, percent_walking <dbl>
#> 
#> $`Common law/live-in`
#> # A tibble: 3 × 9
#>   marital_status         category   total frequency_seeing frequency_hearing
#>   <int+lbl>              <int+lbl>  <int>            <int>             <int>
#> 1 3 [Common law/live-in] 1 [Male]     211                2                 0
#> 2 3 [Common law/live-in] 2 [Female]   213                4                 1
#> 3 3 [Common law/live-in] 0 [Total]    424                6                 1
#> # ℹ 4 more variables: frequency_walking <int>, percent_seeing <dbl>,
#> #   percent_hearing <dbl>, percent_walking <dbl>
#> 
#> $Widowed
#> # A tibble: 3 × 9
#>   marital_status category   total frequency_seeing frequency_hearing
#>   <int+lbl>      <int+lbl>  <int>            <int>             <int>
#> 1 4 [Widowed]    1 [Male]      40                6                 5
#> 2 4 [Widowed]    2 [Female]    98               13                 7
#> 3 4 [Widowed]    0 [Total]    138               19                12
#> # ℹ 4 more variables: frequency_walking <int>, percent_seeing <dbl>,
#> #   percent_hearing <dbl>, percent_walking <dbl>
#> 
#> $Separated
#> # A tibble: 3 × 9
#>   marital_status category   total frequency_seeing frequency_hearing
#>   <int+lbl>      <int+lbl>  <int>            <int>             <int>
#> 1 6 [Separated]  1 [Male]      19                1                 0
#> 2 6 [Separated]  2 [Female]    24                0                 0
#> 3 6 [Separated]  0 [Total]     43                1                 0
#> # ℹ 4 more variables: frequency_walking <int>, percent_seeing <dbl>,
#> #   percent_hearing <dbl>, percent_walking <dbl>
#> 
#> attr(,"label_separator")
#> [1] "__"
#> attr(,"name_separator")
#> [1] "_"
#> attr(,"multiple_columns")
#> [1] TRUE
#> attr(,"multiple_columns_filter")
#> [1] 2
#> attr(,"groups")
#> [1] "marital_status"
#> attr(,"class")
#> [1] "tsg"  "tsgc" "list"
```

------------------------------------------------------------------------

## Full comparison table with hierarchical columns

Setting `multiple_columns_type = "stacked"` changes the layout
fundamentally: instead of filtering for a single response value, **every
category of every column variable becomes its own column**. The column
headers form a hierarchy — the first `...` variable at the top level,
the second at the next level, and so on.

This mode is ideal when you want a complete cross-product view: every
combination of `marital_status × sex` as separate columns, all in one
table.

> `multiple_columns_filter` is ignored in stacked mode — all categories
> appear automatically.

### Basic stacked table

``` r

person_record |>
  generate_crosstab(
    age,
    marital_status,
    sex,
    multiple_columns      = TRUE,
    multiple_columns_type = "stacked"
  )
#> # A tibble: 96 × 32
#>    category total frequency_1 frequency_1_1 frequency_1_2 frequency_2
#>    <chr>    <dbl>       <dbl>         <dbl>         <dbl>       <dbl>
#>  1 0           32          32            19            13           0
#>  2 1           42          42            24            18           0
#>  3 2           44          44            20            24           0
#>  4 3           41          41            18            23           0
#>  5 4           44          44            22            22           0
#>  6 5           54          54            22            32           0
#>  7 6           44          44            22            22           0
#>  8 7           47          47            27            20           0
#>  9 8           56          56            30            26           0
#> 10 9           48          48            28            20           0
#> # ℹ 86 more rows
#> # ℹ 26 more variables: frequency_2_1 <dbl>, frequency_2_2 <dbl>,
#> #   frequency_3 <dbl>, frequency_3_1 <dbl>, frequency_3_2 <dbl>,
#> #   frequency_4 <dbl>, frequency_4_1 <dbl>, frequency_4_2 <dbl>,
#> #   frequency_5 <dbl>, frequency_5_1 <dbl>, frequency_5_2 <dbl>,
#> #   percent_1 <dbl>, percent_1_1 <dbl>, percent_1_2 <dbl>, percent_2 <dbl>,
#> #   percent_2_1 <dbl>, percent_2_2 <dbl>, percent_3 <dbl>, percent_3_1 <dbl>, …
```

The column structure is: - A **subtotal column** for each top-level
category (e.g., all respondents in each marital status group) - **Leaf
columns** for each combination (e.g., single males, single females,
married males, …) - All **frequency** columns come first, then all
**percent** columns — this keeps the Excel column spanners clean

### Custom label separator

Use `label_separator` to control how the hierarchy levels are joined in
column labels. This also determines how
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
splits labels into multi-row header spanners in Excel.

``` r

person_record |>
  generate_crosstab(
    age,
    marital_status,
    sex,
    multiple_columns      = TRUE,
    multiple_columns_type = "stacked",
    label_separator       = " | ",
    add_percent           = FALSE
  )
#> # A tibble: 96 × 17
#>    category total frequency_1 frequency_1_1 frequency_1_2 frequency_2
#>    <chr>    <dbl>       <dbl>         <dbl>         <dbl>       <dbl>
#>  1 0           32          32            19            13           0
#>  2 1           42          42            24            18           0
#>  3 2           44          44            20            24           0
#>  4 3           41          41            18            23           0
#>  5 4           44          44            22            22           0
#>  6 5           54          54            22            32           0
#>  7 6           44          44            22            22           0
#>  8 7           47          47            27            20           0
#>  9 8           56          56            30            26           0
#> 10 9           48          48            28            20           0
#> # ℹ 86 more rows
#> # ℹ 11 more variables: frequency_2_1 <dbl>, frequency_2_2 <dbl>,
#> #   frequency_3 <dbl>, frequency_3_1 <dbl>, frequency_3_2 <dbl>,
#> #   frequency_4 <dbl>, frequency_4_1 <dbl>, frequency_4_2 <dbl>,
#> #   frequency_5 <dbl>, frequency_5_1 <dbl>, frequency_5_2 <dbl>
```

### Three or more column variables

Add more column variables to create deeper hierarchies. Each additional
variable adds another level of column splitting:

``` r

person_record |>
  generate_crosstab(
    age,
    marital_status,
    sex,
    seeing,
    multiple_columns      = TRUE,
    multiple_columns_type = "stacked",
    add_percent           = FALSE
  )
#> # A tibble: 96 × 34
#>    category total frequency_1 frequency_1_1_1 frequency_1_1_2 frequency_1_1_3
#>    <chr>    <dbl>       <dbl>           <dbl>           <dbl>           <dbl>
#>  1 0           32          32               0               0               0
#>  2 1           42          42               0               0               0
#>  3 2           44          44               0               0               0
#>  4 3           41          41               0               0               0
#>  5 4           44          44               0               0               0
#>  6 5           54          54              21               1               0
#>  7 6           44          44              22               0               0
#>  8 7           47          47              25               2               0
#>  9 8           56          56              29               1               0
#> 10 9           48          48              27               1               0
#> # ℹ 86 more rows
#> # ℹ 28 more variables: frequency_1_1_5 <dbl>, frequency_1_2_1 <dbl>,
#> #   frequency_1_2_2 <dbl>, frequency_1_2_5 <dbl>, frequency_2 <dbl>,
#> #   frequency_2_1_1 <dbl>, frequency_2_1_2 <dbl>, frequency_2_1_3 <dbl>,
#> #   frequency_2_2_1 <dbl>, frequency_2_2_2 <dbl>, frequency_2_2_3 <dbl>,
#> #   frequency_3 <dbl>, frequency_3_1_1 <dbl>, frequency_3_1_2 <dbl>,
#> #   frequency_3_2_1 <dbl>, frequency_3_2_2 <dbl>, frequency_3_2_3 <dbl>, …
```

### Combining with grouping

Stacked mode supports all grouping options:

``` r

person_record |>
  filter(age >= 15) |>
  group_by(employed) |>
  generate_crosstab(
    marital_status,
    sex,
    seeing,
    multiple_columns      = TRUE,
    multiple_columns_type = "stacked",
    calculate_per_group   = TRUE,
    add_percent           = FALSE
  )
#> # A tibble: 17 × 12
#>    employed category total frequency_1 frequency_1_1 frequency_1_2 frequency_1_3
#>    <int+lb> <int+lb> <dbl>       <dbl>         <dbl>         <dbl>         <dbl>
#>  1  1 [Yes] 1 [Sing…   190         127           126             0             1
#>  2  1 [Yes] 2 [Marr…   429         274           266             8             0
#>  3  1 [Yes] 3 [Comm…   236         169           168             1             0
#>  4  1 [Yes] 4 [Wido…    44          18            17             1             0
#>  5  1 [Yes] 6 [Sepa…    23          11            11             0             0
#>  6  1 [Yes] 0 [Tota…   922         599           588            10             1
#>  7  2 [No]  1 [Sing…   564         330           329             1             0
#>  8  2 [No]  2 [Marr…   326         108            89            17             2
#>  9  2 [No]  3 [Comm…   183          41            40             1             0
#> 10  2 [No]  4 [Wido…    93          21            16             5             0
#> 11  2 [No]  6 [Sepa…    20           8             7             1             0
#> 12  2 [No]  0 [Tota…  1186         508           481            25             2
#> 13 NA       1 [Sing…    13           6             6             0             0
#> 14 NA       2 [Marr…    13           4             4             0             0
#> 15 NA       3 [Comm…     5           1             1             0             0
#> 16 NA       4 [Wido…     1           1             1             0             0
#> 17 NA       0 [Tota…    32          12            12             0             0
#> # ℹ 5 more variables: frequency_2 <dbl>, frequency_2_1 <dbl>,
#> #   frequency_2_2 <dbl>, frequency_2_3 <dbl>, frequency_2_4 <dbl>
```

------------------------------------------------------------------------

## Export many tables to one Excel file

When your analysis produces many tables, combine them into a named list
and pass the whole list to
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md).
Each list element becomes a separate worksheet.

### Basic multi-sheet export

``` r

tables <- list(
  "Sex"            = person_record |> generate_frequency(sex),
  "Marital Status" = person_record |> generate_frequency(marital_status),
  "Marital × Sex"  = person_record |>
    generate_crosstab(marital_status, sex) |>
    add_table_title("Marital Status by Sex") |>
    add_table_subtitle("Row percentages") |>
    add_footnote("Missing values are excluded from the denominator.")
)

write_xlsx(tables, path = "report.xlsx")
```

### Add an index sheet

Set `include_table_list = TRUE` to prepend an auto-generated index
sheet:

``` r

write_xlsx(tables, path = "multi-sheet-indexed.xlsx", include_table_list = TRUE)
```

### Save each table to its own file

Set `separate_files = TRUE` and provide a folder path:

``` r

write_xlsx(tables, path = "output-tables/", separate_files = TRUE)
```

### End-to-end example

Here is a realistic pipeline that builds several tables, attaches
metadata, applies a style, and exports everything to a single indexed
workbook:

``` r

# 1. Build tables
freq_sex <- person_record |>
  generate_frequency(sex) |>
  add_table_title("Distribution by Sex") |>
  add_source_note("Source: person_record dataset")

crosstab_marital_sex <- person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  add_table_subtitle("Row percentages") |>
  add_footnote("Missing values are excluded from the denominator.")

difficulties_wide <- person_record |>
  generate_crosstab(
    sex,
    seeing, hearing, walking, remembering, self_caring, communicating,
    multiple_columns        = TRUE,
    multiple_columns_filter = 2L   # count "Some difficulty" responses
  ) |>
  add_table_title("Functional Difficulties by Sex (Some difficulty)")

# 2. Combine into a named list
workbook_tables <- list(
  "1. Sex"           = freq_sex,
  "2. Marital x Sex" = crosstab_marital_sex,
  "3. Difficulties"  = difficulties_wide
)

# 3. Export with a style and an index sheet
write_xlsx(
  workbook_tables,
  path               = "report.xlsx",
  facade             = get_tsg_facade("yolo"),
  include_table_list = TRUE
)
```

------------------------------------------------------------------------

## Controlling where footnotes appear

### Left and right placement

[`add_footnote()`](https://yng-me.github.io/tsg/reference/add_footnote.md)
accepts a `placement` argument (`"auto"`, `"left"`, or `"right"`) to
align the footnote text. Use left alignment for source citations and
right alignment for methodological notes — this mirrors APA and AAPOR
conventions.

``` r

person_record |>
  generate_frequency(sex) |>
  add_table_title("Sex distribution") |>
  add_footnote("Source: National Survey 2023.", placement = "left") |>
  add_footnote("Weighted estimates.", placement = "right")
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100
```

### Linking a footnote to a column header (HTML / PDF)

Pass column names via `locations` to place a footnote marker in the
column header. This is supported in HTML and PDF output (via `gt`);
Excel and Word output include the footnote text without cell-level
markers.

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_footnote(
    "Counts exclude respondents with unknown marital status.",
    locations = c("frequency_1", "frequency_2")
  )
#> # A tibble: 6 × 6
#>   category                 total frequency_1 frequency_2 percent_1 percent_2
#>   <int+lbl>                <int>       <int>       <int>     <dbl>     <dbl>
#> 1 1 [Single/never married]  1544         859         685      55.6      44.4
#> 2 2 [Married]                769         387         382      50.3      49.7
#> 3 3 [Common law/live-in]     424         211         213      49.8      50.2
#> 4 4 [Widowed]                138          40          98      29.0      71.0
#> 5 6 [Separated]               43          19          24      44.2      55.8
#> 6 0 [Total]                 2918        1516        1402      52.0      48.0
```

### Chaining multiple footnotes

Each
[`add_footnote()`](https://yng-me.github.io/tsg/reference/add_footnote.md)
call appends to the list. Different footnotes can have different
placements and locations:

``` r

person_record |>
  generate_frequency(sex) |>
  add_footnote("Source: National Survey 2023.") |>
  add_footnote("Weighted estimates.", placement = "right") |>
  add_footnote("Counts may not sum to total due to rounding.",
               locations = "frequency")
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100
```

------------------------------------------------------------------------

## Tips

- When building large workbooks, attach
  [`add_table_title()`](https://yng-me.github.io/tsg/reference/add_table_title.md)
  and
  [`add_table_subtitle()`](https://yng-me.github.io/tsg/reference/add_table_subtitle.md)
  to each element **before** combining into the list — metadata is
  preserved per sheet.
- `group_as_hierarchy = TRUE` with `group_as_list = TRUE` is most useful
  for hierarchical administrative data (e.g., national → regional →
  district breakdowns).
- `multiple_columns = TRUE` is designed for survey modules where several
  indicator columns share the same response scale.
- `multiple_columns_type = "stacked"` is best for producing a complete
  cross-product comparison table.
