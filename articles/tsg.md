# Working with the tsg package

``` r
library(tsg)
```

Throughout the examples, we will use the `person_record` sample dataset,
which is included in the `tsg` package. This dataset contains
demographic information about individuals, including `person_id`, `sex`,
`age`, `marital_status`, `employed` status, and functional difficulties.

``` r
dim(person_record)
#> [1] 2918   11
head(person_record)
#>   person_id age sex marital_status employed seeing hearing walking remembering
#> 1         1  46   1              2        1      1       1       1           1
#> 2         2  44   2              2        1      1       1       1           1
#> 3         3  10   1              1       NA      1       1       1           1
#> 4         4  65   2              2        1      1       1       1           1
#> 5         5  65   1              2        1      1       1       1           1
#> 6         6  28   2              1        2      1       1       1           1
#>   self_caring communicating
#> 1           1             1
#> 2           1             1
#> 3           1             1
#> 4           1             1
#> 5           1             1
#> 6           1             1
```

## Generate frequency table

The
[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
function creates frequency tables for one or more categorical variables
in a data frame. It supports a variety of enhancements, such as sorting,
adding totals and percentages, handling missing values, and customizing
labels. This function is highly versatile and can work with grouped
data, outputting either a single table or a list of tables.

### Basic usage

``` r
person_record |> 
  generate_frequency(sex)
#> # A tibble: 3 × 3
#>   category   frequency percent
#>   <int+lbl>      <int>   <dbl>
#> 1 1 [Male]        1516    52.0
#> 2 2 [Female]      1402    48.0
#> 3 0 [Total]       2918   100
```

### Multiple variables

If you pass multiple variables, it will generate frequency tables for
each variable separately in a list.

``` r
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
```

### Grouping

You can also specify grouping using the `group_by()` from `dplyr` and it
will calculate the frequency table for each group.

``` r
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
```

By default, the function will generate a single frequency table for the
grouped data. If you want to generate a list of frequency tables for
each group, you can set `group_as_list = TRUE`.

``` r
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
```

### Sorting

By default, the output is sorted by frequency in descending order. If
`sort_value` is set to `FALSE`, the output will be sorted by the
variable values in ascending order.

``` r
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
```

If multiple variables are specified, you can indicate which variable/s
is/are excluded from sorting using the `sort_except` argument.

``` r
person_record |>
  generate_frequency(
    sex, 
    age, 
    marital_status, 
    # vector of variable names (character) to exclude from sorting
    sort_except = "age" 
  )
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
```

### Top `n` values

You can specify the top `n` most frequent values to display in the
frequency table, if `sort_value` is `TRUE`. By default, it will show
top-n values plus the remaining values grouped into “Others”.

``` r
person_record |>
  generate_frequency(
    marital_status,
    #top_n = 3
  )
#> # A tibble: 6 × 3
#>   category                 frequency percent
#>   <int+lbl>                    <int>   <dbl>
#> 1 1 [Single/never married]      1544   52.9 
#> 2 2 [Married]                    769   26.4 
#> 3 3 [Common law/live-in]         424   14.5 
#> 4 4 [Widowed]                    138    4.73
#> 5 6 [Separated]                   43    1.47
#> 6 0 [Total]                     2918  100
```

If you want to show only the top-n values and exclude the rest, set
`top_n_only = TRUE`.

``` r
person_record |>
  generate_frequency(
    marital_status, 
    #top_n = 3,
    #top_n_only = TRUE
  )
#> # A tibble: 6 × 3
#>   category                 frequency percent
#>   <int+lbl>                    <int>   <dbl>
#> 1 1 [Single/never married]      1544   52.9 
#> 2 2 [Married]                    769   26.4 
#> 3 3 [Common law/live-in]         424   14.5 
#> 4 4 [Widowed]                    138    4.73
#> 5 6 [Separated]                   43    1.47
#> 6 0 [Total]                     2918  100
```

### Handling missing values

You can also specify whether to include or exclude `NA`s (missing
values) from the frequency table.

``` r
person_record |>
  generate_frequency(
    employed,
    include_na = TRUE # default
  )
#> # A tibble: 4 × 3
#>   category         frequency percent
#>   <int+lbl>            <int>   <dbl>
#> 1 2 [No]                1186    40.6
#> 2 1 [Yes]                922    31.6
#> 3 9 [Not reported]       810    27.8
#> 4 0 [Total]             2918   100

# Exclude NA values
person_record |>
  generate_frequency(
    employed,
    include_na = FALSE
  )
#> # A tibble: 3 × 3
#>   category  frequency percent
#>   <int+lbl>     <int>   <dbl>
#> 1 2 [No]         1186    56.3
#> 2 1 [Yes]         922    43.7
#> 3 0 [Total]      2108   100
```

### Collapse list

If the all variables passed to
[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
are of the same structure (i.e. have the same number of levels or
categories), you can collapse them into a single frequency table by
setting `collapse_list = TRUE`.

``` r
person_record |>
  generate_frequency(
    seeing,
    hearing,
    walking,
    remembering,
    self_caring,
    communicating, 
    collapse_list = TRUE
  )
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

Or equivalently using the
[`collapse_list()`](https://yng-me.github.io/tsg/reference/collapse_list.md)
helper function.

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

### More options

You can also add cumulative frequency and percentage to the frequency
table.

``` r
person_record |>
  generate_frequency(
    sex, 
    add_cumulative = TRUE, 
    add_cumulative_percent = TRUE 
  )
#> # A tibble: 3 × 5
#>   category   frequency percent cumulative cumulative_percent
#>   <int+lbl>      <int>   <dbl>      <int>              <dbl>
#> 1 1 [Male]        1516    52.0       1516               52.0
#> 2 2 [Female]      1402    48.0       2918              100  
#> 3 0 [Total]       2918   100           NA               NA
```

You can also specify whether to express the value as a proportion.

``` r
person_record |>
  generate_frequency(
    marital_status,
    as_proportion = TRUE
  )
#> # A tibble: 6 × 3
#>   category                 frequency proportion
#>   <int+lbl>                    <int>      <dbl>
#> 1 1 [Single/never married]      1544     0.529 
#> 2 2 [Married]                    769     0.264 
#> 3 3 [Common law/live-in]         424     0.145 
#> 4 4 [Widowed]                    138     0.0473
#> 5 6 [Separated]                   43     0.0147
#> 6 0 [Total]                     2918     1
```

You can also position the total row at the top of the table.

``` r
person_record |>
  generate_frequency(
    marital_status,
    position_total = "top"
  )
#> # A tibble: 6 × 3
#>   category                 frequency percent
#>   <int+lbl>                    <int>   <dbl>
#> 1 0 [Total]                     2918  100   
#> 2 1 [Single/never married]      1544   52.9 
#> 3 2 [Married]                    769   26.4 
#> 4 3 [Common law/live-in]         424   14.5 
#> 5 4 [Widowed]                    138    4.73
#> 6 6 [Separated]                   43    1.47
```

NOTE: For labelled data, the value for the row total is automatically
set the lowest numeric value. The default label for the total row is
“Total”; if you want to set a custom label for the total row, you can
use the `label_total` argument.

## Generate cross-tabulation

The
[`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md)
function allows you to create cross-tabulations between two variables,
which is useful for exploring relationships between categorical
variables.

### Basic usage

``` r
person_record |>
  generate_crosstab(marital_status, sex)
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

NOTE: If you pass only one variable, it will fall back to
[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
and generate a frequency table for variable specified.

### Multiple variables

If you pass mutliple variables, it will generate cross-tabulations for
each pair of variables separately in a list.

``` r
person_record |>
  generate_crosstab(
    sex,
    seeing,
    hearing,
    walking,
    remembering,
    self_caring,
    communicating
  )
#> $`Seeing, even if wearing glasses`
#> # A tibble: 3 × 12
#>   category   total frequency_1 frequency_2 frequency_3 frequency_4 frequency_NA
#>   <int+lbl>  <int>       <int>       <int>       <int>       <int>        <int>
#> 1 1 [Male]    1516        1369          40           4           0          103
#> 2 2 [Female]  1402        1254          42           5           1          100
#> 3 0 [Total]   2918        2623          82           9           1          203
#> # ℹ 5 more variables: percent_1 <dbl>, percent_2 <dbl>, percent_3 <dbl>,
#> #   percent_4 <dbl>, percent_NA <dbl>
#> 
#> $`Hearing, even if using hearing aid`
#> # A tibble: 3 × 12
#>   category   total frequency_1 frequency_2 frequency_3 frequency_4 frequency_NA
#>   <int+lbl>  <int>       <int>       <int>       <int>       <int>        <int>
#> 1 1 [Male]    1516        1386          23           4           0          103
#> 2 2 [Female]  1402        1280          20           1           1          100
#> 3 0 [Total]   2918        2666          43           5           1          203
#> # ℹ 5 more variables: percent_1 <dbl>, percent_2 <dbl>, percent_3 <dbl>,
#> #   percent_4 <dbl>, percent_NA <dbl>
#> 
#> $`Walking or climbing steps, even if with cane or artificial leg`
#> # A tibble: 3 × 12
#>   category   total frequency_1 frequency_2 frequency_3 frequency_4 frequency_NA
#>   <int+lbl>  <int>       <int>       <int>       <int>       <int>        <int>
#> 1 1 [Male]    1516        1381          27           4           1          103
#> 2 2 [Female]  1402        1277          21           3           1          100
#> 3 0 [Total]   2918        2658          48           7           2          203
#> # ℹ 5 more variables: percent_1 <dbl>, percent_2 <dbl>, percent_3 <dbl>,
#> #   percent_4 <dbl>, percent_NA <dbl>
#> 
#> $`Remembering or concentrating`
#> # A tibble: 3 × 10
#>   category   total frequency_1 frequency_2 frequency_3 frequency_NA percent_1
#>   <int+lbl>  <int>       <int>       <int>       <int>        <int>     <dbl>
#> 1 1 [Male]    1516        1393          17           3          103      91.9
#> 2 2 [Female]  1402        1287          11           4          100      91.8
#> 3 0 [Total]   2918        2680          28           7          203      91.8
#> # ℹ 3 more variables: percent_2 <dbl>, percent_3 <dbl>, percent_NA <dbl>
#> 
#> $`Self-caring (such as wasking all over or dressing)`
#> # A tibble: 3 × 12
#>   category   total frequency_1 frequency_2 frequency_3 frequency_4 frequency_NA
#>   <int+lbl>  <int>       <int>       <int>       <int>       <int>        <int>
#> 1 1 [Male]    1516        1391          19           2           1          103
#> 2 2 [Female]  1402        1280          18           3           1          100
#> 3 0 [Total]   2918        2671          37           5           2          203
#> # ℹ 5 more variables: percent_1 <dbl>, percent_2 <dbl>, percent_3 <dbl>,
#> #   percent_4 <dbl>, percent_NA <dbl>
#> 
#> $`Communicating using his/her casual (customary) language`
#> # A tibble: 3 × 12
#>   category   total frequency_1 frequency_2 frequency_3 frequency_4 frequency_NA
#>   <int+lbl>  <int>       <int>       <int>       <int>       <int>        <int>
#> 1 1 [Male]    1516        1396          13           3           1          103
#> 2 2 [Female]  1402        1287          12           2           1          100
#> 3 0 [Total]   2918        2683          25           5           2          203
#> # ℹ 5 more variables: percent_1 <dbl>, percent_2 <dbl>, percent_3 <dbl>,
#> #   percent_4 <dbl>, percent_NA <dbl>
#> 
#> attr(,"class")
#> [1] "tsg"  "tsgc" "list"
```

### Grouping

You can also specify grouping with `group_by()` from `dplyr` and it will
calculate the cross-tabulation for each group.

``` r
person_record |>
  dplyr::group_by(sex) |>
  generate_crosstab(marital_status, employed)
#> # A tibble: 12 × 9
#>    sex        category      total frequency_1 frequency_2 frequency_NA percent_1
#>    <int+lbl>  <int+lbl>     <int>       <int>       <int>        <int>     <dbl>
#>  1 1 [Male]   1 [Single/ne…   859         127         330          402     14.8 
#>  2 1 [Male]   2 [Married]     387         274         108            5     70.8 
#>  3 1 [Male]   3 [Common la…   211         169          41            1     80.1 
#>  4 1 [Male]   4 [Widowed]      40          18          21            1     45   
#>  5 1 [Male]   6 [Separated]    19          11           8            0     57.9 
#>  6 1 [Male]   0 [Total]      1516         599         508          409     39.5 
#>  7 2 [Female] 1 [Single/ne…   685          63         234          388      9.20
#>  8 2 [Female] 2 [Married]     382         155         218            9     40.6 
#>  9 2 [Female] 3 [Common la…   213          67         142            4     31.5 
#> 10 2 [Female] 4 [Widowed]      98          26          72            0     26.5 
#> 11 2 [Female] 6 [Separated]    24          12          12            0     50   
#> 12 2 [Female] 0 [Total]      1402         323         678          401     23.0 
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
```

If you want to generate a list of cross-tabulations for each group, you
can set `group_as_list = TRUE`.

``` r
person_record |>
  dplyr::group_by(sex) |>
  generate_crosstab(marital_status, employed, group_as_list = TRUE)
#> $Male
#> # A tibble: 6 × 9
#>   sex       category        total frequency_1 frequency_2 frequency_NA percent_1
#>   <int+lbl> <int+lbl>       <int>       <int>       <int>        <int>     <dbl>
#> 1 1 [Male]  1 [Single/neve…   859         127         330          402      14.8
#> 2 1 [Male]  2 [Married]       387         274         108            5      70.8
#> 3 1 [Male]  3 [Common law/…   211         169          41            1      80.1
#> 4 1 [Male]  4 [Widowed]        40          18          21            1      45  
#> 5 1 [Male]  6 [Separated]      19          11           8            0      57.9
#> 6 1 [Male]  0 [Total]        1516         599         508          409      39.5
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
#> 
#> $Female
#> # A tibble: 6 × 9
#>   sex        category       total frequency_1 frequency_2 frequency_NA percent_1
#>   <int+lbl>  <int+lbl>      <int>       <int>       <int>        <int>     <dbl>
#> 1 2 [Female] 1 [Single/nev…   685          63         234          388      9.20
#> 2 2 [Female] 2 [Married]      382         155         218            9     40.6 
#> 3 2 [Female] 3 [Common law…   213          67         142            4     31.5 
#> 4 2 [Female] 4 [Widowed]       98          26          72            0     26.5 
#> 5 2 [Female] 6 [Separated]     24          12          12            0     50   
#> 6 2 [Female] 0 [Total]       1402         323         678          401     23.0 
#> # ℹ 2 more variables: percent_2 <dbl>, percent_NA <dbl>
#> 
#> attr(,"groups")
#> [1] "sex"
#> attr(,"class")
#> [1] "tsg"  "tsgc" "list"
```

### Percent or proportion by row or column

You can specify whether to calculate the percentage or proportion by row
or column using the `percent_by_column` argument. If it is set to
`TRUE`, the percentage will be calculated by column; if set to `FALSE`,
it will be calculated by row. The default is `FALSE`.

``` r
person_record |>
  generate_crosstab(
    marital_status,
    sex,
    percent_by_column = TRUE
  )
#> # A tibble: 6 × 7
#>   category       frequency_total frequency_1 frequency_2 percent_total percent_1
#>   <int+lbl>                <int>       <int>       <int>         <dbl>     <dbl>
#> 1 1 [Single/nev…            1544         859         685         52.9      56.7 
#> 2 2 [Married]                769         387         382         26.4      25.5 
#> 3 3 [Common law…             424         211         213         14.5      13.9 
#> 4 4 [Widowed]                138          40          98          4.73      2.64
#> 5 6 [Separated]               43          19          24          1.47      1.25
#> 6 0 [Total]                 2918        1516        1402        100       100   
#> # ℹ 1 more variable: percent_2 <dbl>
```

### More options

Just like
[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md),
you can also specify whether to express the value as a proportion.

``` r
person_record |>
  generate_crosstab(
    marital_status,
    sex,
    as_proportion = TRUE
  )
#> # A tibble: 6 × 6
#>   category               total frequency_1 frequency_2 proportion_1 proportion_2
#>   <int+lbl>              <int>       <int>       <int>        <dbl>        <dbl>
#> 1 1 [Single/never marri…  1544         859         685        0.556        0.444
#> 2 2 [Married]              769         387         382        0.503        0.497
#> 3 3 [Common law/live-in]   424         211         213        0.498        0.502
#> 4 4 [Widowed]              138          40          98        0.290        0.710
#> 5 6 [Separated]             43          19          24        0.442        0.558
#> 6 0 [Total]               2918        1516        1402        0.520        0.480
```

You can also position the total row at the top of the table.

``` r
person_record |>
  generate_crosstab(
    marital_status,
    sex,
    position_total = "top"
  )
#> # A tibble: 6 × 6
#>   category                 total frequency_1 frequency_2 percent_1 percent_2
#>   <int+lbl>                <int>       <int>       <int>     <dbl>     <dbl>
#> 1 0 [Total]                 2918        1516        1402      52.0      48.0
#> 2 1 [Single/never married]  1544         859         685      55.6      44.4
#> 3 2 [Married]                769         387         382      50.3      49.7
#> 4 3 [Common law/live-in]     424         211         213      49.8      50.2
#> 5 4 [Widowed]                138          40          98      29.0      71.0
#> 6 6 [Separated]               43          19          24      44.2      55.8
```

## Generate output

You can export your frequency table or cross-tabulation to Excel using
the
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md).

### Basic usage

``` r
person_record |> 
  generate_frequency(sex) |> 
  write_xlsx(path = "table-01.xlsx")
```

### Add table info

You can add a title and subtitle to your table using the
[`add_table_title()`](https://yng-me.github.io/tsg/reference/add_table_title.md)
and
[`add_table_subtitle()`](https://yng-me.github.io/tsg/reference/add_table_subtitle.md)
functions.

``` r
person_record |> 
  generate_crosstab(marital_status, sex) |> 
  add_table_title("Marital Status by Sex") |>
  add_table_subtitle("Sample dataset: person_record") |>
  write_xlsx(path = "table-02.xlsx")
```

You can also add end notes to your table using the
[`add_source_note()`](https://yng-me.github.io/tsg/reference/add_source_note.md)
and
[`add_footnote()`](https://yng-me.github.io/tsg/reference/add_footnote.md)
functions.

``` r
person_record |> 
  generate_crosstab(marital_status, sex) |> 
  add_table_title("Marital Status by Sex") |>
  add_table_subtitle("Sample dataset: person_record") |>
  add_source_note("Source: person_record dataset") |>
  add_footnote("This is a footnote for the table") |>
  write_xlsx(path = "table-03.xlsx")
```

Alternatively, you can directly add table title, subtitle, source_note,
and footnotes by specifying them in the arguments of the
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
function.

``` r
person_record |> 
  generate_crosstab(marital_status, sex) |> 
  write_xlsx(
    path = "table-03.xlsx",
    table_title = "Marital Status by Sex",
    table_subtitle = "Sample dataset: person_record",
    source_note = "Source: person_record dataset",
    footnotes = "This is a footnote for the table"
  )
```

### Facade

You can use the
[`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
function to apply a facade to your table. A facade is a set of styling
options that can be applied to the table to customize its appearance.

``` r
person_record |> 
  generate_frequency(sex) |> 
  add_facade(
    table.offsetRow = 2, 
    table.offsetCol = 1
  ) |> 
  write_xlsx(
    path = "table-04.xlsx",
    # Using built-in facade
    facade = get_tsg_facade("yolo")
  )
```

If you want to further customize the appearance of your table, you can
use the `facade` argument to specify a YAML facade file. The facade file
contains styling options for the table, such as font size, border style,
background color, and text alignment.

``` r
person_record |> 
  generate_frequency(sex) |> 
  write_xlsx(
    path = "table-05.xlsx",
    # Using built-in facade
    facade = get_tsg_facade("yolo")
  )
```

You can generate a template facade file using the
[`generate_template()`](https://yng-me.github.io/tsg/reference/generate_template.md)
function and then customize it to your needs.

### The `generate_output()` function

[`generate_output()`](https://yng-me.github.io/tsg/reference/generate_output.md)
can be used to generate and save the output file in the specified format
(e.g., Excel, HTML, PDF, Word). It supports various formats and can
handle different data structures.

``` r
person_record |> 
  generate_frequency(sex) |> 
  generate_output(path = "table-06.xlsx")
```

NOTE: At the moment, it only supports Excel output. The other formats
are not yet implemented.
