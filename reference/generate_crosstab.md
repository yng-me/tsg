# Generate cross-tabulation

Generate cross-tabulation

## Usage

``` r
generate_crosstab(
  data,
  x,
  ...,
  add_total = TRUE,
  add_total_row = TRUE,
  add_total_column = TRUE,
  add_percent = TRUE,
  as_proportion = FALSE,
  percent_by_column = FALSE,
  name_separator = "_",
  label_separator = "__",
  label_total = "Total",
  label_total_column = NULL,
  label_total_row = NULL,
  label_na = "Not reported",
  include_na = TRUE,
  recode_na = "auto",
  label_as_group_name = TRUE,
  group_separator = " - ",
  group_as_list = FALSE,
  calculate_per_group = TRUE,
  expand_categories = TRUE,
  position_total = "bottom",
  sort_column_names = TRUE,
  collapse_list = FALSE,
  convert_factor = FALSE,
  metadata = NULL
)
```

## Arguments

- data:

  A data frame (typically `tibble`) containing the variables to
  summarize.

- x:

  The variable to use for the rows of the cross-tabulation.

- ...:

  Additional variable(s) to use for the columns of the cross-tabulation.
  If none are provided, a frequency table for `x` will be returned.

- add_total:

  Logical. If `TRUE`, adds total row and/or column.

- add_total_row:

  Logical. If `TRUE`, adds a total row.

- add_total_column:

  Logical. If `TRUE`, adds a total column.

- add_percent:

  Logical. If `TRUE`, adds percent or proportion values to the table.

- as_proportion:

  Logical. If `TRUE`, displays proportions instead of percentages (range
  0–1).

- percent_by_column:

  Logical. If `TRUE`, percentages are calculated by column; otherwise,
  by row.

- name_separator:

  Character. Separator used when constructing variable names in the
  output.

- label_separator:

  Character. Separator used when constructing labels in the output.

- label_total:

  Character. Label used for the total row/category.

- label_total_column:

  Character. Label used for the total column/category.

- label_total_row:

  Character. Label used for the total row/category.

- label_na:

  Character. Label to use for missing (`NA`) values.

- include_na:

  Logical. If `TRUE`, includes missing values in the cross table.

- recode_na:

  Character or `NULL`. Value used to replace missing values in labelled
  vectors; `"auto"` will determine a code automatically.

- label_as_group_name:

  Logical. If `TRUE`, uses the variable label of the grouping
  variable(s) as the name in the output list.

- group_separator:

  Character. Separator used when constructing group names in the output
  list.

- group_as_list:

  Logical. If `TRUE`, the output will be a list of data frames, one for
  each combination of grouping variable(s).

- calculate_per_group:

  Logical. If `TRUE`, calculates the cross-tabulation separately for
  each group defined by the grouping variable(s).

- expand_categories:

  Logical. If `TRUE`, ensures that all categories of `x` are represented
  in the output, even if they have zero counts.

- position_total:

  Character. Position of the total row/column; either `"bottom"` or
  `"top"` for rows, and `"right"` or `"left"` for columns.

- sort_column_names:

  Logical. If `TRUE`, sorts the column names in the output.

- collapse_list:

  Logical (NOT YET IMPLEMENTED). If `TRUE` and `group_as_list = TRUE`,
  collapses the list of frequency tables into a single data frame with
  group identifiers. See also
  [`collapse_list()`](https://yng-me.github.io/tsg/reference/collapse_list.md).

- convert_factor:

  Logical. If `TRUE`, converts labelled variables to factors in the
  output. See also
  [`convert_factor()`](https://yng-me.github.io/tsg/reference/convert_factor.md).

- metadata:

  A named list with optional metadata to attach as attributes, e.g.
  `title`, `subtitle`, and `source_note`.

## Value

A data frame or a list of data frames containing the cross-tabulation
results. If `group_as_list` is `TRUE`, the output will be a list of data
frames, one for each combination of grouping variable(s). Otherwise, a
single data frame is returned. Each data frame includes counts and, if
specified, percentages or proportions for each combination of `x` and
the additional variables provided in `...`.

## See also

[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md),
[`generate_output()`](https://yng-me.github.io/tsg/reference/generate_output.md),
[`rename_label()`](https://yng-me.github.io/tsg/reference/rename_label.md),
[`remove_label()`](https://yng-me.github.io/tsg/reference/remove_label.md)

## Examples

``` r
# Using built-in dataset `person_record`

# Basic usage
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


# Multiple variables
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

 # Grouping
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

# # Percent or proportion by row or column
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
