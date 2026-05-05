# Getting Started with tsg

``` r

library(tsg)
```

`tsg` helps you turn a dataset into a publication-ready summary table —
a count of each category, a two-way comparison, or anything in between —
and save it to Excel in one short pipeline. No manual formatting
required.

This vignette walks through the most common tasks:

1.  Counting values in a column
2.  Comparing two columns against each other
3.  Breaking results down by a group
4.  Saving your table to a file

------------------------------------------------------------------------

## The sample dataset

All examples use `person_record`, a sample household survey dataset
included with the package. It contains one row per respondent and
records their sex, age, marital status, employment status, and ratings
on six functional difficulty questions (seeing, hearing, walking, etc.).

``` r

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

------------------------------------------------------------------------

## Count values in a column

[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
counts how many respondents fall into each category of a column, and
adds a percentage column automatically.

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

The result is a table with one row per category, plus a **Total** row at
the bottom.

### Count several columns at once

Pass more than one column name and you get back a list of tables — one
per column.

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

### Show only the top categories

Use `top_n` to keep only the most frequent categories. By default,
everything else is rolled up into an “Others” row.

``` r

person_record |>
  generate_frequency(
    marital_status,
    top_n = 3
  )
#> # A tibble: 5 × 3
#>   category                 frequency percent
#>   <dbl+lbl>                    <int>   <dbl>
#> 1 1 [Single/never married]      1544   52.9 
#> 2 2 [Married]                    769   26.4 
#> 3 3 [Common law/live-in]         424   14.5 
#> 4 9 [Others]                     181    6.20
#> 5 0 [Total]                     2918  100
```

Set `top_n_only = TRUE` to drop the “Others” row entirely and show only
the top results.

``` r

person_record |>
  generate_frequency(
    marital_status, 
    top_n      = 3,
    top_n_only = TRUE
  )
#> # A tibble: 4 × 3
#>   category                 frequency percent
#>   <int+lbl>                    <int>   <dbl>
#> 1 1 [Single/never married]      1544    52.9
#> 2 2 [Married]                    769    26.4
#> 3 3 [Common law/live-in]         424    14.5
#> 4 0 [Total]                     2918   100
```

### Control how rows are sorted

By default, rows are sorted by frequency (most common first). Set
`sort_value = FALSE` to sort by the category values instead — useful for
ordered variables like `age`.

``` r

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

When counting multiple columns at once, you can exclude specific columns
from sorting with `sort_except`:

``` r

person_record |>
  generate_frequency(
    sex, 
    age, 
    marital_status, 
    sort_except = "age"   # keep age in its natural order
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

### Include or exclude missing values

By default, missing values (`NA`) are counted and shown as a separate
row. Set `include_na = FALSE` to leave them out.

``` r

person_record |>
  generate_frequency(employed, include_na = TRUE)  # default
#> # A tibble: 4 × 3
#>   category         frequency percent
#>   <int+lbl>            <int>   <dbl>
#> 1 2 [No]                1186    40.6
#> 2 1 [Yes]                922    31.6
#> 3 8 [Not reported]       810    27.8
#> 4 0 [Total]             2918   100

person_record |>
  generate_frequency(employed, include_na = FALSE)
#> # A tibble: 3 × 3
#>   category  frequency percent
#>   <int+lbl>     <int>   <dbl>
#> 1 2 [No]         1186    56.3
#> 2 1 [Yes]         922    43.7
#> 3 0 [Total]      2108   100
```

### Combine similar columns into one table

If several columns share the same set of categories (like the six
functional difficulty columns in `person_record`), you can stack them
into a single table with `collapse_list = TRUE`.

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
#> # ℹ 7 more variables: frequency_8 <int>, percent_0 <dbl>, percent_1 <dbl>,
#> #   percent_2 <dbl>, percent_3 <dbl>, percent_4 <dbl>, percent_8 <dbl>
```

### Other options

- **Running totals** — add cumulative counts and percentages with
  `add_cumulative = TRUE` and `add_cumulative_percent = TRUE`.
- **Proportions instead of percentages** — use `as_proportion = TRUE` to
  get values between 0 and 1.
- **Total row at the top** — set `position_total = "top"` to move the
  Total row above the data rows.
- **Custom total label** — use `label_total` to rename the “Total” row.

``` r

person_record |>
  generate_frequency(
    sex, 
    add_cumulative         = TRUE, 
    add_cumulative_percent = TRUE 
  )
#> # A tibble: 3 × 5
#>   category   frequency percent cumulative cumulative_percent
#>   <int+lbl>      <int>   <dbl>      <int>              <dbl>
#> 1 1 [Male]        1516    52.0       1516               52.0
#> 2 2 [Female]      1402    48.0       2918              100  
#> 3 0 [Total]       2918   100           NA               NA
```

------------------------------------------------------------------------

## Compare two columns in a grid (cross-tabulation)

[`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md)
builds a two-way table: the first column you name goes in the rows, and
the second becomes column groups. You get a count and a percentage for
each combination.

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

> **Tip:** If you name only one column,
> [`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md)
> automatically falls back to a frequency table for that column.

### Percentages by column instead of by row

By default, percentages are calculated across each row (what share of
each marital-status group is male vs. female?). Set
`percent_by_column = TRUE` to flip this — percentages are then
calculated down each column (what share of males is in each
marital-status group?).

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

### Cross-tabulate against several columns at once

Pass more than one column after the row variable and you get a list of
cross-tabs — one per column.

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
#> attr(,"label_separator")
#> [1] "__"
#> attr(,"class")
#> [1] "tsg"  "tsgc" "list"
```

### Other options

The same options available for
[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
also work in
[`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md)
— `as_proportion`, `position_total`, `include_na`, `label_total`, and
more.

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

------------------------------------------------------------------------

## Break down results by a group

Pipe a
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) call
before either function to calculate counts separately for each group.
The result is a single merged table with the group labels embedded in
the category column.

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

The same works for cross-tabulations:

``` r

person_record |>
  dplyr::group_by(sex) |>
  generate_crosstab(marital_status, employed)
#> # A tibble: 12 × 9
#>    sex        category      total frequency_1 frequency_2 frequency_NA percent_1
#>    <int+lbl>  <int+lbl>     <int>       <dbl>       <dbl>        <dbl>     <dbl>
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

> **Want separate tables per group?** See the [Grouped Tables and
> Side-by-Side
> Comparisons](https://yng-me.github.io/tsg/articles/advanced.md)
> vignette for `group_as_list` and `group_as_hierarchy`.

------------------------------------------------------------------------

## Save your table to a file

Once you have a table, pipe it to
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
to save it as an Excel file.

``` r

person_record |>
  generate_frequency(sex) |>
  write_xlsx(path = "table-sex.xlsx")
```

### Add a title and notes

Chain
[`add_table_title()`](https://yng-me.github.io/tsg/reference/add_table_title.md),
[`add_table_subtitle()`](https://yng-me.github.io/tsg/reference/add_table_subtitle.md),
[`add_source_note()`](https://yng-me.github.io/tsg/reference/add_source_note.md),
and
[`add_footnote()`](https://yng-me.github.io/tsg/reference/add_footnote.md)
before saving to attach metadata that appears as styled rows above and
below the table.

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  add_table_subtitle("National Sample Survey, 2024") |>
  add_source_note("Source: person_record dataset.") |>
  add_footnote("Missing values are excluded from the denominator.") |>
  write_xlsx(path = "table-marital-sex.xlsx")
```

### Change the look of your table

Pass a built-in style with
[`get_tsg_facade()`](https://yng-me.github.io/tsg/reference/get_tsg_facade.md)
to quickly change the visual appearance of your exported table. The
package ships with two built-in styles: `"default"` (clean and neutral)
and `"yolo"` (bolder colours).

``` r

person_record |>
  generate_frequency(sex) |>
  write_xlsx(
    path   = "table-sex-styled.xlsx",
    facade = get_tsg_facade("yolo")
  )
```

For fine-grained control — changing specific fonts, colours, or cell
sizes — see the [Customizing How Your Tables
Look](https://yng-me.github.io/tsg/articles/facade.md) vignette.

### Save to other formats

`tsg` can also save to HTML, PDF, and Word. The API is identical — just
change the function name.

``` r

tbl <- person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  add_source_note("Source: person_record dataset")

write_xlsx(tbl, path = "table.xlsx")   # Excel
write_html(tbl, path = "table.html")   # HTML  (requires the gt package)
write_pdf(tbl,  path = "table.pdf")    # PDF   (requires gt + webshot2)
write_docx(tbl, path = "table.docx")  # Word  (requires officer + flextable)
```

For a full guide to each format — including multi-sheet workbooks and
managing metadata for large reports — see the [Saving and Sharing Your
Tables](https://yng-me.github.io/tsg/articles/output-formats.md)
vignette.
