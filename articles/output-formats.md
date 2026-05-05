# Saving and Sharing Your Tables

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

Once you have a table, `tsg` can save it to a file in several formats.
The steps are always the same: generate the table, optionally add a
title and notes, then call the appropriate `write_*()` function.

**Which format should I use?**

| Format              | Best for                                 |
|---------------------|------------------------------------------|
| **Excel** (`.xlsx`) | Sharing with colleagues, further editing |
| **HTML**            | Embedding in a website or report         |
| **PDF**             | Print-ready documents                    |
| **Word** (`.docx`)  | Inserting into a Word report or document |

Excel is the most fully-featured output and requires no additional
packages beyond `tsg`. The other formats need a few extra packages —
details in their sections below.

All examples use the `person_record` sample dataset included with the
package.

------------------------------------------------------------------------

## Saving to Excel

Use
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
to save any table to an `.xlsx` file.

### Basic save

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

``` r

person_record |>
  generate_frequency(sex) |>
  write_xlsx(path = "table-sex.xlsx")
```

### Add a title, subtitle, and notes

Chain
[`add_table_title()`](https://yng-me.github.io/tsg/reference/add_table_title.md),
[`add_table_subtitle()`](https://yng-me.github.io/tsg/reference/add_table_subtitle.md),
[`add_source_note()`](https://yng-me.github.io/tsg/reference/add_source_note.md),
and
[`add_footnote()`](https://yng-me.github.io/tsg/reference/add_footnote.md)
before saving. These appear as styled rows above and below the table in
the Excel file.

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  add_table_subtitle("National Sample Survey, 2024") |>
  add_source_note("Source: person_record dataset.") |>
  add_footnote("Missing values are excluded from the denominator.")
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

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  add_table_subtitle("National Sample Survey, 2024") |>
  add_source_note("Source: person_record dataset.") |>
  add_footnote("Missing values are excluded from the denominator.") |>
  write_xlsx(path = "table-marital-sex.xlsx")
```

> **Shortcut:** You can also pass the title, subtitle, and notes
> directly as arguments to
> [`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
> instead of chaining the helper functions.
>
> ``` r
>
> person_record |>
>   generate_crosstab(marital_status, sex) |>
>   write_xlsx(
>     path        = "table-marital-sex.xlsx",
>     title       = "Marital Status by Sex",
>     subtitle    = "National Sample Survey, 2024",
>     source_note = "Source: person_record dataset.",
>     footnotes   = "Missing values are excluded from the denominator."
>   )
> ```

### More table examples

#### Frequency table with running totals

``` r

person_record |>
  generate_frequency(sex, add_cumulative = TRUE, add_cumulative_percent = TRUE)
#> # A tibble: 3 × 5
#>   category   frequency percent cumulative cumulative_percent
#>   <int+lbl>      <int>   <dbl>      <int>              <dbl>
#> 1 1 [Male]        1516    52.0       1516               52.0
#> 2 2 [Female]      1402    48.0       2918              100  
#> 3 0 [Total]       2918   100           NA               NA
```

``` r

person_record |>
  generate_frequency(sex, add_cumulative = TRUE, add_cumulative_percent = TRUE) |>
  write_xlsx(path = "table-sex-cumulative.xlsx")
```

#### Grouped frequency table

Pipe a
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
before
[`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
to stratify the output. The result is a single flat table with group
labels in the category column.

``` r

person_record |>
  group_by(sex) |>
  generate_frequency(employed)
#> # A tibble: 8 × 4
#>   sex        category         frequency percent
#>   <int+lbl>  <int+lbl>            <int>   <dbl>
#> 1 1 [Male]   1 [Yes]                599    39.5
#> 2 1 [Male]   2 [No]                 508    33.5
#> 3 1 [Male]   8 [Not reported]       409    27.0
#> 4 1 [Male]   0 [Total]             1516   100  
#> 5 2 [Female] 1 [Yes]                323    23.0
#> 6 2 [Female] 2 [No]                 678    48.4
#> 7 2 [Female] 8 [Not reported]       401    28.6
#> 8 2 [Female] 0 [Total]             1402   100
```

``` r

person_record |>
  group_by(sex) |>
  generate_frequency(employed) |>
  write_xlsx(path = "table-employed-by-sex.xlsx")
```

#### Basic cross-tabulation

``` r

person_record |>
  generate_crosstab(employed, sex)
#> # A tibble: 4 × 6
#>   category         total frequency_1 frequency_2 percent_1 percent_2
#>   <int+lbl>        <int>       <int>       <int>     <dbl>     <dbl>
#> 1 1 [Yes]            922         599         323      65.0      35.0
#> 2 2 [No]            1186         508         678      42.8      57.2
#> 3 8 [Not reported]   810         409         401      50.5      49.5
#> 4 0 [Total]         2918        1516        1402      52.0      48.0
```

``` r

person_record |>
  generate_crosstab(employed, sex) |>
  write_xlsx(path = "crosstab-employed-sex.xlsx")
```

#### Cross-tabulation with column percentages

``` r

person_record |>
  generate_crosstab(employed, sex, percent_by_column = TRUE)
#> # A tibble: 4 × 7
#>   category       frequency_total frequency_1 frequency_2 percent_total percent_1
#>   <int+lbl>                <int>       <int>       <int>         <dbl>     <dbl>
#> 1 1 [Yes]                    922         599         323          31.6      39.5
#> 2 2 [No]                    1186         508         678          40.6      33.5
#> 3 8 [Not report…             810         409         401          27.8      27.0
#> 4 0 [Total]                 2918        1516        1402         100       100  
#> # ℹ 1 more variable: percent_2 <dbl>
```

``` r

person_record |>
  generate_crosstab(employed, sex, percent_by_column = TRUE) |>
  write_xlsx(path = "crosstab-column-pct.xlsx")
```

### Save multiple tables to one workbook

Pass a **named list** to
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md).
Each element becomes a separate worksheet. The name of each list element
becomes the sheet name.

``` r

tables <- list(
  "Sex"        = person_record |> generate_frequency(sex),
  "Employment" = person_record |> generate_frequency(employed),
  "Crosstab"   = person_record |> generate_crosstab(employed, sex)
)

write_xlsx(tables, path = "multi-sheet.xlsx")
```

### Save each table to its own file

Set `separate_files = TRUE` and provide a folder path instead of a file
name. The folder is created if it does not exist.

``` r

write_xlsx(tables, path = "output-dir/", separate_files = TRUE)
```

### Add an index sheet to a multi-table workbook

Set `include_table_list = TRUE` to prepend an auto-generated index
sheet. This is useful for statistical reports with many tables.

``` r

write_xlsx(tables, path = "report.xlsx", include_table_list = TRUE)
```

------------------------------------------------------------------------

## Managing metadata for large reports

When you have many tables, it is more practical to keep all titles,
subtitles, and notes in one place — a reference spreadsheet — rather
than scattering them across your analysis script. `tsg` supports this
with the `table_list_reference` argument.

### Step 1 — Create a template

``` r

generate_template("table-list-template.xlsx", template = "table-list")
```

The template has one row per table with these columns:

| Column | What it contains |
|----|----|
| `table_id` | A unique identifier that must match the **name** of the list element in [`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md) |
| `table_number` | Display number shown in the index sheet |
| `table_name` | Short label shown in the index sheet |
| `title` | Full table title |
| `subtitle` | Optional subtitle |
| `footnotes` | Optional footnote text |
| `source_note` | Optional source line printed below the table |

### Step 2 — Fill in the reference data

Edit the template in Excel, or build it in R:

``` r

table_ref <- tibble::tibble(
  table_id     = c("table_sex", "table_emp", "table_ct"),
  table_number = 1:3,
  table_name   = c("Sex", "Employment", "Employment × Sex"),
  title        = c(
    "Distribution by Sex",
    "Employment Status",
    "Employment Status by Sex"
  ),
  subtitle     = c(NA, NA, "Cross-tabulation"),
  footnotes    = NA,
  source_note  = "Source: person_record dataset."
)
```

### Step 3 — Export with the reference

The `table_id` values in your reference must match the names of your
list.
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
looks up each table, applies its metadata, and builds the index sheet
automatically.

``` r

tables <- list(
  table_sex = person_record |> generate_frequency(sex),
  table_emp = person_record |> generate_frequency(employed),
  table_ct  = person_record |> generate_crosstab(employed, sex)
)

write_xlsx(
  tables,
  path                 = "report.xlsx",
  include_table_list   = TRUE,
  table_list_reference = table_ref
)
```

------------------------------------------------------------------------

## Saving to HTML

> **Required package:** `gt` — install with `install.packages("gt")`

``` r

person_record |>
  generate_frequency(sex) |>
  add_table_title("Distribution by Sex") |>
  write_html(path = "table-sex.html")
```

Cross-tabulations with grouped column headers are fully supported:

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  write_html(path = "crosstab.html")
```

### Multiple tables in one HTML file

When `data` is a named list, all tables are written to a single
self-contained HTML file by default. Each table is wrapped in its own
section. Set `include_table_list = TRUE` to add a clickable table of
contents.

``` r

tables <- list(
  "Sex"           = person_record |> generate_frequency(sex),
  "Marital Status"= person_record |> generate_frequency(marital_status),
  "Sex × Marital" = person_record |> generate_crosstab(sex, marital_status)
)

write_html(tables, path = "report.html", include_table_list = TRUE)
```

Set `separate_files = TRUE` to write each table to its own `.html` file
in a folder.

``` r

write_html(tables, path = "html-output/", separate_files = TRUE)
```

------------------------------------------------------------------------

## Saving to PDF

> **Required packages:** `gt` and `webshot2` — install with
> `install.packages(c("gt", "webshot2"))`.  
> `webshot2` also requires a Chromium browser; run
> `webshot2::install_phantomjs()` or ensure Chrome/Chromium is
> available.

``` r

person_record |>
  generate_frequency(sex) |>
  add_table_title("Distribution by Sex") |>
  write_pdf(path = "table-sex.pdf")
```

When `data` is a list, the default is one PDF file per table inside the
specified folder:

``` r

write_pdf(tables, path = "pdf-output/")
```

To combine all tables into a single PDF file, set
`separate_files = FALSE` (requires the `qpdf` package):

``` r

write_pdf(tables, path = "report.pdf", separate_files = FALSE)
```

------------------------------------------------------------------------

## Saving to Word

> **Required packages:** `officer` and `flextable` — install with
> `install.packages(c("officer", "flextable"))`

``` r

person_record |>
  generate_frequency(sex) |>
  add_table_title("Distribution by Sex") |>
  add_source_note("Source: person_record dataset") |>
  write_docx(path = "table-sex.docx")
```

Cross-tabulations are fully supported:

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital Status by Sex") |>
  add_footnote("Missing values excluded from the denominator.") |>
  write_docx(path = "crosstab.docx")
```

When `data` is a named list, the default is a single `.docx` file with
one table per page:

``` r

tables <- list(
  "Sex"           = person_record |>
    generate_frequency(sex) |>
    add_table_title("Distribution by Sex"),
  "Marital Status"= person_record |>
    generate_frequency(marital_status) |>
    add_table_title("Distribution by Marital Status"),
  "Sex × Marital" = person_record |>
    generate_crosstab(sex, marital_status) |>
    add_table_title("Sex by Marital Status")
)

write_docx(tables, path = "report.docx")
```

Set `separate_files = TRUE` to write each table to its own `.docx` file:

``` r

write_docx(tables, path = "docx-output/", separate_files = TRUE)
```

------------------------------------------------------------------------

## Summary

| Format | Function | Key packages |
|----|----|----|
| Excel | [`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md) | *(none beyond tsg)* |
| HTML | [`write_html()`](https://yng-me.github.io/tsg/reference/write_html.md) | `gt` |
| PDF | [`write_pdf()`](https://yng-me.github.io/tsg/reference/write_pdf.md) | `gt`, `webshot2` (+ `qpdf` for combined) |
| Word | [`write_docx()`](https://yng-me.github.io/tsg/reference/write_docx.md) | `officer`, `flextable` |
