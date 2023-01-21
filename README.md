## About the package

**`tsg`** stands for "**table summary generator**." As the name suggests, this package is designed to facilitate generation of statistical summary tables with ease. It also adheres to the `tidyverse` specifications.

Features include:

- generate frequency tables, cross-tabulations (2-way table or more);
- extract multiple-letter response variable from survey data;
- include percent distribution (which is default) in the generated tables;
- specify whether the 'percent to total' is computed by row (default) or by column;
- export into an Excel file with default formatting, which can also be customized.

## Installation

You may install the `tsg` package either from GitHub or R-CRAN (waiting for approval).

```r
# Install devtools if not yet installed in your machine
if(!('devtools' %in% installed.packages()[,'Package'])){
   install.packages('devtools')
}

# Install the package from GitHub
devtools::install_github('yng-me/tsg')

# Install via R-CRAN (if approved)
install.packages('tsg')

```

Then load the package after installation.

```r
library(tsg)
```


## `tsg` core functions

### 1. **`generate_frequency`**

**`generate_frequency`** generates a frequency distribution table (marginal table) of a categorical variable `x` specified in the second argument of the function. It returns five (5) columns at the minimum if `x_group` is not specified. These include (1) categories of `x`, (2) frequency of each category, (3) percent to total, (4) cumulative frequency, and (5) cumulative percent to total.

```r
generate_frequency(
  .data,
  x,
  x_group = NULL,
  x_label = tsg_get_config("x_label"),
  sort_frequency = FALSE,
  x_as_group = FALSE,
  ...
)
```


**Parameters**: 

<table>
  <tr>
    <td>`.data`</td>
    <td><b>Required</b>. A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.</td>
  </tr>
  <tr>
    <td>`x`</td>
    <td><b>Required</b>. Variable to be used as categories.</td>
  </tr>
  <tr>
    <td>`x_group`</td>
    <td>Accepts a vector of string/character as grouping variables.</td>
  </tr>
  <tr>
    <td>`x_label`</td>
    <td>Stubhead label or label for `x`.</td>
  </tr>
  <tr>
    <td>`sort_frequency`</td>
    <td>Whether to sort the output. If set to `TRUE`, the frequency will be sorted in descending order.</td>
  </tr>
  <tr>
    <td>`x_as_group`</td>
    <td>Use `x` as top-level grouping. Applicable only if `x_group` is specified.</td>
  </tr>
  <tr>
    <td>`...`</td>
    <td>
      <span>Accepts valid arguments for `generate_frequency_inclusion`, such as: </span>
      <ul>
        <li>`exclude_total` (default is `FALSE`)</li>
        <li>`exclude_cumulative` (default is `FALSE`)</li>
        <li>`exclude_zero_value` (default is `TRUE`)</li>
      </ul>
    </td>
  </tr>
</table>


**Example 1.1**: Basic usage

```r
dplyr::starwars |> 
  generate_frequency(sex) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

**Example 1.2**: Add grouping variable and define label from `x`

```r
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_group = 'gender', 
    x_label = 'Sex'
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

**Example 1.3**: Add grouping variable, use `x` as group, and exclude column total

```r
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_group = 'gender', 
    x_as_group = TRUE, 
    exclude_total = TRUE
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

**Example 1.4**: Exclude cumulative values and sort the output by frequency

```r
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_label = 'Sex', 
    sort_frequency = TRUE, 
    exclude_cumulative = TRUE
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

**Example 1.5**: Exclude cumulative values and define multiple grouping variables

```r
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_group = c('skin_color', 'gender'), 
    x_label = 'Sex', 
    exclude_cumulative = TRUE
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```


### 2. **`generate_crosstab`** 

**`generate_crosstab`** extends the functionality of `generate_frequency` by allowing you to generate cross-tabulations of two (2) or more categorical variables. 

```r
generate_crosstab(
  .data,
  x,
  y,
  x_group = NULL,
  y_group = NULL,
  x_label = tsg_get_config("x_label"),
  x_as_group = FALSE,
  y_group_separator = ">",
  ...
)
```

**Parameters**: 

<table>
  <tr>
    <td>`.data`</td>
    <td><b>Required</b>. A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.</td>
  </tr>
  <tr>
    <td>`x`</td>
    <td><b>Required</b>. Variable to be used as categories.</td>
  </tr>
  <tr>
    <td>`y`</td>
    <td><b>Required</b>. Variable to be used as categories in the column. `NA` will be automatically renamed to `Missing`.</td>
  </tr>
  <tr>
    <td>`x_group`</td>
    <td>Accepts a vector of string/character as grouping variables.</td>
  </tr>
  <tr>
    <td>`y_group`</td>
    <td>Accepts a vector of string/character as grouping variables in the column.</td>
  </tr>
  <tr>
    <td>`x_label`</td>
    <td>Stubhead label or label for `x`.</td>
  </tr>
  <tr>
    <td>`x_as_group`</td>
    <td>Use `x` as top-level grouping. Applicable only if `x_group` is specified.</td>
  </tr>
  <tr>
    <td>`y_group_separator`</td>
    <td>A character string that defines the column separator to be used to show table hierarchy.</td>
  </tr>
  <tr>
    <td>`...`</td>
    <td>
      <p>Accepts valid arguments for `generate_crosstab_inclusion`, such as: </p>
      <ul>
        <li>`exclude_total` (default is `FALSE`)</li>
        <li>`exclude_frequency` (default is `FALSE`)</li>
        <li>`exclude_proportion` (default is `FALSE`)</li>
      </ul>
      <p>Also accepts valid arguments for `generate_crosstab_total`, such as: </p>
      <ul>
        <li>`total_by` (default is `row`). Also accepts `col` or `column` for columnwise total (see examples)</li>
        <li>`group_values_by` (default is `statistics`). Also accepts `indicators` as grouping hierarchy for the columns (see example)</li>
        <li>`format_to_percent` (default is `TRUE`). If `FALSE`, values will be formatted as proportion.</li>
      </ul>
    </td>
  </tr>
</table>

**Example 2.1**: Basic usage

```r
dplyr::starwars |> 
  generate_crosstab(
    x = sex, 
    y = gender
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

**Example 2.2**: Percent/proportion total by column

```r
dplyr::starwars |> 
  generate_crosstab(
    x = sex, 
    y = gender, 
    y_group_separator = '_',
    total_by = 'column'
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

### 3. **`generate_multiple_response`**

```r
generate_multiple_response(
  .data,
  x,
  ...,
  y = NULL,
  x_group = NULL,
  x_as_group = F,
  x_label = tsg_get_config("x_label"),
  y_group_separator = ">",
  format_to_percent = TRUE
  group_values_by = 'statistics'
)
```

**Example 3.1**: Basic usage

```r
dplyr::starwars |>
  generate_multiple_response(
    x = homeworld, 
    y = films
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```


```r
dplyr::starwars |>
  generate_multiple_response(
    x = name, 
    y = films,
    format_to_percent = FALSE,
    y_group_separator = '_'
  ) |>
  gt::gt() |>
  gt::tab_options(table.font.size = 13)
```

### 4. **`generate_as_list`**

```r
generate_as_list(
  .data,
  list_group,
  x,
  ...,
  fn = "generate_frequency",
  list_name_overall = "ALL",
  exclude_overall = FALSE,
  collapse_overall = TRUE,
  save_as_excel = FALSE,
  formatted = FALSE,
  filename = NULL
)
```
