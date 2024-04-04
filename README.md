
## About the `tsgx` package

<br>


**`tsgx`** stands for "**table summary generator**." This package is designed to facilitate generation of statistical summary tables with ease. It also adheres to the `tidyverse` specifications.

The package allows you to:

- generate frequency tables, cross-tabulations (2-way table or more);
- extract multiple-letter response variable from survey data;
- include frequency and/or percent distributions in the generated tables;
- specify whether the 'percent to total' is computed by row (default) or by column;
- export Excel file with default formatting/styling which is can also be customized.

## Installation

You may install the `tsgx` package either from GitHub or R-CRAN.

```r
# Install devtools if not yet installed in your machine
if(!('devtools' %in% installed.packages()[,'Package'])){
   install.packages('devtools')
}

# Install the package from GitHub
devtools::install_github('yng-me/tsgx')

# Install via R-CRAN
install.packages('tsgx')

```

Then load the package after installation.

```r
library(tsgx)
```


## `tsgx` core functions

### 1. **`generate_frequency`**

This function allows you to generates a frequency distribution table (marginal table) of a categorical variable `x` specified in its second argument. It returns five (5) columns by default if `x_group` is not specified. These include (1) categories of `x`, (2) frequency of each category, (3) percent to total, (4) cumulative frequency, and (5) cumulative percent to total.

```r
generate_frequency(
  .data,
  x,
  x_group = NULL,
  x_label = get_config('x_label'),
  sort_frequency = FALSE,
  x_as_group = FALSE,
  include_total = TRUE,
  include_cumulative = TRUE,
  exclude_zero_value = FALSE
)
```


**Parameters**: 

<table>
  <tr>
    <td>`.data`</td>
    <td>
      <b>Required</b>. 
      A data frame, data frame extension (e.g. a `tibble`), a lazy data frame (e.g. from `dbplyr` or `dtplyr`), or Arrow data format.
    </td>
  </tr>
  <tr>
    <td>`x`</td>
    <td><b>Required</b>. Variable to be used as categories.</td>
  </tr>
  <tr>
    <td>`x_group`</td>
    <td>Accepts a vector of string/character as grouping variables present in the input `.data`.</td>
  </tr>
  <tr>
    <td>`x_label`</td>
    <td>Stubhead label or label for `x`.</td>
  </tr>
  <tr>
    <td>`x_as_group`</td>
    <td>Use `x` variable as top level grouping</td>
  </tr>
  <tr>
    <td>`sort_frequency`</td>
    <td>Whether to sort the output. If set to `TRUE`, the frequency will be sorted in descending order.</td>
  </tr>
  <tr>
    <td>`include_total`</td>
    <td>Whether to include row total.</td>
  </tr>
   <tr>
    <td>`include_cumulative`</td>
    <td>Whether to cumulative frequencies.</td>
  </tr>
   <tr>
    <td>`exclude_zero_value`</td>
    <td>Whether to drop categories with zero (0) values</td>
  </tr>
</table>


**Example 1.1**: Basic usage

```r
library(palmerpenguins)

generate_frequency(penguins, species)

```

**Example 1.2**: Add grouping variable and define label for `x`

```r
penguins |> 
  generate_frequency(
    x = sex, 
    x_group = 'species', 
    x_label = 'Sex'
  )
```

**Example 1.3**: Add grouping variable, use `x` as group, and exclude column total

```r
penguins |> 
  generate_frequency(
    x = sex, 
    x_group = 'species', 
    x_as_group = TRUE, 
    include_total = FALSE
  )
```

**Example 1.4**: Exclude cumulative values and sort the output by frequency

```r
penguins |> 
  generate_frequency(
    x = species, 
    x_label = 'Species', 
    sort_frequency = TRUE, 
    include_cumulative = FALSE
  )
```

**Example 1.5**: Exclude cumulative values and define multiple grouping variables

```r
dplyr::starwars |> 
  generate_frequency(
    x = sex, 
    x_group = c('skin_color', 'gender'), 
    x_label = 'Sex', 
    include_cumulative = FALSE
  )
```


### 2. **`generate_crosstab`** 

**`generate_crosstab`** extends the functionality of `generate_frequency` by allowing you to generate cross-tabulations of two (2) or more categorical variables.

```r
generate_crosstab(
  .data,
  x,
  y = NULL,
  x_group = NULL,
  y_group = NULL,
  x_label = get_config('x_label'),
  y_group_separator = '>',
  x_as_group = FALSE,
  total_by = 'row',
  group_values_by = 'statistics',
  include_frequency = TRUE,
  include_proportion = TRUE,
  include_column_total = TRUE,
  convert_to_percent = TRUE,
  format_precision = 2,
  total_label = NULL,
  ...
) 
```

**Parameters**: 

<table>
  <tr>
    <td>`.data`</td>
    <td>
      <b>Required</b>. 
      A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.</td>
  </tr>
  <tr>
    <td>`x`</td>
    <td><b>Required</b>. Variable to be used as categories.</td>
  </tr>
  <tr>
    <td>`y`</td>
    <td>Variable to be used as columns (like in pivot_wider). If not supplied, `generate_frequency` will used in the function call. 
    </td>
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
    <td>Use `x` variable as top level grouping</td>
  </tr>
  <tr>
    <td>`y_group_separator`</td>
    <td>A character string that defines the column separator to be used to show table hierarchy.</td>
  </tr>
  <tr>
    <td>`total_by`</td>
    <td>Accepts `row` | `column`. Whether to apply the sum columnwise or rowwise.</td>
  </tr>
  <tr>
    <td>`group_values_by`</td>
    <td>Accepts `statistics` | `indicators`.</td>
  </tr>
  <tr>
    <td>`include_frequency`</td>
    <td>Whether to include frequency columns.</td>
  </tr>
  <tr>
    <td>`include_proportion`</td>
    <td>Whether to include proportion/percentage columns.</td>
  </tr>
  <tr>
    <td>`include_column_total`</td>
    <td>Whether to include column total.</td>
  </tr>
  <tr>
    <td>`convert_to_percent`</td>
    <td>Whether to format to percent or proportion.</td>
  </tr>
  <tr>
    <td>`format_precision`</td>
    <td>*[Not yet implemented]* Specify the precision of rounding the percent or proportion. Default is `2`.</td>
  </tr>
  <tr>
    <td>`total_label`</td>
    <td>Whether to rename the column total.</td>
  </tr>
  <tr>
    <td>`...`</td>
    <td>Valid arguments for `generate_frequency`.</td>
  </tr>
</table>

**Example 2.1**: Basic usage

```r
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex
  )
```

**Example 2.2**: Percent/proportion total by column

```r
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex,
    total_by = 'column'
  )
```

**Example 2.3**: Exclude frequencies

```r
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex,
    include_frequency = F
  )
```

**Example 2.4**: Exclude percentages/proportions

```r
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex,
    include_proportion = F
  )
```

**Example 2.5**: Add row grouping variable and exclude percentages/proportions

```r
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex, 
    x_group = 'island',
    include_proportion = F
  )
```

**Example 2.6**: Add column grouping variable, exclude frequencies, and convert_to_percent set to `FALSE`.

```r
penguins |> 
  generate_crosstab(
    x = species, 
    y = sex, 
    y_group = 'island',
    convert_to_percent = F,
    include_frequency = F
  )
```


### 3. **`generate_multiple_response`**

This function allows you to generate summary table from a multiple response category.

```r
generate_multiple_response(
  .data,
  x,
  ...,
  y = NULL,
  x_group = NULL,
  x_label = get_config('x_label'),
  x_as_group = FALSE,
  y_group_separator = '>',
  group_values_by = 'statistics',
  value_to_count = 1,
  include_frequency = TRUE,
  include_proportion = TRUE,
  format_precision = 2,
  convert_to_percent = TRUE
) 
```


**Parameters**: 

<table>
  <tr>
    <td>`.data`</td>
    <td>
      <b>Required</b>. 
      A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.</td>
  </tr>
  <tr>
    <td>`x`</td>
    <td><b>Required</b>. Variable to be used as categories.</td>
  </tr>
  <tr>
    <td>`...`</td>
    <td>Columns with binary-coded response (generally). Use tidyselect specification.</td>
  </tr>
  <tr>
    <td>`y`</td>
    <td>Column variable to specify for a letter-coded response.</td>
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
    <td>`x_as_group`</td>
    <td>Use `x` as top-level grouping. Applicable only if `x_group` is specified.</td>
  </tr>
  <tr>
    <td>`y_group_separator`</td>
    <td>A character string that defines the column separator to be used to show table hierarchy.</td>
  </tr>
  <tr>
    <td>`group_values_by`</td>
    <td>Accepts `statistics` | `indicators`.</td>
  </tr>
  <tr>
    <td>`include_frequency`</td>
    <td>Whether to include frequency columns.</td>
  </tr>
  <tr>
    <td>`include_proportion`</td>
    <td>Whether to include proportion/percentage columns.</td>
  </tr>
  <tr>
    <td>`convert_to_percent`</td>
    <td>Whether to format to percent or proportion.</td>
  </tr>
  <tr>
    <td>`format_precision`</td>
    <td>*[Not yet implemented]* Specify the precision of rounding the percent or proportion. Default is `2`.</td>
  </tr>
</table>


**Example 3.1**: Basic usage (extract multiple-letter response)

```r

df <- data.frame(
  category = c("G1", "G1", "G2", "G1", "G2", "G1"),
  response = c("AB", "AC", "B", "ABC", "AB", "C"),
  A = c(1, 1, 0, 1, 1, 0),
  B = c(1, 0, 1, 1, 1, 0),
  C = c(0, 1, 0, 1, 0, 1)
) 

df |> generate_multiple_response(category, y = response)
```

**Example 3.1**: Basic usage (wide format multiple response)

```r
df |> generate_multiple_response(category, A:C)
```

### 4. **`generate_as_list`**

```r
generate_as_list(
  .data,
  list_group,
  x,
  ...,
  fn = 'generate_crosstab',
  list_name_overall = 'ALL',
  exclude_overall = FALSE,
  collapse_overall = TRUE,
  save_as_excel = FALSE,
  formatted = TRUE,
  filename = NULL
)
```


**Example 2.1**: Basic usage

```r
penguins |> 
  generate_as_list(
    list_group = island,
    x = species, 
    sex
  )
```
