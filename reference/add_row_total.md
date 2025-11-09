# Add a row total

Add a row total

## Usage

``` r
add_row_total(
  data,
  position = c("bottom", "top"),
  label_total = "Total",
  fill = "-"
)
```

## Arguments

- data:

  A data frame, tibble, or `tsg` object to which a total row will be
  added.

- position:

  Position to add the total row. Either "bottom" (default) or "top".

- label_total:

  Label for the total row in the category column. Default is "Total".

- fill:

  Character. Value to fill in for missing numeric columns in the total
  row. Default is "-".

## Value

The input data frame with an additional row representing the total of
numeric columns.

## Examples

``` r
# Example data frame
df <- data.frame(
 category = c("A", "B", "C"),
 value1 = c(10, 20, 30),
 value2 = c(5, 15, 25)
)

df_with_total <- add_row_total(df)
df_with_total_top <- add_row_total(df, position = "top")
```
