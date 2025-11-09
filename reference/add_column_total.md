# Add a column total

Add a column total

## Usage

``` r
add_column_total(data, label_total = "Total", ...)
```

## Arguments

- data:

  A data frame, tibble, or `tsg` object to which a column row will be
  added.

- label_total:

  Label for the total column. Default is "Total".

- ...:

  Additional named arguments to be added as columns alongside the total
  column.

## Value

The input data frame with an additional column representing the total of
each row.

## Examples

``` r
# Example data frame
df <- data.frame(
 category = c("A", "B", "C"),
 value1 = c(10, 20, 30),
 value2 = c(5, 15, 25)
 )
add_column_total(df)
#>   value1 value2 total
#> 1     10      5    15
#> 2     20     15    35
#> 3     30     25    55
```
