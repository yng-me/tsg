# Convert labelled factors to regular factors

Convert labelled factors to regular factors

## Usage

``` r
convert_factor(data)
```

## Arguments

- data:

  A data frame, tibble, or `tsg` object containing labelled factors.

## Value

A data frame with labelled factors converted to regular factors.

## Examples

``` r
df <- data.frame(
  category = haven::labelled(
    c(1, 2, 3),
    c("One" = 1, "Two" = 2, "Three" = 3)
   )
 )

df_converted <- convert_factor(df)
```
