# Apply numeric cell formatting to a gt table based on facade settings

Mirrors `xlsx_decimal_format()` for gt output: the columns are chosen by
`number_format_cols()`, decimals use `table.decimalPrecision` (default
2), and `body.numFmt`, `col_first.numFmt` and `col_last.numFmt` override
it.

## Usage

``` r
gt_apply_numeric_format(gt_tbl, data, facade)
```
