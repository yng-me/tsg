# Apply numeric cell formatting to a gt table based on facade settings

Mirrors `xlsx_decimal_format()` for gt output. Integer-like columns (R
`integer` type or double columns whose values are all whole numbers) are
formatted with comma separators and no decimal places. Double columns
with fractional values are formatted with `table.decimalPrecision`
decimal places (default 2). `table.decimalCols`, `body.numFmt`,
`col_first.numFmt`, and `col_last.numFmt` override the auto-detection.

## Usage

``` r
gt_apply_numeric_format(gt_tbl, data, facade)
```
