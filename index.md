# tsg

The `tsg` package provides functions for generating publication-ready
frequency tables and cross-tabulations from data frames. Tables can be
exported to **Excel (XLSX)**, **HTML**, **PDF**, and **Word (DOCX)**
formats with extensive styling options via a unified facade system.

## Installation

``` r

install.packages("tsg")
```

`tsg` requires the following packages: `gt` (HTML/PDF), `officer` and
`flextable` (Word). These are installed automatically. For PDF output,
`webshot2` is also needed:

``` r

install.packages("webshot2")  # optional, for PDF output
```

## Basic usage

``` r

library(tsg)

# Frequency table
generate_frequency(person_record, sex)

# Cross-tabulation
generate_crosstab(person_record, marital_status, sex)
```

## Output formats

``` r

tbl <- person_record |>
  generate_crosstab(marital_status, sex)

# Excel
write_xlsx(tbl, "table.xlsx")

# HTML
write_html(tbl, "table.html")

# PDF
write_pdf(tbl, "table.pdf")   # requires webshot2

# Word
write_docx(tbl, "table.docx")
```

## Styling with facades

The facade system controls table appearance uniformly across all output
formats:

``` r

person_record |>
  generate_frequency(sex) |>
  add_facade(
    header.bgFill         = "#003366",
    header.fontColour     = "#FFFFFF",
    header.textDecoration = "bold",
    body.fontSize         = 11,
    table.lastRowBold     = TRUE
  ) |>
  write_xlsx("styled.xlsx")
```

More examples in the
[vignettes](https://yng-me.github.io/tsg/articles/tsg.html).
