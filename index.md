# tsg

The `tsg` (**T**able and **S**ummary **G**enerator) package provides
functions for generating publication-ready frequency tables and
cross-tabulations from data frames. Tables can be exported to **Excel
(XLSX)**, **HTML**, **PDF**, and **Word (DOCX)** formats with extensive
styling options via a unified facade system.

## Installation

``` r

install.packages("tsg")
```

Excel output works out of the box. The other formats need packages that
are not installed with `tsg`;
[`write_html()`](https://yng-me.github.io/tsg/reference/write_html.md),
[`write_pdf()`](https://yng-me.github.io/tsg/reference/write_pdf.md),
[`write_docx()`](https://yng-me.github.io/tsg/reference/write_docx.md)
and
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
offer to install them the first time you use them, or install them
yourself:

``` r

install.packages("gt")                       # HTML
install.packages(c("gt", "webshot2"))        # PDF (plus qpdf to combine tables into one file)
install.packages(c("officer", "flextable"))  # Word
install.packages("googledrive")              # Google Sheets
```

## Basic usage

``` r

library(tsg)

# Frequency table
generate_frequency(person_record, sex)

# Cross-tabulation
generate_crosstab(person_record, marital_status, sex)

# Summary statistics of numeric variables
generate_summary(person_record, age)
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

# Google Sheets
write_gsheet(tbl, "Marital status by sex")   # requires googledrive
```

## Styling with facades

The facade system controls table appearance uniformly across all output
formats:

``` r

person_record |>
  generate_frequency(sex) |>
  add_facade(
    header.fgFill = "#003366",
    header.fontColour = "#FFFFFF",
    header.textDecoration = "bold",
    body.fontSize = 11,
    table.lastRowBold = TRUE
  ) |>
  write_xlsx("styled.xlsx")
```

Built-in facades are `"default"`, `"yolo"`, `"apa"`, `"booktabs"`,
`"striped"`, `"gridded"`, `"minimal"`, `"financial"`, `"nyt"`, `"jama"`,
`"dark"`, `"pastel"` and `"census"`:

``` r

write_xlsx(tbl, "booktabs.xlsx", facade = "booktabs")
```

More examples in the
[vignettes](https://yng-me.github.io/tsg/articles/tsg.html).
