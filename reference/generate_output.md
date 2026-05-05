# Generate output in specified format (e.g., xlsx, html, pdf, word)

Generate output in specified format (e.g., xlsx, html, pdf, word)

## Usage

``` r
generate_output(data, path, ..., format = c("xlsx", "html", "pdf", "word"))
```

## Arguments

- data:

  Preferably a `tsg` class object for best results. A data frame,
  tibble, and list are also supported.

- path:

  File path to save the output. For HTML/PDF with a list and
  `separate_files = TRUE` (HTML) or any list (PDF), this is treated as a
  directory path.

- ...:

  Additional arguments passed to specific format functions:

  - `"xlsx"`: passed to
    [`write_xlsx`](https://yng-me.github.io/tsg/reference/write_xlsx.md)

  - `"html"`: passed to
    [`write_html`](https://yng-me.github.io/tsg/reference/write_html.md)
    (requires the gt package)

  - `"pdf"`: passed to
    [`write_pdf`](https://yng-me.github.io/tsg/reference/write_pdf.md)
    (requires the gt and webshot2 packages)

  - `"word"`: passed to
    [`write_docx`](https://yng-me.github.io/tsg/reference/write_docx.md)
    (requires the officer and flextable packages)

- format:

  Output format. One of `"xlsx"`, `"html"`, `"pdf"`, or `"word"`.

## Value

Invisibly returns `NULL`. Called for its side-effect of writing output
file(s).

## Examples

``` r
# Generate an xlsx file from a tsg object
data <- generate_frequency(dplyr::starwars, sex)

dir_to <- tempdir()
generate_output(
  data,
  file.path(dir_to, "starwars_frequency.xlsx"),
  format = "xlsx"
)

unlink(file.path(dir_to, "starwars_frequency.xlsx"))
```
