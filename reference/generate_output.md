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

  File path to save the output.

- ...:

  Additional arguments passed to specific format functions.

- format:

  Output format. One of `"xlsx"`, `"html"`, `"pdf"`, or `"word"`.

## Value

Generates and saves the output file in the specified format at the given
path.

## Examples

``` r
#' # Generate an xlsx file from a tsg object
data <- generate_frequency(dplyr::starwars, sex)

dir_to <- tempdir()
generate_output(
  data,
  file.path(dir_to, "starwars_frequency.xlsx"),
  format = "xlsx"
)

unlink(file.path(dir_to, "starwars_frequency.xlsx"))
```
