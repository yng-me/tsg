# Changelog

## tsg 0.1.3

- Fixed bugs when generating table using character labelled/factored
  variables.

## tsg 0.1.2

CRAN release: 2026-02-22

### Features

- Added experimental feature with an option to include grand total when
  generating tables with grouping for both
  [`generate_frequency()`](https://yng-me.github.io/tsg/reference/generate_frequency.md)
  and
  [`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md)
- [`generate_crosstab()`](https://yng-me.github.io/tsg/reference/generate_crosstab.md)
  now allows 1 or more variables to be specified in the `x` argument for
  hierarchical grouping.
- Added `<- back` hyperlink in the generated Excel output when table
  list is included.

### Bug fixes

- Fixed issues when generating a cross tabulation with `NA` values

## tsg 0.1.1

CRAN release: 2026-02-12

### Features

- Implemented new way of adding facade to the generated Excel output.
- Added
  [`generate_template()`](https://yng-me.github.io/tsg/reference/generate_template.md)
  for generating facade and table-list templates.

### Bug fixes

- Fixed column type consistency when generating `top_n` frequencies.

## tsg 0.1.0

CRAN release: 2025-11-09

- Initial CRAN submission.
