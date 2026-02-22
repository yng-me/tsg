# tsg 0.1.2

## Features
* Added experimental feature with an option to include grand total when generating tables with grouping for both `generate_frequency()` and `generate_crosstab()`
* `generate_crosstab()` now allows 1 or more variables to be specified in the `x` argument for hierarchical grouping.
* Added `<- back` hyperlink in the generated Excel output when table list is included.

## Bug fixes
* Fixed issues when generating a cross tabulation with `NA` values

# tsg 0.1.1

## Features
* Implemented new way of adding facade to the generated Excel output.
* Added `generate_template()` for generating facade and table-list templates.

## Bug fixes
* Fixed column type consistency when generating `top_n` frequencies.

# tsg 0.1.0

* Initial CRAN submission.
