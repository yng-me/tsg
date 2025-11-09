# Sample dataset of persons

This is a synthetic dataset containing person information for
demonstration purposes.

## Usage

``` r
person_record
```

## Format

A labelled data frame with 2918 rows and 11 variables:

- person_id:

  Numeric identifier for each person

- sex:

  Factor indicating the sex of the person

- age:

  Numeric age of the person

- marital_status:

  Factor indicating marital status

- employed:

  Employment status

- seeing:

  Functional difficulty in seeing

- hearing:

  Functional difficulty in hearing

- walking:

  Functional difficulty in walking

- remembering:

  Functional difficulty in remembering

- self_caring:

  Functional difficulty in self-caring

- communicating:

  Functional difficulty in communicating

## Examples

``` r
person_record
#> # A tibble: 2,918 × 11
#>    person_id   age sex        marital_status    employed seeing  hearing walking
#>        <int> <int> <int+lbl>  <int+lbl>         <int+lb> <int+l> <int+l> <int+l>
#>  1         1    46 1 [Male]   2 [Married]        1 [Yes] 1 [No … 1 [No … 1 [No …
#>  2         2    44 2 [Female] 2 [Married]        1 [Yes] 1 [No … 1 [No … 1 [No …
#>  3         3    10 1 [Male]   1 [Single/never … NA       1 [No … 1 [No … 1 [No …
#>  4         4    65 2 [Female] 2 [Married]        1 [Yes] 1 [No … 1 [No … 1 [No …
#>  5         5    65 1 [Male]   2 [Married]        1 [Yes] 1 [No … 1 [No … 1 [No …
#>  6         6    28 2 [Female] 1 [Single/never …  2 [No]  1 [No … 1 [No … 1 [No …
#>  7         7    25 2 [Female] 1 [Single/never …  2 [No]  1 [No … 1 [No … 1 [No …
#>  8         8    54 1 [Male]   2 [Married]        2 [No]  1 [No … 1 [No … 1 [No …
#>  9         9    50 2 [Female] 2 [Married]        1 [Yes] 1 [No … 1 [No … 1 [No …
#> 10        10    29 1 [Male]   1 [Single/never …  2 [No]  1 [No … 1 [No … 1 [No …
#> # ℹ 2,908 more rows
#> # ℹ 3 more variables: remembering <int+lbl>, self_caring <int+lbl>,
#> #   communicating <int+lbl>
```
