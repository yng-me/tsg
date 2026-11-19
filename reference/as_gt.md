# Convert a tsg table to a gt table

Builds the gt table that
[`write_html()`](https://yng-me.github.io/tsg/reference/write_html.md)
and [`write_pdf()`](https://yng-me.github.io/tsg/reference/write_pdf.md)
save, styled with a facade, so you can show it in a Quarto or R Markdown
document, a Shiny app, or style it further with gt.

## Usage

``` r
as_gt(
  data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
)
```

## Arguments

- data:

  A `tsg` table or a data frame.

- title:

  Optional title; defaults to the table's title (see
  [`add_table_title()`](https://yng-me.github.io/tsg/reference/add_table_title.md)).

- subtitle:

  Optional subtitle; defaults to the table's subtitle.

- source_note:

  Optional source note; defaults to the table's source note.

- footnotes:

  Optional character vector of footnotes; defaults to the table's
  footnotes.

- names_separator:

  Column name separator for spanners. Default `"__"`.

- facade:

  A built-in facade name (e.g. `"booktabs"`; see
  [`get_tsg_facade()`](https://yng-me.github.io/tsg/reference/get_tsg_facade.md)),
  the path to a `.yaml` or `.json` facade file, or a facade list. Keys
  missing from it fall back to the html default facade; settings added
  with
  [`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
  take precedence over it.

## Value

A `gt_tbl` object.

## Examples

``` r
person_record |>
  generate_crosstab(marital_status, sex) |>
  add_table_title("Marital status by sex") |>
  as_gt(facade = "booktabs")


  


Marital status by sex
```
