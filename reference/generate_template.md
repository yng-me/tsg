# Generate a template

Generate a template

## Usage

``` r
generate_template(path, template = c("facade", "table-list"))
```

## Arguments

- path:

  A character string specifying the path where the template should be
  saved. If a directory is provided, the template will be saved with a
  default name based on the template type.

- template:

  A character string specifying the type of template to generate.
  Options are "facade" for a YAML facade template or "table-list" for an
  Excel table list template.

## Value

Void. A file path where the template has been saved.

## Examples

``` r
template_path_facade <- tempfile(fileext = ".yaml")
generate_template(template_path_facade, template = "facade")

template_path_table_list <- tempfile(fileext = ".xlsx")
generate_template(template_path_table_list, template = "table-list")

unlink(template_path_facade)
unlink(template_path_table_list)
```
