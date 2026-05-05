# Copilot Instructions for `tsg`

## Package overview

`tsg` is an R package that generates publication-ready frequency tables and cross-tabulations from data frames, with export to Excel (xlsx) as the primary output format. HTML, PDF, and Word formats are not yet implemented.

## Build, test, and lint commands

```r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-generate_frequency.R")

# Document (roxygen2 → NAMESPACE + man/)
devtools::document()

# Full package check (CRAN-level)
devtools::check()
```

## Architecture

The package follows a three-layer pipeline:

**1. Compute layer** (`generate_frequency.R`, `generate_crosstab.R`, `tsg.R`)
- Public entry points are `generate_frequency()` and `generate_crosstab()`
- Both return a `tsg`/`tsgf` S3 class object (a data frame with metadata as attributes)
- The internal column name during computation is `.category`; it is renamed to `category` only at the end of the pipeline
- Low-level builder helpers (`build_frequency_table`, `build_crosstab_table`) live in `tsg.R` and are called by the public functions

**2. Styling/facade layer** (`facade.R`, `inst/extdata/facade/`)
- Styling is stored as attributes on the `tsg` object via `add_facade()` / `add_facade_alt()`
- `get_tsg_facade()` loads a named YAML config from `inst/extdata/facade/{format}/{name}.yaml` (default: `xlsx/default.yaml`) or a user-supplied YAML/JSON file path
- Facade parameters use a `section.property` naming convention (e.g., `header.bgFill`, `body.fontSize`, `table.offsetRow`)
- `resolve_facade()` merges a built-in facade with per-object `add_facade()` overrides

**3. Output layer** (`xlsx.R`, `xlsx_writer.R`, `xlsx_facade.R`, `xlsx_components.R`, `xlsx_helpers.R`)
- `write_xlsx()` is the main export function; `generate_output()` is a thin wrapper
- `xlsx_eval_style()` dynamically constructs and evaluates `openxlsx::createStyle()` calls from the facade list
- Multi-sheet exports: when `data` is a named list, each element becomes a separate worksheet (or file when `separate_files = TRUE`)

**Support files**
- `generate_helpers.R`, `helpers.R`: shared internal utilities (category expansion, NA handling, totals)
- `generate_modifiers.R`: public helpers (`add_row_total`, etc.)
- `tsg_format.R`: `format_percent()` to combine frequency + percent into a single display column
- `utils.R`: `%||%` null-coalescing operator, `is_valid_input_data()`, Arrow/lazy frame support

## Key conventions

**`tsg` S3 class and attributes**
- The returned object always has `class = c("tsg", "tsgf", ...)` for frequency tables or `c("tsg", "tsgc", ...)` for cross-tabs
- Metadata (`title`, `subtitle`, `source_note`, `footnotes`, `groups`, `label_xlsx`, etc.) is attached as R attributes, not columns
- `add_facade()` stores styling as `attr(data, "facade")`; `add_facade_alt()` is an alternative that accepts programmatically-built lists via `...`

**`haven` labelled vectors**
- The package is designed to work with `haven`-labelled survey data (SPSS/Stata imports)
- `include_na = TRUE` (default) handles missing values in labelled vectors by injecting a synthetic label via `add_missing_label()`; the NA code is auto-detected unless `recode_na` is set explicitly
- `convert_factor = FALSE` by default; set to `TRUE` to convert labelled cols to R factors in the output

**Null-coalescing pattern**
- `%||%` (defined in `utils.R`) is used extensively: `a %||% b` returns `a` if not `NULL`, else `b`

**Arrow / lazy frames**
- `is_valid_input_data()` explicitly allows `ArrowObject`, `arrow_dplyr_query`, `rcdf_tbl_db`, and `tbl_lazy` in addition to plain `data.frame`

**Internal column naming**
- During computation, the category column is `.category` (to avoid clashes with user columns named `category`)
- It is always renamed to `category` with `dplyr::rename(data_i, category = .category)` as the last step before returning

**Global variables**
- `zzz.R` declares `utils::globalVariables(...)` for variables used in NSE/tidy eval to suppress R CMD CHECK notes

**Documentation**
- All exported functions use Roxygen2 with Markdown (`Roxygen: list(markdown = TRUE)`)
- `README.md` is generated from `README.Rmd` — edit `README.Rmd`, not `README.md`
