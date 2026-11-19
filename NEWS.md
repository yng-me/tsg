# tsg 0.2.0

## Breaking changes
* HTML, PDF and Word tables only draw the borders their facade asks for, so the default HTML and Word tables have slightly different lines than before. The built-in facade files moved to `inst/extdata/facade/<name>.yaml`; the per-format defaults are in `inst/extdata/facade/default/`.
* `gt`, `officer`, `flextable` and `qpdf` are no longer installed with tsg. `write_html()`, `write_pdf()` and `write_docx()` offer to install them the first time they are needed.
* `get_tsg_facade()` now errors on an unknown facade name and lists the built-in ones, instead of silently using the default facade.
* In a facade file, a key set to `~` now turns that setting off (e.g. `body.border: ~` removes the body borders). It used to fall back to the default value.
* `write_xlsx()` no longer cuts sheet names longer than Excel's 31 characters without telling you: such a name is now an error. Set the new `trim_sheet_names = TRUE` to cut them, as before. File names (`separate_files = TRUE`) are no longer cut.
* A grouping variable named `category` is renamed to `category_group`, with a message.
* In a table-list reference (`include_table_list = TRUE`, `table_list_reference`), `table_id` is the list name, matching the sheet name, instead of the table title.

## New features
* `generate_summary()` builds a table of descriptive statistics for numeric variables (`n`, `n_missing`, `mean`, `sd`, `median`, `min`, `max`, `p25`, `p75`), one row per variable, per group with `dplyr::group_by()`.
* `write_gsheet()` writes tables to a Google Sheet, with the same styling as Excel (requires `googledrive`). A name creates a new sheet; an existing sheet (`googledrive::as_id()`) is updated in place, so its link stays the same. `generate_output()` gains `format = "gsheet"`. See `vignette("google-sheets")`.
* New vignette, `vignette("write-xlsx")`, shows the workbook each table structure produces, including `table_list_reference`.
* Database tables (`dplyr::tbl()`) work in `generate_frequency()`, `generate_crosstab()` and `generate_summary()`. The counting runs in the database, and only the results come back. Databases have no labels, so categories show their stored values and `expand_categories` has no effect.
* Eleven new built-in facades: `"apa"`, `"booktabs"`, `"striped"`, `"gridded"`, `"minimal"`, `"financial"`, `"nyt"`, `"jama"`, `"dark"`, `"pastel"` and `"census"`, e.g. `write_xlsx(x, path, facade = get_tsg_facade("booktabs"))`.
* Built-in facades look the same in every output format: HTML, PDF and Word now draw a facade's borders (`border_outer`, `border_header`, `header`, `spanner`, `body`) and banded rows (`body.bandFill`), not only its fonts and fills. Border styles use the Excel names (`"thin"`, `"medium"`, `"dashed"`, ...) everywhere; an unknown one is an error.
* `as_gt()` returns the styled `gt` table that `write_html()` saves, for Quarto, R Markdown or Shiny.
* Every writer takes a built-in facade name or a facade file path as well as a list, e.g. `write_docx(x, path, facade = "booktabs")`.
* New facade settings: `border_outer.border` chooses which outer sides of a table get a border, and `body.bandFill` shades every other body row.
* `generate_crosstab(collapse_list = TRUE)` combines crosstabs against several column variables side by side in one table.
* `add_facade()` accepts every facade setting and errors on a misspelt one. `add_facade_alt()` is deprecated.

## Improvements
* Facade settings follow one order of precedence everywhere: built-in default, then the `facade` given to the writer, then `add_facade()` on a list, then `add_facade()` on a table. A partial `facade` list keeps the defaults for everything it leaves out.
* For a table in a list, its own title, subtitle, source note and footnotes win over the `write_xlsx()` arguments.
* `label_stub` now replaces the header of the category column in Excel output.
* `spanner.*` settings style the upper rows of multi-level headers.
* Faster table generation with many groups.

## Bug fixes
* Grouping variables with missing values: nested group lists (`group_as_list = TRUE` with 2+ groups) keep the missing group as `"NA"` instead of dropping its rows, as one-group lists already did, and `group_as_hierarchy = TRUE` tables no longer get rows of empty values.
* `write_html()`, `write_pdf()` and `write_docx()` no longer fail on a table grouped by a variable with value labels but no variable label.
* Crosstab lists (`group_as_list = TRUE`) keep the group column in the table of a group coded 0 or missing.
* `write_html()`, `write_pdf()` and `write_docx()` with `separate_files = TRUE` clean table names into file names, as `write_xlsx()` does, so a name like "Single/never married" no longer fails.
* `group_as_hierarchy = TRUE` subtotals are no longer wrong when the data has a column named `pv`.
* `generate_crosstab()` checks that `data` is a data frame, as `generate_frequency()` does.
* `generate_crosstab()` no longer fails with "Can't convert `x` <haven_labelled> to <character>" in a fresh session before anything else has loaded `haven`.
* Writing lists from `group_as_list = TRUE` with two or more groups no longer fails in `write_html()`, `write_pdf()`, `write_docx()`, or in `write_xlsx()` and `generate_output()` with three or more groups. Each table is named by its group path, e.g. `"Male - Yes"`.
* The Total row no longer takes a code that a value label uses (e.g. an unused `0 = "None"`), which showed the Total as that category.
* The code for missing values no longer reuses a labelled code absent from the data (e.g. `8 = "Don't know"`).
* `generate_crosstab()` with several `x` variables keeps its Total row, labels missing values with `label_na`, and works with `group_as_hierarchy`.
* `generate_crosstab(add_total_row = FALSE)` drops the Total row of every group, not only the last one.
* `generate_crosstab()` without column variables respects `add_percent`.
* Grouping by a column named `category`, or by a numeric column with a value of 0, no longer fails.
* `group_as_list = TRUE` with two or more groups no longer returns empty lists when the data has a column named `v`.
* `group_as_hierarchy = TRUE` works with labelled grouping variables.
* Labelled columns without a variable label no longer show their value labels as the column header.
* `write_xlsx()` no longer writes an empty workbook for a list without names; unnamed items become sheets "Table 1", "Table 2", ...
* `write_xlsx()` writes one-column tables, numbers sheet names that clash after being cut to 31 characters (`... (2)`), and passes `...` on to `openxlsx` correctly.
* Footnotes: the `footnotes` argument applies to lists of tables, footnotes are no longer overwritten in one-sheet lists, footnotes given through `metadata` work, and placement values such as `"auto"` no longer appear as footnote text.
* `write_xlsx(row_group_as_column = TRUE)` now writes group columns on the sheets of a list grouped by two or more variables; it was ignored there.
* In Excel, categories under a row-group title are indented one level deeper than the title, and the rule-only facades (`"apa"`, `"booktabs"`, `"jama"`, `"nyt"`, `"minimal"`) line the first column up with the title instead of indenting it.
* Table-list links point to the right sheet.
* `write_gsheet(include_table_list = TRUE)`: the table-list and "Back" links open the right tab in Google Sheets.
* `generate_crosstab(multiple_columns = TRUE, multiple_columns_type = "stacked")` on grouped data keeps its stacked column headers and column order (#13).
* In Excel, the category column and grouping columns written with `row_group_as_column` fit their content. The default facade no longer sets `col_first.width` (it was 32); set it, or `row_group.width`, for a fixed width. The columns of the table-list sheet fit their content too, with titles up to 148 characters wide. The category column gets `col_first.width` even when grouping columns come before it.
* An empty `subtitle`, `source_note` or `footnotes` cell in `table_list_reference` (e.g. `NA`) no longer fails, and keeps the table's own text.
* Decimal places are chosen the same way in Excel, HTML, PDF and Word: a column with fractional values gets `table.decimalPrecision` decimals, other numbers are whole with thousands separators, and `table.decimalCols` names exactly the columns with decimals (Word ignored it before, HTML only added decimals). `table.decimalPrecision = 0` no longer leaves a trailing decimal point.
* In a grouped Excel sheet, the number format no longer comes from the last group alone: percents such as 33.33 are no longer shown as 33 when the last group's percents are whole.
* Facade values with apostrophes, and multi-value settings such as `textDecoration = c("bold", "italic")`, now work. A facade file can no longer run R code.
* `add_facade()` accepts variables and negative numbers.
* `remove_labels()` clears value labels in every table of a list.
* With `top_n` and `as_proportion = TRUE`, the "Others" row shows proportions.
* `add_column_total()` works with a custom `name`.
* `collapse_list(pluck = ...)` drops proportion columns when `as_proportion = TRUE`.
* `generate_summary()` reports `NA`, not `Inf`, for a group with no values, and says so clearly when there is no column to summarise.

# tsg 0.1.4

## New features
* `generate_crosstab()` gains `multiple_columns` and `multiple_columns_filter` arguments. When `multiple_columns = TRUE` (requires at least 2 columns in `...`), each column is treated as a binary indicator: rows where the column equals `multiple_columns_filter` (default `1L`) are counted per `x` category and presented as side-by-side frequency/percent columns in a single wide table. All grouping options (`group_as_list`, `calculate_per_group`, `group_as_hierarchy`) are supported.
* `generate_crosstab()` gains a `multiple_columns_type` argument (default `"default"`). When `multiple_columns = TRUE` and `multiple_columns_type = "stacked"`, the columns supplied in `...` are combined as a two-level hierarchical pivot: each level-1 category gets a subtotal column pair (frequency + percent), and the level-2 categories produce individual column pairs beneath it. Column labels follow the pattern `"Frequency__<level-1 label>__<level-2 label>"`. The top-level spanner groups all frequency columns first, followed by all percent/proportion columns.
* `generate_output()` now produces HTML (via the `gt` package) and PDF outputs, with proper spanner/header handling for cross-tabulated tables and consistent numeric formatting matching the XLSX output.
* `generate_frequency()` and `generate_crosstab()`: setting both `group_as_list = TRUE` and `group_as_hierarchy = TRUE` now returns a **nested named list** with a grand-total entry at each level (no warning). The total entry key is formatted as `"{var_label}: {label_group_hierarchy}"`. Previously, `group_as_list` was silently overridden with a warning.
* **Facade unification across formats**: The facade system (`add_facade()`, `get_tsg_facade()`) now applies consistently across XLSX, HTML, and Word output. `get_tsg_facade(which = "html")` and `get_tsg_facade(which = "docx")` load format-specific defaults from new built-in YAML files (`inst/extdata/facade/html/default.yaml`, `inst/extdata/facade/docx/default.yaml`). Per-table `add_facade()` overrides are merged into all output formats automatically.

## Bug fixes
* Fixed `get_tsg_facade()` throwing `object 'facade_path' not found` when called with a named facade that does not exist in the built-in YAML directory (#9).
* Fixed `sort_except` in `generate_frequency()` being applied only to the first variable when two or more variables are supplied (#8).
* Fixed incorrect cumulative frequency and percent/proportion values when `position_total = "top"` in `generate_frequency()`. Cumulative columns are now set to `NA` for the total row when it is placed at the top (#6).

## Documentation
* All four vignettes rewritten for clarity and accessibility: goal-oriented section headers, plain-English introductions, and motivation-before-mechanics structure throughout.
  * *Getting Started with tsg* (was: Introduction)
  * *Saving and Sharing Your Tables* (was: Generating Outputs in Different Formats)
  * *Customizing How Your Tables Look* (was: Customizing Output with Facade)
  * *Grouped Tables and Side-by-Side Comparisons* (was: Advanced Features)

# tsg 0.1.3

* Fixed bugs when generating table using character labelled/factored variables.

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
