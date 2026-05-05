# tsg 0.1.4

## New features
* `generate_crosstab()` gains `multiple_columns` and `multiple_columns_filter` arguments. When `multiple_columns = TRUE` (requires at least 2 columns in `...`), each column is treated as a binary indicator: rows where the column equals `multiple_columns_filter` (default `1L`) are counted per `x` category and presented as side-by-side frequency/percent columns in a single wide table. All grouping options (`group_as_list`, `calculate_per_group`, `group_as_hierarchy`) are supported.
* `generate_crosstab()` gains a `multiple_columns_type` argument (default `"default"`). When `multiple_columns = TRUE` and `multiple_columns_type = "stacked"`, the columns supplied in `...` are combined as a two-level hierarchical pivot: each level-1 category gets a subtotal column pair (frequency + percent), and the level-2 categories produce individual column pairs beneath it. Column labels follow the pattern `"Frequency__<level-1 label>__<level-2 label>"`. The top-level spanner groups all frequency columns first, followed by all percent/proportion columns.
* `generate_output()` now produces HTML (via the `gt` package) and PDF outputs, with proper spanner/header handling for cross-tabulated tables and consistent numeric formatting matching the XLSX output.
* `generate_frequency()` and `generate_crosstab()`: setting both `group_as_list = TRUE` and `group_as_hierarchy = TRUE` now returns a **nested named list** with a grand-total entry at each level (no warning). The total entry key is formatted as `"{var_label}: {label_group_hierarchy}"`. Previously, `group_as_list` was silently overridden with a warning.
* **Facade unification across formats**: The facade system (`add_facade()`, `get_tsg_facade()`) now applies consistently across XLSX, HTML, and Word output. `get_tsg_facade(which = "html")` and `get_tsg_facade(which = "docx")` load format-specific defaults from new built-in YAML files (`inst/extdata/facade/html/default.yaml`, `inst/extdata/facade/docx/default.yaml`). Per-table `add_facade()` overrides are merged into all output formats automatically.
* `resolve_facade()` gains a `which` argument for format-aware fallback when resolving user overrides.
* `gt_apply_facade()` (HTML renderer) now supports the full cross-format key set: `body.*`, `header.*`, `spanner.*`, `col_first.*`, `col_last.*`, `row_group.*`, `source_note.*`, `footnotes.*`, `border_outer.*`, `border_header.*`, and `table.lastRowBold`.
* `tsg_to_flextable()` (Word renderer) gains full facade key coverage with flextable-native styling.

## Bug fixes
* Fixed `get_tsg_facade()` throwing `object 'facade_path' not found` when called with a named facade that does not exist in the built-in YAML directory (#9).
* Fixed `sort_except` in `generate_frequency()` being applied only to the first variable when two or more variables are supplied (#8).
* Fixed incorrect cumulative frequency and percent/proportion values when `position_total = "top"` in `generate_frequency()` — cumulative columns are now set to `NA` for the total row when it is placed at the top (#6).

## Documentation
* All four vignettes rewritten for clarity and accessibility: goal-oriented section headers, plain-English introductions, and motivation-before-mechanics structure throughout.
  * *Getting Started with tsg* (was: Introduction)
  * *Saving and Sharing Your Tables* (was: Generating Outputs in Different Formats)
  * *Customizing How Your Tables Look* (was: Customizing Output with Facade)
  * *Grouped Tables and Side-by-Side Comparisons* (was: Advanced Features)

## Internal changes
* Refactored `generate_frequency()` and `generate_crosstab()` to remove self-recursion. Both functions now use extracted `freq_compute_group()` and `crosstab_compute_group()` helpers with a nest/map/unnest approach, eliminating duplicated pipeline code and improving maintainability.
* Added internal helpers `val_to_group_key()` and `build_nested_group_list()` to support recursive nested-list construction.

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
