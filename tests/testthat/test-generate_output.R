test_that("generate_output with format = 'xlsx' writes a file", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B", "A")), x)
  path <- tempfile(fileext = ".xlsx")
  generate_output(data, path, format = "xlsx")
  expect_true(file.exists(path))
  unlink(path)
})

test_that("generate_output default format is xlsx", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  path <- tempfile(fileext = ".xlsx")
  generate_output(data, path)
  expect_true(file.exists(path))
  unlink(path)
})

test_that("generate_output errors on invalid format", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  expect_error(generate_output(data, tempfile(), format = "csv"))
})

test_that("generate_output errors for xlsx with too-deep data structure", {
  deep <- list(list(list(list(list(data.frame(x = 1))))))
  expect_error(
    generate_output(deep, tempfile(fileext = ".xlsx"), format = "xlsx"),
    "too deep"
  )
})

test_that("generate_output format = 'html' writes a file", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B", "A")), x)
  path <- tempfile(fileext = ".html")
  generate_output(data, path, format = "html")
  expect_true(file.exists(path))
  unlink(path)
})

test_that("generate_output format = 'html' file contains table content", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B", "A")), x)
  path <- tempfile(fileext = ".html")
  generate_output(data, path, format = "html")
  html_content <- readLines(path, warn = FALSE)
  expect_true(any(grepl("<table", html_content, fixed = TRUE)))
  unlink(path)
})

test_that("generate_output format = 'html' list writes stacked single file", {
  data_list <- list(
    a = generate_frequency(dplyr::tibble(x = c("A", "B")), x),
    b = generate_frequency(dplyr::tibble(x = c("C", "D")), x)
  )
  path <- tempfile(fileext = ".html")
  generate_output(data_list, path, format = "html")
  expect_true(file.exists(path))
  html_content <- paste(readLines(path, warn = FALSE), collapse = "")
  tables <- gregexpr("<table", html_content, fixed = TRUE)[[1]]
  expect_gte(length(tables), 2L)
  unlink(path)
})

test_that("write_html list with separate_files writes directory of files", {
  data_list <- list(
    table_a = generate_frequency(dplyr::tibble(x = c("A", "B")), x),
    table_b = generate_frequency(dplyr::tibble(x = c("C", "D")), x)
  )
  dir_path <- tempfile()
  write_html(data_list, dir_path, separate_files = TRUE)
  expect_true(file.exists(file.path(dir_path, "table_a.html")))
  expect_true(file.exists(file.path(dir_path, "table_b.html")))
  unlink(dir_path, recursive = TRUE)
})

test_that("write_html adds .html extension when missing", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  path_no_ext <- tempfile()
  write_html(data, path_no_ext)
  expect_true(file.exists(paste0(path_no_ext, ".html")))
  unlink(paste0(path_no_ext, ".html"))
})

test_that("write_html stacked list contains section separators", {
  data_list <- list(
    a = generate_frequency(dplyr::tibble(x = c("A", "B")), x),
    b = generate_frequency(dplyr::tibble(x = c("C", "D")), x)
  )
  path <- tempfile(fileext = ".html")
  write_html(data_list, path)
  html_content <- paste(readLines(path, warn = FALSE), collapse = "")
  expect_true(grepl("<section", html_content, fixed = TRUE))
  unlink(path)
})

test_that("write_html passes title to each table in stacked list", {
  data_list <- list(
    foo = generate_frequency(dplyr::tibble(x = c("A", "B")), x),
    bar = generate_frequency(dplyr::tibble(x = c("C", "D")), x)
  )
  path <- tempfile(fileext = ".html")
  write_html(data_list, path, title = "Report")
  html_content <- paste(readLines(path, warn = FALSE), collapse = "")
  expect_true(grepl("Report: foo", html_content))
  expect_true(grepl("Report: bar", html_content))
  unlink(path)
})

test_that("write_html uses title/source_note from data attributes", {
  data <- generate_frequency(
    dplyr::tibble(x = c("A", "B")), x,
    metadata = list(title = "Attr Title", source_note = "Attr Source")
  )
  path <- tempfile(fileext = ".html")
  write_html(data, path)
  html_content <- paste(readLines(path, warn = FALSE), collapse = "")
  expect_true(grepl("Attr Title", html_content))
  expect_true(grepl("Attr Source", html_content))
  unlink(path)
})

test_that("generate_output format = 'pdf' writes a file", {
  skip_if_not_installed("webshot2")
  data <- generate_frequency(dplyr::tibble(x = c("A", "B", "A")), x)
  path <- tempfile(fileext = ".pdf")
  generate_output(data, path, format = "pdf")
  expect_true(file.exists(path))
  unlink(path)
})

test_that("write_pdf list writes separate files in directory", {
  skip_if_not_installed("webshot2")
  data_list <- list(
    t1 = generate_frequency(dplyr::tibble(x = c("A", "B")), x),
    t2 = generate_frequency(dplyr::tibble(x = c("C", "D")), x)
  )
  dir_path <- tempfile()
  write_pdf(data_list, dir_path)
  expect_true(file.exists(file.path(dir_path, "t1.pdf")))
  expect_true(file.exists(file.path(dir_path, "t2.pdf")))
  unlink(dir_path, recursive = TRUE)
})

test_that("write_pdf adds .pdf extension when missing", {
  skip_if_not_installed("webshot2")
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  path_no_ext <- tempfile()
  write_pdf(data, path_no_ext)
  expect_true(file.exists(paste0(path_no_ext, ".pdf")))
  unlink(paste0(path_no_ext, ".pdf"))
})

test_that("generate_output format = 'word' calls write_docx", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  path <- tempfile(fileext = ".docx")
  generate_output(data, path, format = "word")
  expect_true(file.exists(path))
  unlink(path)
})

# ── create_table_list() ──────────────────────────────────────────────────────

test_that("create_table_list errors when data is not a list", {
  df <- dplyr::tibble(x = 1:3)
  expect_error(create_table_list(df), "must be a list")
})

test_that("create_table_list returns a tibble with required columns", {
  data_list <- list(
    a = dplyr::tibble(x = 1:2),
    b = dplyr::tibble(x = 3:4)
  )
  result <- create_table_list(data_list)
  expect_true(inherits(result, "data.frame"))
  expect_true(all(c("table_id", "table_number", "table_name", "title") %in% names(result)))
  expect_equal(nrow(result), 2L)
})

test_that("create_table_list uses title attribute when present", {
  item <- dplyr::tibble(x = 1:2)
  attr(item, "title") <- "My Special Table"
  data_list <- list(foo = item)
  result <- create_table_list(data_list)
  expect_equal(result$title[1], "My Special Table")
  expect_equal(result$table_id[1], "My Special Table")
})

test_that("create_table_list falls back to list name when title attr absent", {
  data_list <- list(
    alpha = dplyr::tibble(x = 1:2),
    beta = dplyr::tibble(x = 3:4)
  )
  result <- create_table_list(data_list)
  expect_equal(result$title, c("alpha", "beta"))
  expect_equal(result$table_id, c("alpha", "beta"))
})

test_that("create_table_list assigns sequential table_number", {
  data_list <- list(
    a = dplyr::tibble(x = 1),
    b = dplyr::tibble(x = 2),
    c = dplyr::tibble(x = 3)
  )
  result <- create_table_list(data_list)
  expect_equal(result$table_number, 1:3)
})

# ── tsg_to_gt() ──────────────────────────────────────────────────────────────

test_that("tsg_to_gt returns a gt object", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B", "A")), x)
  gt_tbl <- tsg_to_gt(data, title = "Test Title")
  expect_s3_class(gt_tbl, "gt_tbl")
})

test_that("tsg_to_gt attaches title and source_note", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B", "A")), x)
  gt_tbl <- tsg_to_gt(data, title = "My Title", source_note = "Source: test")
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("My Title", html))
  expect_true(grepl("Source: test", html))
})

test_that("tsg_to_gt attaches subtitle", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  gt_tbl <- tsg_to_gt(data, title = "T", subtitle = "My Subtitle")
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("My Subtitle", html))
})

test_that("tsg_to_gt attaches footnotes", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  gt_tbl <- tsg_to_gt(data, footnotes = c("Note one", "Note two"))
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("Note one", html))
  expect_true(grepl("Note two", html))
})

test_that("tsg_to_gt reads metadata from data attributes", {
  data <- generate_frequency(
    dplyr::tibble(x = c("A", "B")), x,
    metadata = list(title = "AttrTitle", source_note = "AttrSource")
  )
  gt_tbl <- tsg_to_gt(data)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("AttrTitle", html))
  expect_true(grepl("AttrSource", html))
})

test_that("tsg_to_gt explicit args override data attributes", {
  data <- generate_frequency(
    dplyr::tibble(x = c("A", "B")), x,
    metadata = list(title = "AttrTitle")
  )
  gt_tbl <- tsg_to_gt(data, title = "ExplicitTitle")
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("ExplicitTitle", html))
  expect_false(grepl("AttrTitle", html))
})

test_that("tsg_to_gt with no title or subtitle adds no heading", {
  data <- dplyr::tibble(x = c("A", "B"), n = c(1L, 2L))
  gt_tbl <- tsg_to_gt(data, facade = NULL)
  html <- gt::as_raw_html(gt_tbl)
  expect_false(grepl("gt_heading", html))
})

test_that("tsg_to_gt with NULL facade returns valid gt object", {
  data <- dplyr::tibble(x = c("A", "B"), n = c(1L, 2L))
  expect_s3_class(tsg_to_gt(data, facade = NULL), "gt_tbl")
})

test_that("tsg_to_gt applies spanners for __ separated column names", {
  df <- dplyr::tibble(
    category = c("A", "B"),
    `Male__Count` = c(10L, 20L),
    `Male__Percent` = c(33.3, 66.7)
  )
  gt_tbl <- tsg_to_gt(df, facade = NULL)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("Male", html))
  expect_true(grepl("Count", html))
})

test_that("tsg_to_gt applies facade header background color", {
  data <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  facade <- get_tsg_facade()
  facade$header.fgFill <- "#FF0000"
  gt_tbl <- tsg_to_gt(data, facade = facade)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("FF0000", html, ignore.case = TRUE))
})

# ── gt_apply_spanners() ──────────────────────────────────────────────────────

test_that("gt_apply_spanners returns unchanged gt_tbl when no separator present", {
  df <- dplyr::tibble(x = c("A", "B"), n = c(1L, 2L))
  base_tbl <- gt::gt(df)
  result <- gt_apply_spanners(base_tbl, names(df), "__")
  expect_equal(nrow(result[["_spanners"]]), 0L)
})

test_that("gt_apply_spanners creates spanner labels in HTML", {
  df <- dplyr::tibble(
    cat = c("A", "B"),
    `Sex__Male` = c(10L, 20L),
    `Sex__Female` = c(15L, 5L)
  )
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__")
  html <- gt::as_raw_html(result)
  expect_true(grepl("Sex", html))
  expect_true(grepl("Male", html))
  expect_true(grepl("Female", html))
})

test_that("gt_apply_spanners handles multiple distinct spanner groups", {
  df <- dplyr::tibble(
    cat = "A",
    `Age__Under 18` = 1L,
    `Age__Over 18` = 2L,
    `Sex__Male` = 3L,
    `Sex__Female` = 4L
  )
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__")
  html <- gt::as_raw_html(result)
  expect_true(grepl("Age", html))
  expect_true(grepl("Sex", html))
})

test_that("gt_apply_spanners renames column labels to suffix only", {
  df <- dplyr::tibble(`Group__Count` = 10L, `Group__Percent` = 50.0)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__")
  html <- gt::as_raw_html(result)
  # spanner created and display labels show the suffix
  expect_equal(nrow(result[["_spanners"]]), 1L)
  expect_true(grepl("Count", html))
  expect_true(grepl("Percent", html))
})

test_that("gt_apply_spanners handles triple-level name (a__b__c)", {
  df <- dplyr::tibble(`A__B__C` = 1L, `A__B__D` = 2L)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__")
  html <- gt::as_raw_html(result)
  expect_true(grepl("A__B", html))
  expect_true(grepl("C", html))
})

test_that("gt_apply_spanners leaves columns without separator untouched", {
  df <- dplyr::tibble(plain = "x", `A__B` = 1L)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__")
  html <- gt::as_raw_html(result)
  expect_true(grepl("plain", html))
})

# ── gt_apply_facade() ────────────────────────────────────────────────────────

test_that("gt_apply_facade returns gt_tbl unchanged when facade is NULL", {
  df <- dplyr::tibble(x = c("A", "B"), n = c(1L, 2L))
  base_tbl <- gt::gt(df)
  result <- gt_apply_facade(base_tbl, NULL)
  expect_s3_class(result, "gt_tbl")
  # no styles or options were added — spanners and styles lists should be equal
  expect_equal(result[["_spanners"]], base_tbl[["_spanners"]])
  expect_equal(result[["_styles"]], base_tbl[["_styles"]])
})

test_that("gt_apply_facade applies table.fontName to HTML", {
  df <- dplyr::tibble(x = c("A", "B"))
  tbl <- gt::gt(df)
  facade <- list(table.fontName = "Courier New", table.fontSize = NULL)
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("Courier New", html))
})

test_that("gt_apply_facade applies table.fontSize to HTML", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  facade <- list(table.fontName = NULL, table.fontSize = 14)
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expected_px <- round(14 * 1.33)
  expect_true(grepl(as.character(expected_px), html))
})

test_that("gt_apply_facade applies header.fontColour to HTML", {
  df <- dplyr::tibble(x = "A", n = 1L)
  tbl <- gt::gt(df)
  facade <- list(header.fontColour = "#0011FF")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("0011FF", html, ignore.case = TRUE))
})

test_that("gt_apply_facade applies header.textDecoration bold to column labels", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  facade <- list(header.textDecoration = "bold")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("font-weight: bold", html))
})

test_that("gt_apply_facade applies spanner.fgFill overriding header", {
  df <- dplyr::tibble(`G__A` = 1L, `G__B` = 2L)
  tbl <- gt::gt(df) |>
    gt::tab_spanner(label = "G", columns = dplyr::everything())
  facade <- list(spanner.fgFill = "#009900")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("009900", html, ignore.case = TRUE))
})

test_that("gt_apply_facade applies body.fgFill to table body", {
  df <- dplyr::tibble(x = "A", n = 1L)
  tbl <- gt::gt(df)
  facade <- list(body.fgFill = "#FFEE00")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("FFEE00", html, ignore.case = TRUE))
})

test_that("gt_apply_facade applies body.fontColour to table body", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  facade <- list(body.fontColour = "#112233")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("112233", html, ignore.case = TRUE))
})

test_that("gt_apply_facade applies body.textDecoration italic to body", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  facade <- list(body.textDecoration = "italic")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("font-style: italic", html))
})

test_that("gt_apply_facade applies body.halign to body cells", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  facade <- list(body.halign = "right")
  result <- gt_apply_facade(tbl, facade)
  html <- gt::as_raw_html(result)
  expect_true(grepl("text-align: right", html))
})

test_that("gt_apply_facade with default facade (mostly NULLs) does not error", {
  df <- dplyr::tibble(x = "A", n = 1L)
  tbl <- gt::gt(df)
  facade <- get_tsg_facade()
  expect_s3_class(gt_apply_facade(tbl, facade), "gt_tbl")
})

# ── gt_apply_text_decoration() ───────────────────────────────────────────────

test_that("gt_apply_text_decoration applies bold", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  result <- gt_apply_text_decoration(tbl, "bold", gt::cells_body())
  html <- gt::as_raw_html(result)
  expect_true(grepl("font-weight: bold", html))
})

test_that("gt_apply_text_decoration applies italic", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  result <- gt_apply_text_decoration(tbl, "italic", gt::cells_body())
  html <- gt::as_raw_html(result)
  expect_true(grepl("font-style: italic", html))
})

test_that("gt_apply_text_decoration applies underline", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  result <- gt_apply_text_decoration(tbl, "underline", gt::cells_body())
  html <- gt::as_raw_html(result)
  expect_true(grepl("text-decoration", html))
})

test_that("gt_apply_text_decoration applies multiple decorations", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  result <- gt_apply_text_decoration(tbl, c("bold", "italic"), gt::cells_body())
  html <- gt::as_raw_html(result)
  expect_true(grepl("font-weight: bold", html))
  expect_true(grepl("font-style: italic", html))
})

test_that("gt_apply_text_decoration unknown decoration does not error", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  expect_s3_class(
    gt_apply_text_decoration(tbl, "strikethrough", gt::cells_body()),
    "gt_tbl"
  )
})

test_that("gt_apply_text_decoration is case insensitive", {
  df <- dplyr::tibble(x = "A")
  tbl <- gt::gt(df)
  result <- gt_apply_text_decoration(tbl, "BOLD", gt::cells_body())
  html <- gt::as_raw_html(result)
  expect_true(grepl("font-weight: bold", html))
})

# ── Regression: generate_crosstab() HTML/PDF spanner handling ─────────────────

test_that("gt_apply_spanners with col_labels creates spanners from labels, not col names", {
  # Simulate crosstab output: col names are plain, labels have "__"
  df <- dplyr::tibble(category = "A", frequency_1 = 10L, frequency_2 = 20L)
  col_labels <- c("Category", "Frequency__Male", "Frequency__Female")
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__", col_labels = col_labels)
  # A spanner should have been created
  expect_equal(nrow(result[["_spanners"]]), 1L)
  html <- gt::as_raw_html(result)
  expect_true(grepl("Frequency", html))
  expect_true(grepl("Male", html))
  expect_true(grepl("Female", html))
})

test_that("gt_apply_spanners relabels non-spanner columns from col_labels", {
  df <- dplyr::tibble(category = "A", total = 10L)
  col_labels <- c("Marital status", "Total")
  gt_tbl <- gt::gt(df)
  result <- gt_apply_spanners(gt_tbl, names(df), "__", col_labels = col_labels)
  html <- gt::as_raw_html(result)
  expect_true(grepl("Marital status", html))
  expect_true(grepl("Total", html))
})

test_that("tsg_to_gt creates spanners from generate_crosstab output", {
  ct <- generate_crosstab(person_record, marital_status, sex)
  gt_tbl <- tsg_to_gt(ct, facade = NULL)
  # Spanners for Frequency and Percent groups
  expect_gt(nrow(gt_tbl[["_spanners"]]), 0L)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("Frequency", html))
  expect_true(grepl("Percent", html))
  # Leaf labels should be the category values (Male / Female) shown in HTML
  expect_true(grepl("Male", html))
  expect_true(grepl("Female", html))
})

test_that("tsg_to_gt relabels category and total columns from label attributes", {
  ct <- generate_crosstab(person_record, marital_status, sex)
  gt_tbl <- tsg_to_gt(ct, facade = NULL)
  html <- gt::as_raw_html(gt_tbl)
  # "category" col should show its label "Marital status", not "category"
  expect_true(grepl("Marital status", html, fixed = TRUE))
})

test_that("tsg_to_gt creates spanners from collapse_list() output", {
  cl <- person_record |>
    generate_frequency(seeing, hearing) |>
    collapse_list()
  gt_tbl <- tsg_to_gt(cl, facade = NULL)
  expect_gt(nrow(gt_tbl[["_spanners"]]), 0L)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("Frequency", html))
  expect_true(grepl("No difficulty", html, fixed = TRUE))
})

test_that("tsg_to_gt respects label_separator stored in data attributes", {
  ct <- generate_crosstab(person_record, marital_status, sex, label_separator = ">>")
  # label_separator attr should be stored
  expect_equal(attr(ct, "label_separator"), ">>")
  # tsg_to_gt should auto-detect ">>" and still create spanners
  gt_tbl <- tsg_to_gt(ct, names_separator = "__", facade = NULL)
  expect_gt(nrow(gt_tbl[["_spanners"]]), 0L)
})

# ── parse_excel_numfmt_decimals() ─────────────────────────────────────────────

test_that("parse_excel_numfmt_decimals returns 0 for integer-only format", {
  expect_equal(parse_excel_numfmt_decimals("#,##0"), 0L)
  expect_equal(parse_excel_numfmt_decimals("0"), 0L)
  expect_equal(parse_excel_numfmt_decimals("#,###"), 0L)
})

test_that("parse_excel_numfmt_decimals returns correct decimal count", {
  expect_equal(parse_excel_numfmt_decimals("#,##0.00"), 2L)
  expect_equal(parse_excel_numfmt_decimals("#,##0.000"), 3L)
  expect_equal(parse_excel_numfmt_decimals("0.0"), 1L)
})

test_that("parse_excel_numfmt_decimals returns NA for non-numeric formats", {
  expect_true(is.na(parse_excel_numfmt_decimals("@")))
  expect_true(is.na(parse_excel_numfmt_decimals("General")))
  expect_true(is.na(parse_excel_numfmt_decimals("")))
  expect_true(is.na(parse_excel_numfmt_decimals(NULL)))
})

# ── gt_apply_numeric_format() ─────────────────────────────────────────────────

test_that("gt_apply_numeric_format formats integer columns with commas", {
  df <- dplyr::tibble(n = 1234567L, x = "A")
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list())
  html <- gt::as_raw_html(result)
  expect_true(grepl("1,234,567", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format formats whole-number doubles as integers", {
  df <- dplyr::tibble(n = 9876.0)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list())
  html <- gt::as_raw_html(result)
  # Should appear as "9,876" (no decimal point)
  expect_true(grepl("9,876", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format formats decimal doubles with default 2 places", {
  df <- dplyr::tibble(p = 12.3456)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list())
  html <- gt::as_raw_html(result)
  expect_true(grepl("12.35", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format respects table.decimalPrecision", {
  df <- dplyr::tibble(p = 12.3456)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list(table.decimalPrecision = 3))
  html <- gt::as_raw_html(result)
  expect_true(grepl("12.346", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format respects table.decimalCols override", {
  # 'n' would normally be treated as integer (whole-number double), but forced to decimal
  df <- dplyr::tibble(n = 10.0)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list(table.decimalCols = "n", table.decimalPrecision = 1))
  html <- gt::as_raw_html(result)
  expect_true(grepl("10.0", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format body.numFmt is base, auto-detection overrides it", {
  df <- dplyr::tibble(n = 1234L, p = 5.678)
  gt_tbl <- gt::gt(df)
  # body.numFmt "#,##0.0" sets 1-decimal base, but auto-detection wins:
  # n (integer) → fmt_integer → "1,234"; p (double) → fmt_number(decimals=2) → "5.68"
  result <- gt_apply_numeric_format(gt_tbl, df, list(body.numFmt = "#,##0.0"))
  html <- gt::as_raw_html(result)
  expect_true(grepl("1,234", html, fixed = TRUE))   # integer formatted, no ".0"
  expect_false(grepl("1,234.0", html, fixed = TRUE)) # body.numFmt NOT overriding
  expect_true(grepl("5.68", html, fixed = TRUE))     # 2-decimal precision wins
})

test_that("gt_apply_numeric_format applies col_first.numFmt to first column", {
  df <- dplyr::tibble(a = 100L, b = 200L)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list(col_first.numFmt = "#,##0.00"))
  html <- gt::as_raw_html(result)
  expect_true(grepl("100.00", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format applies col_last.numFmt to last column", {
  df <- dplyr::tibble(a = 100L, b = 200L)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, list(col_last.numFmt = "#,##0.0"))
  html <- gt::as_raw_html(result)
  expect_true(grepl("200.0", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format with NULL facade is a no-op", {
  df <- dplyr::tibble(n = 1234567L)
  gt_tbl <- gt::gt(df)
  result <- gt_apply_numeric_format(gt_tbl, df, NULL)
  # Should be unchanged from original gt table
  expect_identical(result, gt_tbl)
})

test_that("tsg_to_gt applies numeric formatting to frequency table output", {
  tbl <- generate_frequency(person_record, sex)
  gt_tbl <- tsg_to_gt(tbl, facade = NULL)
  html <- gt::as_raw_html(gt_tbl)
  # Frequency counts should appear with comma formatting (no raw integers)
  # Row counts are small here but should not trigger decimal formatting
  expect_false(grepl("NaN", html))
  expect_false(grepl("NA", html))
})

# ── Bug regression: haven_labelled, decimal precision, first-col alignment ────

test_that("tsg_to_gt renders haven_labelled category as text labels, not integer codes", {
  # generate_frequency produces a haven_labelled<integer> category column
  tbl <- generate_frequency(person_record, marital_status)
  expect_s3_class(tbl$category, "haven_labelled")
  gt_tbl <- tsg_to_gt(tbl, facade = NULL)
  html <- gt::as_raw_html(gt_tbl)
  # Value labels should appear; raw integer codes should not
  expect_true(grepl("Single/never married", html, fixed = TRUE))
  expect_true(grepl("Married", html, fixed = TRUE))
  # Cell value "1" as a standalone cell text would indicate raw codes
  expect_false(grepl(">1<", html, fixed = TRUE))
})

test_that("tsg_to_gt decimal precision matches facade table.decimalPrecision", {
  # Default facade has table.decimalPrecision: 2 and body.numFmt: "###0"
  # body.numFmt must NOT override the per-column auto-detected decimal format
  tbl <- generate_frequency(person_record, marital_status)
  gt_tbl <- tsg_to_gt(tbl)
  html <- gt::as_raw_html(gt_tbl)
  # percent column should have 2 decimal places (e.g. "52.91"), not rounded to integer
  pct_decimals <- regmatches(html, gregexpr("[0-9]+[.][0-9]{2}", html))[[1]]
  expect_true(length(pct_decimals) > 0)
})

test_that("tsg_to_gt left-aligns the first column by default", {
  tbl <- generate_frequency(person_record, sex)
  gt_tbl <- tsg_to_gt(tbl, facade = NULL)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("text-align: left", html, fixed = TRUE))
})

test_that("gt_apply_facade col_first.halign overrides the default left alignment", {
  df <- dplyr::tibble(label = c("A", "B"), n = 1:2)
  tbl <- gt::gt(df)
  facade <- list(col_first.halign = "center")
  result <- gt_apply_facade(tbl, facade, col_names = c("label", "n"))
  html <- gt::as_raw_html(result)
  expect_true(grepl("text-align: center", html, fixed = TRUE))
})

test_that("gt_apply_facade col_last.halign applies to the last column", {
  df <- dplyr::tibble(label = c("A", "B"), n = 1:2)
  tbl <- gt::gt(df)
  facade <- list(col_last.halign = "center")
  result <- gt_apply_facade(tbl, facade, col_names = c("label", "n"))
  html <- gt::as_raw_html(result)
  expect_true(grepl("text-align: center", html, fixed = TRUE))
})

test_that("gt_apply_numeric_format skips col_first.numFmt when first col is not numeric", {
  # After convert_factor, the first col is a factor — should not apply numFmt
  df <- dplyr::tibble(label = factor(c("A", "B")), n = c(10L, 20L))
  gt_tbl <- gt::gt(df)
  facade <- list(col_first.numFmt = "#,##0.00")
  # Should not error; label column simply won't be formatted
  expect_s3_class(
    gt_apply_numeric_format(gt_tbl, df, facade),
    "gt_tbl"
  )
})


# --- Footnote placement and locations tests ---

test_that(".normalize_footnotes handles character vector input", {
  fn <- .normalize_footnotes(c("Note 1", "Note 2"))
  expect_equal(fn$text, c("Note 1", "Note 2"))
  expect_equal(fn$placement, c("auto", "auto"))
  expect_equal(length(fn$locations), 2L)
  expect_null(fn$locations[[1]])
})

test_that(".normalize_footnotes handles full list input", {
  input <- list(
    text      = c("A", "B"),
    placement = c("left", "right"),
    locations = list(NULL, "frequency")
  )
  fn <- .normalize_footnotes(input)
  expect_equal(fn$placement, c("left", "right"))
  expect_equal(fn$locations[[2]], "frequency")
})

test_that(".normalize_footnotes returns NULL for NULL input", {
  expect_null(.normalize_footnotes(NULL))
})

test_that(".normalize_footnotes fills missing placement/locations slots", {
  fn <- .normalize_footnotes(list(text = c("X")))
  expect_equal(fn$placement, "auto")
  expect_null(fn$locations[[1]])
})

test_that("add_footnote stores placement and locations correctly", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  tbl <- add_footnote(tbl, "Weighted.", placement = "right", locations = "frequency")
  fn <- attr(tbl, "footnotes")
  expect_equal(fn$text, "Weighted.")
  expect_equal(fn$placement, "right")
  expect_equal(fn$locations, "frequency")
})

test_that("tsg_to_gt renders footnote with placement (no error)", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_footnote("Right note.", placement = "right") |>
    add_footnote("Left note.", placement = "left")
  gt_tbl <- tsg_to_gt(tbl)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("Right note", html))
  expect_true(grepl("Left note", html))
})

test_that("tsg_to_gt renders footnote with valid column locations", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_footnote("Unweighted count.", locations = "frequency")
  gt_tbl <- tsg_to_gt(tbl)
  html <- gt::as_raw_html(gt_tbl)
  expect_true(grepl("Unweighted count", html))
})

test_that("tsg_to_gt silently drops unknown column locations", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_footnote("Note.", locations = "nonexistent_col")
  expect_s3_class(tsg_to_gt(tbl), "gt_tbl")
})

test_that("tsg_to_gt does not leak 'auto' as footnote text in HTML output", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_footnote("Only this note.", placement = "auto")
  html <- gt::as_raw_html(tsg_to_gt(tbl))
  # Strip all HTML tags and check plain text
  text <- gsub("<[^>]+>", " ", html)
  expect_true(grepl("Only this note", text))
  # "auto" must not appear as standalone text content (it may appear in CSS)
  # Check that "auto" is NOT in any footnote cell content
  expect_false(grepl("footnote[^>]*>[^<]*\\bauto\\b", html))
})

test_that("write_docx with footnote placement does not error", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_footnote("Left note.", placement = "left") |>
    add_footnote("Right note.", placement = "right")
  tmp <- tempfile(fileext = ".docx")
  expect_no_error(write_docx(tbl, path = tmp))
  expect_true(file.exists(tmp) && file.size(tmp) > 100)
})

# ============================================================
# Cross-format facade unification tests
# ============================================================

test_that("get_tsg_facade supports html format", {
  f <- get_tsg_facade(which = "html")
  expect_type(f, "list")
  expect_true("body.fontSize" %in% names(f))
  expect_true("header.bgFill" %in% names(f))
})

test_that("get_tsg_facade supports docx format", {
  f <- get_tsg_facade(which = "docx")
  expect_type(f, "list")
  expect_true("body.fontSize" %in% names(f))
  expect_true("header.bgFill" %in% names(f))
})

test_that("resolve_facade uses which= for fallback format (html)", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_facade("body.fontSize" = 16)
  facade_override <- attr(tbl, "facade")
  f_base <- get_tsg_facade(which = "html")
  f_resolved <- resolve_facade(f_base, facade_override, which = "html")
  expect_equal(f_resolved[["body.fontSize"]], 16)
})

test_that("resolve_facade uses which= for fallback format (docx)", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_facade("body.fontSize" = 14)
  facade_override <- attr(tbl, "facade")
  f_base <- get_tsg_facade(which = "docx")
  f_resolved <- resolve_facade(f_base, facade_override, which = "docx")
  expect_equal(f_resolved[["body.fontSize"]], 14)
})

test_that("tsg_to_gt applies add_facade overrides for html", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_facade("body.fontSize" = 18)
  # Should not error
  expect_s3_class(tsg_to_gt(tbl), "gt_tbl")
})

test_that("tsg_to_gt handles NULL facade gracefully", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x)
  # Passing NULL facade must not error
  expect_s3_class(tsg_to_gt(tbl, facade = NULL), "gt_tbl")
})

test_that("tsg_to_flextable applies add_facade overrides for docx", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_facade("header.bgFill" = "#FF0000")
  expect_s3_class(tsg_to_flextable(tbl), "flextable")
})

test_that("write_html applies add_facade override without error", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_facade("body.fontSize" = 14)
  tmp <- tempfile(fileext = ".html")
  expect_no_error(write_html(tbl, path = tmp))
  expect_true(file.exists(tmp) && file.size(tmp) > 100)
  unlink(tmp)
})

test_that("write_docx applies add_facade override without error", {
  tbl <- generate_frequency(dplyr::tibble(x = c("A", "B")), x) |>
    add_facade("body.fontSize" = 12)
  tmp <- tempfile(fileext = ".docx")
  expect_no_error(write_docx(tbl, path = tmp))
  expect_true(file.exists(tmp) && file.size(tmp) > 100)
  unlink(tmp)
})

test_that("html facade yaml excludes xlsx-only keys", {
  f <- get_tsg_facade(which = "html")
  expect_false("table.gridLines" %in% names(f))
  expect_false("table.tabColour" %in% names(f))
  expect_false("table.offsetRow" %in% names(f))
})

test_that("docx facade yaml excludes xlsx-only keys", {
  f <- get_tsg_facade(which = "docx")
  expect_false("table.gridLines" %in% names(f))
  expect_false("table.tabColour" %in% names(f))
  expect_false("table.offsetRow" %in% names(f))
})
