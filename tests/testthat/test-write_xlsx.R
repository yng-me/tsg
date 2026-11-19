test_that("write_xlsx works with a basic dataframe", {
  # Create a simple data frame
  df <- data.frame(A = 1:3, B = letters[1:3])

  # Define the path for saving the file
  temp_path <- tempfile(fileext = ".xlsx")

  # Call the write_xlsx function
  write_xlsx(df, temp_path)

  # Check if the file is created
  expect_true(file.exists(temp_path))

  # Read the file back to check its content
  sheet_names <- openxlsx::getSheetNames(temp_path)
  # Check if the default sheet name "Sheet1" exists
  expect_true("Sheet1" %in% sheet_names)

  wb <- openxlsx::loadWorkbook(temp_path)
  expect_equal(as.integer(wb$colWidths[[1]][1]), 8L) # fits the short first column
  expect_equal(as.integer(wb$colWidths[[1]][2]), 14L)
  expect_equal(as.integer(wb$rowHeights[[1]][1]), 34L)
  expect_equal(as.integer(wb$rowHeights[[1]][2]), 20L)

  # Clean up
  unlink(temp_path)

})

test_that("write_xlsx raises an error for invalid `data` when `separate_files` is TRUE", {
  # Create a simple data frame
  df <- data.frame(A = 1:3, B = letters[1:3])

  # Define the path for saving the file
  temp_path <- tempfile(fileext = ".xlsx")

  # Try calling the function with separate_files = TRUE but without a list
  expect_error(
    write_xlsx(df, temp_path, separate_files = TRUE),
    "When `separate_files = TRUE`, `data` must be a list of data frames."
  )

})

test_that("write_xlsx works with separate files for a list of data frames", {
  # Create a list of data frames
  df1 <- data.frame(A = 1:3, B = letters[1:3])
  df2 <- data.frame(X = 1:2, Y = letters[4:5])
  data_list <- list(sheet1 = df1, sheet2 = df2)

  # Define the directory for saving the files
  temp_dir <- tempfile()

  # Ensure the directory exists
  dir.create(temp_dir)

  # Call write_xlsx with separate_files = TRUE
  write_xlsx(data_list, temp_dir, separate_files = TRUE)

  # Check if both files are created
  expect_true(file.exists(file.path(temp_dir, "sheet1.xlsx")))
  expect_true(file.exists(file.path(temp_dir, "sheet2.xlsx")))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})


test_that("write_xlsx works with footnotes and source_note", {
  # Create a simple data frame
  df <- data.frame(A = 1:3, B = letters[1:3])

  # Define the path for saving the file
  temp_path <- tempfile(fileext = ".xlsx")

  # Call the write_xlsx function with footnotes and source_note
  write_xlsx(df, temp_path, footnotes = "This is a footnote.", source_note = "Data source: XYZ")

  # Check if the file is created
  expect_true(file.exists(temp_path))

  # Read the file back
  wb <- openxlsx::loadWorkbook(temp_path)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1)

  # Verify footnotes and source note are in the correct places
  expect_true(any(grepl("This is a footnote.", sheet_data)))
  expect_true(any(grepl("Data source: XYZ", sheet_data)))

  # Clean up
  unlink(temp_path)
})


test_that("write_xlsx works with custom font and title", {
  # Create a simple data frame
  df <- data.frame(A = 1:3, B = letters[1:3])

  # Define the path for saving the file
  temp_path <- tempfile(fileext = ".xlsx")

  # Call the write_xlsx function with custom font and title
  write_xlsx(df, temp_path, title = "Custom Title")

  # Read the file back
  wb <- openxlsx::loadWorkbook(temp_path)

  # Check if the title is set correctly (it may require custom logic to verify in the sheet)
  sheet_data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE)

  # Check if the title appears (you might need a more specific test based on formatting)
  expect_true(any(grepl("Custom Title", sheet_data)))

  # Clean up
  unlink(temp_path)
})


test_that("write_xlsx handles list collapse correctly", {
  # Create a list of data frames
  df1 <- data.frame(A = 1:3, B = letters[1:3])
  df2 <- data.frame(X = 1:2, Y = letters[4:5])
  data_list <- list(df1, df2)

  # Define the path for saving the file
  temp_path <- tempfile(fileext = ".xlsx")

  # Call write_xlsx with collapse_list = TRUE
  write_xlsx(data_list, temp_path, collapse_list = TRUE)

  # Check if the file is created
  expect_true(file.exists(temp_path))

  # Read the file back
  wb <- openxlsx::loadWorkbook(temp_path)
  sheet_names <- openxlsx::getSheetNames(temp_path)

  # Verify that the file contains only one sheet (collapsed list)
  expect_equal(length(sheet_names), 1)

  # Clean up
  unlink(temp_path)
})


test_that("write_xlsx handles missing facade parameters", {
  # Create a simple data frame
  df <- data.frame(A = 1:3, B = letters[1:3])

  # Define the path for saving the file
  temp_path <- tempfile(fileext = ".xlsx")

  # Call the write_xlsx function without specifying the facade (use default)
  write_xlsx(df, temp_path, facade = NULL)

  # Check if the file is created
  expect_true(file.exists(temp_path))

  # Clean up
  unlink(temp_path)
})



test_that("write_xlsx generates output with reference table list", {

  ref <- data.frame(
    table_number = 1:2,
    table_id = c("table1", "table2"),
    table_name = c("Table 1", "Table 2"),
    title = c("Table 1 Title", "Table 2 Title"),
    subtitle = c("Subtitle 1", "Subtitle 2"),
    source_note = c("Source 1", "Source 2"),
    footnotes = c("Footnote 1", "Footnote 2")
  )

  temp_path <- tempfile(fileext = ".xlsx")

  write_xlsx(
    data = list(
      table1 = data.frame(A = 1:3, B = letters[1:3]),
      table2 = data.frame(X = 1:2, Y = letters[4:5])
    ),
    path = temp_path,
    include_table_list = TRUE,
    table_list_reference = ref
  )

  expect_true(file.exists(temp_path))

  data <- openxlsx::read.xlsx(temp_path, sheet = "List of Tables", startRow = 3)

  expect_equal(nrow(data), 2)
  expect_equal(data$Title, c("Table 1 Title", "Table 2 Title"))

  expect_true("List of Tables" %in% openxlsx::getSheetNames(temp_path))

  unlink(temp_path)

})


test_that("write_xlsx generates output without reference table list but `include_table_list` is TRUE", {

  temp_path_no_ref <- tempfile(fileext = ".xlsx")

  write_xlsx(
    data = list(
      table1 = data.frame(A = 1:3, B = letters[1:3]),
      table2 = data.frame(X = 1:2, Y = letters[4:5])
    ),
    path = temp_path_no_ref,
    include_table_list = TRUE
  )

  expect_true(file.exists(temp_path_no_ref))

  data <- openxlsx::read.xlsx(temp_path_no_ref, sheet = "List of Tables", startRow = 3)

  expect_equal(nrow(data), 2)
  expect_equal(data$Title, c("table1", "table2"))
  expect_true("List of Tables" %in% openxlsx::getSheetNames(temp_path_no_ref))

  unlink(temp_path_no_ref)

})




read_cells <- function(path, sheet = 1) {
  unlist(openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE), use.names = FALSE)
}

test_that("label_stub replaces the category header in the exported sheet", {
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(generate_frequency(person_record, sex, label_stub = "Stub"), path)
  expect_contains(read_cells(path), "Stub")
})

test_that("per-table footnotes in a list are written as text only", {
  data <- generate_frequency(person_record, sex, marital_status)
  data[[1]] <- add_footnote(data[[1]], "per-table note")
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(data, path)
  cells <- read_cells(path)
  expect_contains(cells, "per-table note")
  expect_false("auto" %in% cells)
})

test_that("footnotes argument applies to every sheet of a list", {
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(generate_frequency(person_record, sex, marital_status), path, footnotes = "shared note")
  expect_contains(read_cells(path, 1), "shared note")
  expect_contains(read_cells(path, 2), "shared note")
})

test_that("a list collapsed into one sheet keeps categories and every footnote", {
  path <- tempfile(fileext = ".xlsx")
  data <- unclass(generate_frequency(person_record, sex, marital_status))
  write_xlsx(data, path, collapse_list = TRUE, source_note = "PSA", footnotes = c("note a", "note b"))
  cells <- read_cells(path)
  expect_contains(cells, c("Male", "Married"))
  expect_equal(sum(cells == "note b", na.rm = TRUE), 2)
})

test_that("add_facade on a table in a list sets that sheet's offset", {
  data <- generate_frequency(person_record, sex, marital_status)
  data[[2]] <- add_facade(data[[2]], table.offsetRow = 3)
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(data, path)
  wb <- openxlsx::loadWorkbook(path)
  expect_equal(min(as.integer(names(wb$rowHeights[[1]]))), 1)
  expect_equal(min(as.integer(names(wb$rowHeights[[2]]))), 4)
})

test_that("add_facade on a table wins over the facade passed to write_xlsx", {
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(
    add_facade(generate_frequency(person_record, sex), table.offsetRow = 2),
    path,
    facade = list(table.offsetRow = 5, table.fontSize = 9)
  )
  wb <- openxlsx::loadWorkbook(path)
  expect_equal(min(as.integer(names(wb$rowHeights[[1]]))), 3)
})

test_that("write_xlsx passes ... to createWorkbook or addWorksheet by name", {
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(generate_frequency(person_record, sex), path, creator = "tsg test", zoom = 80)
  expect_contains(openxlsx::getCreators(openxlsx::loadWorkbook(path)), "tsg test")
})

test_that("character footnotes from metadata are written", {
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(generate_frequency(person_record, sex, metadata = list(footnotes = "meta note")), path)
  expect_contains(read_cells(path), "meta note")
})

test_that("spanner facade keys style the upper header rows", {
  path <- tempfile(fileext = ".xlsx")
  data <- generate_crosstab(person_record, marital_status, sex) |>
    add_facade(spanner.height = 40, spanner.textDecoration = "bold")
  write_xlsx(data, path)

  wb <- openxlsx::loadWorkbook(path)
  expect_equal(as.numeric(wb$rowHeights[[1]][["1"]]), 40)
  spanner_styles <- Filter(function(s) 1 %in% s$rows, wb$styleObjects)
  expect_true(any(vapply(spanner_styles, function(s) "BOLD" %in% s$style$fontDecoration, logical(1))))
})

test_that("a grouped list collapsed into one sheet has a title row per group", {
  path <- tempfile(fileext = ".xlsx")
  data <- person_record |>
    dplyr::group_by(sex) |>
    generate_frequency(marital_status, group_as_list = TRUE)
  write_xlsx(data, path, collapse_list = TRUE, source_note = "PSA", footnotes = "group note")

  cells <- read_cells(path)
  expect_contains(cells, c("Male", "Female", "Married", "Source: PSA", "group note"))
  expect_equal(sum(cells == "Married", na.rm = TRUE), 2)
})

test_that("a labelled column without a variable label keeps its own header", {
  path <- tempfile(fileext = ".xlsx")
  data <- tibble::tibble(
    id = 1:2,
    emp = haven::labelled(c(1L, 2L), c(Yes = 1L, No = 2L)),
    n = 3:4
  )
  write_xlsx(data, path)

  header <- unlist(openxlsx::read.xlsx(path, colNames = FALSE, rows = 1))
  expect_equal(unname(header), c("id", "emp", "n"))
})

test_that("facade values are passed as data, never run as code", {
  path <- tempfile(fileext = ".xlsx")
  ran <- FALSE
  payload <- "x', indent = {ran <<- TRUE; 1}, valign = 'center"
  data <- generate_frequency(person_record, sex) |>
    add_facade(header.fontName = payload, title.fontName = "O'Brien Sans", header.textDecoration = c("bold", "italic"))

  write_xlsx(data, path, title = "T")
  expect_false(ran)
  expect_true(file.exists(path))
})

test_that("labels sharing their first 31 characters get distinct sheets and working contents links", {
  long <- function(x, label) haven::labelled(as.integer(x), c(A = 1L, B = 2L), label = label)
  data <- person_record |>
    dplyr::mutate(
      q1 = long(sex, "Difficulty in seeing even if wearing glasses"),
      q2 = long(sex, "Difficulty in seeing even if wearing contact lenses")
    ) |>
    generate_frequency(q1, q2)
  data[[1]] <- add_table_title(data[[1]], "Seeing with glasses")

  path <- tempfile(fileext = ".xlsx")
  write_xlsx(data, path, include_table_list = TRUE, trim_sheet_names = TRUE)

  sheets <- openxlsx::getSheetNames(path)
  expect_length(unique(tolower(sheets)), 3)
  expect_true(all(nchar(sheets) <= 31))

  links <- unlist(openxlsx::loadWorkbook(path)$worksheets[[1]]$sheet_data$f)
  targets <- sub(".*#&apos;(.*)&apos;!A1.*", "\\1", grep("HYPERLINK", links, value = TRUE))
  expect_setequal(targets, sheets[-1])
})

test_that("decimalPrecision = 0 gives a whole-number format and decimalCols limits it", {
  path <- tempfile(fileext = ".xlsx")
  write_xlsx(add_facade(generate_frequency(person_record, sex), table.decimalPrecision = 0), path)
  formats <- unlist(lapply(openxlsx::loadWorkbook(path)$styleObjects, function(s) s$style$numFmt$formatCode))
  expect_false("#,##0." %in% formats)

  path2 <- tempfile(fileext = ".xlsx")
  write_xlsx(add_facade(generate_frequency(person_record, sex), table.decimalCols = "none"), path2)
  formats2 <- unlist(lapply(openxlsx::loadWorkbook(path2)$styleObjects, function(s) s$style$numFmt$formatCode))
  expect_false("#,##0.00" %in% formats2)
})

# Style objects of a written workbook: one row per style with its rows, fill and borders.
written_styles <- function(path) {
  styles <- openxlsx::loadWorkbook(path)$styleObjects
  lapply(styles, function(s) list(
    rows = s$rows,
    fill = unname(unlist(s$style$fill$fillFg)),
    left = s$style$borderLeft,
    right = s$style$borderRight
  ))
}

test_that("every built-in facade uses known keys and writes", {
  facades <- sub("\\.yaml$", "", list.files(system.file("extdata/facade", package = "tsg"), pattern = "\\.yaml$"))
  expect_true(all(c("apa", "booktabs", "striped", "gridded", "minimal", "financial", "nyt", "jama", "dark", "pastel", "census") %in% facades))

  tables <- list(
    generate_frequency(person_record, sex),
    generate_crosstab(person_record, marital_status, sex),
    generate_frequency(dplyr::group_by(person_record, sex), employed, group_as_list = TRUE)
  )

  for (name in facades) {
    facade <- get_tsg_facade(name)
    expect_equal(setdiff(names(facade), names(get_tsg_facade())), character(0), info = name)
    for (table in tables) {
      path <- withr::local_tempfile(fileext = ".xlsx")
      expect_no_error(write_xlsx(table, path, facade = facade))
    }
  }
})

test_that("a facade key set to ~ turns the default off", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(generate_frequency(person_record, sex), path, facade = get_tsg_facade("apa"))
  styles <- written_styles(path)
  expect_true(all(vapply(styles, function(s) is.null(s$left) && is.null(s$right), logical(1))))
})

test_that("body.bandFill fills every other body row", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  # header in row 1, body rows below it
  write_xlsx(generate_frequency(person_record, marital_status), path, facade = get_tsg_facade("striped"))
  banded <- unlist(lapply(written_styles(path), function(s) if (identical(s$fill, "FFEAF1F8")) unique(s$rows)))
  body_rows <- seq(2, 1 + nrow(generate_frequency(person_record, marital_status)))
  expect_setequal(banded, body_rows[seq(2, length(body_rows), by = 2)])
})

test_that("lists nested 3 groups deep write one sheet per top group", {
  nested <- person_record |>
    dplyr::group_by(sex, employed, seeing) |>
    generate_frequency(marital_status, group_as_list = TRUE)

  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(nested, path)

  expect_equal(openxlsx::getSheetNames(path), c("Male", "Female"))
  # deeper levels become row-group titles named by their path
  expect_contains(read_cells(path, "Male"), c("Yes - No difficulty", "No - Some difficulty"))

  one_sheet <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(write_xlsx(nested, one_sheet, collapse_list = TRUE))
})

test_that("a one-column table writes", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(data.frame(x = c(1, 2)), path)
  expect_contains(read_cells(path), c("x", "1", "2"))
})

test_that("empty cells in table_list_reference keep the table's own notes", {
  tables <- list(a = add_table_subtitle(generate_frequency(person_record, sex), "own subtitle"))
  ref <- data.frame(
    table_id = "a", table_name = "A", table_number = 1, title = "T",
    subtitle = NA, source_note = "S", footnotes = NA
  )

  path <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(write_xlsx(tables, path, include_table_list = TRUE, table_list_reference = ref))
  expect_contains(read_cells(path, "A"), c("Table 1. T", "own subtitle", "Source: S"))
  expect_false("NA" %in% read_cells(path, "A"))
})

test_that("row_group_as_column writes group columns on the sheets of a nested list", {
  nested <- person_record |>
    dplyr::group_by(sex, employed) |>
    generate_frequency(marital_status, group_as_list = TRUE)

  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(nested, path, row_group_as_column = TRUE)

  male <- openxlsx::read.xlsx(path, sheet = "Male", fillMergedCells = TRUE)
  expect_equal(names(male)[1:3], c("Sex", "Employment.status", "Marital.status"))
  # employed has missing values; their rows are kept (an empty group cell)
  expect_setequal(unique(male$Employment.status), c("Yes", "No", NA))
  # one shared header, no row-group titles
  expect_false(any(c("Yes", "No") %in% male$Marital.status))
})

col_widths <- function(path, sheet = 1) {
  w <- openxlsx::loadWorkbook(path)$colWidths[[sheet]]
  stats::setNames(as.numeric(w), names(w))
}

test_that("category and group columns fit their content when no width is set", {
  nested <- person_record |>
    dplyr::group_by(sex, employed) |>
    generate_frequency(marital_status, group_as_list = TRUE)

  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(nested, path, row_group_as_column = TRUE)
  widths <- col_widths(path)
  # Sex, Employment status and the categories fit their text
  expect_equal(unname(widths[c("1", "2", "3")]), c(8, nchar("Employment status") + 4, nchar("Single/never married") + 4))

  # a width set in the facade wins
  facade <- get_tsg_facade()
  facade$col_first.width <- 32
  facade$row_group.width <- 20
  write_xlsx(nested, path, row_group_as_column = TRUE, facade = facade)
  expect_equal(unname(col_widths(path)[c("1", "2", "3")]), c(20, 20, 32))
})

test_that("sheet names over 31 characters are an error unless trim_sheet_names = TRUE", {
  tables <- list(
    "Hearing, even if using hearing aid" = generate_frequency(person_record, hearing),
    Sex = generate_frequency(person_record, sex)
  )
  path <- withr::local_tempfile(fileext = ".xlsx")

  expect_error(write_xlsx(tables, path), "Hearing, even if using hearing aid")
  expect_error(write_xlsx(tables[[2]], path, sheet_name = strrep("x", 32)), "trim_sheet_names")

  write_xlsx(tables, path, trim_sheet_names = TRUE)
  # cut at 31 characters, without the trailing space
  expect_equal(openxlsx::getSheetNames(path), c("Hearing, even if using hearing", "Sex"))

  # file names have no limit
  dir <- withr::local_tempfile()
  write_xlsx(tables, dir, separate_files = TRUE)
  expect_setequal(list.files(dir), c("Hearing, even if using hearing aid.xlsx", "Sex.xlsx"))
})

test_that("categories under a row-group title are indented one level deeper", {
  by_sex <- person_record |>
    dplyr::group_by(sex) |>
    generate_frequency(marital_status, group_as_list = TRUE)

  indent_at <- function(path, row, col) {
    out <- NA
    for (s in openxlsx::loadWorkbook(path)$styleObjects) {
      if (any(s$rows == row & s$cols == col)) out <- as.numeric(s$style$indent %||% 0)
    }
    out
  }

  path <- withr::local_tempfile(fileext = ".xlsx")
  for (facade in c("default", "booktabs")) {
    write_xlsx(by_sex, path, collapse_list = TRUE, facade = get_tsg_facade(facade))
    base <- get_tsg_facade(facade)$body.indent %||% get_tsg_facade()$body.indent
    # row 1 header, row 2 "Male" title, row 3 its first category
    expect_equal(indent_at(path, 2, 1), base, info = facade)
    expect_equal(indent_at(path, 3, 1), base + 1, info = facade)
  }
})

test_that("a grouped sheet formats its numbers from every group, not the last one", {
  grouped <- dplyr::tibble(grp = c(1, 1, 1, 2, 2), ans = c(1, 2, 2, 1, 2)) |>
    dplyr::group_by(grp) |>
    generate_frequency(ans, group_as_list = TRUE)
  # group 2 has whole percents only (50, 50, 100); group 1 has 33.33 and 66.67
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(grouped, path, collapse_list = TRUE)

  percent_formats <- unlist(lapply(openxlsx::loadWorkbook(path)$styleObjects, function(s) {
    if (3 %in% s$cols && !is.null(s$style$numFmt)) s$style$numFmt$formatCode
  }))
  expect_true("#,##0.00" %in% percent_formats)
  expect_false("#,##0" %in% percent_formats)
})

test_that("the table-list columns fit their content, titles up to 148 characters", {
  tables <- list(
    Sex = add_table_title(generate_frequency(person_record, sex), "Household members by sex"),
    Long = add_table_title(generate_frequency(person_record, marital_status), strrep("x", 200))
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(tables, path, include_table_list = TRUE)

  widths <- col_widths(path, sheet = 1)
  # Tab Name, Table and Title: header or longest value + 4, titles capped at 148
  expect_equal(unname(widths[c("2", "3", "4")]), c(nchar("Tab Name") + 4, nchar("Table 1") + 4, 148))

  tables$Long <- add_table_title(tables$Long, "Marital status")
  write_xlsx(tables, path, include_table_list = TRUE)
  expect_equal(unname(col_widths(path, sheet = 1)["4"]), nchar("Household members by sex") + 4)
})
