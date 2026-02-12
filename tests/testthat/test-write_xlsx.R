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
  expect_equal(as.integer(wb$colWidths[[1]][1]), 32L)
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


