# Mocking up a simple data frame for testing
test_data <- data.frame(A = 1:3, B = 4:6)


# Test 1: Test convert_to_nested_list()
test_that("convert_to_nested_list creates correct nested list structure", {

  key_value_pairs <- list(
    "gridLines" = FALSE,
    "style.body.border" = c("left", "right"),
    "style.body.borderColour" = "#cfcfcf",
    "height.title" = 24
  )

  # Expected output for the convert_to_nested_list function
  expected_nested <- list(
    gridLines = FALSE,
    style = list(
      body = list(
        border = c("left", "right"),
        borderColour = "#cfcfcf"
      )
    ),
    height = list(
      title = 24
    )
  )

  result <- convert_to_nested_list(key_value_pairs)

  # Check if the result matches the expected nested structure
  expect_equal(result, expected_nested)
})


# Test 2: Test flatten_nested_list()
test_that("flatten_nested_list creates correct flat key-value pairs", {

  nested_list <- list(
    gridLines = FALSE,
    style = list(
      body = list(
        border = c("left", "right"),
        borderColour = "#cfcfcf"
      )
    ),
    height = list(
      title = 24
    )
  )

  # Expected output for the flatten_nested_list function
  expected_flat <- list(
    gridLines = FALSE,
    "style.body.border" = c("left", "right"),
    "style.body.borderColour" = "#cfcfcf",
    "height.title" = 24
  )

  result <- flatten_nested_list(nested_list)

  # Check if the result matches the expected flattened structure
  expect_equal(result, expected_flat)
})


# Test 3: Test for edge case - empty input (both functions)
test_that("convert_to_nested_list handles empty input", {
  empty_input <- list()
  result <- convert_to_nested_list(empty_input)
  expect_equal(result, list())  # Should return an empty list
})

test_that("flatten_nested_list handles empty input", {
  empty_input <- list()
  result <- flatten_nested_list(empty_input)
  expect_equal(result, list())  # Should return an empty list
})


# Test 4: Handling nested lists with multiple levels
test_that("convert_to_nested_list handles deep nesting", {

  key_value_pairs <- list(
    "style.title.fontSize" = 13,
    "style.subtitle.fontSize" = 12,
    "style.header.wrapText" = TRUE
  )

  expected_nested <- list(
    style = list(
      title = list(fontSize = 13),
      subtitle = list(fontSize = 12),
      header = list(wrapText = TRUE)
    )
  )

  result <- convert_to_nested_list(key_value_pairs)
  expect_equal(result, expected_nested)
})

test_that("flatten_nested_list handles deep nesting", {

  nested_list <- list(
    style = list(
      title = list(fontSize = 13),
      subtitle = list(fontSize = 12),
      header = list(wrapText = TRUE)
    )
  )

  expected_flat <- list(
    "style.title.fontSize" = 13,
    "style.subtitle.fontSize" = 12,
    "style.header.wrapText" = TRUE
  )

  result <- flatten_nested_list(nested_list)
  expect_equal(result, expected_flat)
})


# Test 5: Handling mixed data types
test_that("convert_to_nested_list handles mixed data types", {

  key_value_pairs <- list(
    "decimal.precision" = 2,
    "height.title" = 24,
    "style.body.valign" = "center",
    "style.body.border" = c("left", "right"),
    "style.body.numFmt" = "#,##0"
  )

  expected_nested <- list(
    decimal = list(precision = 2),
    height = list(title = 24),
    style = list(
      body = list(
        valign = "center",
        border = c("left", "right"),
        numFmt = "#,##0"
      )
    )
  )

  result <- convert_to_nested_list(key_value_pairs)
  expect_equal(result, expected_nested)
})

test_that("flatten_nested_list handles mixed data types", {

  nested_list <- list(
    decimal = list(precision = 2),
    height = list(title = 24),
    style = list(
      body = list(
        valign = "center",
        border = c("left", "right"),
        numFmt = "#,##0"
      )
    )
  )

  expected_flat <- list(
    "decimal.precision" = 2,
    "height.title" = 24,
    "style.body.valign" = "center",
    "style.body.border" = c("left", "right"),
    "style.body.numFmt" = "#,##0"
  )

  result <- flatten_nested_list(nested_list)
  expect_equal(result, expected_flat)
})


test_that("add_facade adds a new facade attribute to a data frame", {
  # Adding a facade
  result <- add_facade(test_data, table.gridLines = TRUE, table.fontSize = 12)

  # Check if the result is still a data frame
  expect_s3_class(result, "data.frame")

  # Check if the "facade" attribute is added
  expect_true("facade" %in% names(attributes(result)))

  # Check if the correct values are added to the facade attribute
  facade <- attributes(result)$facade
  expect_true("table.gridLines" %in% names(facade))
  expect_true("table.fontSize" %in% names(facade))

  expect_equal(facade$table.gridLines, TRUE)
  expect_equal(facade$table.fontSize, 12)
})

test_that("add_facade appends new values to the existing facade", {
  # Add initial facade
  result <- add_facade(test_data, table.gridLines = TRUE)

  # Add additional facade values
  result <- add_facade(result, table.fontSize = 12, table.bgFill = "#FFFFFF")

  # Check if the new values are correctly appended
  facade <- attributes(result)$facade

  expect_equal(facade$table.gridLines, TRUE)
  expect_equal(facade$table.fontSize, 12)
  expect_equal(facade$table.bgFill, "#FFFFFF")
})

test_that("add_facade properly merges the new facade with the existing one", {
  # Add initial facade
  result <- add_facade(test_data, table.gridLines = TRUE, table.fontSize = 12)

  # Add a new value, this should merge into the existing facade
  result <- add_facade(result, table.fontColour = "#000000", table.gridLines = FALSE)

  # Check if the values are merged
  facade <- attributes(result)$facade

  expect_equal(facade$table.gridLines, FALSE)  # Ensure it was overwritten
  expect_equal(facade$table.fontSize, 12)
  expect_equal(facade$table.fontColour, "#000000")
})

test_that("add_facade handles NULL values correctly and does not add NULLs to the facade", {
  # Test that NULL values are ignored
  result <- add_facade(test_data, table.gridLines = NULL, table.fontSize = 12)

  # Check if the NULL value is not added to the facade
  facade <- attributes(result)$facade
  expect_true(!("table.gridLines" %in% names(facade)))
  expect_equal(facade$table.fontSize, 12)
})

test_that("add_facade works for non-data frame objects by adding a facade as an attribute", {
  # Creating a simple list to test with non-data frame
  simple_list <- list(A = 1, B = 2)

  # Adding facade to a list
  result <- add_facade(simple_list, table.fontSize = 14, table.halign = "center")

  # Check if the result is a list and has the "facade" attribute
  expect_true("facade" %in% names(attributes(result)))

  # Check the values in the facade
  facade <- attributes(result)$facade
  expect_equal(facade$table.fontSize, 14)
  expect_equal(facade$table.halign, "center")
})

test_that("add_facade maintains the correct class for data frames", {
  result <- add_facade(test_data, table.fontSize = 16)

  # Check that class remains "data.frame"
  expect_s3_class(result, "data.frame")
})

test_that("add_facade correctly modifies data frames and other objects", {
  # Test with data frame
  result_df <- add_facade(test_data, table.gridLines = TRUE, table.fontSize = 16)
  expect_s3_class(result_df, "data.frame")
  facade_df <- attributes(result_df)$facade
  expect_equal(facade_df$table.gridLines, TRUE)
  expect_equal(facade_df$table.fontSize, 16)

  # Test with list
  result_list <- add_facade(list(A = 1), table.fontSize = 14)
  expect_true("facade" %in% names(attributes(result_list)))
  facade_list <- attr(result_list, "facade")
  expect_equal(facade_list$table.fontSize, 14)
})

test_that("add_facade correctly adds attributes for different sections (title, body, footer)", {
  # Adding attributes for title, body, and footer
  result <- add_facade(test_data, title.fontSize = 18, body.fontSize = 12, footnotes.fontSize = 10)

  # Check if the values for title, body, and footer are correctly added
  facade <- attributes(result)$facade
  expect_equal(facade$title.fontSize, 18)
  expect_equal(facade$body.fontSize, 12)
  expect_equal(facade$footnotes.fontSize, 10)
})


test_that("add_facade does not add invalid attributes (i.e., NULL values)", {

  result <- add_facade(test_data, table.fontName = NULL, table.gridLines = TRUE)

  facade <- attributes(result)$facade
  expect_false("table.fontName" %in% names(facade))
  expect_true("table.gridLines" %in% names(facade))
  expect_equal(facade$table.gridLines, TRUE)
})

# Testing for a non-data frame object and ensuring it adds a "facade" attribute
test_that("add_facade adds facade to non-data frame object", {
  my_vector <- c(1, 2, 3)
  result <- add_facade(my_vector, table.fontSize = 14)

  # Check that the result is a vector but now has the facade attribute
  expect_false("data.frame" %in% class(result))
  expect_true("facade" %in% names(attributes(result)))

  facade <- attributes(result)$facade
  expect_equal(facade$table.fontSize, 14)
})

