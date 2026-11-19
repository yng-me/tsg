# Mocking up a simple data frame for testing
test_data <- data.frame(A = 1:3, B = 4:6)


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



test_that("add_facade stores evaluated values", {
  n <- 2
  result <- add_facade(data.frame(a = 1), table.offsetRow = n, table.offsetCol = -1)
  expect_equal(attr(result, "facade")$table.offsetRow, 2)
  expect_equal(attr(result, "facade")$table.offsetCol, -1)
})

test_that("remove_labels clears value labels in every table of a list", {
  result <- remove_labels(generate_frequency(person_record, sex, marital_status))
  expect_null(attr(result[[1]]$category, "labels"))
  expect_null(attr(result[[2]]$category, "labels"))
})

test_that("add_facade rejects unknown or unnamed settings", {
  expect_error(add_facade(data.frame(a = 1), header.fontsize = 12), "header.fontsize")
  expect_error(add_facade(data.frame(a = 1), 12), "named")
})

test_that("add_facade accepts every key of the default facade", {
  result <- add_facade(data.frame(a = 1), sub_group.height = 30, label.backHyperlink = "Back")
  expect_equal(attr(result, "facade")$sub_group.height, 30)
})

test_that("add_column_total labels a custom-named total column", {
  result <- add_column_total(data.frame(a = 1:2, b = 3:4), name = "all", label_total = "All")
  expect_equal(result$all, c(4, 6), ignore_attr = TRUE)
  expect_equal(attr(result$all, "label"), "All")
})

test_that("collapse_list pluck drops proportion columns too", {
  data <- person_record |>
    dplyr::group_by(sex) |>
    generate_frequency(seeing, hearing, as_proportion = TRUE)
  result <- collapse_list(data, pluck = "frequency", as_proportion = TRUE)
  expect_false(any(grepl("^proportion", names(result))))
})

test_that("get_tsg_facade() errors on an unknown name and lists the built-ins", {
  expect_error(get_tsg_facade("booktab"), "Unknown facade `booktab`.*booktabs")
  # one file per built-in serves every format; only the default differs per format
  expect_equal(get_tsg_facade("striped", which = "html"), get_tsg_facade("striped"))
  expect_false(identical(get_tsg_facade(which = "html"), get_tsg_facade()))
  expect_equal(get_tsg_facade(which = "pdf"), get_tsg_facade(which = "html"))
})
