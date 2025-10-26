# Example data frame for testing
df <- dplyr::tibble(
  category = c("C", "C", "A", "C", "A", "C", "A", "B"),
  type = c("X", "Y", "X", "X", "Y", "Y", "X", "X"),
  value = c(1, 2, 2, 2, 3, 3, 2, 2)
)

df_na <- dplyr::tibble(
  category = c("A", "B", "A", NA, "C", "C", "A", NA),
  type = c("X", "Y", "X", "X", "Y", "Y", "X", "X"),
  value = c(1, 2, 1, 2, 3, 3, 1, 2)
)

df_empty <- dplyr::tibble(
  category = character(0),
  value = numeric(0)
)

df_labelled <- dplyr::tibble(
  category = haven::labelled(
    c(1, 2, 1, 3, 2, 1, 3, 2),
    label = "Category haven",
    labels = c(A = 1, B = 2, C = 3)
  ),
  value = c(10, 20, 10, 30, 20, 10, 30, 20)
)

df_factored <- dplyr::tibble(
  category = factor(c(1, 2, 1, 3, 2, 1, 3, 2), labels = c("A", "B", "C")),
  value = c(5, 10, 5, 15, 10, 5, 15, 10)
)

mock_data_labelled <- dplyr::tibble(
  type = c(1, 2, 1, 2, 3, 1, 3, 2, 1, 1, 3, 1, 1, 2, 2, 3),
  sex =  c(1, 2, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 2, 1),
  status = c("Healthy", "Sick", "Sick", "Healthy", "Healthy", "Sick", "Sick", "Healthy", "Sick", "Healthy", "Healthy", "Sick", "Sick", "Sick", "Healthy", "Healthy"),
  age_group = c("Old", "Young", "Young", "Young", "Old", "Old", "Young", "Young", "Young", "Young", "Old", "Young", "Young", "Young", "Young", "Old")
)

mock_data_labelled <- mock_data_labelled |>
  dplyr::mutate(
    type = haven::labelled(
      type,
      label = "Type",
      labels = c(A = 1, B = 2, C = 3)
    ),
    sex = haven::labelled(
      sex,
      label = "Sex",
      labels = c(Male = 1, Female = 2)
    )
  )

attr(mock_data_labelled$age_group, "label") <- "Age group"
attr(df_factored$category, "label") <- "Category factor"

test_that("generate_frequency returns correct frequency table", {

  result <- generate_frequency(df, category)

  expect_s3_class(result, "tsg")
  expect_true("category" %in% colnames(result))
  expect_true("frequency" %in% colnames(result))
  expect_true("percent" %in% colnames(result))

  expect_true(is.integer(result$frequency))
  expect_true(is.numeric(result$percent) & !is.integer(result$percent))
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 4)
  expect_equal(as.vector(result$category), c("C", "A", "B", "Total"))
  expect_equal(as.vector(result$frequency), c(4, 3, 1, 8))

  expect_equal(attributes(result$frequency)$label, "Frequency")
  expect_equal(attributes(result$percent)$label, "Percent")

  expect_equal(attributes(result$category)$label, "category")

})

test_that("generate_frequency returns correct frequency table for multiple variables", {

  result <- generate_frequency(df)

  expect_s3_class(result, "tsg")
  expect_s3_class(result, "tsgf")
  expect_true(inherits(result, 'list'))
  expect_equal(length(result), 3)
  expect_equal(names(result), c("category", "type", "value"))

})

test_that("generate_frequency handles factors and labelled variables correctly", {

  result_labelled <- generate_frequency(df_labelled, category)
  expect_equal(as.vector(result_labelled$category), c(1, 2, 3, 0))
  expect_equal(attributes(result_labelled$category)$label, "Category haven")

  result_factored <- generate_frequency(df_factored, category)
  expect_equal(as.vector(result_factored$category), c("A", "B", "C", "Total"))
  expect_equal(attributes(result_factored$category)$label, "Category factor")

})


# Sorting by frequency in descending order
test_that("generate_frequency sorts correctly by frequency", {
  result <- generate_frequency(df, category, sort_value = FALSE)
  expect_equal(as.vector(result$category), c("A", "B", "C", "Total"))
  expect_equal(as.vector(result$frequency), c(3, 1, 4, 8))
})


# Excluding variable from sorting (sort_except)
test_that("generate_frequency respects sort_except argument", {
  result <- generate_frequency(df, category, value, sort_value = TRUE, sort_except = "value")
  expect_equal(as.vector(result$category$category), c("C", "A", "B", "Total"))
  expect_equal(as.vector(result$category$frequency), c(4, 3, 1, 8))
  expect_equal(as.vector(result$value$frequency), c(1, 5, 2, 8))
})


# Adding totals and percentages
test_that("generate_frequency adds total and percentages", {
  result <- generate_frequency(df, category, add_total = TRUE, add_percent = TRUE)
  expect_true("percent" %in% colnames(result))
  expect_equal(result$category[nrow(result)], "Total")
  expect_equal(result$percent[nrow(result)], 100)
})


# Excluding total and percentage
test_that("generate_frequency can exclude total and percentage", {

  result <- generate_frequency(df, category, add_total = FALSE, add_percent = FALSE)
  expect_true(!("percent" %in% colnames(result)))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)

})


# Adding cumulative frequencies and percentages
test_that("generate_frequency adds cumulative frequencies and percentages", {
  result <- generate_frequency(df, category, add_cumulative = TRUE, add_cumulative_percent = TRUE)
  expect_true("cumulative" %in% colnames(result))
  expect_true("cumulative_percent" %in% colnames(result))

  expect_equal(ncol(result), 5)
  expect_equal(result$cumulative_percent[nrow(result) - 1], 100)
  expect_equal(result$cumulative[nrow(result) - 1], result$frequency[nrow(result)])
  expect_true(is.na(result$cumulative_percent[nrow(result)]))
  expect_true(is.na(result$cumulative[nrow(result)]))

})


# Metadata handling (title, subtitle)
test_that("generate_frequency attaches metadata correctly", {
  result <- generate_frequency(
    df,
    category,
    metadata = list(
      title = "Frequency Table",
      subtitle = "Category Counts"
    )
  )

  # Check if metadata attributes are added correctly
  expect_equal(attr(result, "title"), "Frequency Table")
  expect_equal(attr(result, "subtitle"), "Category Counts")
})


# Custom labels for categories
test_that("generate_frequency uses custom labels", {
  result <- generate_frequency(df, category, label_stub = "Custom Label")

  # Check if the custom label is used for the output
  expect_equal(attr(result, "label_xlsx"), "Custom Label")
})


# Test position of total row (top vs. bottom)
test_that("generate_frequency places the total row in the correct position", {
  result_top <- generate_frequency(df, category, add_total = TRUE, position_total = "top")
  result_bottom <- generate_frequency(df, category, add_total = TRUE, position_total = "bottom")

  # Ensure total row is in the correct position
  expect_equal(result_top$category[1], "Total")
  expect_equal(result_bottom$category[nrow(result_bottom)], "Total")
})


test_that("generate_frequency returns as proportion instead of percent", {
  result <- generate_frequency(df, category, as_proportion = TRUE)
  expect_true("proportion" %in% colnames(result))
  expect_equal(result$proportion[nrow(result)], 1)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 4)
  expect_equal(attributes(result$proportion)$label, "Proportion")

})



# Including and excluding NA values
test_that("generate_frequency handles NA values correctly", {

  result_without_na <- generate_frequency(df_na, category, include_na = FALSE, add_total = FALSE)
  result_with_na <- generate_frequency(df_na, category, include_na = TRUE, add_total = FALSE)
  result_with_na_labelled <- generate_frequency(df_na, category, include_na = TRUE, label_na = "Missing", add_total = FALSE)

  result_with_na_group <- df_na |>
    dplyr::group_by(type) |>
    generate_frequency(category, include_na = TRUE, add_total = FALSE, group_as_list = TRUE, label_na = "Missing")

  expect_equal(sum(result_without_na$frequency, na.rm = TRUE), 6)
  expect_equal(nrow(result_without_na), 3)  # Only A, B, C
  expect_equal(nrow(result_with_na), 4)     # A, B, C, NA
  expect_equal(nrow(result_with_na_labelled), 4) # A, B, C, "Missing"
  expect_true("Missing" %in% result_with_na_labelled$category)
  expect_true("Not reported" %in% result_with_na$category)

  expect_true("Missing" %in% result_with_na_group[[1]]$category)

})


# Group as list
test_that("generate_frequency calculates per group and returns a list", {

  result <- df |>
    dplyr::group_by(value) |>
    generate_frequency(category, group_as_list = TRUE)

  expect_true(inherits(result, 'list'))
  expect_equal(length(result), 3)  # Three unique values in 'value' column
  expect_true(all(sapply(result, function(x) inherits(x, 'data.frame'))))

  result_warn <- df |>
    dplyr::group_by(category) |>
    generate_frequency(value, group_as_list = TRUE)

  expect_equal(names(result_warn), c("A", "B", "C"))

  expect_warning(result_warn, regexp = NA)


})


# With multiple grouping variables
test_that("generate_frequency works with multiple grouping variables", {

  result_1 <- df |>
    dplyr::group_by(type, category) |>
    generate_frequency(value, group_as_list = FALSE, calculate_per_group = FALSE)

  expect_true(inherits(result_1, 'tsg'))
  expect_equal(nrow(result_1), 8)
  expect_equal(ncol(result_1), 5)

  result_2 <- df |>
    dplyr::group_by(type, category) |>
    generate_frequency(value, group_as_list = TRUE, group_separator = "|", expand_categories = FALSE) |>
    suppressMessages()

  expect_true(inherits(result_2, 'list'))
  expect_equal(length(result_2), 5)
  expect_equal(names(result_2), c("X|A", "X|B", "X|C", "Y|A", "Y|C"))
  expect_true(all(sapply(result_2, function(x) inherits(x, 'data.frame'))))
  expect_equal(nrow(result_2[["X|A"]]), 2)
  expect_equal(nrow(result_2[["Y|C"]]), 3)
  expect_equal(ncol(result_2[["Y|C"]]), 5)
  expect_equal(result_2[["Y|C"]]$frequency[nrow(result_2[["Y|C"]])], 2)
  expect_equal(result_2[["Y|C"]]$percent[nrow(result_2[["Y|C"]])], 100)

  expect_contains(attributes(result_2)$groups, c("type", "category"))

  result_3 <- df |>
    dplyr::group_by(type, category) |>
    generate_frequency(value, group_as_list = FALSE, calculate_per_group = TRUE) |>
    suppressMessages()

  expect_true(inherits(result_3, 'tsg'))
  expect_equal(nrow(result_3), 20)
  expect_equal(ncol(result_3), 5)
  expect_equal(result_3$frequency[nrow(result_3)], 2)
  expect_equal(result_3$percent[nrow(result_3)], 100)
  expect_equal(result_3$category[nrow(result_3)], "Total")
  expect_equal(result_3$type[nrow(result_3)], "Y")

  expect_length(result_3$category[result_3$category == "Total"], 5)
  expect_equal(attributes(result_3)$label_total, "Total")
  expect_contains(attributes(result_3)$groups, c("type", "category"))

})


# Edge case with an empty dataset
test_that("generate_frequency handles empty data frame correctly", {
  result <- generate_frequency(df_empty, category)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 1)
  expect_equal(result$category[1], "Total")
  expect_equal(result$frequency[1], 0)
  expect_equal(result$percent[1], 0)
})


# Edge case where all data is NA
test_that("generate_frequency handles dataset with only NA values", {

  df_na_only <- dplyr::tibble(category = c(NA, NA, NA), value = c(NA, NA, NA))
  result <- generate_frequency(df_na_only, category, include_na = TRUE)

  expect_true("Not reported" %in% result$category)
  expect_equal(result$frequency[1], 3)  # All NA values should be counted as one category
  expect_equal(nrow(result), 2)  # "Missing" and "Total"
  expect_equal(result$percent[1], 100)
  expect_equal(result$category[2], "Total")
})


# Top n categories
test_that("generate_frequency handles top_n parameter correctly", {

  df_large <- dplyr::tibble(category = sample(LETTERS[1:10], 140, replace = TRUE))
  result_top_3 <- generate_frequency(df_large, category, top_n = 3, add_total = FALSE)

  expect_equal(nrow(result_top_3), 4)
  expect_true("Others" %in% result_top_3$category)

})


test_that("generate_frequency handles top_n with NA values correctly", {
  df_large_na <- dplyr::tibble(category = sample(c(LETTERS[1:10], NA), 100, replace = TRUE))
  result_top_3_na <- generate_frequency(df_large_na, category, top_n = 3, include_na = TRUE, add_total = FALSE)
  result_top_3 <- generate_frequency(df_large_na, category, top_n = 3, top_n_only = TRUE, include_na = TRUE, add_total = FALSE)
  result_top_not_sorted <- generate_frequency(df_large_na, category, top_n = 3, include_na = TRUE, add_total = FALSE, sort_value = FALSE)

  expect_equal(nrow(result_top_3_na), 4)
  expect_equal(nrow(result_top_not_sorted), length(unique(df_large_na$category)))
  expect_equal(nrow(result_top_3), 3)
})



test_that("generate_frequency handles collapse list correctly", {

  df_collapse <- dplyr::tibble(
    category_1 = c("A", "B", "C", "D", "E", "F", "G", "H"),
    category_2 = c("A", "C", "D", "D", "D", "F", "A", "H"),
  )

  df <- df_collapse |>
    tsg::generate_frequency(add_percent = FALSE, collapse_list = TRUE)

  df_1 <- df_collapse |>
    tsg::generate_frequency(collapse_list = TRUE)

  df_2 <- df_collapse |>
    generate_frequency() |>
    collapse_list()

  expect_equal(df_1, df_2)
  expect_equal(nrow(df_1), 2)
  expect_equal(ncol(df_1), 19)
  expect_equal(dim(df), c(2, 10))
})


test_that("generate_frequency expand categories correctly", {

  df_1 <- df |>
    dplyr::group_by(type) |>
    generate_frequency(category, expand_categories = TRUE)

  df_2 <- df |>
    dplyr::group_by(type) |>
    generate_frequency(category, expand_categories = FALSE)

  df_3 <- df |>
    dplyr::group_by(type) |>
    generate_frequency(category, expand_categories = FALSE, calculate_per_group = FALSE)

  expect_equal(nrow(df_1), 8)
  expect_equal(nrow(df_2), 7)
  expect_equal(nrow(df_3), 6)


})


test_that("generate_frequency retains label when grouping is applied", {

  df_grouped_1 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_frequency(sex)

  df_grouped_2 <- mock_data_labelled |>
    dplyr::group_by(age_group) |>
    generate_frequency(sex, group_as_list = TRUE)

  expect_equal(attributes(df_grouped_1$type)$label, "Type")
  expect_equal(attributes(df_grouped_2$Young$age_group)$label, "Age group")


})


