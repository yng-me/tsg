df_freq <- dplyr::tibble(
  category_1 = c("A", "B", "C", "A", "B"),
  category_2 = c("X", "X", "Y", "Y", "Y"),
  value      = c(10, 20, 10, 30, 20)
)

df_labelled_freq <- dplyr::tibble(
  status = haven::labelled(
    c(1, 2, 1, 3, 2, 1),
    label  = "Status",
    labels = c(High = 1, Medium = 2, Low = 3)
  ),
  sex = haven::labelled(
    c(1, 2, 1, 2, 1, 2),
    label  = "Sex",
    labels = c(Male = 1, Female = 2)
  )
)

# ---------- collapse_list ------------------------------------------------

test_that("collapse_list errors on non-list input", {
  expect_error(collapse_list(data.frame(a = 1)), "Data must be a list")
})

test_that("collapse_list collapses a simple frequency list", {
  result <- generate_frequency(df_freq, add_percent = FALSE, collapse_list = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("category" %in% names(result))
})

test_that("collapse_list respects col_id and label arguments", {
  freq_list <- generate_frequency(df_freq)
  result <- collapse_list(freq_list, col_id = "variable", label = "My Label")
  expect_true("variable" %in% names(result))
  expect_equal(attr(result$variable, "label"), "My Label")
})

test_that("collapse_list with pluck = 'frequency' drops percent columns", {
  freq_list <- generate_frequency(df_freq)
  result <- collapse_list(freq_list, pluck = "frequency")
  expect_false(any(grepl("^percent_", names(result))))
  expect_true(any(grepl("^frequency_", names(result))))
})

test_that("collapse_list with as_proportion = TRUE uses proportion columns", {
  freq_list <- generate_frequency(df_freq, as_proportion = TRUE)
  result <- collapse_list(freq_list, as_proportion = TRUE)
  expect_true(any(grepl("^proportion_", names(result))))
  expect_false(any(grepl("^percent_", names(result))))
})


# ---------- rename_label -------------------------------------------------

test_that("rename_label renames a single column label", {
  result <- generate_frequency(df_freq, category_1) |>
    rename_label(category = "My Category")
  expect_equal(attr(result$category, "label"), "My Category")
})

test_that("rename_label renames multiple column labels", {
  result <- generate_frequency(df_freq, category_1) |>
    rename_label(category = "Var", frequency = "Count", percent = "Pct")
  expect_equal(attr(result$category,  "label"), "Var")
  expect_equal(attr(result$frequency, "label"), "Count")
  expect_equal(attr(result$percent,   "label"), "Pct")
})

test_that("rename_label warns for non-existent column", {
  result <- generate_frequency(df_freq, category_1)
  expect_warning(rename_label(result, nonexistent = "X"), "not found")
})

test_that("rename_label returns data unchanged when no args given", {
  result <- generate_frequency(df_freq, category_1)
  result2 <- rename_label(result)
  expect_identical(result, result2)
})

test_that("rename_label works on list output", {
  freq_list <- generate_frequency(df_freq)
  result <- rename_label(freq_list, category = "Variable", frequency = "N")
  for(tbl in result) {
    expect_equal(attr(tbl$category,  "label"), "Variable")
    expect_equal(attr(tbl$frequency, "label"), "N")
  }
})


# ---------- remove_label -------------------------------------------------

test_that("remove_label clears label attr for all columns by default", {
  result <- generate_frequency(df_freq, category_1) |>
    remove_label()
  for(col in names(result)) {
    expect_null(attr(result[[col]], "label"))
  }
})

test_that("remove_label clears label attr for specified column only", {
  result <- generate_frequency(df_freq, category_1)
  original_freq_label <- attr(result$frequency, "label")
  result2 <- remove_label(result, category)
  expect_null(attr(result2$category, "label"))
  expect_equal(attr(result2$frequency, "label"), original_freq_label)
})

test_that("remove_label does NOT clear labels or xlsx_label attrs", {
  freq <- generate_frequency(df_labelled_freq, status)
  attr(freq$category, "labels") <- c(High = 1, Medium = 2, Low = 3)
  result <- remove_label(freq, category)
  expect_null(attr(result$category, "label", exact = TRUE))
  expect_false(is.null(attr(result$category, "labels")))
})

test_that("remove_label works on list output", {
  freq_list <- generate_frequency(df_freq)
  result <- remove_label(freq_list)
  for(tbl in result) {
    for(col in names(tbl)) {
      expect_null(attr(tbl[[col]], "label"))
    }
  }
})


# ---------- remove_labels ------------------------------------------------

test_that("remove_labels clears label, labels and xlsx_label attrs", {
  freq <- generate_frequency(df_labelled_freq, status)
  attr(freq$category, "labels")     <- c(A = 1, B = 2)
  attr(freq$category, "xlsx_label") <- "Excel Label"

  result <- remove_labels(freq, category)
  expect_null(attr(result$category, "label"))
  expect_null(attr(result$category, "labels"))
  expect_null(attr(result$category, "xlsx_label"))
})

test_that("remove_labels on list clears attrs in every sub-table", {
  freq_list <- generate_frequency(df_labelled_freq)
  result <- remove_labels(freq_list)
  for(tbl in result) {
    for(col in names(tbl)) {
      expect_null(attr(tbl[[col]], "label"))
      expect_null(attr(tbl[[col]], "labels"))
    }
  }
})
