data_helper <- tibble::tibble(
  group = c("A", "B", "C"),
  value1 = c(1, 2, 3),
  value2 = c(4, 5, 6)
)
test_that("add_column_total works correctly", {

  result <- add_column_total(data_helper)
  expect_equal(ncol(result), ncol(data_helper) + 1)

})


test_that("add_row_total works correctly", {

  result <- add_row_total(data_helper)
  expect_equal(nrow(result), nrow(data_helper) + 1)

})


# ---------- add_row_total (deeper) ----------------------------------------

test_that("add_row_total computes correct column sums", {
  result <- add_row_total(data_helper)
  expect_equal(result$value1[nrow(result)], sum(data_helper$value1))
  expect_equal(result$value2[nrow(result)], sum(data_helper$value2))
})

test_that("add_row_total places total at top when position = 'top'", {
  result <- add_row_total(data_helper, position = "top")
  expect_equal(result$group[1], "Total")
  expect_equal(result$value1[1], sum(data_helper$value1))
})

test_that("add_row_total uses custom label_total", {
  result <- add_row_total(data_helper, label_total = "Grand Total")
  expect_equal(result$group[nrow(result)], "Grand Total")
})

test_that("add_row_total errors when no numeric columns", {
  df_no_num <- tibble::tibble(a = c("x", "y"), b = c("p", "q"))
  expect_error(add_row_total(df_no_num), "No numeric columns")
})


# ---------- add_column_total (deeper) -------------------------------------

test_that("add_column_total computes correct row sums", {
  result <- add_column_total(data_helper)
  expected <- data_helper$value1 + data_helper$value2
  expect_equal(result$total, expected, ignore_attr = TRUE)
})

test_that("add_column_total sets label attribute on total column", {
  result <- add_column_total(data_helper)
  expect_equal(attr(result$total, "label"), "Total")
})

test_that("add_column_total respects custom name and label_total", {
  result <- add_column_total(data_helper, name = "sum_col", label_total = "Sum")
  expect_true("sum_col" %in% names(result))
  expect_equal(attr(result$sum_col, "label"), "Sum")
})


# ---------- add_footnote --------------------------------------------------

test_that("add_footnote attaches footnote attribute", {
  result <- add_footnote(data_helper, footnote = "Note 1.")
  expect_equal(attr(result, "footnotes")$text, "Note 1.")
})

test_that("add_footnote accumulates footnotes on repeated calls", {
  result <- data_helper |>
    add_footnote("First note.") |>
    add_footnote("Second note.")
  expect_length(attr(result, "footnotes")$text, 2)
  expect_equal(attr(result, "footnotes")$text[2], "Second note.")
})

test_that("add_footnote errors on non-string input", {
  expect_error(add_footnote(data_helper, footnote = 123), "character string")
})

test_that("add_footnote errors on length > 1 footnote", {
  expect_error(add_footnote(data_helper, footnote = c("a", "b")), "single character string")
})


# ---------- add_source_note -----------------------------------------------

test_that("add_source_note attaches source_note attribute", {
  result <- add_source_note(data_helper, "Source: XYZ")
  expect_equal(attr(result, "source_note"), "Source: XYZ")
})

test_that("add_source_note errors on non-string input", {
  expect_error(add_source_note(data_helper, source_note = 42), "character string")
})

test_that("add_source_note errors on length > 1", {
  expect_error(add_source_note(data_helper, source_note = c("a", "b")), "single character string")
})


# ---------- add_table_title -----------------------------------------------

test_that("add_table_title attaches title attribute", {
  result <- add_table_title(data_helper, "My Table")
  expect_equal(attr(result, "title"), "My Table")
})

test_that("add_table_title errors on non-string input", {
  expect_error(add_table_title(data_helper, title = TRUE), "character string")
})

test_that("add_table_title errors on length > 1", {
  expect_error(add_table_title(data_helper, title = c("a", "b")), "single character string")
})


# ---------- add_table_subtitle --------------------------------------------

test_that("add_table_subtitle attaches subtitle attribute", {
  result <- add_table_subtitle(data_helper, "Sub")
  expect_equal(attr(result, "subtitle"), "Sub")
})

test_that("add_table_subtitle errors on non-string input", {
  expect_error(add_table_subtitle(data_helper, subtitle = 1L), "character string")
})


# ---------- convert_factor ------------------------------------------------

test_that("convert_factor converts labelled vectors to factors", {
  df_lbl <- dplyr::tibble(
    x = haven::labelled(c(1, 2, 3), labels = c(A = 1, B = 2, C = 3)),
    y = c("p", "q", "r")
  )
  result <- convert_factor(df_lbl)
  expect_true(is.factor(result$x))
  expect_equal(levels(result$x), c("A", "B", "C"))
  expect_false(is.factor(result$y))
})

test_that("convert_factor leaves non-labelled columns unchanged", {
  df_lbl <- dplyr::tibble(
    x = haven::labelled(c(1, 2), labels = c(A = 1, B = 2)),
    y = c(10, 20)
  )
  result <- convert_factor(df_lbl)
  expect_true(is.factor(result$x))
  expect_false(is.factor(result$y))
  expect_equal(result$y, c(10, 20))
})

