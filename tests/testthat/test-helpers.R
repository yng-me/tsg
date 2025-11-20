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
