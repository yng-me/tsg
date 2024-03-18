
test_that("Dimensions are retured correctly where columns with all zero values are romoved", {
  df <- palmerpenguins::penguins
  expect_equal(ncol(generate_crosstab(df, sex)), 5)
  expect_equal(ncol(generate_crosstab(df, sex, species)), 8)
  expect_equal(df |> generate_crosstab(sex, species, island) |> dim(), c(4, 12))
  expect_equal(df |> generate_crosstab(sex, species, island, include_zero_value = T) |> dim(), c(4, 20))
  expect_equal(df |> dplyr::group_by(island) |> generate_crosstab(sex, species) |> dim(), c(10, 9))
  expect_equal(df |> generate_crosstab(sex, species, island, include_frequency = F) |> dim(), c(4, 7))
  expect_equal(df |> generate_crosstab(sex, species, island, include_proportion = F, include_zero_value = T) |> dim(), c(4, 11))
  expect_equal(df |> generate_crosstab(sex, species, island, total_by_col = T) |> dim(), c(4, 11))
  expect_equal(df |> generate_crosstab(sex, species, island, include_row_total = F) |> dim(), c(3, 12))
  expect_equal(df |> generate_crosstab(sex, species, island, total_by_col = T, include_column_total = F) |> dim(), c(4, 11))
})
