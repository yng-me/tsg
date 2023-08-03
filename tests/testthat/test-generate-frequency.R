df <- palmerpenguins::penguins

test_that("basic frequency works", {
  expect_equal(ncol(generate_frequency(df, sex)), 5)
  expect_equal(nrow(generate_frequency(df, sex)), 4)
  expect_equal(nrow(generate_frequency(df, sex, include_total = F)), 3)
  expect_equal(nrow(generate_frequency(df, sex, include_total = F)), 3)
  expect_equal(ncol(generate_frequency(df, sex, include_cumulative = F)), 3)
  expect_equal(df |> dplyr::group_by(species) |> generate_frequency(sex) |> dim(), c(9, 6))
  expect_equal(dim(generate_frequency(df, sex, include_cumulative = F, include_total = F)), c(3, 3))
})


test_that("column labels reflected correctly", {
  expect_identical(
    names(generate_frequency(df, sex)),
    c('sex', 'Frequency', 'Percent', 'Cumulative Total', 'Cumulative Percent')
  )
  expect_identical(
    names(df |> dplyr::group_by(species) |> generate_frequency(sex)),
    c('species', 'sex', 'Frequency', 'Percent', 'Cumulative Total', 'Cumulative Percent')
  )
  expect_identical(
    names(generate_frequency(df, sex, label_stub = 'Sex', include_cumulative = F)),
    c('Sex', 'Frequency', 'Percent')
  )
  expect_equal(
    as.character(generate_frequency(df, sex, sort_frequency = T)[[1]][1]),
    'male'
  )
  expect_equal(
    as.character(generate_frequency(df, sex)[[1]][1]),
    'female'
  )
})
