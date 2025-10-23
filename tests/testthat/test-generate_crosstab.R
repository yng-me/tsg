# Create mock data
mock_data <- dplyr::tibble(
  type = factor(c("A", "B", "A", "B", "C", "A", "C", "B", "A")),
  sex = factor(c("Male", "Female", "Female", "Male", "Male", "Female", "Female", "Male", "Female")),
  status = c("Healthy", "Sick", "Sick", "Healthy", "Healthy", "Sick", "Sick", "Healthy", "Sick"),
  age_group = factor(c("Old", "Young", "Young", "Young", "Old", "Old", "Young", "Young", "Young"))
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

mock_data_factored <- mock_data_labelled |>
  dplyr::mutate(
    type = factor(type, labels = c("A", "B", "C")),
    sex = factor(sex, labels = c("Male", "Female"))
  )

attr(mock_data_labelled$age_group, "label") <- "Age group"

# Unit tests for generate_crosstab function
test_that("generate_crosstab generates frequency table for one variable", {
  result <- generate_crosstab(mock_data, x = type)

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "tsg")
  expect_s3_class(result, "tsgf")
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 4)

})


test_that("generate_crosstab generates cross-tabulation correctly using different inputs", {

  df <- generate_crosstab(mock_data, type, sex)
  df_l <- generate_crosstab(mock_data_labelled, type, sex)
  df_f <- generate_crosstab(mock_data_factored, type, sex)

  df2 <- generate_crosstab(mock_data, sex, type)
  df2_l <- generate_crosstab(mock_data_labelled, sex, type)
  df2_f <- generate_crosstab(mock_data_factored, sex, type)

  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tsg")
  expect_s3_class(df_l, "tsg")
  expect_s3_class(df_l, "tsg")

  expect_s3_class(df, "tsgc")
  expect_s3_class(df_f, "tsgc")
  expect_s3_class(df_f, "tsgc")

  expect_equal(dim(df), c(4, 6))
  expect_equal(dim(df_l), c(4, 6))
  expect_equal(dim(df_f), c(4, 6))

  expect_equal(dim(df2), c(3, 8))
  expect_equal(dim(df2_l), c(3, 8))
  expect_equal(dim(df2_f), c(3, 8))

  expect_equal(attributes(df$category)$label, 'type')
  expect_equal(attributes(df_l$category)$label, 'Type')
  expect_equal(attributes(df_f$category)$label, 'type')
  expect_equal(attributes(df2$category)$label, 'sex')
  expect_equal(attributes(df2_l$category)$label, 'Sex')
  expect_equal(attributes(df2_f$category)$label, 'sex')

  expect_equal(attributes(df$percent_Male)$label, 'Percent__Male')
  expect_equal(attributes(df_l$percent_1)$label, 'Percent__Male')
  expect_equal(attributes(df_f$percent_Male)$label, 'Percent__Male')
  expect_equal(attributes(df2$frequency_A)$label, 'Frequency__A')
  expect_equal(attributes(df2_l$frequency_1)$label, 'Frequency__A')
  expect_equal(attributes(df2_f$frequency_A)$label, 'Frequency__A')

  expect_equal(df$percent_Male[1] + df$percent_Female[1], 100)
  expect_equal(df_l$percent_1[1] + df_l$percent_2[1], 100)
  expect_equal(df_f$percent_Male[1] + df_f$percent_Female[1], 100)
  expect_equal(df2$percent_A[1] + df2$percent_B[1] + df2$percent_C[1], 100)
  expect_equal(df2_l$percent_1[1] + df2_l$percent_2[1] + df2_l$percent_3[1], 100)
  expect_equal(df2_f$percent_A[1] + df2_f$percent_B[1] + df2_f$percent_C[1], 100)

  expect_equal(as.character(df$category[nrow(df)]), "Total")
  expect_equal(as.character(df_l$category[nrow(df_l)]), "0")
  expect_equal(as.character(df_f$category[nrow(df_f)]), "Total")
  expect_equal(as.character(df2$category[nrow(df2)]), "Total")
  expect_equal(as.character(df2_l$category[nrow(df2_l)]), "0")
  expect_equal(as.character(df2_f$category[nrow(df2_f)]), "Total")

  expect_equal(df$total[nrow(df)], 9)
  expect_equal(df$total[nrow(df_l)], 9)
  expect_equal(df$total[nrow(df_f)], 9)

  expect_equal(df$total[nrow(df)], 9)
  expect_equal(df$total[nrow(df_l)], 9)
  expect_equal(df$total[nrow(df_f)], 9)

  expect_equal(
    names(df),
    c("category", "total", "frequency_Female", "frequency_Male", "percent_Female", "percent_Male")
  )

  expect_equal(
    names(df_l),
    c("category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_equal(
    names(df_f),
    c("category", "total", "frequency_Male", "frequency_Female", "percent_Male", "percent_Female")
  )

})


test_that("generate_crosstab calculates proportions if specified", {

  result <- generate_crosstab(mock_data, x = type, sex, add_percent = TRUE, as_proportion = TRUE)

  expect_true(all(result$proportion_Male >= 0 & result$proportion_Male <= 1))
  expect_equal(result$proportion_Male[1] + result$proportion_Female[1], 1)
  expect_equal(result$proportion_Male[nrow(result)] + result$proportion_Female[nrow(result)], 1)

})


test_that("generate_croosstab handles grouping correctly", {

  df_1 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, expand_categories = TRUE)

  df_2 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, expand_categories = FALSE)

  df_3 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, expand_categories = TRUE, group_as_list = TRUE)

  df_4 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, expand_categories = FALSE, group_as_list = TRUE)

  df_5 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, calculate_per_group = FALSE)

  expect_equal(dim(df_1), c(9, 7))
  expect_equal(dim(df_2), c(8, 7))
  expect_equal(length(df_3), 3)
  expect_equal(length(df_4), 3)

  expect_equal(names(df_3), c("A", "B", "C"))
  expect_equal(names(df_4), c("A", "B", "C"))

  expect_equal(dim(df_3$A), c(3, 7))
  expect_equal(dim(df_4$A), c(3, 7))
  expect_equal(dim(df_3$B), c(3, 7))
  expect_equal(dim(df_4$B), c(2, 7))
  expect_equal(dim(df_3$C), c(3, 7))
  expect_equal(dim(df_4$C), c(3, 7))

  expect_identical(
    names(df_1),
    c("type", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_identical(
    names(df_2),
    c("type", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_identical(
    names(df_3[[1]]),
    c("type", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_identical(
    names(df_4[[1]]),
    c("type", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_identical(
    names(df_5),
    c("type", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

})


test_that("generate_croosstab handles multiple grouping correctly", {

  df_1 <- mock_data_labelled |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = FALSE,
      group_as_list = TRUE
    )

  df_2 <- mock_data_labelled |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = TRUE,
      group_as_list = TRUE
    )

  df_3 <- mock_data_labelled |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = FALSE,
      calculate_per_group = TRUE
    )

  df_4 <- mock_data_labelled |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = TRUE,
      calculate_per_group = TRUE
    )

  df_5 <- mock_data_labelled |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = FALSE,
      calculate_per_group = FALSE
    )

  expect_identical(
    names(df_3),
    c("type", "status", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_identical(
    names(df_4),
    c("type", "status", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

  expect_identical(
    names(df_5),
    c("type", "status", "category", "total", "frequency_1", "frequency_2", "percent_1", "percent_2")
  )

})


test_that("generate_croosstab handles multiple grouping with edge cases correctly", {

  df_3 <- mock_data |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = FALSE,
      calculate_per_group = TRUE
    )

  df_4 <- mock_data |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = TRUE,
      calculate_per_group = TRUE
    )

  df_5 <- mock_data |>
    dplyr::group_by(type, status) |>
    generate_crosstab(
      age_group,
      sex,
      expand_categories = FALSE,
      calculate_per_group = FALSE
    )

  expect_identical(
    names(df_3),
    c("type", "status", "category", "total", "frequency_Male", "frequency_Female", "percent_Male", "percent_Female")
  )

  expect_identical(
    names(df_4),
    c("type", "status", "category", "total", "frequency_Male", "frequency_Female", "percent_Male", "percent_Female")
  )

  expect_identical(
    names(df_5),
    c("type", "status", "category", "total", "frequency_Female", "frequency_Male", "percent_Female", "percent_Male")
  )

})


test_that("generate_crosstab works correctly using percent_by_column", {

  df <- generate_crosstab(mock_data, type, sex, percent_by_column = TRUE)
  df_l <- generate_crosstab(mock_data_labelled, type, sex, percent_by_column = TRUE)
  df_f <- generate_crosstab(mock_data_factored, type, sex, percent_by_column = TRUE)

  df2 <- generate_crosstab(mock_data, sex, type, percent_by_column = TRUE)
  df2_l <- generate_crosstab(mock_data_labelled, sex, type, percent_by_column = TRUE)
  df2_f <- generate_crosstab(mock_data_factored, sex, type, percent_by_column = TRUE)

  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tsg")
  expect_s3_class(df_l, "tsg")
  expect_s3_class(df_l, "tsg")

  expect_s3_class(df, "tsgc")
  expect_s3_class(df_f, "tsgc")
  expect_s3_class(df_f, "tsgc")

  expect_equal(dim(df), c(4, 7))
  expect_equal(dim(df_l), c(4, 7))
  expect_equal(dim(df_f), c(4, 7))

  expect_equal(dim(df2), c(3, 9))
  expect_equal(dim(df2_l), c(3, 9))
  expect_equal(dim(df2_f), c(3, 9))

  expect_equal(attributes(df$category)$label, 'type')
  expect_equal(attributes(df_l$category)$label, 'Type')
  expect_equal(attributes(df_f$category)$label, 'type')
  expect_equal(attributes(df2$category)$label, 'sex')
  expect_equal(attributes(df2_l$category)$label, 'Sex')
  expect_equal(attributes(df2_f$category)$label, 'sex')

  expect_equal(attributes(df$percent_Male)$label, 'Percent__Male')
  expect_equal(attributes(df_l$percent_1)$label, 'Percent__Male')
  expect_equal(attributes(df_f$percent_Male)$label, 'Percent__Male')
  expect_equal(attributes(df2$frequency_A)$label, 'Frequency__A')
  expect_equal(attributes(df2_l$frequency_1)$label, 'Frequency__A')
  expect_equal(attributes(df2_f$frequency_A)$label, 'Frequency__A')

  expect_equal(df$percent_Male[nrow(df)], 100)
  expect_equal(df_l$percent_1[nrow(df_l)], 100)
  expect_equal(df_f$percent_Male[nrow(df_f)], 100)
  expect_equal(df2$percent_A[nrow(df2)], 100)
  expect_equal(df2_l$percent_1[nrow(df2_l)], 100)
  expect_equal(df2_f$percent_A[nrow(df2_f)], 100)

  expect_equal(as.character(df$category[nrow(df)]), "Total")
  expect_equal(as.character(df_l$category[nrow(df_l)]), "0")
  expect_equal(as.character(df_f$category[nrow(df_f)]), "Total")
  expect_equal(as.character(df2$category[nrow(df2)]), "Total")
  expect_equal(as.character(df2_l$category[nrow(df2_l)]), "0")
  expect_equal(as.character(df2_f$category[nrow(df2_f)]), "Total")

  expect_equal(df$frequency_total[nrow(df)], 9)
  expect_equal(df$frequency_total[nrow(df_l)], 9)
  expect_equal(df$frequency_total[nrow(df_f)], 9)

  expect_equal(df$frequency_total[nrow(df)], 9)
  expect_equal(df$frequency_total[nrow(df_l)], 9)
  expect_equal(df$frequency_total[nrow(df_f)], 9)

})


test_that("generate_crosstab returns frequency values only if specified", {

  df_0 <- mock_data_labelled |>
    generate_crosstab(type, sex, add_percent = FALSE)

  df_1 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, add_percent = FALSE)

  df_2 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, add_percent = FALSE, group_as_list = TRUE)

  df_3 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, add_percent = FALSE, percent_by_column = TRUE)

  df_4 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, add_percent = FALSE, calculate_per_group = TRUE)

  expect_equal(df_0[['1']][1] + df_0[['2']][1], 7)
  expect_equal(df_0[['1']][nrow(df_0)] + df_0[['2']][nrow(df_0)], 16)

  expect_identical(
    names(df_0),
    c("category", "1", "2", "total")
  )

  expect_equal(dim(df_0), c(4, 4))
  expect_equal(dim(df_1), c(9, 5))
  expect_equal(dim(df_2$A), c(3, 5))
  expect_equal(dim(df_2$B), c(3, 5))
  expect_equal(dim(df_2$C), c(3, 5))
  expect_equal(dim(df_3), c(9, 5))
  expect_equal(dim(df_4), c(9, 5))

})


test_that("generate_crosstab handles missing values correctly", {

  mock_data_with_na <- mock_data
  mock_data_with_na$type[1] <- NA

  result_1 <- generate_crosstab(mock_data_with_na, type, sex, label_na = "Missing")

  result_2 <- mock_data_with_na |>
    dplyr::group_by(age_group) |>
    generate_crosstab(type, sex, include_na = FALSE)

  result_3 <- mock_data_with_na |>
    dplyr::group_by(age_group) |>
    generate_crosstab(type, sex, label_na = "Missing")

  expect_true(any(result_1$category == "Missing"))
  expect_true(all(!is.na(result_2$category)))

})



test_that("generate_crosstab retains label when grouping is applied", {

  df_grouped <- mock_data_labelled |>
    dplyr::group_by(age_group) |>
    generate_crosstab(type, sex, group_as_list = TRUE)

  expect_equal(attributes(df_grouped[[1]]$age_group)$label, "Age group")

})



