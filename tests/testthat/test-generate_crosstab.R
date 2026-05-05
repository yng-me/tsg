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
    status = haven::labelled(
      c("01", "02", "03", "02", "01", "01", "02", "01", "03", "01", "01", "03", "01", "03", "02", "01"),
      label = "Status",
      labels = c(
        High = "01",
        Medium = "02",
        Low = "03"
      )
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

  result <- suppressWarnings(generate_crosstab(mock_data, x = type))

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

test_that("generate_crosstab handles character labelled variable", {

  result <- generate_crosstab(mock_data_labelled, x = status, sex)
  expect_equal(as.character(result$category), c("01", "02", "03", "__Total__"))

  result_convert_factor <- generate_crosstab(mock_data_labelled, x = status, sex, convert_factor = TRUE)
  expect_equal(as.character(result_convert_factor$category), c("High", "Medium", "Low", "Total"))

  result_label_total <- generate_crosstab(mock_data_labelled, x = status, sex, convert_factor = TRUE, label_total = "Priority")
  expect_equal(as.character(result_label_total$category), c("High", "Medium", "Low", "Priority"))

  result_2 <- generate_crosstab(mock_data_labelled, x = sex, status)
  expect_equal(attributes(result_2$frequency_01)$label, 'Frequency__High')


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

  df_6 <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, calculate_per_group = TRUE, group_as_hierarchy = TRUE)

  df_7 <- mock_data_labelled |>
      dplyr::group_by(type) |>
      generate_crosstab(age_group, sex, group_as_list = TRUE, group_as_hierarchy = TRUE)

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

  expect_equal(nrow(df_6), 12)
  # group_as_list + group_as_hierarchy returns nested list with totals (no warning)
  expect_true(inherits(df_7, 'list'))
  expect_equal(length(df_7), 4)  # "Type: All", "A", "B", "C"
  expect_equal(names(df_7)[[1]], "Type: All")
  expect_true(all(sapply(df_7, is.data.frame)))


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


# label_group_hierarchy: named vector support (crosstab)
test_that("generate_crosstab supports named-vector label_group_hierarchy", {

  # Custom string: key should use the custom label
  r_custom <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, group_as_list = TRUE, group_as_hierarchy = TRUE,
      label_group_hierarchy = "Grand Total")

  expect_equal(names(r_custom)[[1]], "Type: Grand Total")

  # Named vector: per-variable label
  r_named <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, group_as_list = TRUE, group_as_hierarchy = TRUE,
      label_group_hierarchy = c(type = "All types"))

  expect_equal(names(r_named)[[1]], "Type: All types")

  # Fallback: unnamed element used when variable not in the named vector
  r_fallback <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, group_as_list = TRUE, group_as_hierarchy = TRUE,
      label_group_hierarchy = c(other_var = "X", "Fallback"))

  expect_equal(names(r_fallback)[[1]], "Type: Fallback")

  # Flat hierarchy with named vector
  r_flat <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, group_as_hierarchy = TRUE, calculate_per_group = TRUE,
      label_group_hierarchy = c(type = "All types"))

  expect_true(inherits(r_flat, "data.frame"))
  type_vals <- as.character(haven::as_factor(r_flat$type))
  expect_true("All types" %in% type_vals)

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

# test_that("generate_crosstab works correctly for multiple-response type cols", {
#
#   mock_data_multi <- dplyr::tibble(
#     v = c("A", "B", "A", "B", "C", "A", "C", "B", "A", "C", "A", "A", "C", "A", "A", "A"),
#     a = c(1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L),
#     b = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L)
#   )
#
#   res <- generate_crosstab(mock_data_multi, v, c(a, b))
#
#
# })

# ---------- Regression: add_total = FALSE / add_total_row = FALSE ---------
# Verifies the label-based filter fix (not index-based removal).

test_that("generate_crosstab add_total = FALSE removes total row (bottom)", {
  result <- generate_crosstab(mock_data, type, sex, add_total = FALSE)
  # Total row must not appear
  expect_false(any(as.character(result$category) == "Total"))
  # Non-total rows must all be present (A, B, C)
  expect_equal(nrow(result), 3)
})

test_that("generate_crosstab add_total = FALSE removes total row (top)", {
  result <- generate_crosstab(mock_data, type, sex,
    add_total = FALSE, position_total = "top")
  expect_false(any(as.character(result$category) == "Total"))
  expect_equal(nrow(result), 3)
})

test_that("generate_crosstab add_total_row = FALSE removes only the total row", {
  result_with    <- generate_crosstab(mock_data, type, sex, add_total_row = TRUE)
  result_without <- generate_crosstab(mock_data, type, sex, add_total_row = FALSE)
  expect_equal(nrow(result_without), nrow(result_with) - 1)
  expect_false(any(as.character(result_without$category) == "Total"))
})

test_that("generate_crosstab add_total_row = FALSE works with position_total = 'top'", {
  result <- generate_crosstab(mock_data, type, sex,
    add_total_row = FALSE, position_total = "top")
  expect_equal(nrow(result), 3)
  expect_false(any(as.character(result$category) == "Total"))
})

test_that("generate_crosstab add_total = FALSE works in grouped (data_j) path", {
  result <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, add_total = FALSE, expand_categories = TRUE)
  expect_equal(nrow(result), 6)  # 2 categories × 3 groups (no total row)
  expect_false(any(grepl("^0$", as.character(result$category))))
})

test_that("generate_crosstab add_total = FALSE works in grouped list path", {
  result <- mock_data_labelled |>
    dplyr::group_by(type) |>
    generate_crosstab(age_group, sex, add_total = FALSE,
      expand_categories = TRUE, group_as_list = TRUE)
  for(tbl in result) {
    expect_equal(nrow(tbl), 2)  # 2 categories, no total row
    expect_false(any(grepl("^0$", as.character(tbl$category))))
  }
})

test_that("generate_crosstab add_total = FALSE with custom label_total", {
  result <- generate_crosstab(mock_data, type, sex,
    add_total = FALSE, label_total = "Grand Total")
  expect_false(any(as.character(result$category) == "Grand Total"))
  expect_equal(nrow(result), 3)
})

# ── Regression tests: name_separator / label_separator ───────────────────────

test_that("generate_crosstab works with non-default name_separator", {
  result <- generate_crosstab(mock_data, type, sex, name_separator = ".")
  col_names <- names(result)
  # Columns must use "." as separator, not "_"
  expect_true(any(startsWith(col_names, "frequency.")))
  expect_false(any(startsWith(col_names, "frequency_")))
})

test_that("generate_crosstab column labels use label_separator correctly", {
  result <- generate_crosstab(mock_data, type, sex,
    name_separator = "_", label_separator = ">>")
  freq_cols <- names(result)[startsWith(names(result), "frequency_")]
  if (length(freq_cols) > 0) {
    labels <- vapply(result[freq_cols], function(x) attr(x, "label") %||% "", character(1))
    # All prefixed column labels must use ">>" not "__"
    expect_true(all(grepl(">>", labels, fixed = TRUE)))
    expect_false(any(grepl("__", labels, fixed = TRUE)))
  }
})

test_that("add_column_label handles category values with regex special characters", {
  # Category values containing "(", ")", "." should not break label extraction
  data_special <- dplyr::tibble(
    group = c("Yes (all)", "No (none)", "Yes (all)", "No (none)", "Yes (all)"),
    sex   = c("Male", "Female", "Male", "Female", "Male")
  )
  result <- generate_crosstab(data_special, group, sex,
    name_separator = "_", label_separator = "__")
  freq_cols <- names(result)[startsWith(names(result), "frequency_")]
  expect_true(length(freq_cols) > 0)
  labels <- vapply(result[freq_cols], function(x) attr(x, "label") %||% "", character(1))
  # Labels should be like "Frequency__Male", "Frequency__Female" – not garbled
  expect_true(all(startsWith(labels, "Frequency__")))
  # The suffix after "__" should match the category value (Male/Female)
  suffixes <- sub("^Frequency__", "", labels)
  expect_true(all(suffixes %in% c("Male", "Female", "Total")))
})

test_that("get_header splits labels by fixed string, not regex", {
  # If names_separator were treated as regex, "." would split on every character
  data_dot <- dplyr::tibble(x = 1:3)
  attr(data_dot$x, "label") <- "Spanner.Column"
  result <- get_header(data_dot, names_separator = ".")
  # Fixed split on "." should give c("Spanner", "Column")
  expect_equal(result[["x"]], c("Spanner", "Column"))
  # Sanity: a label without "." should not be split
  data_no_sep <- dplyr::tibble(y = 1:3)
  attr(data_no_sep$y, "label") <- "Plain Label"
  result2 <- get_header(data_no_sep, names_separator = ".")
  expect_equal(result2[["y"]], "Plain Label")
})



# ─── multiple_columns feature ─────────────────────────────────────────────────

mock_mc <- dplyr::tibble(
  sex      = factor(c("M","F","M","F","M","F","M","F","M","F")),
  seeing   = c(1,1,2,1,1,2,1,1,2,1),
  hearing  = c(1,2,1,1,2,1,1,2,1,1),
  walking  = c(2,1,1,1,1,2,1,1,2,1)
)

mock_mc_labelled <- dplyr::tibble(
  sex     = haven::labelled(c(1,2,1,2,1,2,1,2), labels = c(Male=1, Female=2), label="Sex"),
  seeing  = c(1,1,2,1,1,2,1,1),
  hearing = c(1,2,1,1,2,1,1,2)
)

test_that("multiple_columns returns single wide data.frame with correct class", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing, walking,
    multiple_columns = TRUE)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "tsg")
  expect_s3_class(result, "tsgc")
  expect_true(isTRUE(attr(result, "multiple_columns")))
  expect_equal(attr(result, "multiple_columns_filter"), 1L)
})

test_that("multiple_columns output has correct columns", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing, walking,
    multiple_columns = TRUE)
  expect_true("category" %in% names(result))
  expect_true("total" %in% names(result))
  expect_true("frequency_seeing" %in% names(result))
  expect_true("percent_seeing" %in% names(result))
  expect_true("frequency_hearing" %in% names(result))
  expect_true("percent_hearing" %in% names(result))
  expect_true("frequency_walking" %in% names(result))
  expect_true("percent_walking" %in% names(result))
})

test_that("multiple_columns computes correct frequencies and percentages", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing, walking,
    multiple_columns = TRUE)
  # 5 M and 5 F rows; seeing==1: F=4, M=3
  result_data <- result[as.character(result$category) != "Total", ]
  f_row <- result_data[as.character(result_data$category) == "F", ]
  m_row <- result_data[as.character(result_data$category) == "M", ]
  expect_equal(f_row$frequency_seeing, 4L, ignore_attr = TRUE)
  expect_equal(m_row$frequency_seeing, 3L, ignore_attr = TRUE)
  expect_equal(round(f_row$percent_seeing, 1), 80, ignore_attr = TRUE)
  expect_equal(round(m_row$percent_seeing, 1), 60, ignore_attr = TRUE)
  expect_equal(f_row$total, 5L, ignore_attr = TRUE)
  expect_equal(m_row$total, 5L, ignore_attr = TRUE)
})

test_that("multiple_columns total row is correct", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing, walking,
    multiple_columns = TRUE)
  total_row <- result[as.character(result$category) == "Total", ]
  expect_equal(nrow(total_row), 1L)
  expect_equal(total_row$total, 10L, ignore_attr = TRUE)
  expect_equal(total_row$frequency_seeing, 7L, ignore_attr = TRUE)
  expect_equal(round(total_row$percent_seeing, 1), 70, ignore_attr = TRUE)
})

test_that("multiple_columns respects add_total = FALSE", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, add_total = FALSE)
  expect_false("total" %in% names(result))
  expect_false("Total" %in% as.character(result$category))
})

test_that("multiple_columns respects multiple_columns_filter", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_filter = 2L)
  result_data <- result[as.character(result$category) != "Total", ]
  # seeing==2: F=1, M=2
  f_row <- result_data[as.character(result_data$category) == "F", ]
  m_row <- result_data[as.character(result_data$category) == "M", ]
  expect_equal(f_row$frequency_seeing, 1L, ignore_attr = TRUE)
  expect_equal(m_row$frequency_seeing, 2L, ignore_attr = TRUE)
})

test_that("multiple_columns uses as_proportion when requested", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, as_proportion = TRUE)
  expect_true("proportion_seeing" %in% names(result))
  expect_false("percent_seeing" %in% names(result))
  result_data <- result[as.character(result$category) == "F", ]
  expect_true(result_data$proportion_seeing <= 1)
})

test_that("multiple_columns warns when fewer than 2 columns given", {
  expect_warning(
    generate_crosstab(mock_mc, sex, seeing, multiple_columns = TRUE),
    "requires at least 2 columns"
  )
})

test_that("multiple_columns sets column label attributes correctly", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE)
  expect_equal(attr(result$category, "label"), "sex")
  expect_equal(attr(result$total, "label"), "Total")
  expect_match(attr(result$frequency_seeing, "label"), "^Frequency")
  expect_match(attr(result$percent_seeing, "label"), "^Percent")
  expect_match(attr(result$frequency_hearing, "label"), "^Frequency")
  # Labels survive add_total = TRUE (dplyr::bind_rows must not strip them)
  result_no_total <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, add_total = FALSE)
  expect_match(attr(result_no_total$frequency_seeing, "label"), "^Frequency")
})

test_that("multiple_columns works with haven_labelled x variable", {
  result <- generate_crosstab(mock_mc_labelled, sex, seeing, hearing,
    multiple_columns = TRUE)
  expect_s3_class(result, "tsg")
  expect_equal(nrow(result), 3L)  # 2 categories + Total
})

test_that("multiple_columns works with group_as_list", {
  mock_grp <- dplyr::mutate(mock_mc, region = rep(c("A","B"), 5))
  result <- mock_grp |>
    dplyr::group_by(region) |>
    generate_crosstab(sex, seeing, hearing, multiple_columns = TRUE,
      group_as_list = TRUE)
  expect_type(result, "list")
  expect_s3_class(result, "tsg")
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
  expect_true("region" %in% names(result$A))
})

test_that("multiple_columns works with calculate_per_group", {
  mock_grp <- dplyr::tibble(
    sex    = factor(c("M","F","M","F","M","F","M","F","M","F","M","M")),
    region = factor(c("A","A","A","A","A","A","B","B","B","B","B","B")),
    seeing = c(1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1),
    hearing = c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1)
  )
  result <- mock_grp |>
    dplyr::group_by(region) |>
    generate_crosstab(sex, seeing, hearing, multiple_columns = TRUE,
      calculate_per_group = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("region" %in% names(result))
  # 2 groups × (2 categories + Total) = 6 rows
  expect_equal(nrow(result), 6L)
})

# ── tidyselect support ────────────────────────────────────────────────────────

mock_ts <- dplyr::tibble(
  sex      = factor(c("M","F","M","F","M","F","M","F","M","F","M","M")),
  col_see  = c(1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L),
  col_hear = c(1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L),
  other    = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
)

test_that("tidyselect contains() works in regular crosstab mode", {
  result <- generate_crosstab(mock_ts, sex, dplyr::contains("col_"))
  expect_type(result, "list")
  expect_named(result, c("col_see", "col_hear"))
  expect_s3_class(result$col_see, "data.frame")
})

test_that("tidyselect starts_with() works with multiple_columns = TRUE", {
  result <- generate_crosstab(mock_ts, sex, dplyr::starts_with("col_"),
    multiple_columns = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("frequency_col_see", "frequency_col_hear") %in% names(result)))
})

test_that("tidyselect resolving to 0 columns falls back to generate_frequency()", {
  expect_warning(
    result <- generate_crosstab(mock_ts, sex, dplyr::contains("nonexistent")),
    "No column variable/s specified or no matching columns found"
  )
  expect_s3_class(result, "tsg")
})

test_that("tidyselect resolving to 1 column with multiple_columns = TRUE warns and falls back", {
  expect_warning(
    result <- generate_crosstab(mock_ts, sex, dplyr::contains("col_see"),
      multiple_columns = TRUE),
    "'multiple_columns = TRUE' requires at least 2 columns"
  )
  # Falls back to regular single-column crosstab (returns a data.frame, not mc table)
  expect_s3_class(result, "tsg")
  expect_false(isTRUE(attr(result, "multiple_columns")))
})

test_that("tidyselect with multiple_columns = TRUE and grouped data works", {
  mock_grp_ts <- mock_ts |>
    dplyr::mutate(region = factor(rep(c("A", "B"), 6))) |>
    dplyr::group_by(region)
  result <- generate_crosstab(mock_grp_ts, sex, dplyr::starts_with("col_"),
    multiple_columns = TRUE, group_as_list = TRUE)
  expect_type(result, "list")
  expect_named(result, c("A", "B"))
  expect_true(all(c("frequency_col_see", "frequency_col_hear") %in% names(result$A)))
})

# ============================================================
# multiple_columns_type = "stacked" tests
# New semantics: x = row variable; ... columns form hierarchical column header.
# e.g. generate_crosstab(df, x=sex, seeing, hearing, multiple_columns=TRUE,
#      multiple_columns_type="stacked") produces:
#   rows = sex categories; cols = frequency_1_1 (seeing=1,hearing=1),
#   frequency_1_2 (seeing=1,hearing=2), frequency_2_1 (seeing=2,hearing=1), ...
# ============================================================

test_that("stacked mode returns correct class and attrs", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked")
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "tsg")
  expect_s3_class(result, "tsgc")
  expect_true(isTRUE(attr(result, "multiple_columns")))
  expect_equal(attr(result, "multiple_columns_type"), "stacked")
})

test_that("stacked mode has category as first column (no variable column)", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked")
  expect_equal(names(result)[1], "category")
  expect_false("variable" %in% names(result))
})

test_that("stacked mode category column carries x variable label", {
  result <- generate_crosstab(mock_mc_labelled, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked")
  expect_equal(attr(result$category, "label"), "Sex")
})

test_that("stacked mode rows are x categories, not ... column categories", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked")
  # x = sex (factor M/F), so rows should be M, F (plus Total)
  cats <- as.character(result$category)
  expect_true("M" %in% cats)
  expect_true("F" %in% cats)
  expect_false("1" %in% cats)   # seeing/hearing values should NOT be rows
  expect_false("2" %in% cats)
})

test_that("stacked mode wide columns are combinations of ... column categories with subtotals", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    add_total = FALSE)
  # seeing in {1,2}, hearing in {1,2} -> up to 4 leaf combos; (2,2) is zero -> removed
  # Plus one subtotal per first-level category (seeing=1, seeing=2)
  leaf_cols    <- grep("^frequency_[0-9]+_[0-9]+$", names(result), value = TRUE)
  subtot_cols  <- grep("^frequency_[0-9]+$",        names(result), value = TRUE)
  expect_true(length(leaf_cols) >= 1L)
  expect_true(length(subtot_cols) >= 1L)  # at least one subtotal per first-level cat
  # Subtotal columns contain exactly one index in suffix (no extra separator)
  expect_true(all(grepl("^frequency_[0-9]+$", subtot_cols)))
})

test_that("stacked mode column labels use label_separator between levels", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    label_separator = "__", add_total = FALSE)
  leaf_cols   <- grep("^frequency_[0-9]+_[0-9]+$", names(result), value = TRUE)
  subtot_cols <- grep("^frequency_[0-9]+$",        names(result), value = TRUE)
  # Leaf columns (2 indices) must have exactly 2 occurrences of "__"
  for (col in leaf_cols) {
    lbl <- attr(result[[col]], "label")
    expect_equal(
      lengths(regmatches(lbl, gregexpr("__", lbl, fixed = TRUE))), 2L,
      info = paste("Leaf column:", col, "Label:", lbl)
    )
  }
  # Subtotal columns (1 index) are padded to full depth, so they also have exactly 2 "__"
  for (col in subtot_cols) {
    lbl <- attr(result[[col]], "label")
    expect_equal(
      lengths(regmatches(lbl, gregexpr("__", lbl, fixed = TRUE))), 2L,
      info = paste("Subtotal column:", col, "Label:", lbl)
    )
  }
  # Global ordering: ALL frequency columns (all groups) come before ANY percent column
  col_names  <- names(result)
  freq_positions <- grep("^frequency_", col_names)
  pct_positions  <- grep("^percent_",   col_names)
  expect_true(length(freq_positions) > 0 && length(pct_positions) > 0)
  expect_true(max(freq_positions) < min(pct_positions),
    info = "All frequency columns must precede all percent columns")
  # Within frequency block, group 1 subtotal and leaves come before group 2
  freq_1_pos  <- which(col_names == "frequency_1")
  freq_2_pos  <- which(col_names == "frequency_2")
  if (length(freq_1_pos) > 0 && length(freq_2_pos) > 0) {
    expect_true(freq_1_pos < freq_2_pos)
  }
})

test_that("stacked mode name_separator used in column names", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    name_separator = "_", add_total = FALSE)
  # Leaf columns (2 column variables): frequency_<n>_<m> — suffix "<n>_<m>" has 1 "_"
  leaf_cols <- grep("^frequency_[0-9]+_[0-9]+$", names(result), value = TRUE)
  expect_true(length(leaf_cols) >= 1L)
  for (col in leaf_cols) {
    suffix <- sub("^frequency_", "", col)
    n_sep  <- lengths(regmatches(suffix, gregexpr("_", suffix, fixed = TRUE)))
    expect_equal(n_sep, 1L, info = paste("Column:", col))
  }
})

test_that("stacked mode respects position_total = 'top'", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    position_total = "top")
  expect_equal(as.character(result$category[1]), "Total")
})

test_that("stacked mode respects add_total = FALSE", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    add_total = FALSE)
  expect_false("total" %in% names(result))
  expect_false("Total" %in% as.character(result$category))
})

test_that("stacked mode respects add_total_column = FALSE", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    add_total_column = FALSE)
  expect_false("total" %in% names(result))
})

test_that("stacked mode total column equals rowSums of leaf frequency columns", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    add_percent = FALSE)
  # Use only leaf columns (two indices) to avoid double-counting subtotals
  leaf_cols <- grep("^frequency_[0-9]+_[0-9]+$", names(result), value = TRUE)
  data_rows <- result[as.character(result$category) != "Total", ]
  expected_total <- rowSums(data_rows[, leaf_cols, drop = FALSE], na.rm = TRUE)
  expect_equal(as.integer(data_rows$total), as.integer(expected_total))
})

test_that("stacked mode works with group_as_list", {
  mock_grp <- dplyr::mutate(mock_mc, region = rep(c("A", "B"), 5)) |>
    dplyr::group_by(region)
  result <- generate_crosstab(mock_grp, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    group_as_list = TRUE)
  expect_type(result, "list")
  expect_named(result, c("A", "B"))
  expect_equal(names(result$A)[1], "region")
  expect_equal(names(result$A)[2], "category")
})

test_that("stacked mode works with calculate_per_group", {
  mock_grp <- dplyr::mutate(mock_mc, region = rep(c("A", "B"), 5)) |>
    dplyr::group_by(region)
  result <- generate_crosstab(mock_grp, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    calculate_per_group = TRUE)
  expect_s3_class(result, "data.frame")
  expect_equal(names(result)[1], "region")
  expect_equal(names(result)[2], "category")
  expect_equal(length(unique(result$region)), 2L)
})

test_that("stacked mode multiple_columns_filter is ignored", {
  result1 <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    multiple_columns_filter = 1L)
  result2 <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    multiple_columns_filter = 99L)
  expect_equal(result1, result2)
})

test_that("stacked mode subtotal columns equal sum of their sub-columns", {
  result <- generate_crosstab(mock_mc, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked",
    add_percent = FALSE, add_total = FALSE)
  # seeing=1 subtotal: frequency_1 == frequency_1_1 + frequency_1_2
  expect_true("frequency_1" %in% names(result))
  expect_true("frequency_2" %in% names(result))
  leaf_1 <- grep("^frequency_1_[0-9]+$", names(result), value = TRUE)
  leaf_2 <- grep("^frequency_2_[0-9]+$", names(result), value = TRUE)
  expect_equal(
    as.integer(result$frequency_1),
    as.integer(rowSums(result[, leaf_1, drop = FALSE], na.rm = TRUE))
  )
  expect_equal(
    as.integer(result$frequency_2),
    as.integer(rowSums(result[, leaf_2, drop = FALSE], na.rm = TRUE))
  )
})


test_that("stacked mode with labelled x uses haven labels in category column", {
  result <- generate_crosstab(mock_mc_labelled, sex, seeing, hearing,
    multiple_columns = TRUE, multiple_columns_type = "stacked")
  # x = sex (labelled Male=1, Female=2); category col should contain labelled values
  expect_true(haven::is.labelled(result$category) || is.factor(result$category) ||
                any(c("Male", "Female") %in% as.character(result$category)))
})
