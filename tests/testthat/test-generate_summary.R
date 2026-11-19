df <- dplyr::tibble(
  region = c("a", "a", "b", "b", "b"),
  age = haven::labelled(c(10L, 20L, 30L, NA, 50L), label = "Age"),
  income = c(100, 200, 300, 400, 500),
  sex = haven::labelled(c(1, 2, 1, 2, 1), labels = c(Male = 1, Female = 2))
)

test_that("generate_summary() returns one row per variable and one column per statistic", {
  out <- generate_summary(df, age, income)

  expect_s3_class(out, c("tsg", "tsgs"))
  expect_equal(names(out), c("category", "n", "mean", "sd", "median", "min", "max"))
  expect_equal(out$category, c("Age", "income"), ignore_attr = TRUE)
  expect_equal(out$n, c(4L, 5L), ignore_attr = TRUE)
  expect_type(out$n, "integer")
  expect_equal(out$mean, c(27.5, 300), ignore_attr = TRUE)
  expect_equal(out$median, c(25, 300), ignore_attr = TRUE)
  expect_equal(out$min, c(10, 100), ignore_attr = TRUE)
  expect_equal(out$max, c(50, 500), ignore_attr = TRUE)
  expect_equal(out$sd, c(stats::sd(c(10, 20, 30, 50)), stats::sd(df$income)), ignore_attr = TRUE)
  expect_equal(attr(out$mean, "label"), "Mean")
})

test_that("generate_summary() computes the chosen statistics only", {
  out <- generate_summary(df, age, stats = c("n_missing", "p25", "p75"))
  expect_equal(names(out), c("category", "n_missing", "p25", "p75"))
  expect_equal(out$n_missing, 1L, ignore_attr = TRUE)
  expect_equal(out$p25, 17.5, ignore_attr = TRUE)
  expect_equal(out$p75, 35, ignore_attr = TRUE)
  expect_error(generate_summary(df, age, stats = "mode"))
})

test_that("generate_summary() uses every numeric non-categorical column by default", {
  expect_equal(generate_summary(df)$category, c("Age", "income"), ignore_attr = TRUE)
})

test_that("generate_summary() rejects categorical variables", {
  expect_error(generate_summary(df, sex), "generate_frequency")
  expect_error(generate_summary(df, region), "generate_frequency")
})

test_that("generate_summary() summarises per group", {
  out <- generate_summary(dplyr::group_by(df, region), income)
  expect_equal(out$region, c("a", "b"), ignore_attr = TRUE)
  expect_equal(out$mean, c(150, 400), ignore_attr = TRUE)
  expect_equal(attr(out, "groups"), "region")

  out <- generate_summary(dplyr::group_by(df, region), age, income, group_as_list = TRUE)
  expect_named(out, c("a", "b"))
  expect_equal(out$b$category, c("Age", "income"), ignore_attr = TRUE)
  expect_equal(out$b$mean, c(40, 400), ignore_attr = TRUE)
  expect_equal(attr(out, "groups"), "region")
})

test_that("generate_summary() renames a group named category", {
  grouped <- dplyr::group_by(df, category = region)
  expect_message(out <- generate_summary(grouped, income), "category_group")
  expect_equal(names(out)[1:2], c("category_group", "category"))
  expect_equal(out$category_group, c("a", "b"), ignore_attr = TRUE)
  expect_equal(attr(out, "groups"), "category_group")

  out <- suppressMessages(generate_summary(grouped, income, group_as_list = TRUE))
  expect_named(out, c("a", "b"))
  expect_equal(attr(out, "groups"), "category_group")

  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(out, path)
  # one sheet per group
  expect_equal(openxlsx::getSheetNames(path), c("a", "b"))
  written <- openxlsx::read.xlsx(path, sheet = "b", colNames = FALSE)
  expect_equal(written[1, 1:2], data.frame(X1 = "category_group", X2 = "Variable"))
  expect_equal(written[2, 1:2], data.frame(X1 = "b", X2 = "income"), ignore_attr = TRUE)
})

test_that("generate_summary() renames a group named category on a lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  db <- dplyr::copy_to(con, dplyr::tibble(category = c("a", "b"), income = c(1, 2)), "t")
  out <- suppressMessages(generate_summary(dplyr::group_by(db, category), income))
  expect_equal(names(out)[1:2], c("category_group", "category"))
})

test_that("generate_summary() keeps table metadata", {
  out <- generate_summary(df, income, metadata = list(title = "Income"))
  expect_equal(attr(out, "title"), "Income")
})

test_that("generate_summary() writes to xlsx", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_xlsx(generate_summary(dplyr::group_by(df, region), age, income, group_as_list = TRUE), path)
  expect_true(file.exists(path))
})

test_that("generate_summary() works on a lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  local_df <- haven::zap_label(haven::zap_labels(df))
  db <- dplyr::copy_to(con, local_df, "df")

  expect_equal(
    generate_summary(dplyr::group_by(db, region), age, income),
    generate_summary(dplyr::group_by(local_df, region), age, income),
    ignore_attr = "label"
  )

  # SQLite has no quantile(); the error names the statistic.
  expect_error(generate_summary(db, income, stats = c("mean", "p25")), "p25")
})

test_that("an all-missing group gives NA, in memory and in a database", {
  d <- dplyr::tibble(g = c("a", "a", "b"), inc = c(1, 2, NA))
  expect_no_warning(out <- generate_summary(dplyr::group_by(d, g), inc))
  b <- out[out$g == "b", ]
  expect_equal(b$n, 0L, ignore_attr = TRUE)
  expect_true(all(is.na(unlist(b[c("mean", "sd", "median", "min", "max")]))))

  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  db <- dplyr::copy_to(con, d, "d")
  expect_equal(generate_summary(dplyr::group_by(db, g), inc), out, ignore_attr = "label")
})

test_that("generate_summary() says so when no column can be summarised", {
  d <- dplyr::tibble(inc = haven::labelled(c(100, 200, 99), c(DK = 99)), s = c("a", "b", "c"))
  expect_error(generate_summary(d), "No numeric variables")
})
