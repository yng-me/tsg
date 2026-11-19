# Real survey grouping variables have missing values, 0 codes, and value labels without
# a variable label; person_record has none of these.
edge_data <- function() {
  d <- person_record
  d$employed[seq(1, nrow(d), by = 10)] <- NA
  d$emp0 <- haven::labelled(ifelse(d$employed == 1, 1, 0), c(No = 0, Yes = 1)) # no variable label
  d
}

test_that("html and docx take a group with value labels but no variable label", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  tbl <- edge_data() |> dplyr::group_by(emp0) |> generate_frequency(sex)
  expect_s3_class(as_gt(tbl), "gt_tbl")
  expect_s3_class(tsg_to_flextable(tbl), "flextable")
})

test_that("a hierarchy with a missing group value has no empty rows and keeps that group", {
  d <- edge_data()
  h <- d |> dplyr::group_by(sex, employed) |> generate_frequency(marital_status, group_as_hierarchy = TRUE)
  expect_false(anyNA(h$frequency))
  totals <- dplyr::filter(h, as.character(haven::as_factor(category)) == "Total")
  # per sex: the leaves (employed No/Yes/NA) add up to the sex subtotal ("All")
  for (s in c("Male", "Female")) {
    rows <- totals[as.character(haven::as_factor(totals$sex)) == s, ]
    is_all <- as.character(haven::as_factor(rows$employed)) %in% "All"
    expect_equal(sum(rows$frequency[!is_all]), rows$frequency[is_all])
    expect_true(anyNA(rows$employed))
  }
})

test_that("nested group lists keep a missing group, so the parts add up", {
  d <- edge_data()
  x <- d |> dplyr::group_by(sex, employed) |> generate_frequency(marital_status, group_as_list = TRUE)
  expect_true("NA" %in% names(x$Male))
  total <- function(t) t$frequency[as.character(haven::as_factor(t$category)) == "Total"]
  expect_equal(sum(vapply(x$Male, total, numeric(1))), sum(haven::as_factor(d$sex) == "Male"))
})

test_that("crosstab lists keep the group column for a group coded 0 or missing", {
  x <- edge_data() |> dplyr::group_by(emp0) |> generate_crosstab(marital_status, sex, group_as_list = TRUE)
  expect_true(all(vapply(x, function(t) "emp0" %in% names(t), logical(1))))
})

test_that("a data column named like a loop variable does not change hierarchy subtotals", {
  d <- edge_data()
  plain <- d |> dplyr::group_by(sex, employed) |> generate_crosstab(marital_status, seeing, group_as_hierarchy = TRUE)
  d$pv <- 1L
  with_pv <- d |> dplyr::group_by(sex, employed) |> generate_crosstab(marital_status, seeing, group_as_hierarchy = TRUE)
  expect_equal(with_pv$total, plain$total)
})

test_that("separate files take table names with a slash", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  d <- edge_data()
  d$grp <- haven::labelled(ifelse(d$sex == 1, 1, 2), c("Single/never married" = 1, "Other" = 2), label = "Group")
  x <- d |> dplyr::group_by(grp) |> generate_frequency(sex, group_as_list = TRUE)
  dir <- withr::local_tempdir()
  write_html(x, file.path(dir, "html"), separate_files = TRUE)
  write_docx(x, file.path(dir, "docx"), separate_files = TRUE)
  expect_setequal(list.files(file.path(dir, "html")), c("Single or never married.html", "Other.html"))
  expect_setequal(list.files(file.path(dir, "docx")), c("Single or never married.docx", "Other.docx"))
})

test_that("generate_crosstab() rejects input that is not a data frame", {
  expect_error(generate_crosstab(person_record$sex, sex, age), "valid data frame")
})
