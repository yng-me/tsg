# A lazy table must give the same table as the same data in memory, minus labels
# and expansion (a database has neither).

skip_if_not_installed("dbplyr")
skip_if_not_installed("RSQLite")

df <- dplyr::tibble(
  sex = c("M", "F", "M", NA, "F", "M", "F", "M"),
  region = c("a", "a", "b", "b", "a", "b", "b", "a"),
  urban = c("u", "r", "u", "u", "r", "r", "u", "u"),
  age = c(10L, 20L, 20L, 30L, 30L, 10L, 20L, 40L)
)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
withr::defer(DBI::dbDisconnect(con), teardown_env())
db <- dplyr::copy_to(con, df, "df")

expect_same <- function(lazy, local) {
  expect_equal(lazy, local, ignore_attr = "label")
}

test_that("generate_frequency() works on a lazy table", {
  expect_same(
    generate_frequency(db, sex),
    generate_frequency(df, sex, expand_categories = FALSE)
  )
  expect_same(
    generate_frequency(db, sex, region),
    generate_frequency(df, sex, region, expand_categories = FALSE)
  )
})

test_that("generate_frequency() works on a grouped lazy table", {
  expect_same(
    generate_frequency(dplyr::group_by(db, region), sex),
    generate_frequency(dplyr::group_by(df, region), sex, expand_categories = FALSE)
  )
  expect_same(
    generate_frequency(dplyr::group_by(db, region), sex, group_as_list = TRUE),
    generate_frequency(dplyr::group_by(df, region), sex, group_as_list = TRUE, expand_categories = FALSE)
  )
  expect_same(
    generate_frequency(dplyr::group_by(db, region), sex, group_as_hierarchy = TRUE),
    generate_frequency(dplyr::group_by(df, region), sex, group_as_hierarchy = TRUE, expand_categories = FALSE)
  )
})

test_that("generate_crosstab() works on a lazy table", {
  expect_same(
    generate_crosstab(db, sex, region),
    generate_crosstab(df, sex, region, expand_categories = FALSE)
  )
  # one variable falls back to generate_frequency(), with a warning
  expect_same(
    suppressWarnings(generate_crosstab(db, sex)),
    suppressWarnings(generate_crosstab(df, sex, expand_categories = FALSE))
  )
  expect_same(
    generate_crosstab(db, c(sex, urban), region),
    generate_crosstab(df, c(sex, urban), region, expand_categories = FALSE)
  )
})

test_that("generate_crosstab() works on a grouped lazy table", {
  expect_same(
    generate_crosstab(dplyr::group_by(db, region), sex, urban),
    generate_crosstab(dplyr::group_by(df, region), sex, urban, expand_categories = FALSE)
  )
  expect_same(
    generate_crosstab(dplyr::group_by(db, region), sex, urban, group_as_list = TRUE),
    generate_crosstab(dplyr::group_by(df, region), sex, urban, group_as_list = TRUE, expand_categories = FALSE)
  )
  expect_same(
    generate_crosstab(dplyr::group_by(db, region), sex, urban, group_as_hierarchy = TRUE),
    generate_crosstab(dplyr::group_by(df, region), sex, urban, group_as_hierarchy = TRUE, expand_categories = FALSE)
  )
})

test_that("nested group lists work on a lazy table", {
  # a database returns groups in no set order, so they are sorted; compare by name
  lazy <- generate_frequency(dplyr::group_by(db, region, urban), sex, group_as_list = TRUE)
  local <- generate_frequency(dplyr::group_by(df, region, urban), sex, group_as_list = TRUE, expand_categories = FALSE)
  expect_setequal(names(lazy), names(local))
  for (r in names(local)) {
    expect_setequal(names(lazy[[r]]), names(local[[r]]))
    for (u in names(local[[r]])) expect_same(lazy[[r]][[u]], local[[r]][[u]])
  }

  hier <- generate_frequency(dplyr::group_by(db, region, urban), sex, group_as_list = TRUE, group_as_hierarchy = TRUE)
  expect_equal(names(hier), c("region: All", "a", "b"))
})

test_that("nested group lists on a lazy table do not query the database per group", {
  # 5 regions x 4 areas = 20 groups; the query count must not grow with them
  big <- dplyr::tibble(
    sex = rep(c("M", "F"), 20),
    emp = rep(c("y", "n", "n", "y", "n"), 8),
    region = rep(letters[1:5], 8),
    area = rep(c("u", "r", "x", "y"), each = 10)
  )
  big_db <- dplyr::copy_to(con, big, "big")
  grouped <- dplyr::group_by(big_db, region, area)

  # the tracer runs inside RSQLite, so it counts in an option rather than a local variable
  withr::local_options(tsg_test_queries = 0)
  suppressMessages(trace(
    "dbSendQuery", tracer = quote(options(tsg_test_queries = getOption("tsg_test_queries") + 1)), print = FALSE,
    signature = c("SQLiteConnection", "character"), where = asNamespace("RSQLite")
  ))
  withr::defer(suppressMessages(untrace(
    "dbSendQuery", signature = c("SQLiteConnection", "character"), where = asNamespace("RSQLite")
  )))
  count_queries <- function(expr) {
    options(tsg_test_queries = 0)
    suppressMessages(force(expr))
    getOption("tsg_test_queries")
  }

  expect_lt(count_queries(generate_frequency(grouped, sex, group_as_list = TRUE)), 5)
  expect_lt(count_queries(generate_frequency(grouped, sex, group_as_list = TRUE, group_as_hierarchy = TRUE)), 5)
  expect_lt(count_queries(generate_crosstab(grouped, sex, emp, group_as_list = TRUE)), 5)
  expect_lt(count_queries(generate_crosstab(grouped, sex, emp, group_as_list = TRUE, group_as_hierarchy = TRUE)), 5)

  # same tables as in memory
  local <- generate_crosstab(dplyr::group_by(big, region, area), sex, emp, group_as_list = TRUE, expand_categories = FALSE)
  lazy <- generate_crosstab(grouped, sex, emp, group_as_list = TRUE)
  for (r in names(local)) {
    for (a in names(local[[r]])) expect_same(lazy[[r]][[a]], local[[r]][[a]])
  }
})

test_that("a lazy table shows stored values, names the stub after the column, and ignores expand_categories", {
  d <- dplyr::tibble(sex = c("M", "F", "M", "M"), region = c("a", "a", "b", "b"))
  d_db <- dplyr::copy_to(con, d, "d_stub")

  out <- generate_frequency(d_db, sex)
  expect_equal(attr(out$category, "label"), "sex")
  expect_setequal(as.character(out$category), c("M", "F", "Total"))

  # region b has no F: in memory expand_categories adds a zero row, a database cannot
  grouped <- generate_frequency(dplyr::group_by(d_db, region), sex, expand_categories = TRUE)
  expect_false(any(grouped$region == "b" & grouped$category == "F"))
  expanded <- generate_frequency(dplyr::group_by(d, region), sex, expand_categories = TRUE)
  expect_true(any(expanded$region == "b" & expanded$category == "F"))
})

test_that("multiple_columns on a lazy table counts in the database, with the same result", {
  d <- dplyr::tibble(
    sex = rep(c("M", "F"), 20),
    a = rep(c(1L, 2L, 1L, 1L, 2L), 8),
    b = rep(c(2L, 1L), 20),
    kind = rep(c("x", "y", "x", "z", "y"), 8),
    region = rep(c("p", "q", "r", "s"), each = 10)
  )
  d_db <- dplyr::copy_to(con, d, "d_mc")

  both <- function(f) expect_same(f(d_db), f(d))
  both(function(x) generate_crosstab(x, sex, a, b, multiple_columns = TRUE))
  both(function(x) generate_crosstab(dplyr::group_by(x, region), sex, a, b, multiple_columns = TRUE))
  both(function(x) generate_crosstab(dplyr::group_by(x, region), sex, a, b, multiple_columns = TRUE, group_as_list = TRUE))
  both(function(x) generate_crosstab(x, sex, kind, b, multiple_columns = TRUE, multiple_columns_type = "stacked"))
  both(function(x) generate_crosstab(dplyr::group_by(x, region), sex, kind, b, multiple_columns = TRUE, multiple_columns_type = "stacked"))
})
