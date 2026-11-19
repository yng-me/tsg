skip_if_not_installed("googledrive")

# Drive is never called: the upload functions are mocked to record what they get.
mock_drive <- function(env = parent.frame()) {
  calls <- new.env()
  record <- function(fn) function(...) {
    args <- list(...)
    expect_true(file.exists(args$media))
    expect_no_error(openxlsx::read.xlsx(args$media))
    calls[[fn]] <- args
    structure(list(name = "sheet"), class = "fake_dribble")
  }
  testthat::local_mocked_bindings(
    drive_upload = record("drive_upload"),
    drive_update = record("drive_update"),
    .package = "googledrive",
    .env = env
  )
  calls
}

test_that("write_gsheet() uploads a new Google Sheet from a name", {
  calls <- mock_drive()
  out <- write_gsheet(generate_frequency(person_record, sex), "reports/Sex")

  expect_equal(calls$drive_upload$path, "reports/Sex")
  expect_equal(calls$drive_upload$type, "spreadsheet")
  expect_null(calls$drive_update)
  expect_s3_class(out, "fake_dribble")
  expect_false(file.exists(calls$drive_upload$media))
})

test_that("write_gsheet() replaces an existing sheet in place from an id", {
  calls <- mock_drive()
  id <- googledrive::as_id("abc123")
  write_gsheet(generate_frequency(person_record, sex), id, facade = get_tsg_facade("booktabs"))

  expect_equal(calls$drive_update$file, id)
  expect_null(calls$drive_upload)
})

test_that("write_gsheet() rejects separate_files", {
  mock_drive()
  expect_error(
    write_gsheet(list(a = generate_frequency(person_record, sex)), "x", separate_files = TRUE),
    "separate_files"
  )
})

test_that("generate_output() writes a Google Sheet", {
  calls <- mock_drive()
  generate_output(generate_frequency(person_record, sex), "Sex", format = "gsheet")
  expect_equal(calls$drive_upload$path, "Sex")
})

test_that("table-list links are rewritten to Google Sheets tab links", {
  gids <- c("List of Tables" = "0", "Sex" = "11", "O'Brien" = "7")
  expect_equal(
    gsheet_link_formula("=HYPERLINK(\"#'Sex'!A1\", \"Sex\")", gids),
    "=HYPERLINK(\"#gid=11&range=A1\", \"Sex\")"
  )
  expect_equal(gsheet_link_formula("=HYPERLINK(\"#'O''Brien'!A1\",\"x\")", gids), "=HYPERLINK(\"#gid=7&range=A1\",\"x\")")
  # other formulas, plain text and links to unknown sheets stay as they are
  expect_equal(gsheet_link_formula("=HYPERLINK(\"#'Gone'!A1\",\"x\")", gids), "=HYPERLINK(\"#'Gone'!A1\",\"x\")")
  expect_equal(gsheet_link_formula("=SUM(A1:A2)", gids), "=SUM(A1:A2)")
  expect_equal(gsheet_link_formula("Sex", gids), "Sex")
})

test_that("write_gsheet() fixes the table-list links through the Sheets API", {
  mock_drive()
  sent <- new.env()
  testthat::local_mocked_bindings(drive_token = function() NULL, .package = "googledrive")
  testthat::local_mocked_bindings(
    request_make = function(req, ...) {
      if (grepl("values:batchUpdate", req$url)) {
        sent$body <- req$body
        return(list())
      }
      if (grepl("values:batchGet", req$url)) {
        return(list(valueRanges = list(
          list(values = list(list(), list(), list(), list("", "Tab Name"), list("", "=HYPERLINK(\"#'Sex'!A1\",\"Sex\")"))),
          list(values = list(list("=HYPERLINK(\"#'List of Tables'!A1\",\"Back\")"), list("Table 1. Sex")))
        )))
      }
      list(sheets = list(
        list(properties = list(sheetId = 0, title = "List of Tables")),
        list(properties = list(sheetId = 11, title = "Sex"))
      ))
    },
    response_process = function(resp, ...) resp,
    .package = "gargle"
  )

  write_gsheet(list(Sex = generate_frequency(person_record, sex)), "x", include_table_list = TRUE)

  ranges <- vapply(sent$body$data, function(d) d$range, character(1))
  formulas <- vapply(sent$body$data, function(d) d$values[[1]][[1]], character(1))
  expect_equal(ranges, c("'List of Tables'!B5", "'Sex'!A1"))
  expect_equal(formulas, c("=HYPERLINK(\"#gid=11&range=A1\",\"Sex\")", "=HYPERLINK(\"#gid=0&range=A1\",\"Back\")"))
  expect_equal(sent$body$valueInputOption, "USER_ENTERED")
})
