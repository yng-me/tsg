#' Write data to a Google Sheet
#'
#' Writes \code{data} with [write_xlsx()] and uploads the file to Google Drive, which
#' converts it to a Google Sheet, keeping the facade styling (merged headers, borders,
#' fonts and fills). Requires the \code{googledrive} package; sign in with
#' \code{googledrive::drive_auth()}.
#'
#' Signing in needs access to your Drive files: on Google's consent screen, tick the
#' box to see, edit, create and delete your Google Drive files (it is unticked by
#' default). Without it, writing fails with "(403) Forbidden: Request had
#' insufficient authentication scopes"; sign in again with
#' \code{googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive", cache = FALSE)}
#' and tick the box.
#'
#' @param data A \code{tsg} table or a list of them, as for [write_xlsx()].
#' @param path Where to write. A name or Drive path (e.g. \code{"reports/Table 1"}) creates a new Google Sheet. An existing sheet, given as \code{googledrive::as_id("<file id>")} or a \code{dribble}, has its contents replaced in place, so its link stays the same.
#' @param ... Arguments passed to [write_xlsx()], e.g. \code{facade} or \code{include_table_list}. \code{separate_files = TRUE} is not supported. With \code{include_table_list = TRUE}, the links of the table list are rewritten after the upload so they open the right tab in Google Sheets.
#'
#' @return Invisibly, the Google Sheet as a \code{googledrive::dribble}.
#' @export
#'
#' @examples
#' \dontrun{
#' googledrive::drive_auth()
#' sheet <- write_gsheet(generate_frequency(person_record, sex), "Sex distribution")
#'
#' # Replace the contents of the same sheet later; its link stays the same
#' write_gsheet(generate_frequency(person_record, marital_status), sheet)
#' }
write_gsheet <- function(data, path, ...) {
  rlang::check_installed("googledrive", reason = "to write Google Sheets.")

  if (isTRUE(list(...)$separate_files)) {
    stop("`separate_files = TRUE` is not supported by write_gsheet().", call. = FALSE)
  }

  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  write_xlsx(data, file, ...)

  if (inherits(path, c("dribble", "drive_id"))) {
    sheet <- googledrive::drive_update(file = path, media = file)
  } else {
    sheet <- googledrive::drive_upload(media = file, path = path, type = "spreadsheet")
  }

  if (isTRUE(list(...)$include_table_list)) {
    gsheet_fix_links(sheet$id)
  }

  invisible(sheet)
}


# The table-list links point to sheets as Excel does ("#'Sheet'!A1"), which Google Sheets
# does not follow; it links to a tab by its id ("#gid=123&range=A1"). Rewrite them in
# place through the Sheets API, with the token googledrive already holds.
gsheet_fix_links <- function(id) {
  token <- googledrive::drive_token()
  sheets_api <- function(method, path, params = list(), body = NULL) {
    req <- gargle::request_build(
      method = method, path = paste0("v4/spreadsheets/", id, path),
      params = params, body = body, token = token,
      base_url = "https://sheets.googleapis.com"
    )
    gargle::response_process(gargle::request_make(req))
  }

  tabs <- sheets_api("GET", "", list(fields = "sheets.properties(sheetId,title)"))$sheets
  gids <- stats::setNames(
    vapply(tabs, function(s) as.character(s$properties$sheetId), character(1)),
    vapply(tabs, function(s) s$properties$title, character(1))
  )
  ranges <- gsheet_quote(names(gids))
  values <- sheets_api(
    "GET", "/values:batchGet",
    c(list(valueRenderOption = "FORMULA"), stats::setNames(as.list(ranges), rep("ranges", length(ranges))))
  )$valueRanges

  updates <- list()
  for (k in seq_along(values)) {
    rows <- values[[k]]$values
    for (i in seq_along(rows)) {
      for (j in seq_along(rows[[i]])) {
        cell <- as.character(rows[[i]][[j]])
        fixed <- gsheet_link_formula(cell, gids)
        if (!identical(fixed, cell)) {
          updates[[length(updates) + 1]] <- list(
            range = paste0(ranges[k], "!", xlsx_col_letter(j), i),
            values = list(list(fixed))
          )
        }
      }
    }
  }

  if (length(updates) > 0) {
    sheets_api("POST", "/values:batchUpdate", body = list(valueInputOption = "USER_ENTERED", data = updates))
  }
  invisible(length(updates))
}

# "Sheet name" -> "'Sheet name'" for A1 ranges.
gsheet_quote <- function(title) paste0("'", gsub("'", "''", title, fixed = TRUE), "'")

# =HYPERLINK("#'Sheet'!A1", "text") -> =HYPERLINK("#gid=123&range=A1", "text"); other
# text, and links to sheets not in `gids`, are returned unchanged.
gsheet_link_formula <- function(formula, gids) {
  match <- regmatches(formula, regexec("^(=HYPERLINK\\(\"#)'?((?:[^']|'')*?)'?!([A-Z]+[0-9]+)(\".*)$", formula, perl = TRUE))[[1]]
  if (length(match) == 0) return(formula)
  title <- gsub("''", "'", match[3], fixed = TRUE)
  if (!title %in% names(gids)) return(formula)
  paste0(match[2], "gid=", gids[[title]], "&range=", match[4], match[5])
}

xlsx_col_letter <- function(j) {
  out <- ""
  while (j > 0) {
    out <- paste0(LETTERS[(j - 1) %% 26 + 1], out)
    j <- (j - 1) %/% 26
  }
  out
}
