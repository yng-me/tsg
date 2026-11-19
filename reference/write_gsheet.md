# Write data to a Google Sheet

Writes `data` with
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
and uploads the file to Google Drive, which converts it to a Google
Sheet, keeping the facade styling (merged headers, borders, fonts and
fills). Requires the `googledrive` package; sign in with
[`googledrive::drive_auth()`](https://googledrive.tidyverse.org/reference/drive_auth.html).

## Usage

``` r
write_gsheet(data, path, ...)
```

## Arguments

- data:

  A `tsg` table or a list of them, as for
  [`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md).

- path:

  Where to write. A name or Drive path (e.g. `"reports/Table 1"`)
  creates a new Google Sheet. An existing sheet, given as
  `googledrive::as_id("<file id>")` or a `dribble`, has its contents
  replaced in place, so its link stays the same.

- ...:

  Arguments passed to
  [`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md),
  e.g. `facade` or `include_table_list`. `separate_files = TRUE` is not
  supported. With `include_table_list = TRUE`, the links of the table
  list are rewritten after the upload so they open the right tab in
  Google Sheets.

## Value

Invisibly, the Google Sheet as a
[`googledrive::dribble`](https://googledrive.tidyverse.org/reference/dribble.html).

## Details

Signing in needs access to your Drive files: on Google's consent screen,
tick the box to see, edit, create and delete your Google Drive files (it
is unticked by default). Without it, writing fails with "(403)
Forbidden: Request had insufficient authentication scopes"; sign in
again with
`googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive", cache = FALSE)`
and tick the box.

## Examples

``` r
if (FALSE) { # \dontrun{
googledrive::drive_auth()
sheet <- write_gsheet(generate_frequency(person_record, sex), "Sex distribution")

# Replace the contents of the same sheet later; its link stays the same
write_gsheet(generate_frequency(person_record, marital_status), sheet)
} # }
```
