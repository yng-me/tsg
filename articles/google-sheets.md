# Writing tables to Google Sheets

``` r

library(tsg)
```

[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
writes tsg tables to a Google Sheet. It takes the same input and styling
as
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md):
titles, merged headers, footnotes and facades all carry over.

## How it works

[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
writes the tables with
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
to a temporary `.xlsx` file and uploads that file to Google Drive, which
converts it to a Google Sheet. Anything
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
can produce,
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
can too, and the styling is whatever Google’s conversion keeps. In
practice that covers merged cells, borders, fonts, fills and number
formats.

## Setup

[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
talks to Google Drive through the `googledrive` package, which is not
installed with tsg:

``` r

install.packages("googledrive")
```

Sign in once per session:

``` r

googledrive::drive_auth()
```

A browser window opens for you to choose a Google account. **On the
consent screen, tick the box that lets the app “See, edit, create and
delete all of your Google Drive files”.** Google leaves it unticked by
default, and without it every write fails (see
[Troubleshooting](#troubleshooting)).

To skip the browser in later sessions, let googledrive reuse the cached
sign-in for your account:

``` r

googledrive::drive_auth(email = "you@example.com")
```

## Create a sheet

Give a name, and
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
creates a new Google Sheet in your Drive:

``` r

sheet <- person_record |>
  generate_frequency(sex) |>
  add_table_title("Population by sex") |>
  add_source_note("Source: person_record") |>
  write_gsheet("Population by sex")
```

It returns the new file, invisibly, as a `dribble` (googledrive’s file
object). Keep it to open, share or update the sheet later:

``` r

googledrive::drive_browse(sheet) # open it in the browser
googledrive::drive_link(sheet)   # its URL
```

To create the sheet inside a folder, give a path. The folder must
already exist:

``` r

googledrive::drive_mkdir("reports")

person_record |>
  generate_frequency(sex) |>
  write_gsheet("reports/Population by sex")
```

Each call with a name creates a new file, even if a sheet with that name
already exists. Google Drive allows several files with the same name.

## Update a sheet in place

Pass an existing sheet instead of a name, and
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
replaces its contents. It stays the same file, so its link, sharing
settings and location in Drive don’t change. This suits a report others
have bookmarked and that you refresh with new data.

``` r

person_record |>
  generate_frequency(marital_status) |>
  write_gsheet(sheet)
```

The existing sheet can be:

- the `dribble` an earlier
  [`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
  call returned,
- its file ID, wrapped in
  [`googledrive::as_id()`](https://googledrive.tidyverse.org/reference/drive_id.html).
  The ID is the long string in the sheet’s URL,
  `https://docs.google.com/spreadsheets/d/<file id>/edit`:

``` r

person_record |>
  generate_frequency(marital_status) |>
  write_gsheet(googledrive::as_id("1AbC...xyz"))
```

A plain string is always treated as a name for a **new** sheet. Wrap an
ID in
[`googledrive::as_id()`](https://googledrive.tidyverse.org/reference/drive_id.html)
to update.

## Styling

Pass a facade as you would to
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md),
e.g. a built-in one by name. See
[`vignette("facade")`](https://yng-me.github.io/tsg/articles/facade.md)
for the full list:

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  write_gsheet("Marital status by sex", facade = "striped")
```

Settings added to a table with
[`add_facade()`](https://yng-me.github.io/tsg/reference/add_facade.md)
apply too, as do all other
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md)
arguments, such as `title`, `subtitle`, `source_note`, `footnotes` and
`include_table_list`:

``` r

person_record |>
  generate_frequency(sex) |>
  add_facade(table.offsetRow = 1, table.offsetCol = 1) |>
  write_gsheet(
    "Population by sex",
    title = "Population by sex",
    footnotes = "Excludes persons with no reported sex."
  )
```

## Several tables in one spreadsheet

A list of tables becomes one spreadsheet with one tab per table, named
after the list items:

``` r

person_record |>
  generate_frequency(sex, marital_status, employed) |>
  write_gsheet("Population profile")
```

The same goes for tables generated per group with
`group_as_list = TRUE`:

``` r

person_record |>
  dplyr::group_by(sex) |>
  generate_summary(age, group_as_list = TRUE) |>
  write_gsheet("Age by sex")
```

Add `include_table_list = TRUE` for a first tab listing the tables. Its
links, and the “Back” link on each tab, open the right tab in Google
Sheets:
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
rewrites them after the upload, because Google Sheets does not follow
Excel’s links between sheets.

``` r

person_record |>
  generate_frequency(sex, marital_status, employed) |>
  write_gsheet("Population profile", include_table_list = TRUE)
```

`separate_files = TRUE`, which writes each table to its own xlsx file,
is not supported. Call
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
once per table instead.

## With `generate_output()`

[`generate_output()`](https://yng-me.github.io/tsg/reference/generate_output.md)
writes a Google Sheet with `format = "gsheet"`. Its `path` is passed to
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
unchanged, so it can be a name or an existing sheet:

``` r

person_record |>
  generate_crosstab(marital_status, sex) |>
  generate_output("Marital status by sex", format = "gsheet")
```

## Sharing

New sheets are private to the account that created them. Share with
googledrive:

``` r

# anyone with the link can view
googledrive::drive_share_anyone(sheet)

# a specific person can edit
googledrive::drive_share(sheet, role = "writer", type = "user", emailAddress = "colleague@example.com")
```

Updating a sheet in place with
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md)
keeps its sharing settings.

## Scheduled scripts

A script run by a scheduler (cron, CI) has no browser to sign in with.
Use a Google Cloud service account and its JSON key instead:

``` r

googledrive::drive_auth(path = "service-account-key.json")
```

Files a service account creates belong to the service account, not to
you. To write into your own Drive, share a folder with the service
account’s email address and create sheets in that folder, or share an
existing sheet with it and update the sheet in place.

## Troubleshooting

**“(403) Forbidden: Request had insufficient authentication scopes”**:
you signed in without ticking the Drive box on Google’s consent screen.
Clear the token and sign in again, asking for Drive access explicitly,
and tick the box:

``` r

googledrive::drive_deauth()
googledrive::drive_auth(
  scopes = "https://www.googleapis.com/auth/drive",
  cache = FALSE
)
```

**“(401) Unauthorized: Request had invalid authentication
credentials”**: no valid token is in use, either because you haven’t
signed in this session or because the cached token has expired or been
revoked. Check, then sign in again:

``` r

googledrive::drive_has_token() # FALSE: run googledrive::drive_auth()
googledrive::drive_user()      # the signed-in account
gargle::gargle_oauth_sitrep()  # tokens cached on disk
```

To test the sign-in apart from tsg, list one file. If this fails, the
problem is the sign-in, not
[`write_gsheet()`](https://yng-me.github.io/tsg/reference/write_gsheet.md):

``` r

googledrive::drive_find(n_max = 1)
```

**A styling detail is missing in the sheet**: the xlsx file is converted
by Google, and a few Excel features have no Google Sheets equivalent.
Check the same output with
[`write_xlsx()`](https://yng-me.github.io/tsg/reference/write_xlsx.md);
if it looks right there, the difference comes from Google’s conversion,
not from tsg.
