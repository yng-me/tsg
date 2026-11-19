# Builds the Excel screenshots of the vignettes with Microsoft Excel (macOS).
#
# For each vignette it runs the vignette's code (so the pictures always match the code
# shown), has Excel export the sheets below to PDF, and rasterises the PDF to
# man/figures/<vignette>-<name>.png. Run from the package root:
#   Rscript data-raw/vignette-screenshots.R                 # every vignette below
#   Rscript data-raw/vignette-screenshots.R facade advanced # only these
# Needs Microsoft Excel, Xcode command line tools (swift) and the magick package.

devtools::load_all(quiet = TRUE)

# vignette = list(c(name of the figure, workbook, sheet), ...)
screenshots <- list(
  "write-xlsx" = list(
    c("frequency", "frequency.xlsx", 1),
    c("crosstab", "crosstab.xlsx", 1),
    c("crosstab-multi", "crosstab-multi.xlsx", 1),
    c("summary", "summary.xlsx", 1),
    c("list-sheet", "tables-titled.xlsx", 1),
    c("stacked", "stacked.xlsx", 1),
    c("group-list", "group-list.xlsx", 1),
    c("hierarchy", "hierarchy.xlsx", 1),
    c("nested", "nested.xlsx", 1),
    c("group-one-sheet", "group-one-sheet.xlsx", 1),
    c("group-column", "group-column.xlsx", 1),
    c("nested-column", "nested-column.xlsx", 1),
    c("toc", "indexed.xlsx", 1),
    c("reference-index", "reference.xlsx", 1),
    c("reference-table", "reference.xlsx", 2),
    c("facade-booktabs", "booktabs.xlsx", 1),
    c("facade-financial", "financial.xlsx", 1),
    c("facade-dark", "dark.xlsx", 1),
    c("facade-offset", "offset.xlsx", 1)
  ),
  "advanced" = list(
    c("group-list", "group-list.xlsx", 1),
    c("nested", "nested.xlsx", 1),
    c("crosstab-list", "crosstab-list.xlsx", 1),
    c("hierarchy-flat", "hierarchy-flat.xlsx", 1),
    c("hierarchy-list", "hierarchy-list.xlsx", 1),
    c("hierarchy-nested", "hierarchy-nested.xlsx", 1),
    c("multiple", "multiple.xlsx", 1),
    c("multiple-grouped", "multiple-grouped.xlsx", 1),
    c("multiple-list", "multiple-list.xlsx", 1),
    c("stacked", "stacked.xlsx", 1),
    c("stacked-deep", "stacked-deep.xlsx", 1),
    c("stacked-grouped", "stacked-grouped.xlsx", 1),
    c("report-toc", "report.xlsx", 1),
    c("report-difficulties", "report.xlsx", 4),
    c("footnotes", "footnotes.xlsx", 1)
  ),
  "facade" = c(
    list(c("tweaks", "styled-table.xlsx", 1)),
    lapply(
      c("default", "yolo", "apa", "booktabs", "striped", "gridded", "minimal",
        "financial", "nyt", "jama", "dark", "pastel", "census"),
      function(f) c(f, paste0("facade-", f, ".xlsx"), 1)
    )
  )
)

only <- commandArgs(trailingOnly = TRUE)
if (length(only) > 0) screenshots <- screenshots[only]

root <- getwd()
# Excel may only touch files inside its sandbox container
excel_dir <- path.expand("~/Library/Containers/com.microsoft.Excel/Data/tsg-shots")
print_scale <- 50

for (vignette in names(screenshots)) {

  shots <- screenshots[[vignette]]
  out_dir <- file.path(tempdir(), paste0(vignette, "-shots"))
  dir.create(out_dir, showWarnings = FALSE)
  Sys.setenv(TSG_VIGNETTE_OUT = out_dir)

  code <- tempfile(fileext = ".R")
  knitr::purl(file.path("vignettes", paste0(vignette, ".Rmd")), code, documentation = 0, quiet = TRUE)
  setwd(out_dir)
  source(code, echo = FALSE, local = new.env())
  setwd(root)

  unlink(excel_dir, recursive = TRUE)
  dir.create(excel_dir)

  # One copy per shot holding only that sheet
  args <- character()
  for (s in shots) {
    wb <- openxlsx::loadWorkbook(file.path(out_dir, s[2]))
    keep <- openxlsx::sheets(wb)[as.integer(s[3])]
    for (other in setdiff(openxlsx::sheets(wb), keep)) openxlsx::removeWorksheet(wb, other)
    # Excel on macOS ignores paper size and fit-to-page here, but honours a print scale:
    # at 50% the widest sheet fits on one landscape page, rasterised at 4x below.
    openxlsx::pageSetup(
      wb, 1, orientation = "landscape", scale = print_scale,
      left = 0.3, right = 0.3, top = 0.3, bottom = 0.3
    )
    xlsx <- file.path(excel_dir, paste0(s[1], ".xlsx"))
    openxlsx::saveWorkbook(wb, xlsx, overwrite = TRUE)
    args <- c(args, xlsx, file.path(excel_dir, paste0(s[1], ".pdf")))
  }
  system2("osascript", c("data-raw/xlsx2pdf.scpt", shQuote(args)))

  for (s in shots) {
    png <- file.path(root, "man/figures", paste0(vignette, "-", s[1], ".png"))
    system2("swift", c("data-raw/pdf2png.swift", shQuote(file.path(excel_dir, paste0(s[1], ".pdf"))), shQuote(png), 2 * 100 / print_scale))
    img <- magick::image_read(png)
    img <- magick::image_border(magick::image_trim(img), "white", "16x16")
    # 64 colours keep the file small; the tables only use a handful
    magick::image_write(magick::image_quantize(img, 64, dither = FALSE), png)
  }
}

unlink(excel_dir, recursive = TRUE)
