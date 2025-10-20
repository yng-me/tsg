utils::globalVariables(
  c(
    "n",
    ".",
    ":=",
    ".category",
    "category",
    "frequency"
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tsg <- list(
    tsg.options = list(

    ),
    tsg.options.facade = list(
      gridLines = FALSE,
      decimal = 3,
      lastRowBold = FALSE,
      heights = list(
        title = 24,
        subtitle = 22,
        header = 24,
        bottomHeader = 46,
        body = 20,
        sourceNote = 20,
        group = 30
      ),
      widths = list(
        offset = 2,
        first = 24,
        last = 14,
        all = 14
      ),
      styles = list(
        body = list(
          indent = 1,
          valign = 'center',
          # border = c('top', 'bottom', 'left', 'right'),
          # border = c('top', 'bottom'),
          border = c('left', 'right'),
          numFmt = '#,##0',
          borderStyle = 'dashed',
          borderColour = '#cfcfcf'
        ),
        title = list(
          fontSize = 13,
          textDecoration = "bold"
        ),
        subtitle = list(
          fontSize = 12,
          textDecoration = "bold",
          valign = 'center'
        ),
        footnote = list(
          fontSize = 10,
          textDecoration = "italic"
        ),
        source_note = list(
          fontSize = 10,
          textDecoration = "italic",
          valign = 'center'
        ),
        header = list(
          wrapText = TRUE,
          fgFill = '#f5f5f5',
          border = c('top', 'bottom', 'left', 'right'),
          # border = c('top', 'bottom'),
          # border = c('left', 'right'),
          borderStyle = 'dashed',
          borderColour = '#cfcfcf'
        ),
        indent = list(
          indent = 1,
          valign = 'center'
        ),
        border_outer = list(
          borderColour = '#8f8f8f'
        ),
        border_header = list(
          border = 'bottom',
          borderColour = '#9f9e9e',
          borderStyle = 'medium'
        )
      )
    )
  )

  to_set <- !(names(op.tsg) %in% names(op))
  if (any(to_set)) options(op.tsg[to_set])

  invisible()
}
