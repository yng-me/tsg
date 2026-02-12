utils::globalVariables(
  c(
    "n",
    ".",
    ":=",
    ".category",
    "category",
    "frequency",
    "title",
    "subtitle",
    "table_id",
    "table_number",
    "table_name"
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tsg <- list(
    tsg.options = list(
      facade = list(
        xlsx = "default",
        pdf = "default",
        html = "default"
      )
    )
  )

  to_set <- !(names(op.tsg) %in% names(op))
  if (any(to_set)) options(op.tsg[to_set])

  invisible()
}
