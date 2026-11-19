# Table metadata lives in attributes of a tsg table (or list of tables).
# This module is the only place that knows their names and shapes.

table_meta <- function(data) {
  a <- attributes(data)
  list(
    title = a$title,
    subtitle = a$subtitle,
    source_note = a$source_note,
    footnotes = .normalize_footnotes(a$footnotes),
    facade = a$facade,
    groups = a$groups
  )
}

set_table_meta <- function(data, metadata) {
  for (key in names(metadata)) {
    attr(data, key) <- metadata[[key]]
  }
  data
}

new_tsg <- function(data, subclass, groups = NULL, group_attrs = NULL, metadata = NULL) {
  if (length(groups) > 0) {
    attr(data, "groups") <- groups
    attr(data, "group_attrs") <- group_attrs
  }
  data <- set_table_meta(data, metadata)
  class(data) <- unique(c("tsg", subclass, class(data)))
  data
}

# Copy the attributes of `from` (table metadata, groups, ...) onto a rebuilt `to`,
# leaving the attributes in `except` that describe the object's own shape.
copy_table_attrs <- function(to, from, except = c("names", "row.names", "class")) {
  for (a in setdiff(names(attributes(from)), except)) {
    attr(to, a) <- attr(from, a)
  }
  to
}
