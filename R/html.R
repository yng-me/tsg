
#' Convert a tsg table to a gt object
#'
#' @param data A \code{tsg} or data frame object.
#' @param title Optional title string.
#' @param subtitle Optional subtitle string.
#' @param source_note Optional source note string.
#' @param footnotes Optional character vector of footnotes.
#' @param names_separator Column name separator for spanners. Default \code{"__"}.
#' @param facade A facade list for styling. Defaults to the global tsg facade.
#'
#' @return A \code{gt_tbl} object.
#' @keywords internal

tsg_to_gt <- function(
  data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
) {

  attrs <- attributes(data)
  title       <- title       %||% attrs$title
  subtitle    <- subtitle    %||% attrs$subtitle
  source_note <- source_note %||% attrs$source_note
  footnotes   <- .normalize_footnotes(footnotes %||% attrs$footnotes)

  facade <- resolve_facade(facade %||% get_tsg_facade(which = "html"), attrs$facade, which = "html")

  col_names <- names(data)

  # Extract display labels from original data BEFORE convert_factor() since
  # haven::as_factor() may drop label_xlsx attributes
  col_labels <- vapply(col_names, function(cn) {
    attr(data[[cn]], "label_xlsx") %||% attr(data[[cn]], "label") %||% cn
  }, character(1))

  # Convert haven_labelled columns to factor so gt renders value labels, not codes
  # (mirrors the explicit convert_factor() call in xlsx_writer.R)
  data_for_gt <- convert_factor(dplyr::ungroup(data))

  gt_tbl <- gt::gt(data_for_gt)

  # Auto-detect separator from stored label_separator attribute; fall back to parameter
  sep <- attrs$label_separator %||% names_separator

  gt_tbl <- gt_apply_spanners(gt_tbl, col_names, sep, col_labels = col_labels)

  if (!is.null(title) || !is.null(subtitle)) {
    gt_tbl <- gt::tab_header(gt_tbl, title = title %||% "", subtitle = subtitle)
  }

  if (!is.null(source_note)) {
    gt_tbl <- gt::tab_source_note(gt_tbl, source_note = source_note)
  }

  if (!is.null(footnotes)) {
    for (i in seq_along(footnotes$text)) {
      fn_text      <- footnotes$text[[i]]
      fn_placement <- footnotes$placement[[i]] %||% "auto"
      fn_locs      <- footnotes$locations[[i]]

      gt_locs <- if (!is.null(fn_locs) && length(fn_locs) > 0) {
        # Map column names to gt column-label locations; silently drop unknown names
        valid_locs <- intersect(fn_locs, col_names)
        if (length(valid_locs) > 0) gt::cells_column_labels(columns = dplyr::all_of(valid_locs)) else NULL
      } else {
        NULL
      }

      gt_tbl <- gt::tab_footnote(
        gt_tbl,
        footnote  = fn_text,
        locations = gt_locs,
        placement = fn_placement
      )
    }
  }

  gt_tbl <- gt_apply_numeric_format(gt_tbl, data_for_gt, facade)

  gt_tbl <- gt_apply_facade(gt_tbl, facade, col_names, n_body_rows = nrow(data_for_gt))

  gt_tbl
}


#' Apply spanner labels to a gt table based on separator-split column names
#'
#' @keywords internal

gt_apply_spanners <- function(gt_tbl, col_names, sep, col_labels = NULL) {

  # Fall back to col_names as labels when not provided (backward compatibility)
  if (is.null(col_labels)) col_labels <- col_names

  if (!any(grepl(sep, col_labels, fixed = TRUE))) {
    # No spanners needed; still relabel columns whose display label differs from name
    label_map <- list()
    for (i in seq_along(col_names)) {
      if (col_labels[i] != col_names[i]) {
        label_map[[col_names[i]]] <- col_labels[i]
      }
    }
    if (length(label_map) > 0) {
      gt_tbl <- do.call(gt::cols_label, c(list(gt_tbl), label_map))
    }
    return(gt_tbl)
  }

  spanner_groups <- list()
  leaf_label_map <- list()

  for (i in seq_along(col_names)) {
    cn  <- col_names[i]
    lbl <- col_labels[i]

    if (grepl(sep, lbl, fixed = TRUE)) {
      parts        <- strsplit(lbl, sep, fixed = TRUE)[[1]]
      leaf_label   <- parts[length(parts)]
      spanner_label <- paste(parts[-length(parts)], collapse = sep)
      spanner_groups[[spanner_label]] <- c(spanner_groups[[spanner_label]], cn)
      leaf_label_map[[cn]] <- leaf_label
    } else {
      # No spanner for this column; relabel if display label differs from col name
      if (lbl != cn) {
        leaf_label_map[[cn]] <- lbl
      }
    }
  }

  for (spanner_label in names(spanner_groups)) {
    cols <- spanner_groups[[spanner_label]]
    gt_tbl <- gt::tab_spanner(
      gt_tbl,
      label = spanner_label,
      columns = dplyr::all_of(cols)
    )
  }

  if (length(leaf_label_map) > 0) {
    gt_tbl <- do.call(gt::cols_label, c(list(gt_tbl), leaf_label_map))
  }

  gt_tbl
}


#' Apply numeric cell formatting to a gt table based on facade settings
#'
#' Mirrors \code{xlsx_decimal_format()} for gt output. Integer-like columns
#' (R \code{integer} type or double columns whose values are all whole numbers)
#' are formatted with comma separators and no decimal places.
#' Double columns with fractional values are formatted with
#' \code{table.decimalPrecision} decimal places (default 2).
#' \code{table.decimalCols}, \code{body.numFmt}, \code{col_first.numFmt}, and
#' \code{col_last.numFmt} override the auto-detection.
#'
#' @keywords internal

gt_apply_numeric_format <- function(gt_tbl, data, facade) {

  if (is.null(facade)) return(gt_tbl)

  precision <- facade$table.decimalPrecision
  if (is.null(precision) || !is.numeric(precision)) precision <- 2L
  precision <- as.integer(precision)

  explicit_decimal_cols <- facade$table.decimalCols

  # Identify integer-like columns
  int_names  <- names(dplyr::select(data, dplyr::where(is.integer)))
  dbl_names  <- names(dplyr::select(data, dplyr::where(is.double)))

  maybe_int <- dbl_names[vapply(dbl_names, function(col) {
    vals <- data[[col]]
    !all(is.na(vals)) &&
      isTRUE(all.equal(sum(vals, na.rm = TRUE), sum(as.integer(vals), na.rm = TRUE)))
  }, logical(1))]

  int_cols <- c(int_names, maybe_int)
  dec_cols <- setdiff(dbl_names, maybe_int)

  # table.decimalCols forces those columns into decimal (even if whole-number)
  if (!is.null(explicit_decimal_cols)) {
    int_cols <- setdiff(int_cols, explicit_decimal_cols)
    dec_cols <- union(dec_cols, intersect(explicit_decimal_cols, names(data)))
  }

  all_num_cols <- c(int_cols, dec_cols)

  # body.numFmt is the BASE format for all numeric columns. It is applied first
  # so that the auto-detected per-column formats (below) take precedence — mirroring
  # how xlsx_decimal_format() overrides body.numFmt in the XLSX pipeline.
  if (!is.null(facade$body.numFmt) && length(all_num_cols) > 0) {
    body_prec <- parse_excel_numfmt_decimals(facade$body.numFmt)
    if (!is.na(body_prec)) {
      if (body_prec == 0L) {
        gt_tbl <- gt::fmt_integer(gt_tbl, columns = dplyr::all_of(all_num_cols), use_seps = TRUE)
      } else {
        gt_tbl <- gt::fmt_number(gt_tbl, columns = dplyr::all_of(all_num_cols), decimals = body_prec, use_seps = TRUE)
      }
    }
  }

  # Auto-detected formats override body.numFmt (higher priority)
  if (length(int_cols) > 0) {
    gt_tbl <- gt::fmt_integer(gt_tbl, columns = dplyr::all_of(int_cols), use_seps = TRUE)
  }
  if (length(dec_cols) > 0) {
    gt_tbl <- gt::fmt_number(gt_tbl, columns = dplyr::all_of(dec_cols), decimals = precision, use_seps = TRUE)
  }

  # col_first.numFmt / col_last.numFmt are the highest-priority overrides.
  # Only applied when the target column is still numeric (e.g., not converted to factor).
  col_names <- names(data)
  if (!is.null(facade$col_first.numFmt) && length(col_names) > 0) {
    first_col_name <- col_names[[1L]]
    if (first_col_name %in% all_num_cols) {
      first_prec <- parse_excel_numfmt_decimals(facade$col_first.numFmt)
      if (!is.na(first_prec)) {
        if (first_prec == 0L) {
          gt_tbl <- gt::fmt_integer(gt_tbl, columns = dplyr::all_of(first_col_name), use_seps = TRUE)
        } else {
          gt_tbl <- gt::fmt_number(gt_tbl, columns = dplyr::all_of(first_col_name), decimals = first_prec, use_seps = TRUE)
        }
      }
    }
  }

  if (!is.null(facade$col_last.numFmt) && length(col_names) > 0) {
    last_col_name <- col_names[[length(col_names)]]
    if (last_col_name %in% all_num_cols) {
      last_prec <- parse_excel_numfmt_decimals(facade$col_last.numFmt)
      if (!is.na(last_prec)) {
        if (last_prec == 0L) {
          gt_tbl <- gt::fmt_integer(gt_tbl, columns = dplyr::all_of(last_col_name), use_seps = TRUE)
        } else {
          gt_tbl <- gt::fmt_number(gt_tbl, columns = dplyr::all_of(last_col_name), decimals = last_prec, use_seps = TRUE)
        }
      }
    }
  }

  gt_tbl
}


#' Parse decimal places from an Excel number format string
#'
#' Returns the number of decimal places encoded in the format string, or
#' \code{NA_integer_} if the string is not a recognised numeric format.
#'
#' @param numfmt A single character string (e.g. \code{"#,##0.00"}).
#' @return An \code{integer} scalar, or \code{NA_integer_}.
#' @keywords internal

parse_excel_numfmt_decimals <- function(numfmt) {
  if (is.null(numfmt) || length(numfmt) != 1L || !nzchar(numfmt)) return(NA_integer_)
  # Integer-like patterns: no decimal point
  if (grepl("^[#0,]+$", numfmt)) return(0L)
  # Match decimal portion after "."
  m <- regmatches(numfmt, regexpr("\\.(0+)", numfmt))
  if (length(m) == 0L) return(NA_integer_)
  as.integer(nchar(sub(".", "", m[[1L]], fixed = TRUE)))
}




# Map tsg/XLSX valign values ("center") to gt v_align values ("middle")
.gt_valign <- function(valign) {
  if (is.null(valign)) return(NULL)
  if (valign == "center") "middle" else valign
}

gt_apply_facade <- function(gt_tbl, facade, col_names = NULL, n_body_rows = NULL) {

  if (is.null(facade)) return(gt_tbl)
  font_opts <- list()
  if (!is.null(facade$table.fontName)) {
    font_opts[["table.font.names"]] <- facade$table.fontName
  }
  if (!is.null(facade$table.fontSize)) {
    font_opts[["table.font.size"]] <- gt::px(round(facade$table.fontSize * 1.33))
  }
  if (!is.null(facade$table.fontColour)) {
    font_opts[["table.font.color"]] <- facade$table.fontColour
  }
  if (!is.null(facade$table.bgFill %||% facade$table.fgFill)) {
    font_opts[["table.background.color"]] <- facade$table.fgFill %||% facade$table.bgFill
  }
  if (length(font_opts) > 0) {
    gt_tbl <- do.call(gt::tab_options, c(list(gt_tbl), font_opts))
  }

  # Table-level alignment
  if (!is.null(facade$table.halign) && !is.null(col_names) && length(col_names) > 0) {
    gt_tbl <- gt::cols_align(gt_tbl, align = facade$table.halign)
  }

  # Title styling
  if (!is.null(facade$title.fontSize) || !is.null(facade$title.fontColour) ||
      !is.null(facade$title.textDecoration) || !is.null(facade$title.fgFill) ||
      !is.null(facade$title.bgFill) || !is.null(facade$title.fontName)) {
    title_styles <- list()
    if (!is.null(facade$title.fontName)) title_styles[["font.names"]] <- facade$title.fontName
    if (!is.null(facade$title.fontSize)) title_styles[["size"]] <- gt::px(round(facade$title.fontSize * 1.33))
    if (!is.null(facade$title.fontColour)) title_styles[["color"]] <- facade$title.fontColour
    if (length(title_styles) > 0) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = do.call(gt::cell_text, title_styles),
        locations = gt::cells_title("title")
      )
    }
    if (!is.null(facade$title.textDecoration)) {
      gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$title.textDecoration, gt::cells_title("title"))
    }
    title_fill <- facade$title.fgFill %||% facade$title.bgFill
    if (!is.null(title_fill)) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = gt::cell_fill(color = title_fill),
        locations = gt::cells_title("title")
      )
    }
  }

  # Subtitle styling
  if (!is.null(facade$subtitle.fontSize) || !is.null(facade$subtitle.fontColour) ||
      !is.null(facade$subtitle.textDecoration) || !is.null(facade$subtitle.fgFill) ||
      !is.null(facade$subtitle.bgFill) || !is.null(facade$subtitle.fontName)) {
    sub_styles <- list()
    if (!is.null(facade$subtitle.fontName)) sub_styles[["font.names"]] <- facade$subtitle.fontName
    if (!is.null(facade$subtitle.fontSize)) sub_styles[["size"]] <- gt::px(round(facade$subtitle.fontSize * 1.33))
    if (!is.null(facade$subtitle.fontColour)) sub_styles[["color"]] <- facade$subtitle.fontColour
    if (length(sub_styles) > 0) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = do.call(gt::cell_text, sub_styles),
        locations = gt::cells_title("subtitle")
      )
    }
    if (!is.null(facade$subtitle.textDecoration)) {
      gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$subtitle.textDecoration, gt::cells_title("subtitle"))
    }
    sub_fill <- facade$subtitle.fgFill %||% facade$subtitle.bgFill
    if (!is.null(sub_fill)) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = gt::cell_fill(color = sub_fill),
        locations = gt::cells_title("subtitle")
      )
    }
  }

  # Header background (applies to both column labels and spanners)
  if (!is.null(facade$header.fgFill)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = facade$header.fgFill),
      locations = list(gt::cells_column_labels(), gt::cells_column_spanners())
    )
  }

  # Header text color
  if (!is.null(facade$header.fontColour)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(color = facade$header.fontColour),
      locations = list(gt::cells_column_labels(), gt::cells_column_spanners())
    )
  }

  # Header font name / size
  header_text_extra <- list()
  if (!is.null(facade$header.fontName)) header_text_extra[["font.names"]] <- facade$header.fontName
  if (!is.null(facade$header.fontSize)) header_text_extra[["size"]] <- gt::px(round(facade$header.fontSize * 1.33))
  if (!is.null(facade$header.halign))   header_text_extra[["align"]] <- facade$header.halign
  if (!is.null(facade$header.valign))   header_text_extra[["v_align"]] <- .gt_valign(facade$header.valign)
  if (length(header_text_extra) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, header_text_extra),
      locations = list(gt::cells_column_labels(), gt::cells_column_spanners())
    )
  }

  # Header text decoration
  if (!is.null(facade$header.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(
      gt_tbl,
      facade$header.textDecoration,
      list(gt::cells_column_labels(), gt::cells_column_spanners())
    )
  }

  # Spanner-specific overrides (applied after header, so they win)
  spanner_overrides <- list()
  if (!is.null(facade$spanner.fontName)) spanner_overrides[["font.names"]] <- facade$spanner.fontName
  if (!is.null(facade$spanner.fontSize)) spanner_overrides[["size"]] <- gt::px(round(facade$spanner.fontSize * 1.33))
  if (!is.null(facade$spanner.halign))   spanner_overrides[["align"]] <- facade$spanner.halign
  if (!is.null(facade$spanner.valign))   spanner_overrides[["v_align"]] <- .gt_valign(facade$spanner.valign)
  if (!is.null(facade$spanner.fontColour)) spanner_overrides[["color"]] <- facade$spanner.fontColour
  if (length(spanner_overrides) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, spanner_overrides),
      locations = gt::cells_column_spanners()
    )
  }
  if (!is.null(facade$spanner.fgFill)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = facade$spanner.fgFill),
      locations = gt::cells_column_spanners()
    )
  }
  if (!is.null(facade$spanner.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(
      gt_tbl,
      facade$spanner.textDecoration,
      gt::cells_column_spanners()
    )
  }

  # Body background
  if (!is.null(facade$body.fgFill)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = facade$body.fgFill),
      locations = gt::cells_body()
    )
  }

  # Body text color
  if (!is.null(facade$body.fontColour)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(color = facade$body.fontColour),
      locations = gt::cells_body()
    )
  }

  # Body font name / size / valign
  body_text_extra <- list()
  if (!is.null(facade$body.fontName)) body_text_extra[["font.names"]] <- facade$body.fontName
  if (!is.null(facade$body.fontSize)) body_text_extra[["size"]] <- gt::px(round(facade$body.fontSize * 1.33))
  if (!is.null(facade$body.valign))   body_text_extra[["v_align"]] <- .gt_valign(facade$body.valign)
  if (length(body_text_extra) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, body_text_extra),
      locations = gt::cells_body()
    )
  }

  # Body text decoration
  if (!is.null(facade$body.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(
      gt_tbl,
      facade$body.textDecoration,
      gt::cells_body()
    )
  }

  # Body alignment
  if (!is.null(facade$body.halign)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(align = facade$body.halign),
      locations = gt::cells_body()
    )
  }

  # Row group styling
  row_group_styles <- list()
  if (!is.null(facade$row_group.fontName))  row_group_styles[["font.names"]] <- facade$row_group.fontName
  if (!is.null(facade$row_group.fontSize))  row_group_styles[["size"]] <- gt::px(round(facade$row_group.fontSize * 1.33))
  if (!is.null(facade$row_group.fontColour)) row_group_styles[["color"]] <- facade$row_group.fontColour
  if (!is.null(facade$row_group.halign))    row_group_styles[["align"]] <- facade$row_group.halign
  if (!is.null(facade$row_group.valign))    row_group_styles[["v_align"]] <- .gt_valign(facade$row_group.valign)
  if (length(row_group_styles) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, row_group_styles),
      locations = gt::cells_row_groups()
    )
  }
  if (!is.null(facade$row_group.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$row_group.textDecoration, gt::cells_row_groups())
  }
  rg_fill <- facade$row_group.fgFill %||% facade$row_group.bgFill
  if (!is.null(rg_fill)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = rg_fill),
      locations = gt::cells_row_groups()
    )
  }

  # Source note styling
  sn_tab_opts <- list()
  if (!is.null(facade$source_note.fontSize)) {
    sn_tab_opts[["source_notes.font.size"]] <- gt::px(round(facade$source_note.fontSize * 1.33))
  }
  if (length(sn_tab_opts) > 0) {
    gt_tbl <- do.call(gt::tab_options, c(list(gt_tbl), sn_tab_opts))
  }
  sn_styles <- list()
  if (!is.null(facade$source_note.fontName))  sn_styles[["font.names"]] <- facade$source_note.fontName
  if (!is.null(facade$source_note.fontColour)) sn_styles[["color"]] <- facade$source_note.fontColour
  if (!is.null(facade$source_note.halign))    sn_styles[["align"]] <- facade$source_note.halign
  if (length(sn_styles) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, sn_styles),
      locations = gt::cells_source_notes()
    )
  }
  if (!is.null(facade$source_note.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$source_note.textDecoration, gt::cells_source_notes())
  }
  sn_fill <- facade$source_note.fgFill %||% facade$source_note.bgFill
  if (!is.null(sn_fill)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = sn_fill),
      locations = gt::cells_source_notes()
    )
  }

  # Footnote styling
  fn_tab_opts <- list()
  if (!is.null(facade$footnotes.fontSize)) {
    fn_tab_opts[["footnotes.font.size"]] <- gt::px(round(facade$footnotes.fontSize * 1.33))
  }
  if (length(fn_tab_opts) > 0) {
    gt_tbl <- do.call(gt::tab_options, c(list(gt_tbl), fn_tab_opts))
  }
  fn_styles <- list()
  if (!is.null(facade$footnotes.fontName))  fn_styles[["font.names"]] <- facade$footnotes.fontName
  if (!is.null(facade$footnotes.fontColour)) fn_styles[["color"]] <- facade$footnotes.fontColour
  if (length(fn_styles) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, fn_styles),
      locations = gt::cells_footnotes()
    )
  }
  if (!is.null(facade$footnotes.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$footnotes.textDecoration, gt::cells_footnotes())
  }

  # Outer border (table outline)
  border_outer_colour <- facade$border_outer.borderColour
  if (!is.null(border_outer_colour)) {
    gt_tbl <- do.call(gt::tab_options, list(
      gt_tbl,
      table.border.top.color    = border_outer_colour,
      table.border.bottom.color = border_outer_colour
    ))
  }

  # Header-body border
  bh_col   <- facade$border_header.borderColour
  bh_style <- facade$border_header.borderStyle %||% "solid"
  if (!is.null(bh_col)) {
    gt_tbl <- do.call(gt::tab_options, list(
      gt_tbl,
      column_labels.border.bottom.color = bh_col,
      column_labels.border.bottom.style = bh_style
    ))
  }

  # Last-row bold
  if (isTRUE(facade$table.lastRowBold) && !is.null(n_body_rows) && n_body_rows > 0L) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = n_body_rows)
    )
  }

  # First column alignment: default "left" (matches Excel behavior for label columns).
  # Applied AFTER body.halign so it takes precedence. facade$col_first.halign overrides.
  first_align <- facade$col_first.halign %||% "left"
  first_col_sel <- if (!is.null(col_names) && length(col_names) > 0) col_names[[1L]] else 1L
  gt_tbl <- gt::cols_align(gt_tbl, align = first_align, columns = first_col_sel)

  # First column additional styles
  col_first_styles <- list()
  if (!is.null(facade$col_first.fontName))  col_first_styles[["font.names"]] <- facade$col_first.fontName
  if (!is.null(facade$col_first.fontSize))  col_first_styles[["size"]] <- gt::px(round(facade$col_first.fontSize * 1.33))
  if (!is.null(facade$col_first.fontColour)) col_first_styles[["color"]] <- facade$col_first.fontColour
  if (length(col_first_styles) > 0) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, col_first_styles),
      locations = gt::cells_body(columns = first_col_sel)
    )
  }
  if (!is.null(facade$col_first.textDecoration)) {
    gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$col_first.textDecoration,
      gt::cells_body(columns = first_col_sel))
  }
  col_first_fill <- facade$col_first.fgFill %||% facade$col_first.bgFill
  if (!is.null(col_first_fill)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = col_first_fill),
      locations = gt::cells_body(columns = first_col_sel)
    )
  }

  # Last column alignment (only when explicitly set in facade)
  last_col_sel <- if (!is.null(col_names) && length(col_names) > 0) {
    col_names[[length(col_names)]]
  } else {
    NULL
  }
  if (!is.null(last_col_sel) && !is.null(facade$col_last.halign)) {
    gt_tbl <- gt::cols_align(gt_tbl, align = facade$col_last.halign, columns = last_col_sel)
  }

  # Last column additional styles
  col_last_styles <- list()
  if (!is.null(facade$col_last.fontName))  col_last_styles[["font.names"]] <- facade$col_last.fontName
  if (!is.null(facade$col_last.fontSize))  col_last_styles[["size"]] <- gt::px(round(facade$col_last.fontSize * 1.33))
  if (!is.null(facade$col_last.fontColour)) col_last_styles[["color"]] <- facade$col_last.fontColour
  if (length(col_last_styles) > 0 && !is.null(last_col_sel)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = do.call(gt::cell_text, col_last_styles),
      locations = gt::cells_body(columns = last_col_sel)
    )
  }
  if (!is.null(facade$col_last.textDecoration) && !is.null(last_col_sel)) {
    gt_tbl <- gt_apply_text_decoration(gt_tbl, facade$col_last.textDecoration,
      gt::cells_body(columns = last_col_sel))
  }
  col_last_fill <- facade$col_last.fgFill %||% facade$col_last.bgFill
  if (!is.null(col_last_fill) && !is.null(last_col_sel)) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_fill(color = col_last_fill),
      locations = gt::cells_body(columns = last_col_sel)
    )
  }

  gt_tbl
}


#' Apply text decoration (bold/italic/underline) to gt locations
#'
#' @keywords internal

gt_apply_text_decoration <- function(gt_tbl, decoration, locations) {
  decs <- tolower(as.character(decoration))
  if ("bold" %in% decs) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(weight = "bold"),
      locations = locations
    )
  }
  if ("italic" %in% decs) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(style = "italic"),
      locations = locations
    )
  }
  if ("underline" %in% decs) {
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(decorate = "underline"),
      locations = locations
    )
  }
  gt_tbl
}


#' Write a tsg table (or list of tables) to an HTML file
#'
#' Saves one or more \code{tsg} tables as an HTML file using the \pkg{gt} package
#' (which is already a hard dependency of \pkg{tsg}).
#'
#' When \code{data} is a named list and \code{separate_files = FALSE} (default), all
#' tables are written into a single self-contained HTML document. Set
#' \code{include_table_list = TRUE} to prepend a clickable table-of-contents. Set
#' \code{separate_files = TRUE} to write one HTML file per table into a subdirectory.
#'
#' @param data A \code{tsg} or data frame, or a named list of them.
#' @param path File path for the HTML output. A \code{.html} extension is added if missing.
#'   When \code{separate_files = TRUE} the path (minus extension) is used as the directory.
#' @param ... Additional arguments passed to \code{tsg_to_gt()}.
#' @param title Optional title string (overrides data attribute).
#' @param subtitle Optional subtitle string.
#' @param source_note Optional source note string.
#' @param footnotes Optional character vector of footnotes.
#' @param separate_files Logical. When \code{TRUE} and \code{data} is a list, each table
#'   is saved to its own HTML file inside a subdirectory derived from \code{path}.
#' @param include_table_list Logical. When \code{TRUE} and \code{data} is a named list
#'   with \code{separate_files = FALSE}, prepends a clickable table of contents.
#' @param names_separator Column name separator for spanners. Default \code{"__"}.
#' @param facade Styling options. Defaults to the global tsg facade.
#'
#' @return Invisibly returns \code{NULL}.
#' @export

write_html <- function(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = FALSE,
  include_table_list = FALSE,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
) {

  facade <- facade %||% get_tsg_facade(which = "html")

  if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
    path <- paste0(path, ".html")
  }

  if (inherits(data, "list")) {

    if (separate_files) {

      path_dir <- sub("\\.html?$", "", path, ignore.case = TRUE)
      fs::dir_create(path_dir)

      for (i in seq_along(data)) {
        nm <- names(data)[i]
        path_i <- file.path(path_dir, paste0(nm, ".html"))
        data_i <- data[[i]]
        attrs_i <- attributes(data_i)

        title_i <- if (!is.null(title)) glue::glue("{title}: {nm}") else attrs_i$title
        gt_tbl <- tsg_to_gt(
          data_i,
          title = title_i,
          subtitle = subtitle %||% attrs_i$subtitle,
          source_note = source_note %||% attrs_i$source_note,
          footnotes = footnotes %||% attrs_i$footnotes,
          names_separator = names_separator,
          facade = facade
        )
        gt::gtsave(gt_tbl, filename = path_i)
      }

    } else {

      html_parts <- character(0)
      toc_items  <- character(0)

      for (i in seq_along(data)) {
        nm      <- names(data)[i] %||% as.character(i)
        anchor  <- paste0("tsg-table-", i)
        data_i  <- data[[i]]
        attrs_i <- attributes(data_i)

        title_i <- if (!is.null(title)) glue::glue("{title}: {nm}") else attrs_i$title
        display_label <- title_i %||% nm

        gt_tbl <- tsg_to_gt(
          data_i,
          title = title_i,
          subtitle = subtitle %||% attrs_i$subtitle,
          source_note = source_note %||% attrs_i$source_note,
          footnotes = footnotes %||% attrs_i$footnotes,
          names_separator = names_separator,
          facade = facade
        )

        section_html <- paste0(
          '<section id="', anchor, '" style="margin-bottom:3em">',
          gt::as_raw_html(gt_tbl),
          '</section>'
        )
        html_parts <- c(html_parts, section_html)
        toc_items  <- c(toc_items, paste0(
          '<li><a href="#', anchor, '">', .html_escape(display_label), '</a></li>'
        ))
      }

      toc_html <- ""
      if (include_table_list && length(toc_items) > 0) {
        toc_html <- paste0(
          '<nav style="margin-bottom:2.5em;padding:1em;border:1px solid #ddd;',
          'border-radius:4px;background:#fafafa">',
          '<strong style="display:block;margin-bottom:.5em">Tables</strong>',
          '<ol style="margin:0;padding-left:1.2em">', paste(toc_items, collapse = ""),
          '</ol></nav>'
        )
      }

      css <- paste0(
        '<style>',
        'body{font-family:Arial,sans-serif;max-width:960px;margin:2rem auto;padding:0 1rem}',
        'nav a{color:#2c7fb8}',
        '</style>'
      )

      html_combined <- paste0(
        '<!DOCTYPE html><html lang="en"><head>',
        '<meta charset="UTF-8">',
        '<meta name="viewport" content="width=device-width,initial-scale=1">',
        css,
        '</head><body>',
        toc_html,
        paste(html_parts, collapse = "\n"),
        '</body></html>'
      )
      writeLines(html_combined, path)
    }

  } else {

    attrs <- attributes(data)
    gt_tbl <- tsg_to_gt(
      data,
      title = title %||% attrs$title,
      subtitle = subtitle %||% attrs$subtitle,
      source_note = source_note %||% attrs$source_note,
      footnotes = footnotes %||% attrs$footnotes,
      names_separator = names_separator,
      facade = facade
    )
    gt::gtsave(gt_tbl, filename = path)
  }

  invisible(NULL)
}


#' Write a tsg table (or list of tables) to a PDF file
#'
#' Saves tables as PDF using \code{gt::gtsave()}, which requires the \pkg{webshot2}
#' package (and a Chromium installation reachable by \pkg{chromote}).
#'
#' When \code{data} is a named list, the default (\code{separate_files = TRUE}) writes
#' each table to its own \code{.pdf} file inside a directory. Set
#' \code{separate_files = FALSE} to merge all tables into a single PDF (requires the
#' \pkg{qpdf} package).
#'
#' @param data A \code{tsg} or data frame, or a named list of them.
#' @param path File path for the PDF output. A \code{.pdf} extension is added if missing.
#'   When \code{data} is a list and \code{separate_files = TRUE}, this is used as a directory.
#' @param ... Additional arguments passed to \code{tsg_to_gt()}.
#' @param title Optional title string (overrides data attribute).
#' @param subtitle Optional subtitle string.
#' @param source_note Optional source note string.
#' @param footnotes Optional character vector of footnotes.
#' @param separate_files Logical. When \code{data} is a list, \code{TRUE} (default) saves
#'   each table as a separate PDF file inside a subdirectory; \code{FALSE} merges them into
#'   a single PDF (requires \pkg{qpdf}).
#' @param names_separator Column name separator for spanners. Default \code{"__"}.
#' @param facade Styling options. Defaults to the global tsg facade.
#'
#' @return Invisibly returns \code{NULL}.
#' @export

write_pdf <- function(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = TRUE,
  names_separator = "__",
  facade = get_tsg_facade(which = "html")
) {

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop(
      "Package 'webshot2' is required for PDF output. ",
      "Install it with: install.packages('webshot2')"
    )
  }

  facade <- facade %||% get_tsg_facade(which = "html")

  if (inherits(data, "list")) {

    if (separate_files) {

      path_dir <- sub("\\.pdf$", "", path, ignore.case = TRUE)
      fs::dir_create(path_dir)

      for (i in seq_along(data)) {
        nm     <- names(data)[i]
        path_i <- file.path(path_dir, paste0(nm, ".pdf"))
        data_i <- data[[i]]
        attrs_i <- attributes(data_i)

        title_i <- if (!is.null(title)) glue::glue("{title}: {nm}") else attrs_i$title
        gt_tbl <- tsg_to_gt(
          data_i,
          title = title_i,
          subtitle = subtitle %||% attrs_i$subtitle,
          source_note = source_note %||% attrs_i$source_note,
          footnotes = footnotes %||% attrs_i$footnotes,
          names_separator = names_separator,
          facade = facade
        )
        gt::gtsave(gt_tbl, filename = path_i)
      }

    } else {

      if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
        path <- paste0(path, ".pdf")
      }

      tmp_dir <- tempfile("tsg_pdf_")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

      tmp_paths <- character(length(data))
      for (i in seq_along(data)) {
        nm     <- names(data)[i] %||% as.character(i)
        path_i <- file.path(tmp_dir, paste0(sprintf("%03d", i), "_", nm, ".pdf"))
        data_i <- data[[i]]
        attrs_i <- attributes(data_i)

        title_i <- if (!is.null(title)) glue::glue("{title}: {nm}") else attrs_i$title
        gt_tbl <- tsg_to_gt(
          data_i,
          title = title_i,
          subtitle = subtitle %||% attrs_i$subtitle,
          source_note = source_note %||% attrs_i$source_note,
          footnotes = footnotes %||% attrs_i$footnotes,
          names_separator = names_separator,
          facade = facade
        )
        gt::gtsave(gt_tbl, filename = path_i)
        tmp_paths[i] <- path_i
      }

      qpdf::pdf_combine(input = tmp_paths, output = path)
    }

  } else {

    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".pdf")
    }

    attrs <- attributes(data)
    gt_tbl <- tsg_to_gt(
      data,
      title = title %||% attrs$title,
      subtitle = subtitle %||% attrs$subtitle,
      source_note = source_note %||% attrs$source_note,
      footnotes = footnotes %||% attrs$footnotes,
      names_separator = names_separator,
      facade = facade
    )
    gt::gtsave(gt_tbl, filename = path)
  }

  invisible(NULL)
}


# Minimal HTML entity escaping for safe inline TOC labels.
.html_escape <- function(x) {
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}
