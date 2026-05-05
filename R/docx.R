
#' Write a tsg table (or list of tables) to a Word (.docx) file
#'
#' Saves one or more \code{tsg} tables as a Word document using the \pkg{officer} and
#' \pkg{flextable} packages (both must be installed).
#'
#' When \code{data} is a named list and \code{separate_files = FALSE} (default), all
#' tables are written into a single \code{.docx} document, one per page. Set
#' \code{separate_files = TRUE} to write one \code{.docx} file per table inside a
#' subdirectory derived from \code{path}.
#'
#' @param data A \code{tsg} or data frame, or a named list of them.
#' @param path File path for the Word output. A \code{.docx} extension is added if missing.
#'   When \code{separate_files = TRUE} the path (minus extension) is used as the directory.
#' @param ... Currently unused; reserved for future arguments.
#' @param title Optional title string (overrides data attribute).
#' @param subtitle Optional subtitle string.
#' @param source_note Optional source note string.
#' @param footnotes Optional character vector of footnotes.
#' @param separate_files Logical. When \code{data} is a list, \code{FALSE} (default) writes
#'   all tables into a single combined \code{.docx}; \code{TRUE} writes one \code{.docx}
#'   per table inside a subdirectory.
#' @param names_separator Column name separator for detecting cross-tab spanners. Default
#'   \code{"__"}.
#' @param facade Styling options. Defaults to the global tsg facade.
#'
#' @return Invisibly returns \code{NULL}.
#' @export

write_docx <- function(
  data,
  path,
  ...,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  separate_files = FALSE,
  names_separator = "__",
  facade = get_tsg_facade(which = "docx")
) {

  facade <- facade %||% get_tsg_facade(which = "docx")

  if (inherits(data, "list")) {

    if (separate_files) {

      path_dir <- sub("\\.docx$", "", path, ignore.case = TRUE)
      fs::dir_create(path_dir)

      for (i in seq_along(data)) {
        nm     <- names(data)[i] %||% as.character(i)
        path_i <- file.path(path_dir, paste0(nm, ".docx"))
        data_i <- data[[i]]
        attrs_i <- attributes(data_i)

        title_i <- if (!is.null(title)) glue::glue("{title}: {nm}") else attrs_i$title
        .write_single_docx(
          data_i,
          path = path_i,
          title = title_i,
          subtitle = subtitle %||% attrs_i$subtitle,
          source_note = source_note %||% attrs_i$source_note,
          footnotes = footnotes %||% attrs_i$footnotes,
          names_separator = names_separator,
          facade = resolve_facade(facade, attrs_i$facade, which = "docx")
        )
      }

    } else {

      if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
        path <- paste0(path, ".docx")
      }

      doc <- officer::read_docx()

      for (i in seq_along(data)) {
        nm     <- names(data)[i] %||% as.character(i)
        data_i <- data[[i]]
        attrs_i <- attributes(data_i)

        title_i <- if (!is.null(title)) glue::glue("{title}: {nm}") else attrs_i$title
        resolved_title    <- title_i
        resolved_subtitle <- subtitle %||% attrs_i$subtitle
        resolved_source   <- source_note %||% attrs_i$source_note
        resolved_footnotes <- footnotes %||% attrs_i$footnotes

        if (i > 1L) {
          doc <- officer::body_add_break(doc)
        }

        doc <- .add_table_to_doc(
          doc,
          data_i,
          title = resolved_title,
          subtitle = resolved_subtitle,
          source_note = resolved_source,
          footnotes = resolved_footnotes,
          names_separator = names_separator,
          facade = resolve_facade(facade, attrs_i$facade, which = "docx")
        )
      }

      invisible(print(doc, target = path))
    }

  } else {

    if (!grepl("\\.docx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".docx")
    }

    attrs <- attributes(data)
    .write_single_docx(
      data,
      path = path,
      title = title %||% attrs$title,
      subtitle = subtitle %||% attrs$subtitle,
      source_note = source_note %||% attrs$source_note,
      footnotes = footnotes %||% attrs$footnotes,
      names_separator = names_separator,
      facade = resolve_facade(facade, attrs$facade, which = "docx")
    )
  }

  invisible(NULL)
}


# Write a single tsg table to its own .docx file.
.write_single_docx <- function(
  data,
  path,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  names_separator = "__",
  facade = NULL
) {

  doc <- officer::read_docx()
  doc <- .add_table_to_doc(
    doc, data,
    title = title,
    subtitle = subtitle,
    source_note = source_note,
    footnotes = footnotes,
    names_separator = names_separator,
    facade = facade
  )
  invisible(print(doc, target = path))
}


# Add one tsg table (with metadata) as paragraphs + flextable to an officer doc.
.add_table_to_doc <- function(
  doc,
  data,
  title = NULL,
  subtitle = NULL,
  source_note = NULL,
  footnotes = NULL,
  names_separator = "__",
  facade = NULL
) {

  facade <- facade %||% get_tsg_facade(which = "docx")

  # Helper: build officer fpar from facade section and text
  .make_fpar <- function(text, section_prefix) {
    fn_name    <- facade[[paste0(section_prefix, ".fontName")]]
    fn_size    <- facade[[paste0(section_prefix, ".fontSize")]]
    fn_colour  <- facade[[paste0(section_prefix, ".fontColour")]]
    fn_dec     <- tolower(as.character(facade[[paste0(section_prefix, ".textDecoration")]] %||% ""))
    fn_halign  <- facade[[paste0(section_prefix, ".halign")]]

    fp_t <- officer::fp_text_lite(
      font.family = fn_name %||% (facade$table.fontName %||% "Arial"),
      font.size   = as.numeric(fn_size %||% (facade$table.fontSize %||% 11)),
      color       = fn_colour %||% "black",
      bold        = "bold" %in% fn_dec,
      italic      = "italic" %in% fn_dec,
      underline   = "underline" %in% fn_dec
    )
    fp_p <- officer::fp_par(
      text.align = fn_halign %||% "left"
    )
    officer::fpar(text, fp_t = fp_t, fp_p = fp_p)
  }

  if (!is.null(title) && nzchar(title)) {
    doc <- officer::body_add_fpar(doc, .make_fpar(title, "title"), pos = "after")
  }

  if (!is.null(subtitle) && nzchar(subtitle)) {
    doc <- officer::body_add_fpar(doc, .make_fpar(subtitle, "subtitle"), pos = "after")
  }

  ft <- tsg_to_flextable(data, names_separator = names_separator, facade = facade)
  doc <- flextable::body_add_flextable(doc, ft)

  footnotes <- .normalize_footnotes(footnotes)

  # Source note
  if (!is.null(source_note) && nzchar(source_note)) {
    doc <- officer::body_add_fpar(doc, .make_fpar(source_note, "source_note"), pos = "after")
  }

  # Footnotes — respect per-footnote placement; apply footnotes.* facade styling
  if (!is.null(footnotes)) {
    fn_name   <- facade$footnotes.fontName %||% (facade$table.fontName %||% "Arial")
    fn_size   <- as.numeric(facade$footnotes.fontSize %||% facade$table.fontSize %||% 10)
    fn_colour <- facade$footnotes.fontColour %||% "black"
    fn_dec    <- tolower(as.character(facade$footnotes.textDecoration %||% ""))

    for (i in seq_along(footnotes$text)) {
      fn_text      <- footnotes$text[[i]]
      fn_placement <- footnotes$placement[[i]] %||% "auto"
      text_align   <- if (fn_placement == "right") "right" else "left"

      fp_t_fn <- officer::fp_text_lite(
        font.family = fn_name,
        font.size   = fn_size,
        color       = fn_colour,
        bold        = "bold" %in% fn_dec,
        italic      = "italic" %in% fn_dec,
        underline   = "underline" %in% fn_dec
      )
      fp_p_fn  <- officer::fp_par(text.align = text_align)
      fpar_obj <- officer::fpar(fn_text, fp_t = fp_t_fn, fp_p = fp_p_fn)
      doc <- officer::body_add_fpar(doc, fpar_obj, pos = "after")
    }
  }

  doc
}


#' Convert a tsg table to a flextable object
#'
#' Internal helper. Builds a \code{flextable} from a \code{tsg} data frame,
#' detects cross-tabulation spanner headers (columns whose display label contains
#' \code{names_separator}), and applies basic facade styling (font, header
#' background, column alignment, decimal precision).
#'
#' @param data A \code{tsg} or data frame.
#' @param names_separator Separator string for spanner detection. Default \code{"__"}.
#' @param facade A facade list. Defaults to the global tsg facade.
#'
#' @return A \code{flextable} object.
#' @keywords internal

tsg_to_flextable <- function(data, names_separator = "__", facade = get_tsg_facade(which = "docx")) {

  facade <- facade %||% get_tsg_facade(which = "docx")
  facade <- resolve_facade(facade, attributes(data)$facade, which = "docx")

  attrs    <- attributes(data)
  sep      <- attrs$label_separator %||% names_separator
  col_names <- names(data)

  # Extract display labels (same logic as tsg_to_gt)
  col_labels <- vapply(col_names, function(cn) {
    attr(data[[cn]], "label_xlsx") %||% attr(data[[cn]], "label") %||% cn
  }, character(1))

  # Convert labelled columns to factor
  data_for_ft <- convert_factor(dplyr::ungroup(data))

  # --- Detect spanners ----------------------------------------------------------
  has_spanner <- any(grepl(sep, col_labels, fixed = TRUE))
  spanner_groups <- list()
  leaf_labels    <- col_labels

  if (has_spanner) {
    for (i in seq_along(col_names)) {
      lbl <- col_labels[i]
      if (grepl(sep, lbl, fixed = TRUE)) {
        parts <- strsplit(lbl, sep, fixed = TRUE)[[1]]
        leaf_labels[i] <- parts[length(parts)]
        spanner_label  <- paste(parts[-length(parts)], collapse = sep)
        spanner_groups[[spanner_label]] <- c(spanner_groups[[spanner_label]], col_names[i])
      }
    }
  }

  # --- Build flextable ----------------------------------------------------------
  ft <- flextable::flextable(data_for_ft)

  # Apply leaf column labels
  label_args <- as.list(leaf_labels)
  names(label_args) <- col_names
  ft <- do.call(flextable::set_header_labels, c(list(ft), label_args))

  # Add spanner rows
  if (has_spanner && length(spanner_groups) > 0) {
    for (spanner_label in rev(names(spanner_groups))) {
      cols_for_spanner <- spanner_groups[[spanner_label]]
      ft <- flextable::add_header_row(
        ft,
        values = stats::setNames(
          rep("", length(col_names)),
          col_names
        ) |> (\(v) { v[cols_for_spanner] <- spanner_label; v })(),
        colwidths = rep(1L, length(col_names))
      )
    }
  }

  # --- Numeric formatting -------------------------------------------------------
  precision <- as.integer(facade$table.decimalPrecision %||% 2L)

  int_names <- col_names[vapply(col_names, function(cn) {
    x <- data_for_ft[[cn]]
    is.integer(x) || (is.double(x) && !all(is.na(x)) &&
      isTRUE(all.equal(sum(x, na.rm = TRUE), sum(as.integer(x), na.rm = TRUE))))
  }, logical(1))]

  dbl_names <- setdiff(
    col_names[vapply(col_names, function(cn) is.double(data_for_ft[[cn]]), logical(1))],
    int_names
  )

  if (length(int_names) > 0) {
    ft <- flextable::colformat_int(ft, j = int_names, big.mark = ",")
  }
  if (length(dbl_names) > 0) {
    ft <- flextable::colformat_double(ft, j = dbl_names, digits = precision, big.mark = ",")
  }

  # --- Facade styling -----------------------------------------------------------
  font_name <- facade$table.fontName %||% "Arial"
  font_size <- as.numeric(facade$table.fontSize %||% 11)

  ft <- flextable::font(ft, fontname = font_name, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")

  # Table-level font colour
  if (!is.null(facade$table.fontColour)) {
    ft <- flextable::color(ft, color = facade$table.fontColour, part = "all")
  }

  # Table-level background fill
  table_fill <- facade$table.fgFill %||% facade$table.bgFill
  if (!is.null(table_fill)) {
    ft <- flextable::bg(ft, bg = table_fill, part = "all")
  }

  # Header background
  if (!is.null(facade$header.fgFill)) {
    ft <- flextable::bg(ft, bg = facade$header.fgFill, part = "header")
  }

  # Header text colour
  if (!is.null(facade$header.fontColour)) {
    ft <- flextable::color(ft, color = facade$header.fontColour, part = "header")
  }

  # Header font name / size
  if (!is.null(facade$header.fontName)) {
    ft <- flextable::font(ft, fontname = facade$header.fontName, part = "header")
  }
  if (!is.null(facade$header.fontSize)) {
    ft <- flextable::fontsize(ft, size = as.numeric(facade$header.fontSize), part = "header")
  }

  # Header bold/italic/underline
  header_decs <- tolower(as.character(facade$header.textDecoration %||% ""))
  if ("bold" %in% header_decs)   ft <- flextable::bold(ft, part = "header")
  if ("italic" %in% header_decs) ft <- flextable::italic(ft, part = "header")

  # Spanner rows: apply spanner-specific overrides to top header rows when spanners exist
  if (has_spanner && length(spanner_groups) > 0) {
    n_spanner_rows <- length(spanner_groups)
    spanner_row_idx <- seq_len(n_spanner_rows) + 1L  # rows 2..N+1 in flextable header

    spanner_fill <- facade$spanner.fgFill %||% facade$spanner.bgFill
    if (!is.null(spanner_fill)) {
      ft <- flextable::bg(ft, bg = spanner_fill, i = spanner_row_idx, part = "header")
    }
    if (!is.null(facade$spanner.fontColour)) {
      ft <- flextable::color(ft, color = facade$spanner.fontColour, i = spanner_row_idx, part = "header")
    }
    if (!is.null(facade$spanner.fontName)) {
      ft <- flextable::font(ft, fontname = facade$spanner.fontName, i = spanner_row_idx, part = "header")
    }
    if (!is.null(facade$spanner.fontSize)) {
      ft <- flextable::fontsize(ft, size = as.numeric(facade$spanner.fontSize), i = spanner_row_idx, part = "header")
    }
    spanner_decs <- tolower(as.character(facade$spanner.textDecoration %||% ""))
    if ("bold" %in% spanner_decs)   ft <- flextable::bold(ft, i = spanner_row_idx, part = "header")
    if ("italic" %in% spanner_decs) ft <- flextable::italic(ft, i = spanner_row_idx, part = "header")
  }

  # Body background fill
  body_fill <- facade$body.fgFill %||% facade$body.bgFill
  if (!is.null(body_fill)) {
    ft <- flextable::bg(ft, bg = body_fill, part = "body")
  }

  # Body text colour
  if (!is.null(facade$body.fontColour)) {
    ft <- flextable::color(ft, color = facade$body.fontColour, part = "body")
  }

  # Body font name / size
  if (!is.null(facade$body.fontName)) {
    ft <- flextable::font(ft, fontname = facade$body.fontName, part = "body")
  }
  if (!is.null(facade$body.fontSize)) {
    ft <- flextable::fontsize(ft, size = as.numeric(facade$body.fontSize), part = "body")
  }

  # Body text decoration
  body_decs <- tolower(as.character(facade$body.textDecoration %||% ""))
  if ("bold" %in% body_decs)   ft <- flextable::bold(ft, part = "body")
  if ("italic" %in% body_decs) ft <- flextable::italic(ft, part = "body")

  # Body alignment
  body_halign <- facade$body.halign %||% "center"
  ft <- flextable::align(ft, align = body_halign, part = "body")

  # First column styling
  first_align <- facade$col_first.halign %||% "left"
  ft <- flextable::align(ft, j = 1L, align = first_align, part = "body")
  if (!is.null(facade$col_first.fontName)) {
    ft <- flextable::font(ft, j = 1L, fontname = facade$col_first.fontName, part = "body")
  }
  if (!is.null(facade$col_first.fontSize)) {
    ft <- flextable::fontsize(ft, j = 1L, size = as.numeric(facade$col_first.fontSize), part = "body")
  }
  if (!is.null(facade$col_first.fontColour)) {
    ft <- flextable::color(ft, j = 1L, color = facade$col_first.fontColour, part = "body")
  }
  col_first_fill <- facade$col_first.fgFill %||% facade$col_first.bgFill
  if (!is.null(col_first_fill)) {
    ft <- flextable::bg(ft, j = 1L, bg = col_first_fill, part = "body")
  }
  col_first_decs <- tolower(as.character(facade$col_first.textDecoration %||% ""))
  if ("bold" %in% col_first_decs)   ft <- flextable::bold(ft, j = 1L, part = "body")
  if ("italic" %in% col_first_decs) ft <- flextable::italic(ft, j = 1L, part = "body")

  # Last column styling
  last_col_idx <- length(col_names)
  if (!is.null(facade$col_last.halign)) {
    ft <- flextable::align(ft, j = last_col_idx, align = facade$col_last.halign, part = "body")
  }
  if (!is.null(facade$col_last.fontName)) {
    ft <- flextable::font(ft, j = last_col_idx, fontname = facade$col_last.fontName, part = "body")
  }
  if (!is.null(facade$col_last.fontSize)) {
    ft <- flextable::fontsize(ft, j = last_col_idx, size = as.numeric(facade$col_last.fontSize), part = "body")
  }
  if (!is.null(facade$col_last.fontColour)) {
    ft <- flextable::color(ft, j = last_col_idx, color = facade$col_last.fontColour, part = "body")
  }
  col_last_fill <- facade$col_last.fgFill %||% facade$col_last.bgFill
  if (!is.null(col_last_fill)) {
    ft <- flextable::bg(ft, j = last_col_idx, bg = col_last_fill, part = "body")
  }
  col_last_decs <- tolower(as.character(facade$col_last.textDecoration %||% ""))
  if ("bold" %in% col_last_decs)   ft <- flextable::bold(ft, j = last_col_idx, part = "body")
  if ("italic" %in% col_last_decs) ft <- flextable::italic(ft, j = last_col_idx, part = "body")

  # Header alignment
  header_halign <- facade$header.halign %||% "center"
  ft <- flextable::align(ft, align = header_halign, part = "header")
  ft <- flextable::align(ft, j = 1L, align = first_align, part = "header")

  # Last row bold
  n_body <- nrow(data_for_ft)
  if (isTRUE(facade$table.lastRowBold) && n_body > 0L) {
    ft <- flextable::bold(ft, i = n_body, part = "body")
  }

  # Borders — facade-driven
  outer_colour  <- facade$border_outer.borderColour %||% "#999999"
  inner_colour  <- facade$body.borderColour %||% "#cccccc"
  header_colour <- facade$border_header.borderColour %||% outer_colour
  ft <- flextable::border_outer(ft, part = "all",
    border = officer::fp_border(color = outer_colour, width = 0.5))
  ft <- flextable::border_inner_h(ft,
    border = officer::fp_border(color = inner_colour, width = 0.25))
  ft <- flextable::border_inner_v(ft,
    border = officer::fp_border(color = inner_colour, width = 0.25))
  ft <- flextable::hline_bottom(ft, part = "header",
    border = officer::fp_border(color = header_colour, width = 0.75))

  ft <- flextable::autofit(ft)
  ft
}
