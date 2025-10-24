xlsx_eval_style <- function(wb, sheet_name, style, cols, rows) {

  style_fn_args <- list()
  style_names <- names(style)

  for(i in seq_along(style_names)) {

    style_i <- style[[i]]

    if(is.null(style_i)) next

    if(length(style_i) > 1) {

      if(is.numeric(style_i) | is.logical(style_i)) {
        style_arg <- glue::glue("c({paste0(as.character(style_i), collapse = ", ")})")
      } else {
        style_arg_c <- paste0(paste0("'", style$border, "'"), collapse = ", ")
        style_arg <- glue::glue("c({style_arg_c})")
      }

    } else {

      if(is.numeric(style_i) | is.logical(style_i)) {
        style_arg <- as.character(style_i)
      } else {
        style_arg <- glue::glue("'{style_i}'")
      }

    }

    style_fn_args[[i]] <- glue::glue("{style_names[i]} = {style_arg}")

  }

  if(length(style_fn_args) > 0) {

    args <- paste0(unlist(style_fn_args), collapse = ", ")
    fn <- as.character(glue::glue("openxlsx::createStyle({args})"))

    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      stack = TRUE,
      gridExpand = TRUE,
      cols = cols,
      rows = rows,
      style = eval(parse(text = fn))
    )
  }

  return(wb)
}


xlsx_decimal_format <- function(wb, data, sheet_name, rows, offset, cols = NULL, precision = 3) {

  if(is.null(precision)) { precision <- 2 }
  if(!is.numeric(precision)) { precision <- 2 }

  is_dbl <- names(dplyr::select(data, dplyr::where(is.double)))
  which_dbl <- which(names(data) %in% c(is_dbl, cols))

  if(length(which_dbl) > 0) {

    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      cols = which_dbl + offset,
      rows = rows,
      gridExpand = TRUE,
      stack = TRUE,
      style = openxlsx::createStyle(
        numFmt = paste0('#,#0.', paste0(rep(0, as.integer(precision)), collapse = ''))
      )
    )
  }

  return(wb)
}

xlsx_corner_borders <- function(
  wb,
  sheet_name,
  row_start,
  row_end,
  col_start,
  col_end,
  facade
) {

  corners <- c("top", "bottom", "left", "right")

  corner_rows <- list(row_start, row_end, row_start:row_end, row_start:row_end)
  corner_cols <- list(col_start:col_end, col_start:col_end, col_start, col_end)

  for(i in 1:4) {

    facade$style$border_outer$border <- corners[i]

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = facade$style$border_outer,
      rows = corner_rows[[i]],
      cols = corner_cols[[i]]
    )
  }

  return(wb)
}


xlsx_colwidths <- function(wb, sheet_name, cols, facade = get_tsg_facade(), offset = 0) {

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols,
    widths = facade$width$all
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[1],
    widths = facade$width$first
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[length(cols)],
    widths = facade$width$last
  )

  if(offset > 0) {
    openxlsx::setColWidths(
      wb = wb,
      sheet = sheet_name,
      cols = 1:offset,
      widths = facade$width$offset
    )
  }

  return(wb)
}


xlsx_extract_facade <- function(built_in, user_defined) {

  if(is.null(user_defined)) {
    return(built_in)
  }

  # level 1
  built_in$gridLines <- user_defined$gridLines %||% built_in$gridLines
  built_in$lastRowBold <- user_defined$lastRowBold %||% built_in$lastRowBold

  built_in$decimal$cols <- user_defined$decimal$cols %||% built_in$decimal$cols
  built_in$decimal$precision <- user_defined$decimal$precision %||% built_in$decimal$precision

  # level 2 (heights)
  built_in$height$title <- user_defined$height$title %||% built_in$height$title
  built_in$height$subtitle <- user_defined$height$subtitle %||% built_in$height$subtitle
  built_in$height$header <- user_defined$height$header %||% built_in$height$header
  built_in$height$bottomHeader <- user_defined$height$bottomHeader %||% built_in$height$bottomHeader
  built_in$height$body <- user_defined$height$body %||% built_in$height$body
  built_in$height$sourceNote <- user_defined$height$sourceNote %||% built_in$height$sourceNote
  built_in$height$group <- user_defined$height$group %||% built_in$height$group

  # level 2 (widths)
  built_in$width$offset <- user_defined$width$offset %||% built_in$width$offset
  built_in$width$first <- user_defined$width$first %||% built_in$width$first
  built_in$width$all <- user_defined$width$all %||% built_in$width$all
  built_in$width$last <- user_defined$width$last %||% built_in$width$last

  if(is.null(user_defined$width$last) & !is.null(user_defined$width$all)) {
    built_in$width$last <- user_defined$width$all
  }

  built_in$style$group <- user_defined$height$group %||% built_in$height$group

  built_in$style$body$indent <- user_defined$style$body$indent %||% built_in$style$body$indent
  built_in$style$body$valign <- user_defined$style$body$valign %||% built_in$style$body$valign
  built_in$style$body$border <- user_defined$style$body$border %||% built_in$style$body$border
  built_in$style$body$numFmt <- user_defined$style$body$numFmt %||% built_in$style$body$numFmt
  built_in$style$body$borderStyle <- user_defined$style$body$borderStyle %||% built_in$style$body$borderStyle
  built_in$style$body$borderColour <- user_defined$style$body$borderColour %||% built_in$style$body$borderColour

  built_in$style$title$fontSize <- built_in$style$title$fontSize %||% built_in$style$title$fontSize
  built_in$style$title$textDecoration <- built_in$style$title$textDecoration %||% built_in$style$title$textDecoration
  built_in$style$subtitle$fontSize <- built_in$style$subtitle$fontSize %||% built_in$style$subtitle$fontSize
  built_in$style$subtitle$textDecoration <- built_in$style$subtitle$textDecoration %||% built_in$style$subtitle$textDecoration
  built_in$style$subtitle$valign <- built_in$style$subtitle$valign %||% built_in$style$subtitle$valign
  built_in$style$footnote$fontSize <- built_in$style$footnote$fontSize %||% built_in$style$footnote$fontSize
  built_in$style$footnote$textDecoration <- built_in$style$footnote$textDecoration %||% built_in$style$footnote$textDecoration
  built_in$style$source_note$fontSize <- built_in$style$source_note$fontSize %||% built_in$style$source_note$fontSize
  built_in$style$source_note$textDecoration <- built_in$style$source_note$textDecoration %||% built_in$style$source_note$textDecoration
  built_in$style$source_note$valign <- built_in$style$source_note$valign %||% built_in$style$source_note$valign
  built_in$style$header$wrapText <- built_in$style$header$wrapText %||% built_in$style$header$wrapText
  built_in$style$header$fgFill <- built_in$style$header$fgFill %||% built_in$style$header$fgFill
  built_in$style$header$bgFill <- built_in$style$header$bgFill %||% built_in$style$header$bgFill
  built_in$style$header$border <- built_in$style$header$border %||% built_in$style$header$border
  built_in$style$header$borderStyle <- built_in$style$header$borderStyle %||% built_in$style$header$borderStyle
  built_in$style$header$borderColour <- built_in$style$header$borderColour %||% built_in$style$header$borderColour
  built_in$style$indent$indent <- built_in$style$indent$indent %||% built_in$style$indent$indent
  built_in$style$indent$valign <- built_in$style$indent$valign %||% built_in$style$indent$valign
  built_in$style$border_outer$borderColour <- built_in$style$border_outer$borderColour %||% built_in$style$border_outer$borderColour
  built_in$style$border_header$border <- built_in$style$border_header$border %||% built_in$style$border_header$border
  built_in$style$border_header$borderColour <- built_in$style$border_header$borderColour %||% built_in$style$border_header$borderColour
  built_in$style$border_header$borderStyle <- built_in$style$border_header$borderStyle %||% built_in$style$border_header$borderStyle

  return(built_in)
}

