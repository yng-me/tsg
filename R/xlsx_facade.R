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


xlsx_decimal_format <- function(wb, data, sheet_name, rows, offset, precision = 3) {

  if(is.null(precision)) { precision <- 3 }
  if(!is.numeric(precision)) { precision <- 3 }

  is_dbl <- names(dplyr::select(data, dplyr::where(is.double)))
  which_dbl <- which(names(data) %in% is_dbl)

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

    facade$styles$border_outer$border <- corners[i]

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = facade$styles$border_outer,
      rows = corner_rows[[i]],
      cols = corner_cols[[i]]
    )
  }

  return(wb)
}


xlsx_colwidths <- function(wb, sheet_name, cols, facade = getOption("tsg.options.facade"), offset = 0) {

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols,
    widths = facade$widths$all
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[1],
    widths = facade$widths$first
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[length(cols)],
    widths = facade$widths$last
  )

  if(offset > 0) {
    openxlsx::setColWidths(
      wb = wb,
      sheet = sheet_name,
      cols = 1:offset,
      widths = facade$widths$offset
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
  built_in$decimal <- user_defined$gridLines %||% built_in$decimal
  built_in$lastRowBold <- user_defined$lastRowBold %||% built_in$lastRowBold

  # level 2 (heights)
  built_in$heights$title <- user_defined$heights$title %||% built_in$heights$title
  built_in$heights$subtitle <- user_defined$heights$subtitle %||% built_in$heights$subtitle
  built_in$heights$header <- user_defined$heights$header %||% built_in$heights$header
  built_in$heights$bottomHeader <- user_defined$heights$bottomHeader %||% built_in$heights$bottomHeader
  built_in$heights$body <- user_defined$heights$body %||% built_in$heights$body
  built_in$heights$sourceNote <- user_defined$heights$sourceNote %||% built_in$heights$sourceNote
  built_in$heights$group <- user_defined$heights$group %||% built_in$heights$group

  # level 2 (widths)
  built_in$widths$offset <- user_defined$widths$offset %||% built_in$widths$offset
  built_in$widths$first <- user_defined$widths$first %||% built_in$widths$first
  built_in$widths$all <- user_defined$widths$all %||% built_in$widths$all
  built_in$widths$last <- user_defined$widths$last %||% built_in$widths$last

  if(is.null(user_defined$widths$last) & !is.null(user_defined$widths$all)) {
    built_in$widths$last <- user_defined$widths$all
  }

  # TODO
  # level 3
  return(built_in)
}
