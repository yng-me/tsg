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

  is_int <- names(dplyr::select(data, dplyr::where(is.integer)))
  maybe_int <- names(dplyr::select(data, dplyr::where(is.numeric)))
  maybe_int <- maybe_int[!(maybe_int %in% is_int)]

  for(i in maybe_int) {

    if(sum(data[[i]], na.rm = TRUE) == sum(as.integer(data[[i]]), na.rm = TRUE)) {
      is_int <- c(is_int, i)
    }
  }

  which_int <- which(names(data) %in% c(is_int, maybe_int))

  if(length(which_int) > 0) {
    openxlsx::addStyle(
      wb = wb,
      sheet = sheet_name,
      cols = which_int + offset,
      rows = rows,
      gridExpand = TRUE,
      stack = TRUE,
      style = openxlsx::createStyle(numFmt = "#,##0")
    )
  }

  if(is.null(precision)) { precision <- 2 }
  if(!is.numeric(precision)) { precision <- 2 }

  is_dbl <- names(dplyr::select(data, dplyr::where(is.double)))
  is_dbl <- is_dbl[!(is_dbl %in% is_int)]

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
        numFmt = paste0('#,##0.', paste0(rep(0, as.integer(precision)), collapse = ''))
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

    facade$border_outer.border <- corners[i]

    xlsx_eval_style(
      wb = wb,
      sheet_name = sheet_name,
      style = extract_facade(facade, 'border_outer'),
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
    widths = extract_facade(facade, 'table', 'width')
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[1],
    widths = extract_facade(facade, 'col_first', 'width')
  )

  openxlsx::setColWidths(
    wb = wb,
    sheet = sheet_name,
    cols = cols[length(cols)],
    widths = extract_facade(facade, 'col_last', 'width')
  )

  if(offset > 0) {
    openxlsx::setColWidths(
      wb = wb,
      sheet = sheet_name,
      cols = 1:offset,
      widths = extract_facade(facade, 'table', 'widthOffset')
    )
  }

  return(wb)
}
