#' Write data into an Excel workbook
#'
#' @param data A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.
#' @param ... \code{openxlsx} workbook object and sheet name.
#' @param title Table title.
#' @param description Table description.
#' @param options Formatting options.
#' @param start_col Column position to start write the data.
#' @param footnote Table footnote.
#' @param separator Column separator that defines the table hierarchy.
#'
#' @return A formatted workbook object.
#' @export
#'
#' @examples

tse_write_excel <- function(
  data,
  ...,
  title = NULL,
  description = NULL,
  start_col = 2,
  footnote = NULL,
  options = tsg_get_config('facade'),
  separator = '>'
) {

  depth <- NULL
  col_from <- NULL
  type <- NULL
  n <- NULL
  r <- NULL
  value <- NULL


  start_row_init <- 3
  start_row <- start_row_init
  openxlsx::addWorksheet(..., gridLines = F)

  if(!is.null(title)) {
    openxlsx::writeData(
      ...,
      x = title,
      startCol = start_col,
      startRow = start_row
    )
    start_row <- start_row + 1
  }
  if(!is.null(description)) {
    openxlsx::writeData(
      ...,
      x = description,
      startCol = start_col,
      startRow = start_row
    )
    start_row <- start_row + 1
  }

  merge_colnames <- tse_util_extract_col(
    data,
    start_col = start_col,
    start_row = start_row,
    separator = separator
  )

  row_depth <- max(merge_colnames$depth)

  if(row_depth == 1) {

    openxlsx::writeData(
      ...,
      x = data,
      startCol = start_col,
      startRow = row_depth + start_row,
      borders = 'all',
      borderStyle = 'dashed',
      borderColour = 'gray'
    )

  } else {

    row_depth_all <- row_depth + 1
    row_depth_inner <- row_depth - 1

    openxlsx::writeData(
      ...,
      x = data,
      startCol = start_col,
      startRow = row_depth_all + start_row,
      colNames = F,
      borders = 'all',
      borderStyle = 'dashed',
      borderColour = 'gray'
    )

    merge_rows <- merge_colnames |> dplyr::filter(depth == 1)

    for(m in 1:nrow(merge_rows)) {

      row_from <- merge_rows$depth[m]

      row_range <- row_from:row_depth + start_row
      openxlsx::writeData(
        ...,
        x = merge_rows$value[m],
        startRow = row_from + start_row,
        startCol = merge_rows$col_from[m]
      )

      openxlsx::mergeCells(
        ...,
        cols = merge_rows$col_from[m],
        rows = row_range
      )
    }

    # TOP COLUMN HEADER
    top_col <- merge_colnames |>
      dplyr::filter(row_from == start_row, depth > 1) |>
      dplyr::mutate(col_to = col_from + r - 1) |>
      dplyr::distinct(value, .keep_all = T)

    for(i in 1:nrow(top_col)) {

      top_col_from <- top_col$col_from[i]
      top_col_to <- top_col$col_to[i]

      openxlsx::writeData(
        ...,
        x = top_col$value[i],
        startRow = start_row + 1,
        startCol = top_col$col_from[i]
      )

      openxlsx::mergeCells(
        ...,
        cols = top_col_from:top_col_to,
        rows = start_row + 1
      )
    }

    # MIDDLE COLUMN HEADER
    if(row_depth_inner > 1) {

      for(j in 2:row_depth_inner) {

        inner <- merge_colnames |> dplyr::filter(row_from == j + start_row - 1)

        inner_seq <- tibble::as_tibble_col(
          tse_util_increment_val(inner$value),
          column_name = 'seq'
        )

        inner_col <- inner |>
          dplyr::mutate(col_to = col_from + max(merge_colnames$r) - (r + 1)) |>
          tibble::add_column(inner_seq) |>
          dplyr::group_by(value, seq) |>
          dplyr::summarise(
            min = min(col_from),
            max = max(col_from),
            .groups = 'drop'
          ) |>
          dplyr::arrange(seq)

        openxlsx::writeData(
          ...,
          x = t(inner$value),
          startCol = min(inner$col_from),
          startRow = j + start_row,
          colNames = F
        )

        for(k in 1:nrow(inner_col)) {

          inner_col_from <- inner_col$min[k]
          inner_col_to <- inner_col$max[k]

          openxlsx::mergeCells(
            ...,
            cols = inner_col_from:inner_col_to,
            rows = j + start_row
          )
        }

      }
    }

    # BOTTOM COLUMN HEADER
    bottom_col <- merge_colnames |>
      dplyr::filter(row_from == row_depth + start_row - 1)

    openxlsx::writeData(
      ...,
      x = t(bottom_col$value),
      startCol = min(bottom_col$col_from),
      startRow = row_depth + start_row,
      colNames = F
    )
  }


  row_length <- nrow(data) + start_row + row_depth
  col_length <- ncol(data) + start_col - 1

  if(!is.null(footnote)) {
    openxlsx::writeData(
      ...,
      x = footnote,
      startCol = start_col,
      startRow = row_length + 2
    )
  }

  tse_set_facade(
    ...,
    header_depth = row_depth,
    start_row_init = start_row_init,
    start_row = start_row + 1,
    start_col = start_col,
    end_row = row_length,
    end_col = col_length,
    options = options
  )
}
