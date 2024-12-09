#' Generate by aggregation level
#'
#' @param .data
#' @param .x_cols
#' @param ...
#' @param .agg_var
#' @param .agg_area_level
#' @param .agg_area_length
#' @param .total_by_col
#' @param .switch_col
#' @param .split_multiple_response
#' @param .pivot_cols_to_row
#' @param .pivot_cols_to_row_vs
#'
#' @return
#' @export
#'
#' @examples
#'

generate_tab_by_level <- function(
  .data,
  .x_cols,
  ...,
  .agg_var = 'barangay_geo',
  .agg_area_level = NULL,
  .agg_area_length = c(2, 5, 7, 10),
  .total_by_col = F,
  .switch_col = F,
  .split_multiple_response = F,
  .split_multiple_response_width = NULL,
  .pivot_cols_to_row = F,
  .pivot_cols_to_row_vs = NULL,
  .flatten = FALSE
) {

  if(nrow(.data) == 0) return(NULL)

  df_all <- list()

  y_cols <- sapply(substitute(list(...))[-1], deparse)
  agg_area_length <- .agg_area_length[.agg_area_level]
  is_logical_cols <- FALSE
  retain_header <- FALSE

  if('arrow_dplyr_query' %in% class(.data) | 'ArrowObject' %in% class(.data)) {
    data_1 <- .data |>
      head(1) |>
      dplyr::select(!!as.name(.x_cols[1])) |>
      dplyr::collect()
    x_attr <- attributes(data_1[[.x_cols[1]]])

  } else {
    x_attr <- attributes(.data[[.x_cols[1]]])
  }

  if(.split_multiple_response & length(.x_cols) == 1) {

    n_row <- .data |>
      dplyr::filter(!is.na(!!as.name(.x_cols[1]))) |>
      nrow()

    if(n_row == 0) { return(NULL) }

    regex <- '^[A-Z]$'
    if(!is.null(.split_multiple_response_width)) {
      regex <- paste0('^[0-9A-Z]{', .split_multiple_response_width, '}$')
    }

    .data <- .data |>
      dplyr::collect() |>
      dplyr::mutate(
        !!as.name(.x_cols[1]) := toupper(stringr::str_trim(!!as.name(.x_cols[1]))),
        !!as.name(.x_cols[1]) := purrr::map(!!as.name(.x_cols[1]), \(x) {
          x |>
            str_dice(.split_multiple_response_width) |>
            tibble::as_tibble() |>
            dplyr::mutate(VALUE__ = 1L) |>
            dplyr::mutate(value = as.character(value)) |>
            dplyr::full_join(
              x_attr$valueset |>
                dplyr::select(value, label) |>
                dplyr::mutate(value = as.character(value)),
              by = 'value'
            ) |>
            dplyr::mutate(VALUE__ = dplyr::if_else(is.na(VALUE__), 2L, VALUE__)) |>
            dplyr::rename(!!as.name(.x_cols[1]) := value)
        })
      ) |>
      tidyr::unnest(!!as.name(.x_cols[1]), keep_empty = T) |>
      dplyr::filter(!is.na(!!as.name(.x_cols[1])), grepl(regex, !!as.name(.x_cols[1])))


    if(nrow(.data) == 0) { return(NULL) }

    if(!is.null(x_attr)) {
      attr(.data[[.x_cols[1]]], 'label') <- x_attr$label
      attr(.data[[.x_cols[1]]], 'valueset') <- x_attr$valueset
    }

    if(length(y_cols) == 0) {

      y_cols <- 'VALUE__'
      attr(.data[[y_cols]], 'valueset') <- data.frame(
        value = c(1L, 2L),
        label = c("Yes", "No")
      )
      retain_header <- TRUE
    } else {
      .data <- .data |>
        dplyr::filter(VALUE__ == 1L)
    }
  }

  if(.pivot_cols_to_row) {

    .data <- .data |>
      dplyr::collect() |>
      tidyr::pivot_longer(
        dplyr::any_of(.x_cols),
        names_to = 'NAME__',
        values_to = 'VALUE__'
      )

    is_logical_cols <- typeof(.data$VALUE__) == 'logical'

    if(!is.null(.pivot_cols_to_row_vs)) {

      vs <- .pivot_cols_to_row_vs |>
        dplyr::filter(value %in% .x_cols) |>
        dplyr::select(
          COL__ = name,
          NAME__ = value,
          LABEL__ = label
        )

      .x_cols <- vs$COL__[1]

      .data <- .data |>
        dplyr::left_join(vs, by = 'NAME__', multiple = 'first') |>
        dplyr::select(-dplyr::any_of(c('COL__', 'NAME__'))) |>
        dplyr::rename(!!as.name(.x_cols) := LABEL__) |>
        dplyr::select(!!as.name(.x_cols), dplyr::everything())

    } else {
      .data <- .data |> dplyr::rename(x = NAME__)
      .x_cols <- 'x'
    }

    y_cols <- 'VALUE__'

    if(!is.null(x_attr)) {
      attr(.data$VALUE__, 'valueset') <- x_attr$valueset
    }

  }

  if(.switch_col & length(y_cols) > 0) { x_col <- y_cols[1] }

  for(i in seq_along(.x_cols)) {

    df_list <- list()

    if(.switch_col & length(y_cols) > 0) {
      y_cols <- .x_cols[i]
    } else {
      x_col <- .x_cols[i]
    }

    if(length(y_cols) > 0 & !is_logical_cols) {
      meta <- gtab_get_meta(.data, !!as.name(x_col), dplyr::any_of(y_cols))
    } else {
      meta <- gtab_get_meta(.data, !!as.name(x_col))
    }

    if(is.null(.agg_area_level)) {

      if(length(y_cols) > 0) {
        .data <- .data |>
          dplyr::group_by(!!as.name(y_cols))
      }

      df <- .data |>
        gtab_frequency(
          .x = x_col,
          .y = cols,
          .total_by_col = .total_by_col
        ) |>
        dplyr::mutate(
          area_code = paste0(rep('0', .agg_area_length[length(.agg_area_length)]), collapse = ''),
          level = 0L
        ) |>
        gtab_add_factor(meta)


    } else {

      for(j in seq_along(agg_area_length)) {

        df_temp <- .data |>
          dplyr::mutate(
            area_code = stringr::str_sub(!!as.name(.agg_var), 1, agg_area_length[j]) |>
              stringr::str_pad(
                width = .agg_area_length[length(.agg_area_length)],
                pad = '0',
                side = 'right'
              )
          ) |>
          dplyr::ungroup()

        if(length(y_cols) > 0) {
          df_temp <- df_temp |>
            dplyr::group_by(!!as.name(y_cols))
        }

        df_list[[j]] <- df_temp |>
          gtab_frequency(
            x_col,
            area_code,
            .y = y_cols,
            .total_by_col = .total_by_col
          ) |>
          dplyr::mutate(
            level = as.integer(.agg_area_level[j]),
            .before = 1
          )
      }

      df <- dplyr::bind_rows(df_list)

      if(.pivot_cols_to_row & is_logical_cols) {

        df <- df |>
          dplyr::select(-dplyr::contains(c('FALSE', 'percent'))) |>
          dplyr::select(1:4, frequency = 5) |>
          dplyr::mutate(
            percent = (frequency / total) * 100
          ) |>
          dplyr::select(-total)

        attr(df$frequency, 'label') <- 'Frequency'
        attr(df$percent, 'label') <- 'Percent'

      }

      df <- df |>  gtab_add_factor(meta)
      header <- gtab_create_header(df, meta, y_cols)

      if(.split_multiple_response & length(.x_cols) == 1) {

        df <- df |>
          dplyr::select(-dplyr::any_of('total')) |>
          dplyr::filter(!!as.name(.x_cols[1]) != 'Total')

        header <- header |>
          dplyr::filter(field != 'total')
      }

      if(.pivot_cols_to_row) {
        df <- df |>
          dplyr::filter(!!as.name(.x_cols[1]) != 'Total')
      }

    }

    df_all[[.x_cols[i]]] <- list(
      df = df,
      meta = meta,
      header = header,
      attr = list(
        agg_area_level = .agg_area_level,
        is_logical_cols = is_logical_cols,
        retain_header = retain_header
      )
    )
  }

  return(df_all)

}


#' Title
#'
#' @param .data
#' @param .x_cols
#' @param .y_cols
#' @param .agg_var
#' @param .agg_area_level
#' @param .agg_area_length
#' @param .pivot_cols_to_row_vs
#'
#' @return
#' @export
#'
#' @examples
#'

generate_tab <- function(
  .data,
  .x_cols,
  .y_cols,
  .agg_var = 'barangay_geo',
  .agg_area_level = NULL,
  .agg_area_length = c(2, 5, 7, 10),
  .pivot_cols_to_row_vs = NULL
) {

  df_list <- list()
  agg_area_length <- .agg_area_length[.agg_area_level]

  .data <- dplyr::collect(.data)
  y_attr <- attributes(.data[[.y_cols[1]]])$valueset

  for(j in seq_along(agg_area_length)) {

    df_list[[as.character(j)]] <- .data |>
      dplyr::collect() |>
      dplyr::mutate(
        area_code = stringr::str_sub(!!as.name(.agg_var), 1, agg_area_length[j]) |>
          stringr::str_pad(
            width = .agg_area_length[length(.agg_area_length)],
            pad = '0',
            side = 'right'
          )
      ) |>
      tidyr::pivot_longer(dplyr::any_of(.x_cols), names_to = 'NAME__') |>
      dplyr::group_by(area_code, NAME__, value, !!as.name(.y_cols[1])) |>
      dplyr::count() |>
      dplyr::group_by(area_code, NAME__) |>
      tidyr::nest() |>
      dplyr::mutate(
        data = purrr::map(data, \(x) {
          y <- x |>
            tidyr::pivot_wider(
              names_from = value,
              values_from = n
            ) |>
            janitor::adorn_totals(
              where = c('col', 'row'),
              name = c('Total', 'total')
            )

          if(!('TRUE' %in% names(y))) {
            y <- y |>
              dplyr::mutate(`TRUE` = 0)
          }

          y <- y |>
            dplyr::transmute(
              !!as.name(.y_cols[1]),
              total,
              frequency = `TRUE`,
              percent = (`TRUE` / total ) * 100
            ) |>
            tidyr::pivot_wider(
              names_from = !!as.name(.y_cols[1]),
              values_from = c(total, frequency, percent),
              names_sep = '__'
            )
        })
      ) |>
      tidyr::unnest(data) |>
      dplyr::ungroup() |>
      gtab_set_pivot_label(.x_cols, .pivot_cols_to_row_vs) |>
      dplyr::mutate(
        level = as.integer(.agg_area_level[j]),
        .before = 1
      )
  }

  df <- df_list |>
    dplyr::bind_rows() |>
    dplyr::select(
      level,
      area_code,
      dplyr::everything()
    )

  get_header <- function(.df, .y_attr) {

    h <- data.frame(
      order = 1:ncol(.df),
      hidden = c(TRUE, TRUE, rep(FALSE, (ncol(.df) - 2))),
      field = names(.df),
      label = names(.df)
    ) |>
      dplyr::mutate(
        label = stringr::str_remove_all(label, '^(frequency|total|percent)__') |>
          stringr::str_replace_all('_', ' ') |>
          stringr::str_to_sentence()
      )

    if(!is.null(.y_attr)) {
      h <- h |>
        dplyr::left_join(
          .y_attr |>
            dplyr::transmute(
              name = label,
              label = as.character(value)
            ),
          by = 'label'
        ) |>
        dplyr::mutate(
          label = dplyr::if_else(
            !is.na(name),
            name,
            label
          )
        ) |>
        dplyr::select(-name)
    }

    h |>
      dplyr::mutate(
        label = dplyr::if_else(
          grepl('^percent__', field) & !grepl('^percent__', label),
          paste0(label, ' (%)'),
          label
        ),
        label = dplyr::if_else(
          grepl('^total__', field) & !grepl('^total__', label),
          paste0(label, ' (overall)'),
          label
        ),
        hidden = dplyr::if_else(
          grepl('\\(overall\\)', label),
          TRUE,
          hidden
        )
      )
  }

  return(
    list(
      DEFAULT = list(
        df = df,
        header = get_header(df, y_attr),
        meta = list(),
        attr = list(
          agg_area_level = .agg_area_level,
          retain_header = TRUE
        )
      )
    )
  )

}


gtab_frequency <- function(.data, .x, ..., .y = list(), .total_by_col = FALSE) {

  if(length(.y) > 0) {

    .data <- .data |>
      dplyr::group_by(..., !!as.name(.x), .add = T) |>
      dplyr::count(name = 'frequency') |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      gtab_arrange(.x) |>
      tidyr::pivot_wider(
        names_from = dplyr::any_of(.y),
        names_sort = T,
        names_sep = '__',
        names_expand = T,
        names_prefix = 'frequency__',
        values_from = frequency,
        values_fill = 0,
        values_fn = sum
      ) |>
      gtab_add_total(.x, .total_by_col = .total_by_col)

  } else {

    .data <- .data |>
      dplyr::group_by(..., !!as.name(.x), .add = T) |>
      dplyr::count(name = 'frequency') |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      gtab_arrange(.x) |>
      dplyr::group_by(...) |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(data, \(x) {
        x |>
          janitor::adorn_totals(fill = 'Total')
      })) |>
      tidyr::unnest(data) |>
      dplyr::ungroup()

  }


  return(.data)

}

gtab_get_meta <- function(.data, ...) {

  attr_df <- .data |>
    head(1) |>
    dplyr::select(..., dplyr::any_of(dplyr::group_vars(.data))) |>
    dplyr::collect()

  setNames(
    lapply(names(attr_df), function(x) {
      attributes(attr_df[[x]])
    }),
    names(attr_df)
  )
}

gtab_conform_type <- function(v) {
  if(grepl('\\d+', v[1])) v <- as.integer(v)
  return(v)
}

gtab_create_header <- function(.data, meta, y_cols = list()) {

  if(!(inherits(.data, 'gtab_tbl'))) return(NULL)

  attr <- attributes(.data)$meta

  if('total' %in% names(.data)) {
    attr <- attr |>
      dplyr::add_row(
        order = ncol(.data) + 1,
        field = 'total',
        label = 'Total',
        hidden = FALSE
      )
  }

  if(length(y_cols) > 0) {

    pivot_cols <- names(.data)
    pivot_cols <- pivot_cols[grepl('^(frequency|percent)__', pivot_cols)]

    header <- pivot_cols |>
      dplyr::as_tibble(rownames = 'n') |>
      dplyr::rename(field = value) |>
      dplyr::mutate(
        order = max(attr$order) + as.integer(n),
        label = stringr::str_remove_all(field, '^(frequency|percent)__'),
        hidden = FALSE
      ) |>
      dplyr::select(-n)


    labels <- meta[[y_cols]]$valueset

    if(!is.null(labels)) {

      labels <- labels |>
        dplyr::mutate(value = as.character(value)) |>
        dplyr::add_row(value = 'total', label = 'Total')

      header <- header |>
        dplyr::mutate(label = factor(label, labels$value, labels$label))
    }

    header <- header |>
      dplyr::mutate(
        label = dplyr::if_else(
          grepl('^percent__', field) & !grepl('^percent__', label),
          paste0(label, ' (%)'),
          label
        ),
        label = dplyr::if_else(
          is.na(label),
          'NA',
          label
        )
      )

    attr <- attr |>
      dplyr::filter(!(field %in% header$field)) |>
      dplyr::bind_rows(header)
  }
  attr
}

gtab_add_factor <- function(.data, .meta, .retain_original = T, .suffix = '_coded_response') {

  df_names <- names(.data)
  meta_labels <- NULL
  meta_names <- NULL

  for(i in seq_along(df_names)) {

    df_name <- df_names[i]
    meta <- .meta[[df_name]]$valueset

    meta_label <- .meta[[df_name]]$label
    if(is.null(meta_label)) meta_label <- df_name |>
      stringr::str_to_sentence() |>
      stringr::str_replace_all('_', ' ')

    meta_labels <- c(meta_labels, meta_label)

    if(is.null(meta)) next
    if(!('value' %in% names(meta)) | !('label' %in% names(meta))) next

    df_name_original <- df_name

    if(.retain_original) {
      df_name_original <- paste0(df_name, .suffix)
      meta_labels <- c(meta_labels, meta_label)
    }

    .data <- .data |>
      dplyr::rename(value = !!as.name(df_name)) |>
      dplyr::mutate(value = as.character(value)) |>
      dplyr::left_join(
        meta |>
          dplyr::mutate(value = as.character(value)) |>
          dplyr::select(label, value) |>
          dplyr::add_row(
            value = 'Total',
            label = 'Total'
          ),
        by = 'value',
        multiple = 'first'
      ) |>
      dplyr::rename(
        !!as.name(df_name_original) := value,
        !!as.name(df_name) := label
      ) |>
      dplyr::relocate(
        dplyr::matches('^[Tt]otal'),
        dplyr::matches('^([Ff]requency|[Pp]ercent)'),
        .after = dplyr::any_of(c(df_name_original, df_name))
      )
  }

  if(is.null(meta_labels)) {
    meta_labels <- names(.data)
  }

  meta <- data.frame(
    order = 1:ncol(.data),
    field = names(.data),
    label = meta_labels
  ) |>
    dplyr::mutate(
      order = dplyr::if_else(
        field %in% c('level', 'area_code'),
        0L,
        order
      ),
      hidden = field %in% c('level', 'area_code') |
        grepl('_coded_response$', field),
      .after = order
    ) |>
    dplyr::arrange(order)

  for(i in seq_along(meta$field)) {
    name_i <- meta$field[i]
    attr(.data[[name_i]], 'label') <- meta$label[i]
  }

  attr(.data, "meta") <- meta
  class(.data) <- c('gtab_tbl', class(.data))

  .data |>
    dplyr::select(
      area_code,
      level,
      dplyr::everything()
    )
}

gtab_set_pivot_label <- function(.data, .x_cols, .pivot_cols_to_row_vs = NULL) {

  if(!is.null(.pivot_cols_to_row_vs)) {

    vs <- .pivot_cols_to_row_vs |>
      dplyr::filter(value %in% .x_cols) |>
      dplyr::select(
        COL__ = name,
        NAME__ = value,
        LABEL__ = label
      )

    .x_cols <- vs$COL__[1]

    .data <- .data |>
      dplyr::left_join(vs, by = 'NAME__', multiple = 'first') |>
      dplyr::select(-dplyr::any_of(c('COL__', 'NAME__'))) |>
      dplyr::rename(!!as.name(.x_cols) := LABEL__) |>
      dplyr::select(!!as.name(.x_cols), dplyr::everything())
  }

  return(.data)

}

gtab_add_total <- function(.data, .col, .total_by_col = F) {

  if(.total_by_col) {

    df <- .data |>
      dplyr::group_by(area_code) |>
      tidyr::nest() |>
      dplyr::mutate(
        data = purrr::map(data, \(x) {
          x |>
            janitor::adorn_totals(
              where = 'col',
              name = 'frequency__total'
            ) |>
            dplyr::mutate_at(
              dplyr::vars(dplyr::matches('^frequency')),
              list(PV_TOTAL_ALL_INTERNAL = ~ 100 * (. / sum(., na.rm = T)))
            ) |>
            janitor::adorn_totals(
              name = 'Total',
              fill = 'Total'
            )
        })
      ) |>
      tidyr::unnest(data) |>
      dplyr::ungroup()

  } else {

    df <- .data |>
      dplyr::group_by(area_code) |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(data, \(x) {
        x |>
          janitor::adorn_totals(
            c('row', 'col'),
            name = c('Total', 'total'),
            fill = 'Total'
          )
      })) |>
      tidyr::unnest(data) |>
      dplyr::ungroup() |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::matches('^frequency')),
        list(PV_TOTAL_ALL_INTERNAL = ~ 100 * (. / total))
      ) |>
      dplyr::select(
        area_code,
        dplyr::starts_with(.col),
        total,
        dplyr::everything()
      )

    df_names <- names(df)
    df_names <- df_names[grepl('^frequency', df_names)]
    if(length(df_names) == 1) {
      df_name <- df_names[1] |>
        stringr::str_replace('^frequency', 'percent')
      df <- df |>
        dplyr::rename(
          !!as.name(df_name) := PV_TOTAL_ALL_INTERNAL
        )
    }
  }

  df |>
    dplyr::rename_at(
      dplyr::vars(dplyr::ends_with('PV_TOTAL_ALL_INTERNAL')),
      ~ stringr::str_replace(., '^frequency', 'percent') |>
        stringr::str_remove('_PV_TOTAL_ALL_INTERNAL$')
    )
}

gtab_arrange <- function(.data, .x) {
  .data |>
    dplyr::mutate(v__ = stringr::str_remove_all(!!as.name(.x), '[^0-9]')) |>
    dplyr::arrange(as.integer(v__), !!as.name(.x)) |>
    dplyr::select(-v__)
}

