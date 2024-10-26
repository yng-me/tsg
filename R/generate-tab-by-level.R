#' Generate by aggregation level
#'
#' @param .data
#' @param .x_cols
#' @param ...
#' @param .agg_var
#' @param .agg_area_level
#' @param .agg_area_length
#' @param .total_by_col
#' @param .reducer
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
  .reducer = NULL
) {

  df_all <- list()

  y_cols <- sapply(substitute(list(...))[-1], deparse)
  agg_area_length <- .agg_area_length[.agg_area_level]

  if(.switch_col & length(y_cols) > 0) { x_col <- y_cols[1] }

  for(i in seq_along(.x_cols)) {

    df_list <- list()

    if(.switch_col & length(y_cols) > 0) {
      y_cols <- .x_cols[i]
    } else {
      x_col <- .x_cols[i]
    }

    if(length(y_cols) > 0) {
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
          .total_by_col = .total_by_col,
          .reducer = .reducer
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
            .total_by_col = .total_by_col,
            .reducer = .reducer
          ) |>
          dplyr::mutate(
            level = as.integer(.agg_area_level[j]),
            .before = 1
          )
      }

      df <- dplyr::bind_rows(df_list) |>
        gtab_add_factor(meta)
    }

    df_all[[.x_cols[i]]] <- list(
      df = df,
      meta = meta,
      header = gtab_create_header(df, meta, y_cols),
      attr = list(agg_area_level = .agg_area_level)
    )
  }

  return(df_all)

}


gtab_frequency <- function(.data, .x, ..., .y = list(), .total_by_col = FALSE, .reducer = NULL) {

  if(!is.null(.reducer)) {
    func <- eval(as.name(.reducer))
    .data <- .data |> func(.x, ..., .y)
  } else {

    if(length(.y) > 0) {

      .data <- .data |>
        dplyr::group_by(..., !!as.name(.x), .add = T) |>
        dplyr::count(name = 'frequency') |>
        dplyr::ungroup() |>
        dplyr::collect() |>
        gtab_arrange(.x) |>
        tidyr::pivot_wider(
          names_from = dplyr::any_of(.y),
          values_from = frequency,
          names_sort = T,
          names_sep = '__',
          values_fill = 0,
          names_expand = T,
          names_prefix = 'frequency__'
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
  }


  return(.data)

}

gtab_get_meta <- function(.data, .x, ...) {

  attr_df <- .data |>
    head(1) |>
    dplyr::select({{.x}}, ..., dplyr::any_of(dplyr::group_vars(.data))) |>
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
        by = 'value'
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




