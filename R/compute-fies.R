#' Title
#'
#' @param .data
#' @param .x_cols
#' @param ...
#' @param .type
#' @param .agg_var
#' @param .agg_area_level
#' @param .agg_area_length
#'
#' @return
#' @export
#'
#' @examples
#'

compute_fies <- function(
  .data,
  .x_cols,
  ...,
  .type = 'prevalence',
  .agg_var = 'barangay_geo',
  .agg_area_level = NULL,
  .agg_area_length = c(2, 5, 7, 10)
) {

  .data <- .data |>
    dplyr::select(dplyr::any_of(c(.agg_var, .x_cols))) |>
    dplyr::collect()

  agg_area_length <- .agg_area_length[.agg_area_level]

  if(is.null(.agg_area_level)) {

    fies <- .data |>
      dplyr::select(dplyr::any_of(.x_cols)) |>
      dplyr::mutate_all(~ dplyr::if_else(. == 1, 1L, 0L)) |>
      compute_food_insecurity(.type)

    df <- fies[[.type]] |>
      dplyr::mutate(
        level = 0L,
        area_code = paste0(
          rep('0', .agg_area_length[length(.agg_area_length)]),
          collapse = ''
        ),
        .before = 1
      )

  } else {

    df_list <- list()

    for(j in seq_along(agg_area_length)) {

      pad_width <- .agg_area_length[length(.agg_area_length)]

      df_list[[paste0('v', agg_area_length[j])]] <- .data |>
        dplyr::mutate(
          area_code = stringr::str_sub(!!as.name(.agg_var), 1, agg_area_length[j]) |>
            stringr::str_pad(width = pad_width, pad = '0', side = 'right')
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-dplyr::any_of(.agg_var)) |>
        dplyr::group_by(area_code) |>
        tidyr::nest() |>
        dplyr::mutate(
          data = purrr::map(data, \(x) {
            x |>
              dplyr::mutate_all(~ dplyr::if_else(. == 1, 1L, 0L)) |>
              compute_food_insecurity(.type)
          })
        ) |>
        dplyr::ungroup() |>
        tidyr::unnest(data) |>
        dplyr::mutate(
          level = as.integer(.agg_area_level[j]),
          .before = 1
        )
    }

    df <- dplyr::bind_rows(df_list)

  }

  df <- do.call(paste0('set_fies_meta_', .type), list(.data = df))

  list(
    DEFAULT = list(
      df = df,
      meta = gtab_get_meta(df, dplyr::everything()),
      header = attributes(df)$header,
      attr = list(
        agg_area_level = .agg_area_level,
        retain_header = TRUE
      )
    )
  )
}



compute_food_insecurity <- function(.data, .type = 'prevalence') {
  rm <- RM.weights::RM.w(.data)
  do.call(paste0('get_fies_', .type), list(
    rm = rm,
    df = .data
  ))
}

get_fies_prevalence <- function(rm, df) {

  rm_prob <- apply_equating(rm)
  n <- nrow(df)
  mod_sev <- rm_prob$prevs[[1]]
  sev <- rm_prob$prevs[[2]]

  rm_by_prevalence <- tibble::tibble(
    food_insecurity = c('Moderate and severe', 'Severe'),
    total = c(n, n),
    frequency = c(as.integer(round(n * mod_sev)), as.integer(round(n * sev))),
    prevalence = c(100 * mod_sev, 100 * sev)
  )

  attr(rm_by_prevalence, 'reliability') <- rm$reliab
  attr(rm_by_prevalence, 'reliability_equal_weights') <- rm$reliab.fl

  return(rm_by_prevalence)

}


set_fies_meta_prevalence <- function(.data) {

  labels <- c(
    'Level',
    'Area code',
    'Food insecurity measure',
    'Total',
    'Frequency',
    'Prevalence'
  )

  df_names <- names(.data)
  for(i in seq_along(df_names)) {
    attr(.data[[df_names[i]]], 'names') <- NULL
    attr(.data[[df_names[i]]], 'label') <- labels[i]
  }

  attr(.data, 'header') <- data.frame(
    order = 1:6,
    hidden = c(TRUE, TRUE, rep(FALSE, 4)),
    type = c('i', 'nc', 'c', 'i', 'i', 'd'),
    field = df_names,
    label = labels
  )

  return(.data)
}

get_fies_item <- function(rm, df) {

  rm_item_b <- rm$b |> tibble::as_tibble_col("item_severity")
  rm_item_se.b <- rm$se.b |> tibble::as_tibble_col("item_severity_se")
  rm_item_infit <- rm$infit |> tibble::as_tibble_col("infit_stat")
  rm_item_se.infit <- rm$se.infit |> tibble::as_tibble_col("infit_stat_se")
  rm_item_outfit <- rm$outfit |> tibble::as_tibble_col("outfit_stat")

  tibble::as_tibble_col(names(df), "item") |>
    tibble::add_column(rm_item_b) |>
    tibble::add_column(rm_item_se.b) |>
    tibble::add_column(rm_item_infit) |>
    tibble::add_column(rm_item_se.infit) |>
    tibble::add_column(rm_item_outfit) |>
    dplyr::mutate(
      item = stringr::str_remove_all(item, '[a-zA-Z]\\d{2}_') |>
        stringr::str_replace_all('_', ' ') |>
        stringr::str_to_sentence()
    )
}

set_fies_meta_item <- function(.data) {

  labels <- c(
    'Level',
    'Area code',
    'Item',
    'Item severity',
    'Item severity (standard error)',
    'Infit statistics',
    'Infit statistics (standard error)',
    'Outfit statistics'
  )

  df_names <- names(.data)

  for(i in seq_along(df_names)) {
    attr(.data[[df_names[i]]], 'names') <- NULL
    attr(.data[[df_names[i]]], 'label') <- labels[i]
  }

  attr(.data, 'header') <- data.frame(
    order = 1:8,
    hidden = c(TRUE, TRUE, rep(FALSE, 6)),
    type = c('i', 'nc', 'c', rep('d', 5)),
    field = df_names,
    label = labels
  )

  return(.data)
}

get_fies_score <- function(rm, df) {

  rm_score_a <- rm$a |> tibble::as_tibble_col("severity_raw_score")
  rm_score_se.a <- rm$se.a |> tibble::as_tibble_col("severity_raw_score_se")
  rm_score_wt.rs <- rm$wt.rs |> tibble::as_tibble_col("total_hh")

  rm_score_wt.rel.rs <- rm$wt.rel.rs |>
    tibble::as_tibble_col("percent") |>
    dplyr::mutate(percent = percent * 100)

  tibble::as_tibble_col(c(0:8), "raw_score") |>
    dplyr::mutate(raw_score = as.character(raw_score)) |>
    tibble::add_column(rm_score_a) |>
    tibble::add_column(rm_score_se.a)

}

set_fies_meta_score <- function(.data) {

  labels <- c(
    'Level',
    'Area code',
    'Raw score',
    'Severity raw score',
    'Severity raw score (standard error)'
  )

  df_names <- names(.data)

  for(i in seq_along(df_names)) {
    attr(.data[[df_names[i]]], 'names') <- NULL
    attr(.data[[df_names[i]]], 'label') <- labels[i]
  }

  attr(.data, 'header') <- data.frame(
    order = 1:5,
    hidden = c(TRUE, TRUE, rep(FALSE, 3)),
    type = c('i', 'nc', 'c', 'd', 'd'),
    field = df_names,
    label = labels
  )

  return(.data)
}

apply_equating <- function(
    rr1,
    st = NULL,
    tol = 0.35,
    spec.com1 = 1:8,
    spec.com2 = 1:8,
    thres = NULL,
    maxuniq = 3,
    write.file = FALSE,
    plot = FALSE,
    iterative = TRUE,
    excl.prior1 = NULL,
    excl.prior2 = NULL,
    wt.spec = NULL
) {

  tol2 = tol
  b1 = rr1$b
  se1 = rr1$se.b

  if (is.null(st)) {
    st = c(
      -1.2230564,
      -0.847121,
      -1.1056616,
      0.3509848,
      -0.3117999,
      0.5065051,
      0.7546138,
      1.8755353
    )
  }
  b.tot = st
  if (is.null(thres)) {
    thres = b.tot[c(5, 8)]
    truetresh = T
  } else {
    truetresh = F
  }

  common = rep(F, length(b1))
  common[spec.com1] = T
  plus = sum(!common)
  common2 = rep(F, length(b.tot))
  common2[spec.com2] = T
  plus2 = sum(!common2)
  b.st1 = (b1 - mean(b1[spec.com1]))/sd(b1[spec.com1]) * sd(b.tot[spec.com2]) +
    mean(b.tot[spec.com2])
  a = 1
  oldcommon = common
  oldcommon2 = common2
  if (iterative) {
    while (a <= length(b.st1)) {
      oldcommon = common
      oldcommon2 = common2
      diff = rep(100, length(b1))
      diff[spec.com1] = abs(b.st1[spec.com1] - b.tot[spec.com2])
      diff2 = rep(100, length(b.tot))
      diff2[spec.com2] = abs(b.st1[spec.com1] - b.tot[spec.com2])
      ord = order(diff, decreasing = T)
      w = ord[a + plus]
      ord2 = order(diff2, decreasing = T)
      w2 = ord2[a + plus2]
      if (diff[w] >= tol) {
        common[w] = F
      }
      else {
        common[w] = T
      }
      if (diff2[w2] >= tol) {
        common2[w2] = F
      }
      else {
        common2[w2] = T
      }
      scale1 = sd(b.tot[common2])/sd(b.st1[common])
      shift1 = mean(b.tot[common2]) - mean(b.st1[common]) *
        scale1
      b.st1 = shift1 + b.st1 * scale1
      if (sum(oldcommon == common) == length(b.st1) |
          sum(!common) > maxuniq)
        break
      else {
        a = a + 1
      }
    }
  }
  if (!iterative) {
    common[excl.prior1] = F
    common2[excl.prior2] = F
  }
  scale = sd(b.tot[common2])/sd(b1[common])
  shift = mean(b.tot[common2]) - mean(b1[common]) * scale


  newthres = (thres - shift)/scale
  a = rr1$a
  se.a = rr1$se.a
  d = rr1$d
  kk = length(b1) + 1
  if (length(d) > 2) {
    if (d[2] < 0) {
      a = a[c(1, 2:(kk + 1))]
      se.a = se.a[c(1, 2:(kk + 1))]
    }
    else {
      a = a[-kk]
      se.a = se.a[-kk]
    }
  }

  prevs.rs = matrix(NA, length(a), length(newthres))
  wt = rr1$wt
  if (!is.null(wt.spec))
    wt = wt.spec
  XX = rr1$XX
  rv = rowSums(XX)
  k = ncol(XX)
  n_j = sapply(0:(k), function(i) sum(wt[rv == i], na.rm = T))
  f_j = n_j/sum(n_j)
  f_j[1] = 0
  for (i in 1:length(newthres)) {
    prevs.rs[, i] = (1 - pnorm(newthres[i], a, se.a)) *
      f_j
  }

  prevs = colSums(prevs.rs)

  return(
    list(
      prevs = prevs,
      prevs.rs = prevs.rs
    )
  )
}
