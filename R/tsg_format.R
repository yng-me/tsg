format_percent <- function(
  data,
  as_proportion = FALSE,
  name_separator = "_",
  label_separator = "__",
  convert_factor = TRUE
) {

  cols <- names(data)

  freq_prefix <- paste0("frequency", name_separator)
  prop_prefixes <- c(paste0("percent", name_separator), paste0("proportion", name_separator))
  freq_label_prefix <- paste0("Frequency", label_separator)

  freq <- cols[startsWith(cols, freq_prefix)]
  prop <- cols[startsWith(cols, prop_prefixes[1]) | startsWith(cols, prop_prefixes[2])]

  if(length(freq) != length(prop)) {
    stop('Mismatched frequency and percent columns')
  }

  cols_start <- cols[!(cols %in% c(freq, prop))]
  df_combined <- dplyr::select(data, dplyr::any_of(cols_start))

  if(convert_factor) {

    for(i in cols_start) {

      label_i <- attributes(data[[i]])$label

      if(!is.null(label_i)) {
        df_combined <- remove_labels(
          dplyr::rename(df_combined, !!as.name(label_i) := !!as.name(i))
        )
      }

    }

  }


  for(i in seq_along(freq)) {

    col <- freq[i]
    pct <- prop[i]
    col_i <- substr(col, nchar(freq_prefix) + 1L, nchar(col))

    label_i <- attributes(data[[col]])$label
    if(!is.null(label_i)) {
      label_i <- substr(label_i, nchar(freq_label_prefix) + 1L, nchar(label_i))
    } else {
      label_i <- col_i
    }

    if(convert_factor) { col_i <- label_i }

    df_i <- dplyr::transmute(
      data,
      !!as.name(col_i) := paste0(
        formatC(!!as.name(col), big.mark = ","),
        " (",
        round(!!as.name(pct), 2),
        "%)"
      )
    )

    if(!convert_factor & !is.null(attributes(data[[col]])$label)) {
      attr(df_i[[col_i]], 'label') <- label_i
    }

    df_combined <- dplyr::bind_cols(df_combined, df_i)

  }

  df_combined

}
