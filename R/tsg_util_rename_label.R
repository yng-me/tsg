tsg_util_rename_label <- function(data, label, var) {

  `:=` <- NULL

  data
  if(!is.null(label)) {
    data |> dplyr::rename((!!as.name(label)) := {{var}})
  }
}
