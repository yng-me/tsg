tsg_util_as_string <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
}
