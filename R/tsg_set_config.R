tsg_set_config <- function(config_file, cwd = NULL) {

  valid_type_ext <- c('yml', 'json')
  ext <- stringr::str_extract('\\..*$')

  if(!(ext %in% valid_type_ext)) {
    stop("Accepts valid file type only for the config. Use either '.yml' or '.json'.")
  }

  if(!is.null(cwd)) {
    source(paste0(cwd, '/', config_file))
  } else {
    source(config_file)
  }
}
