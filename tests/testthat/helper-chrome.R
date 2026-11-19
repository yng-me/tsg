# write_pdf() renders through webshot2/chromote; skip when headless Chrome cannot start.
skip_if_no_chrome <- function() {
  skip_if_not_installed("webshot2")
  skip_if_not_installed("chromote")
  ok <- tryCatch({
    chromote::default_chromote_object()
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ok, "headless Chrome is not available")
}
