#' @importFrom iSEE panelDefaults
.onLoad <- function(libname, pkgname) {
  
  # Set panel defaults
  panelDefaults(
    RoundDigits = FALSE,
    SignifDigits = 3L
  )
  
  invisible()
}
