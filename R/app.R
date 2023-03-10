#' @import shiny
#' @export
run_dtson <- function() {
  shiny::runApp(system.file("shiny", package = "dtson"))
}
