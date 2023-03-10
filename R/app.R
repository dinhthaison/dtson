#' @import shiny
#' @export
run_dtson <- function() {
  shiny::runApp(system.file(" app.R", package = "dtson"))
}
