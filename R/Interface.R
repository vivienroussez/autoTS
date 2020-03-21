#' Lauches the shiny demo app for testing the package from a simple CSV file
#' @export
#'
#'
runUserInterface <- function() {
  appDir <- system.file("shiny_app", "Viz_autoTS", package = "autoTS")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `autoTS`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
