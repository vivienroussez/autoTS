#' Demo graphical user interface
#'
#' @description A shiny application that allows the user to load a properly formated CSV file,
#' benchmark the algorithms, make a prediction and download the results. Requires additional packages
#' shiny, shinycssloaders, tidyr and plotly to be installed
#'
#' @export
#' @examples
#' \dontrun{
#' autoTS::runUserInterface()
#' }
#'
#'
runUserInterface <- function() {
  appDir <- system.file("shiny_app", "Viz_autoTS", package = "autoTS")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `autoTS`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
