

#' runHandwritingViewer
#' @description This function opens and runs a shiny app that allows for viewing of an object 
#' that comes from the `processHandwriting` function.`
#' 
#' @importFrom shiny runApp
#' 
#' @return None
#' @export
#' 
runHandwritingViewer = function()
{
  appDir <- system.file("ShinyHWViewer", package = "handwriter")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `handwriter`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}