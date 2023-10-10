#' Handwriter Example
#' 
#' List the files in handwriter's "extdata" folder if the path argument is NULL. If
#' the path argument is the filename of the a file in handwriter's "extdata" folder, 
#' then the function returns the absolute file path to that file.
#'
#' @param path 
#'
#' @return Filenames or filepath
#' @export
#'
#' @examples
#' handwriter_example()
#' handwriter_example("phrase_example.png")
handwriter_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "handwriter"))
  } else {
    system.file("extdata", path, package = "handwriter", mustWork = TRUE)
  }
}