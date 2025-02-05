# Delete a subfolder of tempdir()
empty_tempdir <- function(subfolder) {
  unlink(file.path(tempdir(), subfolder), recursive = TRUE)
}