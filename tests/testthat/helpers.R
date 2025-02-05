# Delete a subfolder of tempdir()
empty_tempdir <- function(subfolder) {
  unlink(file.path(tempdir(), subfolder), recursive = TRUE)
}

# Copy png and rds files from one folder to another
copy_files <- function(output_dir, input_dir) {
  create_dir(output_dir, recursive = TRUE)
  files <- list.files(input_dir, pattern = ".png|.rds")
  file.copy(file.path(input_dir, files), file.path(output_dir, files))
}
