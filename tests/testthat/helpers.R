# Delete a subfolder of tempdir()
empty_tempdir <- function(subfolder) {
  unlink(file.path(tempdir(), subfolder), recursive = TRUE)
}

# Copy png and rds files from one folder to another
copy_files <- function(output_dir, input_dir, extensions = ".png|.rds") {
  create_dir(output_dir, recursive = TRUE)
  if (is.null(extensions)) {
    files <- list.files(input_dir)
  } else {
    files <- list.files(input_dir, pattern = extensions)
  }
  file.copy(file.path(input_dir, files), file.path(output_dir, files))
}

# create_dummy_image_list, creates a list of dummy data, 
# which is often useful for testing purposes. The function takes two arguments: is_proto, 
# which is a boolean indicating whether the output should be in "proto" format or not, 
# and num_paths, which is the number of paths to be created.
create_dummy_image_list <- function(is_proto = TRUE, num_paths = 1) {
  numPathCuts <- 5
  path_ends <- matrix(1:(4 * num_paths), nrow = num_paths, byrow = TRUE)
  path_quarters <- matrix(1:(2 * (numPathCuts - 1) * num_paths), nrow = num_paths, byrow = TRUE)
  path_center <- matrix(1:(2 * num_paths), nrow = num_paths, byrow = TRUE)
  lengths <- 1:num_paths
  
  if (is_proto) {
    image_list <- list(
      pathEnds = path_ends,
      pathQuarters = path_quarters,
      pathCenter = path_center,
      lengths = lengths
    )
  } else {
    pathends0 = array(path_ends, dim = c(2, 2, num_paths))
    image_list <- list(
      allPaths = lapply(1:num_paths, function(x) 1:x),
      pathEndsrc = lapply(1:num_paths, function(x) pathends0[,,x]),
      pathQuarters = path_quarters,
      pathCenter = path_center,
      lengths = lengths,
      centroid = c(5, 5),
      image = matrix(0, nrow = 10, ncol = 10)
    )
  }
  
  return(image_list)
}

# enumerate the variables in fitted model created by fit_model()
list_model_variables <- function(num_writers, num_clusters) {
  etas <- gammas <- mus <- pis <- taus <- c()
  for (j in 1:num_clusters) {
    etas <- c(etas, paste0("eta[", j, "]"))
    gammas <- c(gammas, paste0("gamma[", j, "]"))
    for (i in 1:num_writers) {
      mus <- c(mus, paste0("mu[", i, ",", j, "]"))
      pis <- c(pis, paste0("pi[", i, ",", j, "]"))
      taus <- c(taus, paste0("tau[", i, ",", j, "]"))
    }
  }
  variables <- c(etas, gammas, mus, pis, taus)
  return(variables)
}