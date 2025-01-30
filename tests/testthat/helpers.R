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

# Delete a subfolder of tempdir()
empty_tempdir <- function(subfolder) {
  unlink(file.path(tempdir(), subfolder), recursive = TRUE)
}


# copy graphs and docs from fixtures > template to tempdir
files2tempdir <- function(type = "template") {
  copy2tempdir <- function(type = "template", type2 = "docs", ext = ".png") {
    fixtures_dir <- testthat::test_path("fixtures", "template", "data", paste0(type, "_", type2))
    files <- list.files(fixtures_dir, pattern = ext)
    
    temp_dir <- file.path(tempdir(), "main_dir", "data", paste0(type, "_", type2))
    create_dir(temp_dir, recursive = TRUE)
    file.copy(file.path(fixtures_dir, files), file.path(temp_dir, files), overwrite = TRUE)
  }
  
  copy2tempdir(type = type, type2 = "docs", ext = ".png")
  copy2tempdir(type = type, type2 = "graphs", ext = ".rds")
  if (type %in% c("model", "questioned")) {
    copy2tempdir(type = type, type2 = "clusters", ext = ".rds")
    
    # copy template to tempdir > main_dir > data
    saveRDS(example_cluster_template, file.path(tempdir(), 'main_dir', 'data', 'template.rds'))
  }
}
