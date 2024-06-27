save_proclist <- function(sample_name) {
  doc = list()
  doc$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
  doc$thin = thinImage(doc$image)
  doc$process = processHandwriting(doc$thin, dim(doc$image))
  doc$docname <- sample_name
  saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))
}

samples <- paste0("sample", 1:4)
for (sample in samples) {
  save_proclist(sample)
}
