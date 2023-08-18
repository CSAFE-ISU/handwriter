sample_name <- 'sample1'
doc = list()
doc$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
doc$thin = thinImage(doc$image)
doc$process = processHandwriting(doc$thin, dim(doc$image))
doc$docname <- sample_name
saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))

sample_name <- 'sample2'
doc = list()
doc$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
doc$thin = thinImage(doc$image)
doc$process = processHandwriting(doc$thin, dim(doc$image))
doc$docname <- sample_name
saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))

sample_name <- 'sample3'
doc = list()
doc$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
doc$thin = thinImage(doc$image)
doc$process = processHandwriting(doc$thin, dim(doc$image))
doc$docname <- sample_name
saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))
