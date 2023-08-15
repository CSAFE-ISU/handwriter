sample_name <- 'sample1'
doc = list()
doc$sample <- readPNGBinary(test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
doc$thin = thinImage(doc$sample)
doc$process = processHandwriting(doc$thin, dim(doc$sample))
doc$docname <- sample_name
saveRDS(doc, test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))

sample_name <- 'sample2'
doc = list()
doc$sample <- readPNGBinary(test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
doc$thin = thinImage(doc$sample)
doc$process = processHandwriting(doc$thin, dim(doc$sample))
doc$docname <- sample_name
saveRDS(doc, test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))
