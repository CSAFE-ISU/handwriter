test_that("processHandwriting works on sample1.png", {
  actual <- list()
  actual$sample <- readPNGBinary(test_path("fixtures", "processHandwriting", "samples", "sample1.png"))
  actual$thin <- thinImage(actual$sample)
  actual$process <- processHandwriting(actual$thin, dim(actual$sample))
  actual$docname <- 'sample1'
  
  expected <- readRDS(test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  expect_identical(actual, expected)
})

test_that("processHandwriting works on sample2.png", {
  actual <- list()
  actual$sample <- readPNGBinary(test_path("fixtures", "processHandwriting", "samples", "sample2.png"))
  actual$thin <- thinImage(actual$sample)
  actual$process <- processHandwriting(actual$thin, dim(actual$sample))
  actual$docname <- 'sample2'
  
  expected <- readRDS(test_path("fixtures", "processHandwriting", "graphs", "sample2_proclist.rds"))
  
  expect_identical(actual, expected)
})

