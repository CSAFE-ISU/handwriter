test_that("processHandwriting works on sample1.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", "sample1.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'sample1'
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processHandwriting works on sample2.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", "sample2.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'sample2'
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample2_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processHandwriting works on sample3.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", "sample3.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'sample3'
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample3_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processHandwriting works on sample4.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", "sample4.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'sample4'
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample4_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})

