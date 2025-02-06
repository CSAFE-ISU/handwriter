# test processDocument ----------------------------------------------------

test_that("processDocument works on w0016_s01_pLND_r01.png", {
  # use the same fixtures as "processHandwriting works on sample1.png"
  actual <- processDocument(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"))
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0016_s01_pLND_r01_proclist.rds"))
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processDocument works on w0080_s01_pLND_r01.png", {
  actual <- processDocument(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0080_s01_pLND_r01.png"))
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0080_s01_pLND_r01_proclist.rds"))
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processDocument works on w0124_s01_pLND_r01.png", {
  actual <- processDocument(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0124_s01_pLND_r01.png"))
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0124_s01_pLND_r01_proclist.rds"))
  expect_equal(actual, expected, tolerance = 1e-08)
})


# test processHandwriting -------------------------------------------------

test_that("processHandwriting works on w0016_s01_pLND_r01.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'w0016_s01_pLND_r01'
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0016_s01_pLND_r01_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processHandwriting works on w0080_s01_pLND_r01.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0080_s01_pLND_r01.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'w0080_s01_pLND_r01'
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0080_s01_pLND_r01_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})

test_that("processHandwriting works on w0124_s01_pLND_r01.png", {
  actual <- list()
  actual$image <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0124_s01_pLND_r01.png"))
  actual$thin <- thinImage(actual$image)
  actual$process <- processHandwriting(actual$thin, dim(actual$image))
  actual$docname <- 'w0124_s01_pLND_r01'
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0124_s01_pLND_r01_proclist.rds"))
  
  expect_equal(actual, expected, tolerance = 1e-08)
})
