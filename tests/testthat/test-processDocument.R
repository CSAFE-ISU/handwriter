test_that("processDocument works on sample1.png", {
  # use the same fixtures as "processHandwriting works on sample1.png"
  actual <- processDocument(testthat::test_path("fixtures", "processHandwriting", "samples", "sample1.png"))
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  expect_equal(actual, expected)
})

test_that("processDocument works on sample2.png", {
  # use the same fixtures as "processHandwriting works on sample2.png"
  actual <- processDocument(testthat::test_path("fixtures", "processHandwriting", "samples", "sample2.png"))
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample2_proclist.rds"))
  expect_equal(actual, expected)
})

test_that("processDocument works on sample3.png", {
  # use the same fixtures as "processHandwriting works on sample3.png"
  actual <- processDocument(testthat::test_path("fixtures", "processHandwriting", "samples", "sample3.png"))
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample3_proclist.rds"))
  expect_equal(actual, expected)
})
