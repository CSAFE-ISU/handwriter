test_that("extractGraphs runs without errors", {
  
  expect_no_error(
    extractGraphs(source_folder = testthat::test_path("fixtures", "processHandwriting", "samples"),
                  save_folder = tempdir())
  )
  
})

test_that("getGraphs works on sample1.png", {
  # use the same fixtures as "processHandwriting works on sample1.png"
  actual <- getGraphs(image = testthat::test_path("fixtures", "processHandwriting", "samples", "sample1.png"),
                      save_folder = tempdir())
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  expect_equal(actual, expected, tolerance = 1e-08)
})

