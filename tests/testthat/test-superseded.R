testthat::test_that("extractGraphs runs without errors", {
  
  testthat::expect_no_error(
    extractGraphs(source_folder = testthat::test_path("fixtures", "processHandwriting", "samples"),
                  save_folder = tempdir())
  )
  
})

testthat::test_that("read_and_process works on sample1.png", {
  # use the same fixtures as "processHandwriting works on sample1.png"
  actual <- read_and_process(
    image_name = testthat::test_path("fixtures", "processHandwriting", "samples", "sample1.png"),
    transform_output = "document"
  )
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  # Add file extension to expected docname to match actual docname
  expected$docname <- paste0(expected$docname, ".png")

  testthat::expect_equal(actual, expected, tolerance = 1e-08)
})

testthat::test_that("getGraphs works on sample1.png", {
  # use the same fixtures as "processHandwriting works on sample1.png"
  actual <- getGraphs(image = testthat::test_path("fixtures", "processHandwriting", "samples", "sample1.png"),
                      save_folder = tempdir())
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  testthat::expect_equal(actual, expected, tolerance = 1e-08)
})

