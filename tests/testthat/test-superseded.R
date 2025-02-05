testthat::test_that("extractGraphs runs without errors", {
  empty_tempdir("graphs")
  
  make_dir(file.path(tempdir(), "graphs", "w0016"), recursive = TRUE)
  make_dir(file.path(tempdir(), "graphs", "w0080"))
  make_dir(file.path(tempdir(), "graphs", "w0124"))
  make_dir(file.path(tempdir(), "graphs", "w0138"))
  make_dir(file.path(tempdir(), "graphs", "w0229"))
  
  testthat::expect_no_error(
    extractGraphs(source_folder = testthat::test_path("fixtures", "temp1qd", "data", "template_docs"),
                  save_folder = file.path(tempdir(), "graphs"))
  )
})

testthat::test_that("read_and_process works on w0016_s01_pLND_r01.png", {
  actual <- read_and_process(
    image_name = testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"),
    transform_output = "document"
  )
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0016_s01_pLND_r01_proclist.rds"))
  
  # Add file extension to expected docname to match actual docname
  expected$docname <- paste0(expected$docname, ".png")

  testthat::expect_equal(actual, expected, tolerance = 1e-08)
})

testthat::test_that("getGraphs works on w0016_s01_pLND_r01.png", {
  actual <- getGraphs(image = testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"),
                      save_folder = tempdir())
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_graphs", "w0016_s01_pLND_r01_proclist.rds"))
  testthat::expect_equal(actual, expected, tolerance = 1e-08)
})
