testthat::test_that("plotImage runs without errors", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  testthat::expect_no_error(plotImage(doc))
})

testthat::test_that("plotImageThinned runs without errors", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  testthat::expect_no_error(plotImageThinned(doc))
})

testthat::test_that("plotNodes runs without errors", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  testthat::expect_no_error(plotNodes(doc, plot_break_pts = TRUE))
  testthat::expect_no_error(plotNodes(doc, plot_break_pts = FALSE))
})

testthat::test_that("plotLine runs without errors", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  dims = dim(doc$image)
  testthat::expect_no_error(plotLine(doc$process$letterList, 1, dims))
})

testthat::test_that("plotLine throws an error if non-existant line is selected", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  dims = dim(doc$image)
  testthat::expect_error(plotLine(doc$process$letterList, 800, dims), "no letters found on that path - valid lines are 1:max")
})

testthat::test_that("plotLetter runs without errors", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  testthat::expect_no_error(plotLetter(doc, 1, showPaths = FALSE))
  testthat::expect_no_error(plotLetter(doc, 1, showPaths = TRUE))
  
  testthat::expect_no_error(plotLetter(doc, 1, showCentroid = FALSE))
  testthat::expect_no_error(plotLetter(doc, 1, showCentroid = TRUE))
  
  testthat::expect_no_error(plotLetter(doc, 1, showSlope = FALSE))
  testthat::expect_no_error(plotLetter(doc, 1, showSlope = TRUE))
  
  testthat::expect_no_error(plotLetter(doc, 1, showNodes = FALSE))
  testthat::expect_no_error(plotLetter(doc, 1, showNodes = TRUE))
})

testthat::test_that("plot_graphs runs without errors", {
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  
  testthat::expect_no_error(plot_graphs(doc))
})


