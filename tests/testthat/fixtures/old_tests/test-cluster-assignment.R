test_that("get_clusters_batch works sequentially", {
  unlink(file.path(tempdir(), "clusters"), recursive = TRUE)
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "processHandwriting", "graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(7, 7),
                               doc_indices = c(9, 16),
                               num_cores = 1)
  expected <- readRDS(testthat::test_path("fixtures", "clusters", "clusters.rds"))
  expect_identical(actual, expected)
})

test_that("get_clusters_batch works in parallel", {
  unlink(file.path(tempdir(), "clusters"), recursive = TRUE)
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "processHandwriting", "graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(7, 7),
                               doc_indices = c(9, 16),
                               num_cores = 2)
  expected <- readRDS(testthat::test_path("fixtures", "clusters", "clusters.rds"))
  expect_identical(actual, expected)
})

test_that("get_clusters_batch works sequentially if cluster assignments already exist", {
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "processHandwriting", "graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(7, 7),
                               doc_indices = c(9, 16),
                               num_cores = 1)
  expected <- readRDS(testthat::test_path("fixtures", "clusters", "clusters.rds"))
  expect_identical(actual, expected)
})

test_that("get_clusters_batch works in parallel if cluster assignments already exist", {
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "processHandwriting", "graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(7, 7),
                               doc_indices = c(9, 16),
                               num_cores = 1)
  expected <- readRDS(testthat::test_path("fixtures", "clusters", "clusters.rds"))
  expect_identical(actual, expected)
})

test_that("get_clusters_batch works sequentially without writer or doc indices", {
  unlink(file.path(tempdir(), "clusters"), recursive = TRUE)
  actual <- get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "processHandwriting", "graphs"),
    file.path(tempdir(), "clusters"))
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "clusters_wo_indices.rds"))
  
  expect_identical(actual, expected)
})

test_that("get_clusters_batch works in parallel without writer or doc indices", {
  unlink(file.path(tempdir(), "clusters"), recursive = TRUE)
  actual <- get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "processHandwriting", "graphs"),
    file.path(tempdir(), "clusters"))
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "clusters_wo_indices.rds"))
  
  expect_identical(actual, expected)
})

testthat::test_that("get_clusters_batch throws expected errors", {
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "processHandwriting", "graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = c(1,2)),
    "num_cores is longer than 1")
  
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "processHandwriting", "graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = "all"),
    "num_cores is not numeric")
  
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "processHandwriting", "graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = 0.5),
    "num_cores is not an integer")
  
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "processHandwriting", "graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = 0),
    "num_cores is not greater than or equal to 1")
})

testthat::test_that("get_clusters_batch moves problem file problem_files folder", {
  empty_tempdir(subfolder = "clusters")
  
  # Make a file without a docname
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  doc$docname <- NULL
  saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname", "sample1_proclist.rds"))
  
  testthat::expect_message(get_clusters_batch(
    template = example_cluster_template,
    input_dir = testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 1))
  
  # Check that file was moved to problem_files folder
  testthat::expect_true(file.exists(file.path(tempdir(), "clusters", "problem_files", "sample1_proclist.rds")))
  testthat::expect_false(file.exists(file.path(testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname", "sample1_proclist.rds"))))
})

testthat::test_that("get_clusters_batch throws warning if file doesn't contain graphs and not running in parallel", {
  empty_tempdir(subfolder = "clusters")
  
  # Make a file without a docname
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  doc$process$letterList <- NULL
  saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname", "sample1_proclist.rds"))
  
  testthat::expect_warning(get_clusters_batch(
    template = example_cluster_template,
    input_dir = testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 1), "The document does not contain any graphs.")
})

testthat::test_that("get_clusters_batch returns NULL if file doesn't contain graphs and running in parallel", {
  empty_tempdir(subfolder = "clusters")
  
  # Make a file without a docname
  doc <- readRDS(testthat::test_path("fixtures", "processHandwriting", "graphs", "sample1_proclist.rds"))
  doc$process$letterList <- NULL
  saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname", "sample1_proclist.rds"))
  
  actual <- get_clusters_batch(
    template = example_cluster_template,
    input_dir = testthat::test_path("fixtures", "processHandwriting", "graphs_missing_docname"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 2)
  testthat::expect_null(actual)
})
