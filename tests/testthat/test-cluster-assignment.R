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
