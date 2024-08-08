test_that("get_clusters_batch works sequentially", {
  unlink(file.path(tempdir(), "clusters"), recursive = TRUE)
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "processHandwriting", "graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(7, 7),
                               doc_indices = c(9, 16),
                               num_cores = 1)
  actual <- readRDS(file.path(tempdir(), "clusters", "all_clusters.rds"))
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
  actual <- readRDS(file.path(tempdir(), "clusters", "all_clusters.rds"))
  expected <- readRDS(testthat::test_path("fixtures", "clusters", "clusters.rds"))
  expect_identical(actual, expected)
})

