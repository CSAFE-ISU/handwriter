# get_cluster_fill_counts -------------------------------------------------

testthat::test_that("Get cluster fill counts works without writer or doc indices", {
  clusters <- readRDS(testthat::test_path("fixtures", "processHandwriting", "clusters_wo_indices.rds"))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "counts_wo_indices.rds"))
  
  expect_equal(actual, expected)
  
})

testthat::test_that("Get cluster fill counts works with writer and doc indices", {
  clusters <- readRDS(testthat::test_path("fixtures", "processHandwriting", "clusters.rds"))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "counts.rds"))
  
  expect_equal(actual, expected)
  
})

# get_cluster_fill_rates -------------------------------------------------

testthat::test_that("Get cluster fill rates works without writer or doc indices", {
  clusters <- readRDS(testthat::test_path("fixtures", "processHandwriting", "clusters_wo_indices.rds"))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "rates_wo_indices.rds"))
  
  expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill rates works with writer and doc indices", {
  clusters <- readRDS(testthat::test_path("fixtures", "processHandwriting", "clusters.rds"))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "rates.rds"))
  
  expect_equal(actual, expected)
})


# get_writer_profiles -----------------------------------------------------

testthat::test_that("Get writer profiles works with counts without writer or doc indices", {

  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "processHandwriting", "samples"),
    template = example_cluster_template,
    measure = "counts"
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "counts_wo_indices.rds"))
  
  expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with counts with writer and doc indices", {
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "processHandwriting", "samples"),
    template = example_cluster_template,
    writer_indices = c(2,5),
    doc_indices = c(7,18),
    measure = "counts"
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "counts.rds"))
  
  expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with rates without writer or doc indices", {
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "processHandwriting", "samples"),
    template = example_cluster_template,
    measure = "rates"
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "rates_wo_indices.rds"))
  
  expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with rates with writer and doc indices", {
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "processHandwriting", "samples"),
    template = example_cluster_template,
    writer_indices = c(2,5),
    doc_indices = c(7,18),
    measure = "rates"
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "processHandwriting", "rates.rds"))
  
  expect_equal(actual, expected)
})
