# format_template_data ----------------------------------------------------

testthat::test_that("format template data works with outliers", {
  actual <- format_template_data(example_cluster_template)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_data_w_outliers.rds"))
  testthat::expect_identical(actual, expected)
})

testthat::test_that("format template data works without outliers", {
  
  new_template <- example_cluster_template
  not_outliers <- new_template$cluster != -1
  new_template$cluster <- new_template$cluster[not_outliers]
  new_template$writers <- new_template$writers[not_outliers]
  new_template$doc <- new_template$doc[not_outliers]
  
  actual <- format_template_data(new_template)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "template_data_wo_outliers.rds"))
  testthat::expect_identical(actual, expected)
})


# test format_model_data --------------------------------------------------

test_that("format model data works", {
  model_clusters <- readRDS(test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  actual <- format_model_data(model_clusters=model_clusters, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_data.rds"))
  testthat::expect_equal(actual, expected)
})


# test format_questioned_data ---------------------------------------------

test_that("formatted questioned data is formatted correctly", {
  model <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model.rds"))
  questioned_clusters <- readRDS(test_path("fixtures", "temp1qd", "data", "questioned_clusters.rds"))
  actual <- format_questioned_data(model=model,
                                 questioned_clusters=questioned_clusters, 
                                 writer_indices=c(1,5), 
                                 doc_indices=c(7,18))
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "questioned_data.rds"))
  testthat::expect_equal(actual, expected)
})
