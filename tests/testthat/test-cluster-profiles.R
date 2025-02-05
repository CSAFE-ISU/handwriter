# test get_cluster_fill_counts --------------------------------------------

testthat::test_that("Get cluster fill counts works with writer and doc columns", {
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill counts works without writer and doc columns", {
  
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # drop writer and doc columns from clusters
  clusters <- clusters %>% dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc"))) %>%
    dplyr::group_by(docname)
  testthat::expect_equal(actual, expected)
})

# get_cluster_fill_rates -------------------------------------------------

testthat::test_that("Get cluster fill rates works with writer and doc columns", {
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill rates works without writer and doc columns", {
  
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # drop writer and doc columns from clusters
  clusters <- clusters %>% dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  testthat::expect_equal(actual, expected)
})


# get_writer_profiles -----------------------------------------------------

testthat::test_that("Get writer profiles works with counts with writer and doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs and model_clusters to tempdir to reduce processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "main_dir", "clusters")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    writer_indices = c(1,5),
    doc_indices = c(7,18),
    measure = "counts",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with counts without writer or doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs to tempdir to reduce processing time. NOTE: model_clusters
  # contain writer and doc indices, so recalculate clusters instead of copying
  # model_clusters files
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    measure = "counts",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc"))) %>%
    dplyr::group_by(docname)
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with rates with writer and doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs and model_clusters to tempdir to reduce processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "main_dir", "clusters")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    writer_indices = c(1,5),
    doc_indices = c(7,18),
    measure = "rates",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with rates without writer or doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs to tempdir to reduce processing time. NOTE: model_clusters
  # contain writer and doc indices, so recalculate clusters instead of copying
  # model_clusters files
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    measure = "rates",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  
  testthat::expect_equal(actual, expected)
})
