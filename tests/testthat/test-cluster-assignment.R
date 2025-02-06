# test get_clusters_batch -------------------------------------------------

test_that("get_clusters_batch works sequentially", {
  empty_tempdir("clusters")
  
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(1, 5),
                               doc_indices = c(7, 18),
                               num_cores = 1)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

test_that("get_clusters_batch works in parallel", {
  empty_tempdir("clusters")
  
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(1, 5),
                               doc_indices = c(7, 18),
                               num_cores = 2)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

test_that("get_clusters_batch works sequentially if cluster assignments already exist", {
  empty_tempdir("clusters")
  
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "clusters")
  )
  
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(1, 5),
                               doc_indices = c(7, 18),
                               num_cores = 1)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

test_that("get_clusters_batch works in parallel if cluster assignments already exist", {
  empty_tempdir("clusters")
  
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "clusters")
  )
  
  actual <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(1, 5),
                               doc_indices = c(7, 18),
                               num_cores = 2)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

test_that("get_clusters_batch works sequentially without writer or doc indices", {
  empty_tempdir("clusters")
  
  actual <- get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    file.path(tempdir(), "clusters"))
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  expected <- expected %>% dplyr::select(-tidyselect::any_of(c("writer", "doc")))
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

test_that("get_clusters_batch can save master file ", {
  empty_tempdir("clusters")
  
  results <- get_clusters_batch(example_cluster_template,
                               testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
                               file.path(tempdir(), "clusters"),
                               writer_indices = c(1, 5),
                               doc_indices = c(7, 18),
                               num_cores = 1,
                               save_master_file = TRUE)
  actual <- readRDS(file.path(tempdir(), "clusters", "all_clusters.rds"))
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

test_that("get_clusters_batch works in parallel without writer or doc indices", {
  empty_tempdir("clusters")
  
  actual <- get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = 2)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  expected <- expected %>% dplyr::select(-tidyselect::any_of(c("writer", "doc")))
  # TODO: Why aren't pc_rotation and pc_wrapped values exactly the same?
  expect_equal(actual, expected, tolerance = 10^-3)
})

testthat::test_that("get_clusters_batch throws expected errors", {
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = c(1,2)),
    "num_cores is longer than 1")
  
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = "all"),
    "num_cores is not numeric")
  
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = 0.5),
    "num_cores is not an integer")
  
  testthat::expect_error(get_clusters_batch(
    example_cluster_template,
    testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    file.path(tempdir(), "clusters"),
    num_cores = 0),
    "num_cores is not greater than or equal to 1")
})

testthat::test_that("get_clusters_batch running sequentially moves problem file to problem_files folder", {
  empty_tempdir("graphs")
  empty_tempdir("clusters")
  
  # Make a graphs file without a docname
  doc <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  doc$docname <- NULL
  create_dir(file.path(tempdir(), "graphs"))
  saveRDS(doc, file.path(tempdir(), "graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  
  testthat::expect_message(get_clusters_batch(
    template = example_cluster_template,
    input_dir = file.path(tempdir(), "graphs"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 1))
  
  # Check that file was moved to problem_files folder
  testthat::expect_true(file.exists(file.path(tempdir(), "clusters", "problem_files", "w0009_s01_pWOZ_r01_proclist.rds")))
  testthat::expect_false(file.exists(file.path(tempdir(), "graphs", "w0009_s01_pWOZ_r01_proclist.rds")))
})

testthat::test_that("get_clusters_batch running in parallel moves problem file to problem_files folder", {
  empty_tempdir("graphs")
  empty_tempdir("clusters")
  
  # Make a graphs file without a docname
  doc <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  doc$docname <- NULL
  create_dir(file.path(tempdir(), "graphs"))
  saveRDS(doc, file.path(tempdir(), "graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  
  testthat::expect_message(get_clusters_batch(
    template = example_cluster_template,
    input_dir = file.path(tempdir(), "graphs"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 2))
  
  # Check that file was moved to problem_files folder
  testthat::expect_true(file.exists(file.path(tempdir(), "clusters", "problem_files", "w0009_s01_pWOZ_r01_proclist.rds")))
  testthat::expect_false(file.exists(file.path(tempdir(), "graphs", "w0009_s01_pWOZ_r01_proclist.rds")))
})


testthat::test_that("get_clusters_batch throws warning if file doesn't contain graphs and running sequentially", {
  empty_tempdir("graphs")
  empty_tempdir("clusters")
  
  # Make a graphs file without graphs
  doc <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  doc$process$letterList <- NULL
  create_dir(file.path(tempdir(), "graphs"))
  saveRDS(doc, file.path(tempdir(), "graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  
  testthat::expect_warning(get_clusters_batch(
    template = example_cluster_template,
    input_dir = file.path(tempdir(), "graphs"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 1), "The document does not contain any graphs.")
})

testthat::test_that("get_clusters_batch returns NULL if file doesn't contain graphs and running in parallel", {
  empty_tempdir("graphs")
  empty_tempdir("clusters")
  
  # Make a graphs file without graphs
  doc <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  doc$process$letterList <- NULL
  create_dir(file.path(tempdir(), "graphs"))
  saveRDS(doc, file.path(tempdir(), "graphs", "w0009_s01_pWOZ_r01_proclist.rds"))
  
  actual <- get_clusters_batch(
    template = example_cluster_template,
    input_dir = file.path(tempdir(), "graphs"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 2)
  testthat::expect_null(actual)
})
