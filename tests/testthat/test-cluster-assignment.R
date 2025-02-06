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
  
  output <- get_clusters_batch(
    template = example_cluster_template,
    input_dir = file.path(tempdir(), "graphs"),
    output_dir = file.path(tempdir(), "clusters"),
    num_cores = 2)
  
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


# test get_clusterassignment ----------------------------------------------

testthat::test_that("get_clusterassignment returns an error if input_type is not 'model' or 'questioned' ", {
  empty_tempdir("clusters")
  
  testthat::expect_error(
    get_clusterassignment(
      main_dir = file.path(tempdir(), "clusters"),
      input_type = "template",
      writer_indices = c(1, 5),
      doc_indices = c(7, 18),
      num_cores = 1
    ),
    "Unknown input type. Use model or questioned."
  )
})

testthat::test_that("get_clusterassignment loads clusters file if it already exists", {
  empty_tempdir("main_dir")
  
  create_dir(file.path(tempdir(), "main_dir", "data"), recursive = TRUE)
  # copy model_clusters.rds to tempdir
  file.copy(
    testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"),
    file.path(tempdir(), "main_dir", "data", "model_clusters.rds")
  )
  
  actual <- get_clusterassignment(
    main_dir = file.path(tempdir(), "main_dir"),
    input_type = "model",
    writer_indices = c(1, 5),
    doc_indices = c(7, 18),
    num_cores = 1
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  
  testthat::expect_identical(actual, expected)
})

testthat::test_that("get_clusterassignment throws an error if template.rds is not in data folder in the main directory", {
  empty_tempdir("main_dir")
  
  testthat::expect_error(
    get_clusterassignment(
      main_dir = file.path(tempdir(), "main_dir"),
      input_type = "model",
      writer_indices = c(1, 5),
      doc_indices = c(7, 18),
      num_cores = 1
    ),
    paste0("There is no cluster template in ", file.path(tempdir(), "main_dir"))
  )
})

testthat::test_that("get_clusterassignment creates the output directory if it doesn't exist", {
  empty_tempdir("main_dir")
  
  # copy graph files to tempdir
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "questioned_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "data", "questioned_graphs")
  )
  # save template in tempdir
  saveRDS(example_cluster_template, file.path(tempdir(), "main_dir", "data", "template.rds"))
  
  testthat::expect_no_error(get_clusterassignment(
      main_dir = file.path(tempdir(), "main_dir"),
      input_type = "questioned",
      writer_indices = c(1, 5),
      doc_indices = c(7, 18),
      num_cores = 1
  ))
  
  testthat::expect_true(dir.exists(file.path(tempdir(), "main_dir", "data", "questioned_clusters")))
})
