testthat::test_that("template creation works", {
  # Creating a cluster template takes too much memory to run on CRAN or CI
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  
  empty_tempdir(subfolder = "main_dir")
  
  # copy docs and graphs to tempdir > main_dir > data to save processing time on
  # the next step
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "template_docs"),
    output_dir = file.path(tempdir(), "main_dir", "data", "template_docs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "template_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "data", "template_graphs")
  )
  
  warnings <- capture_warnings(actual <- make_clustering_template(main_dir = file.path(tempdir(), 'main_dir'),
                                                                  template_docs = file.path(tempdir(), 'main_dir', 'data', 'template_docs'),
                                                                  writer_indices = c(1, 5),
                                                                  K = 5,
                                                                  num_dist_cores = 1,
                                                                  max_iters = 3,
                                                                  centers_seed = 100))
  
  testthat::expect_equal(actual, example_cluster_template)
  testthat::expect_match(warnings, "For case-work, the maximum number of iterations must be greater than or equal to 25. Fewer iterations are only intended for development testing.", all = FALSE)
  testthat::expect_match(warnings, "For case-work, the number of clusters K must be 40. Other numbers of clusters are only intended for development testing.", all = FALSE)
})


# test make_dir -----------------------------------------------------------

testthat::test_that("Make directory works", {
  
  # delete tempdir() > main_dir
  empty_tempdir(subfolder = "main_dir")
  testthat::expect_false(file.exists(file.path(tempdir(), "main_dir")))
  
  make_dir(file.path(tempdir(), "main_dir"))
  testthat::expect_true(file.exists(file.path(tempdir(), "main_dir")))
})

testthat::test_that("Make directory works recursively", {
  
  empty_tempdir(subfolder = "main_dir")
  testthat::expect_false(file.exists(file.path(tempdir(), "main_dir", "data")))
  
  make_dir(file.path(tempdir(), "main_dir", "data"), recursive = TRUE)
  testthat::expect_true(file.exists(file.path(tempdir(), "main_dir")))
})
