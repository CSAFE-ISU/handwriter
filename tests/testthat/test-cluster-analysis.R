# test analyze_questioned_docuemnts ---------------------------------------

testthat::test_that("analyze_questioned_documents works with example_cluster_template and 1 qd", {

  empty_tempdir(subfolder = "main_dir")
  
  # copy questioned_graphs and questioned_clusters to tempdir to reduce
  # processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "questioned_graphs"), 
    output_dir = file.path(tempdir(), "main_dir", "data", "questioned_graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "questioned_clusters"), 
    output_dir = file.path(tempdir(), "main_dir", "data", "questioned_clusters")
  )
  
  # save template in tempdir
  saveRDS(example_cluster_template, file.path(tempdir(), "main_dir", "data", "template.rds"))
  
  # load model
  model <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model.rds"))

  actual <- analyze_questioned_documents(main_dir = file.path(tempdir(), 'main_dir'),
                                         questioned_docs = testthat::test_path("fixtures", "temp1qd", "data", "questioned_docs"),
                                         model = model,
                                         num_cores = 1,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "analysis.rds"))

  testthat::expect_identical(actual, expected)
})

testthat::test_that("analyze_questioned_documents works with templateK40 and 2 qd", {
  
  empty_tempdir(subfolder = "main_dir")
  
  # copy questioned_graphs and questioned_clusters to tempdir to reduce
  # processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "tempK402qd", "data", "questioned_graphs"), 
    output_dir = file.path(tempdir(), "main_dir", "data", "questioned_graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "tempK402qd", "data", "questioned_clusters"), 
    output_dir = file.path(tempdir(), "main_dir", "data", "questioned_clusters")
  )
  
  # save template in tempdir
  saveRDS(templateK40, file.path(tempdir(), "main_dir", "data", "template.rds"))
  
  # load model
  model <- readRDS(testthat::test_path("fixtures", "tempK402qd", "data", "model.rds"))
  
  actual <- analyze_questioned_documents(main_dir = file.path(tempdir(), 'main_dir'),
                                         questioned_docs = testthat::test_path("fixtures", "tempK402qd", "data", "questioned_docs"),
                                         model = model,
                                         num_cores = 1,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))
  
  expected <- readRDS(testthat::test_path("fixtures", "tempK402qd", "data", "analysis.rds"))
  
  testthat::expect_identical(actual, expected)
})


# test calculate_accuracy -------------------------------------------------

testthat::test_that("calculate_accuracy works with example_cluster_template and 1 qd", {
  
  analysis <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "analysis.rds"))
  actual <- calculate_accuracy(analysis)

  testthat::expect_equal(actual, 1)
})

testthat::test_that("calculate_accuracy works with templateK40 and 2 qd", {
  
  analysis <- readRDS(testthat::test_path("fixtures", "tempK402qd", "data", "analysis.rds"))
  actual <- calculate_accuracy(analysis)
  
  testthat::expect_equal(actual, 0.62)
})


# test get_posterior_probabilities ----------------------------------------

testthat::test_that("Get posterior probabilities works with example_cluster_template and 1 qd", {
  analysis <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "analysis.rds"))
  actual <- get_posterior_probabilities(
    analysis = analysis,
    questioned_doc = "w0030_s03_pWOZ_r01"
  )

  expected <- data.frame(known_writer = c("known_writer_w0009", "known_writer_w0030", "known_writer_w0238"),
                         w0030_s03_pWOZ_r01 = c(0, 1, 0))
  expected <- expected[order(expected[, 2], decreasing = TRUE), ]

  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get posterior probabilities works with templateK40 and 2 qd", {
  analysis <- readRDS(testthat::test_path("fixtures", "tempK402qd", "data", "analysis.rds"))
  actual <- get_posterior_probabilities(
    analysis = analysis,
    questioned_doc = "w0030_s03_pWOZ_r01"
  )
  
  expected <- data.frame(known_writer = c("known_writer_w0009", "known_writer_w0030", "known_writer_w0238"),
                         w0030_s03_pWOZ_r01 = c(0, 0.245, 0.755))
  expected <- expected[order(expected[, 2], decreasing = TRUE), ]
  
  testthat::expect_equal(actual, expected)
})
