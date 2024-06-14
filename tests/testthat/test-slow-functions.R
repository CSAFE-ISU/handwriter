# The tests in this script are slow and will be skipped on CRAN and CI

# Create template ----
test_that("template creation works", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()

  # use docs from fixtures folder
  template_docs <- test_path("fixtures", "template", "data", "template_docs")

  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir')
  if (!dir.exists(main_dir)){dir.create(main_dir)}

  warnings <- capture_warnings(actual <- make_clustering_template(main_dir = main_dir,
                                                                  template_docs = template_docs,
                                                                  writer_indices = c(2,5),
                                                                  K = 5,
                                                                  num_dist_cores = 1,
                                                                  max_iters = 3,
                                                                  centers_seed = 100))

  expect_identical(actual, example_cluster_template)
  expect_match(warnings, "For case-work, the maximum number of iterations must be greater than or equal to 25. Fewer iterations are only intended for development testing.", all = FALSE)
  expect_match(warnings, "For case-work, the number of clusters K must be 40. Other numbers of clusters are only intended for development testing.", all = FALSE)
})

# Fit model and analyze qd when writer IDs only contain numbers ----
test_that("fit model works when writer IDs only contain numbers", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()

  # use docs from examples folder
  model_docs <- test_path("fixtures", "template", "data", "model_docs")
  
  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir_numbers_only')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  if (!dir.exists(file.path(main_dir, "data"))){dir.create(file.path(main_dir, "data"))}

  # save example_template in temporary directory
  saveRDS(example_cluster_template, file.path(main_dir, "data", "template.rds"))

  actual <- fit_model(main_dir = main_dir,
                      model_docs = model_docs,
                      num_iters = 200,
                      num_chains = 1,
                      num_cores = 1,
                      writer_indices = c(2, 5),
                      doc_indices = c(7, 18))

  # because it uses MCMC the model will not be exactly the same each time so
  # we cannot use expect_identical(actual, example_model). Instead, we check
  # features of the model

  # names
  expect_named(actual, c("fitted_model",
                         "rjags_data",
                         "graph_measurements",
                         "cluster_fill_counts"))

  # check that model is an mcmc object
  expect_true(coda::is.mcmc(actual$fitted_model[[1]]))

  # check dimensions
  K <- actual$rjags_data$G
  W <- actual$rjags_data$W
  expect_length(actual$fitted_model, 1)
  expect_equal(dim(actual$fitted_model[[1]]), c(200, 2*K + 3*K*W))

  # check variable names
  expect_equal(colnames(actual$fitted_model[[1]]), list_model_variables(num_writers = W, num_clusters = K))
})

test_that("analyze questioned document works when writer IDs only contain numbers", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()

  # use docs from examples folder
  questioned_docs <- test_path("fixtures", "template", "data", "questioned_docs")

  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir_numbers_only')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  if (!dir.exists(file.path(main_dir, "data"))){dir.create(file.path(main_dir, "data"))}

  actual <- analyze_questioned_documents(main_dir = main_dir,
                                         questioned_docs = questioned_docs,
                                         model = example_model,
                                         num_cores = 1,
                                         writer_indices = c(2,5),
                                         doc_indices = c(7,18))

  expect_identical(actual, example_analysis)
})

# Fit model and analyze qd when writer IDs contain numbers and letters ----
test_that("fit model works when writer IDs contain numbers and letters", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # use docs from examples folder
  model_docs <- test_path("fixtures", "template", "data", "model_docs")
  
  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir_numbers_and_letters')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  if (!dir.exists(file.path(main_dir, "data"))){dir.create(file.path(main_dir, "data"))}
  
  # save example_template in temporary directory
  saveRDS(example_cluster_template, file.path(main_dir, "data", "template.rds"))
  
  actual <- fit_model(main_dir = main_dir,
                      model_docs = model_docs,
                      num_iters = 200,
                      num_chains = 1,
                      num_cores = 1,
                      writer_indices = c(1, 5),
                      doc_indices = c(7, 18))
  
  # because it uses MCMC the model will not be exactly the same each time so we
  # cannot use expect_identical and compare the actual model to a fixture model
  
  # names
  expect_named(actual, c("fitted_model",
                         "rjags_data",
                         "graph_measurements",
                         "cluster_fill_counts"))
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(actual$fitted_model[[1]]))
  
  # check dimensions
  K <- actual$rjags_data$G
  W <- actual$rjags_data$W
  expect_length(actual$fitted_model, 1)
  expect_equal(dim(actual$fitted_model[[1]]), c(200, 2*K + 3*K*W))
  
  # check variable names
  expect_equal(colnames(actual$fitted_model[[1]]), list_model_variables(num_writers = W, num_clusters = K))
})

test_that("analyze questioned document works when writer IDs contain numbers and letters", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # use docs from examples folder
  questioned_docs <- test_path("fixtures", "template", "data", "questioned_docs")
  
  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir_numbers_and_letters')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  if (!dir.exists(file.path(main_dir, "data"))){dir.create(file.path(main_dir, "data"))}
  
  model <- readRDS(testthat::test_path("fixtures", "template", "data", "model.rds"))
  actual <- analyze_questioned_documents(main_dir = main_dir,
                                         questioned_docs = questioned_docs,
                                         model = model,
                                         num_cores = 1,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))
  
  expected <- readRDS(testthat::test_path("fixtures", "template", "data", "analysis.rds"))
  expect_identical(actual, expected)
})
