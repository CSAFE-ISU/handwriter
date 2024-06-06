# The tests in this script are slow and will be skipped on CRAN and CI

test_that("template creation works", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # use docs from examples folder
  template_docs <- file.path(system.file("examples", package="handwriter"), "example_template", "data", "template_docs")
  
  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  warnings <- capture_warnings(actual <- make_clustering_template(main_dir = main_dir,
                                                                  template_docs = template_docs,
                                                                  writer_indices = c(2,5),
                                                                  K = 10,
                                                                  num_dist_cores = 5,
                                                                  max_iters = 3,
                                                                  centers_seed = 100))
  
  expect_identical(actual, example_cluster_template)
  expect_match(warnings, "For case-work, the maximum number of iterations must be greater than or equal to 25. Fewer iterations are only intended for development testing.", all = FALSE)
  expect_match(warnings, "For case-work, the number of clusters K must be 40. Other numbers of clusters are only intended for development testing.", all = FALSE)
})

test_that("fit model works", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # use docs from examples folder
  model_docs <- file.path(system.file("examples", package="handwriter"), "example_template", "data", "model_docs")
  
  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  actual <- fit_model(main_dir = main_dir, 
                      model_docs = model_docs,
                      num_iters = 200, 
                      num_chains = 1, 
                      num_cores = 5,
                      writer_indices = c(2,5), 
                      doc_indices = c(7,18))
  
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

test_that("analyze questioned document works", {
  # this test is slow so skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # use docs from examples folder
  questioned_docs <- file.path(system.file("examples", package="handwriter"), "example_template", "data", "questioned_docs")
  
  # write to temporary directory
  main_dir <- file.path(tempdir(), 'main_dir')
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  actual <- analyze_questioned_documents(main_dir = main_dir, 
                                         questioned_docs = questioned_docs, 
                                         model = example_model, 
                                         num_cores = 5,
                                         writer_indices = c(2,5), 
                                         doc_indices = c(7,18))
  
  expect_identical(actual, example_analysis)
})
