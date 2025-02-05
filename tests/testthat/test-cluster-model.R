# test fit_model ----------------------------------------------------------

testthat::test_that("fit_model works with a single chain", {
  
  # delete tempdir() > main_dir
  empty_tempdir(subfolder = "main_dir")
  
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "data", "model_graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "main_dir", "data", "model_clusters")
  )
  saveRDS(example_cluster_template, file.path(tempdir(), "main_dir", "data", "template.rds"))
  
  actual <- fit_model(main_dir = file.path(tempdir(), 'main_dir'),
                      model_docs = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
                      num_iters = 200,
                      num_chains = 1,
                      num_cores = 1,
                      writer_indices = c(1, 5),
                      doc_indices = c(7, 18))
  
  # because it uses MCMC the model will not be exactly the same each time so we
  # cannot use expect_identical and compare the actual model to a fixture model
  
  # names
  testthat::expect_named(actual, c("fitted_model",
                         "rjags_data",
                         "graph_measurements",
                         "cluster_fill_counts"))
  
  # check that model is an mcmc object
  testthat::expect_true(coda::is.mcmc(actual$fitted_model[[1]]))
  
  # check dimensions
  K <- actual$rjags_data$G
  W <- actual$rjags_data$W
  testthat::expect_length(actual$fitted_model, 1)
  testthat::expect_equal(dim(actual$fitted_model[[1]]), c(200, 2*K + 3*K*W))
  
  # check variable names
  testthat::expect_equal(colnames(actual$fitted_model[[1]]), list_model_variables(num_writers = W, num_clusters = K))
})


# test drop_burnin --------------------------------------------------------

testthat::test_that("drop burn-in works on a single chain", {
  iters = 200
  burnin = 25
  model <- drop_burnin(model = example_model, burn_in = burnin)
  
  # number of clusters and writers
  K <- model$rjags_data$G
  W <- model$rjags_data$W
  
  # check that model is an mcmc object
  testthat::expect_true(coda::is.mcmc(model$fitted_model[[1]]))
  
  # check dimensions
  testthat::expect_length(model$fitted_model, 1)
  testthat::expect_equal(dim(model$fitted_model[[1]]), c(iters-burnin, 2*K + 3*K*W))
})


# test about_variable -----------------------------------------------------

testthat::test_that("about variable works on a single chain", {
  testthat::expect_equal(about_variable(variable = "pi[1,3]", model = example_model), 
               "Pi is the cluster fill probability for writer ID w0009 and cluster 3")
  
  testthat::expect_equal(about_variable(variable = "mu[2,5]", model = example_model), 
               "Mu is the location parameter of a wrapped-Cauchy distribution for writer ID w0030 and cluster 5")
  
  testthat::expect_equal(about_variable(variable = "tau[3,5]", model = example_model), 
               "Tau is the scale parameter of a wrapped-Cauchy distribution for writer ID w0238 and cluster 5")
  
  testthat::expect_equal(about_variable(variable = "gamma[4]", model = example_model), 
               "Gamma is the mean cluster fill probability across all writers for cluster 4")
  
  testthat::expect_equal(about_variable(variable = "eta[3]", 
                              model = example_model), 
               "Eta is the mean, or the location parameter, of the hyper prior for mu for cluster 3")
})
