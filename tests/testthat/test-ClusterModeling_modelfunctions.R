# Single Chain ------------------------------------------------------------
test_that("fit model with a single chain works", {
  # fit model. (Will fit model using testthat > fixtures > template > data > model_clusters.rds)
  template_dir <- test_path("fixtures", "template")
  model_docs <- system.file("extdata/example_images/model_docs", package = "handwriter")
  iters <- 50
  model <- fit_model(template_dir = template_dir, 
                     model_images_dir = model_docs,
                     num_iters = iters,
                     num_chains = 1,
                     writer_indices = c(2,5),
                     doc_indices = c(7,18))
  
  # number of clusters and writers
  K <- model$rjags_data$G
  W <- model$rjags_data$W
  
  # names
  expect_named(model, c("fitted_model", 
                        "rjags_data", 
                        "graph_measurements",
                        "cluster_fill_counts"))
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(model$fitted_model[[1]]))

  # check dimensions
  expect_length(model$fitted_model, 1)
  expect_equal(dim(model$fitted_model[[1]]), c(iters, 2*K + 3*K*W))
  
  # check variable names
  expect_equal(colnames(model$fitted_model[[1]]), list_model_variables(num_writers = W, num_clusters = K))
})

test_that("drop burn-in works on a single chain", {
  iters = 200
  burnin = 25
  model <- drop_burnin(model = example_model_1chain, burn_in = burnin)
  
  # number of clusters and writers
  K <- model$rjags_data$G
  W <- model$rjags_data$W
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(model$fitted_model[[1]]))
  
  # check dimensions
  expect_length(model$fitted_model, 1)
  expect_equal(dim(model$fitted_model[[1]]), c(iters-burnin, 2*K + 3*K*W))
})

test_that("about variable works on a single chain", {
  expect_equal(about_variable(variable = "pi[1,3]", 
                              model = example_model_1chain), 
               "Pi is the cluster fill probability for writer ID 9 and cluster 3")
  
  expect_equal(about_variable(variable = "mu[2,5]", 
                              model = example_model_1chain), 
               "Mu is the location parameter of a wrapped-Cauchy distribution for writer ID 30 and cluster 5")
  
  expect_equal(about_variable(variable = "tau[3,7]", 
                              model = example_model_1chain), 
               "Tau is the scale parameter of a wrapped-Cauchy distribution for writer ID 203 and cluster 7")
  
  expect_equal(about_variable(variable = "gamma[4]", 
                              model = example_model_1chain), 
               "Gamma is the mean cluster fill probability across all writers for cluster 4")
  
  expect_equal(about_variable(variable = "eta[6]", 
                              model = example_model_1chain), 
               "Eta is the mean, or the location parameter, of the hyper prior for mu for cluster 6")
})


# Multiple Chains ---------------------------------------------------------
test_that("fit model with multiple chains works", {
  # fit model. (Will fit model using testthat > fixtures > template > data > model_clusters.rds)
  template_dir <- test_path("fixtures", "template")
  model_docs <- system.file("extdata/example_images/model_docs", package = "handwriter")
  iters <- 50
  model <- fit_model(template_dir = template_dir, 
                     model_images_dir = model_docs,
                     num_iters = iters,
                     num_chains = 3,
                     writer_indices = c(2,5),
                     doc_indices = c(7,18))
  
  # number of clusters and writers
  K <- model$rjags_data$G
  W <- model$rjags_data$W
  
  # check that fitted model contains mcmc objects
  expect_true(coda::is.mcmc(model$fitted_model[[1]]))
  expect_true(coda::is.mcmc(model$fitted_model[[2]]))
  expect_true(coda::is.mcmc(model$fitted_model[[3]]))
  
  # check dimensions of fitted model
  expect_length(model$fitted_model, 3)
  expect_equal(dim(model$fitted_model[[1]]), c(iters, 2*K+3*K*W))
  expect_equal(dim(model$fitted_model[[2]]), c(iters, 2*K+3*K*W))
  expect_equal(dim(model$fitted_model[[3]]), c(iters, 2*K+3*K*W))
  
  # check variable names of fitted model
  expect_equal(colnames(model$fitted_model[[1]]), list_model_variables(num_writers = W, num_clusters = K))
  expect_equal(colnames(model$fitted_model[[2]]), list_model_variables(num_writers = W, num_clusters = K))
  expect_equal(colnames(model$fitted_model[[3]]), list_model_variables(num_writers = W, num_clusters = K))
})

test_that("drop burn-in works on multiple chains", {
  iters <- 200
  burnin <- 25
  model <- drop_burnin(example_model_2chains, burn_in = burnin)
  
  # number of clusters and writers
  K <- model$rjags_data$G
  W <- model$rjags_data$W
  
  # check that model is contains mcmc objects
  expect_true(coda::is.mcmc(model$fitted_model[[1]]))
  expect_true(coda::is.mcmc(model$fitted_model[[2]]))
  
  # check dimensions
  expect_length(model$fitted_model, 2)
  expect_equal(dim(model$fitted_model[[1]]), c(iters-burnin, 2*K + 3*K*W))
  expect_equal(dim(model$fitted_model[[2]]), c(iters-burnin, 2*K + 3*K*W))
})

test_that("about variable works on multiple chains", {
  expect_equal(about_variable(variable = "pi[1,3]", 
                              model = example_model_2chains), 
               "Pi is the cluster fill probability for writer ID 9 and cluster 3")
  
  expect_equal(about_variable(variable = "mu[2,5]", 
                              model = example_model_2chains), 
               "Mu is the location parameter of a wrapped-Cauchy distribution for writer ID 30 and cluster 5")
  
  expect_equal(about_variable(variable = "tau[3,7]", 
                              model = example_model_2chains), 
               "Tau is the scale parameter of a wrapped-Cauchy distribution for writer ID 203 and cluster 7")
  
  expect_equal(about_variable(variable = "gamma[4]", 
                              model = example_model_2chains), 
               "Gamma is the mean cluster fill probability across all writers for cluster 4")
  
  expect_equal(about_variable(variable = "eta[6]", 
                              model = example_model_2chains), 
               "Eta is the mean, or the location parameter, of the hyper prior for mu for cluster 6")
})
