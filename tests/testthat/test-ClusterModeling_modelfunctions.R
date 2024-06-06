# Single Chain ------------------------------------------------------------
# test_that("fit model with a single chain works", {
#   # fit model. (Will fit model using testthat > fixtures > template > data > model_clusters.rds)
#   main_dir <- test_path("fixtures", "template")
#   model_docs <- system.file("extdata/example_images/model_docs", package = "handwriter")
#   iters <- 50
#   model <- fit_model(main_dir = main_dir, 
#                      model_docs = model_docs,
#                      num_iters = iters,
#                      num_chains = 1,
#                      writer_indices = c(2,5),
#                      doc_indices = c(7,18))
#   
#   # number of clusters and writers
#   K <- model$rjags_data$G
#   W <- model$rjags_data$W
#   
#   # names
#   expect_named(model, c("fitted_model", 
#                         "rjags_data", 
#                         "graph_measurements",
#                         "cluster_fill_counts"))
#   
#   # check that model is an mcmc object
#   expect_true(coda::is.mcmc(model$fitted_model[[1]]))
# 
#   # check dimensions
#   expect_length(model$fitted_model, 1)
#   expect_equal(dim(model$fitted_model[[1]]), c(iters, 2*K + 3*K*W))
#   
#   # check variable names
#   expect_equal(colnames(model$fitted_model[[1]]), list_model_variables(num_writers = W, num_clusters = K))
# })

test_that("drop burn-in works on a single chain", {
  iters = 200
  burnin = 25
  model <- drop_burnin(model = example_model, burn_in = burnin)
  
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
                              model = example_model), 
               "Pi is the cluster fill probability for writer ID 9 and cluster 3")
  
  expect_equal(about_variable(variable = "mu[2,5]", 
                              model = example_model), 
               "Mu is the location parameter of a wrapped-Cauchy distribution for writer ID 30 and cluster 5")
  
  expect_equal(about_variable(variable = "tau[3,7]", 
                              model = example_model), 
               "Tau is the scale parameter of a wrapped-Cauchy distribution for writer ID 203 and cluster 7")
  
  expect_equal(about_variable(variable = "gamma[4]", 
                              model = example_model), 
               "Gamma is the mean cluster fill probability across all writers for cluster 4")
  
  expect_equal(about_variable(variable = "eta[6]", 
                              model = example_model), 
               "Eta is the mean, or the location parameter, of the hyper prior for mu for cluster 6")
})
