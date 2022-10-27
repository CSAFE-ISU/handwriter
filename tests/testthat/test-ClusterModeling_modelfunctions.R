test_that("fit model with a single chain works", {
  # format model training data
  model_data <- format_model_data(example_model_clusters, writer_indices = c(2,5), doc_indices = c(7,18))
  iters <- 50
  model <- fit_model(model_data = model_data, num_iters = iters)
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(model[[1]]))
  
  # check dimensions
  expect_length(model, 1)
  expect_equal(dim(model[[1]]), c(iters, 136))
})

test_that("drop burn-in works on a single chain", {
  iters = 50
  burnin = 25
  model <- drop_burnin(model = example_model_1chain, burn_in = burnin)
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(model[[1]]))
  
  # check dimensions
  expect_length(model, 1)
  expect_equal(dim(model[[1]]), c(iters-burnin, 136))
})

test_that("analyze questioned documents works with a single chain", {
  analysis <- analyze_questioned_documents(example_model_data, 
                                           example_model_1chain, 
                                           example_questioned_data, 
                                           num_cores = 2)
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities"))
  
  # check vote totals
  sapply(analysis$votes, function(x) expect_equal(sum(x), 50))
  
  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
  
})

test_that("fit model with multiple chains works", {
  iters <- 50
  model <- fit_model(model_data = example_model_data, 
                     num_iters = iters,
                     num_chains = 3)
  
  # check that model is contains mcmc objects
  expect_true(coda::is.mcmc(model[[1]]))
  expect_true(coda::is.mcmc(model[[2]]))
  expect_true(coda::is.mcmc(model[[3]]))
  
  # check dimensions
  expect_length(model, 3)
  expect_equal(dim(model[[1]]), c(iters, 136))
  expect_equal(dim(model[[2]]), c(iters, 136))
  expect_equal(dim(model[[3]]), c(iters, 136))
})

test_that("drop burn-in works on multiple chains", {
  iters <- 50
  burnin <- 25
  model <- drop_burnin(example_model_2chains, burn_in = burnin)
  
  # check that model is contains mcmc objects
  expect_true(coda::is.mcmc(model[[1]]))
  expect_true(coda::is.mcmc(model[[2]]))
  
  # check dimensions
  expect_length(model, 2)
  expect_equal(dim(model[[1]]), c(iters-burnin, 136))
  expect_equal(dim(model[[2]]), c(iters-burnin, 136))
})

test_that("analyze questioned documents works with multiple chains", {
  analysis <- analyze_questioned_documents(example_model_data, 
                                           example_model_2chains, 
                                           example_questioned_data, 
                                           num_cores = 2)
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities"))
  
  # check vote totals (2 chains x 50 iters)
  sapply(analysis$votes, function(x) expect_equal(sum(x), 2*(50)))
  
  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
})

test_that("about variable works on a single chain", {
  expect_equal(about_variable(variable = "pi[1,3]", 
                              model_data = example_model_data, 
                              model = example_model_1chain), 
               "Pi is the cluster fill probability for writer 9 and cluster 3")
  
  expect_equal(about_variable(variable = "mu[2,5]", 
                              model_data = example_model_data, 
                              model = example_model_1chain), 
               "Mu is the location parameter of a wrapped-Cauchy distribution for writer 30 and cluster 5")
  
  expect_equal(about_variable(variable = "tau[3,7]", 
                              model_data = example_model_data, 
                              model = example_model_1chain), 
               "Tau is the scale parameter of a wrapped-Cauchy distribution for writer 203 and cluster 7")
  
  expect_equal(about_variable(variable = "gamma[4]", 
                              model_data = example_model_data, 
                              model = example_model_1chain), 
               "Gamma is the pseudo-cluster count across all writers for cluster 4")
  
  expect_equal(about_variable(variable = "eta[6]", 
                              model_data = example_model_data, 
                              model = example_model_1chain), 
               "Eta is a hyper prior for cluster 6")
})

test_that("about variable works with multiple chains", {
  expect_equal(about_variable(variable = "pi[1,3]", 
                              model_data = example_model_data, 
                              model = example_model_2chains), 
               "Pi is the cluster fill probability for writer 9 and cluster 3")
  
  expect_equal(about_variable(variable = "mu[2,5]", 
                              model_data = example_model_data, 
                              model = example_model_2chains), 
               "Mu is the location parameter of a wrapped-Cauchy distribution for writer 30 and cluster 5")
  
  expect_equal(about_variable(variable = "tau[3,7]", 
                              model_data = example_model_data, 
                              model = example_model_2chains), 
               "Tau is the scale parameter of a wrapped-Cauchy distribution for writer 203 and cluster 7")
  
  expect_equal(about_variable(variable = "gamma[4]", 
                              model_data = example_model_data, 
                              model = example_model_2chains), 
               "Gamma is the pseudo-cluster count across all writers for cluster 4")
  
  expect_equal(about_variable(variable = "eta[6]", 
                              model_data = example_model_data, 
                              model = example_model_2chains), 
               "Eta is a hyper prior for cluster 6")
})
