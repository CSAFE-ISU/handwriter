test_that("fit model with a single chain works", {
  # format model training data
  model_data <- format_model_data(example_model_clusters, writer_indices = c(2,5), doc_indices = c(7,18))
  iters <- 50
  model <- fit_model(model_data = model_data, num_iters = iters)
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(model[[1]]))
  
  # check dimensions
  expect_length(model, 1)
  expect_equal(dim(model[[1]]), c(iters, 3051))
})

test_that("drop burn-in works on a single chain", {
  iters <- 50
  burnin <- 25
  model <- fit_model(model_data = example_model_data, num_iters = iters, num_chains = 1)
  model <- drop_burnin(model, burn_in = burnin)
  
  # check that model is an mcmc object
  expect_true(coda::is.mcmc(model[[1]]))
  
  # check dimensions
  expect_length(model, 1)
  expect_equal(dim(model[[1]]), c(iters-burnin, 3051))
})

test_that("analyze questioned documents works with a single chain", {
  iters <- 50
  burnin <- 25
  model <- fit_model(model_data = example_model_data, num_iters = iters, num_chains = 1)
  model <- drop_burnin(model, burn_in = burnin)
  analysis <- analyze_questioned_documents(example_model_data, 
                                           model, 
                                           example_questioned_data, 
                                           num_cores = 2)
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities"))
  
  # check vote totals
  sapply(analysis$votes, function(x) expect_equal(sum(x), iters-burnin))
  
  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
  
})

test_that("fit model with multiple chains works", {
  # format model training data
  model_data <- format_model_data(example_model_clusters, 
                                  writer_indices = c(2,5), 
                                  doc_indices = c(7,18))
  iters <- 50
  model <- fit_model(model_data = model_data, 
                     num_iters = iters,
                     num_chains = 3)
  
  # check that model is contains mcmc objects
  expect_true(coda::is.mcmc(model[[1]]))
  expect_true(coda::is.mcmc(model[[2]]))
  expect_true(coda::is.mcmc(model[[3]]))
  
  # check dimensions
  expect_length(model, 3)
  expect_equal(dim(model[[1]]), c(iters, 3051))
  expect_equal(dim(model[[2]]), c(iters, 3051))
  expect_equal(dim(model[[3]]), c(iters, 3051))
})

test_that("drop burn-in works on multiple chains", {
  iters <- 50
  burnin <- 25
  model <- fit_model(model_data = example_model_data, num_iters = iters, num_chains = 3)
  model <- drop_burnin(model, burn_in = burnin)
  
  # check that model is contains mcmc objects
  expect_true(coda::is.mcmc(model[[1]]))
  expect_true(coda::is.mcmc(model[[2]]))
  expect_true(coda::is.mcmc(model[[3]]))
  
  # check dimensions
  expect_length(model, 3)
  expect_equal(dim(model[[1]]), c(iters-burnin, 3051))
  expect_equal(dim(model[[2]]), c(iters-burnin, 3051))
  expect_equal(dim(model[[3]]), c(iters-burnin, 3051))
})

test_that("analyze questioned documents works with multiple chains", {
  iters <- 50
  burnin <- 25
  n_chains <- 3
  model <- fit_model(model_data = example_model_data, num_iters = iters, num_chains = n_chains)
  model <- drop_burnin(model, burn_in = burnin)
  analysis <- analyze_questioned_documents(example_model_data, 
                                           model, 
                                           example_questioned_data, 
                                           num_cores = 2)
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities"))
  
  # check vote totals
  sapply(analysis$votes, function(x) expect_equal(sum(x), n_chains*(iters-burnin)))
  
  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
})
