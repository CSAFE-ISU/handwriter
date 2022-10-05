test_that("fit model works", {
  # format model training data
  mtd <- format_model_data(example_model_clusters, writer_indices = c(2,5), doc_indices = c(7,18))
  iters <- 2000
  draws <- fit_model(model_training_data = mtd$rjags_data, num_iters = iters)
  
  # check named list
  expect_named(draws, c("thetas", "mus", "gammas", "rhos", "etas", "nll_datamodel", "nld_locationparam"))
  
  # check type
  expect_s3_class(draws$thetas, "data.frame")
  expect_s3_class(draws$mus, "data.frame")
  expect_s3_class(draws$gammas, "data.frame")
  expect_s3_class(draws$rhos, "data.frame")
  expect_s3_class(draws$etas, "data.frame")
  expect_s3_class(draws$nll_datamodel, "data.frame")
  expect_s3_class(draws$nld_locationparam, "data.frame")
  
  # check length
  expect_equal(nrow(draws$thetas), iters)
  expect_equal(nrow(draws$mus), iters)
  expect_equal(nrow(draws$gammas), iters)
  expect_equal(nrow(draws$rhos), iters)
  expect_equal(nrow(draws$etas), iters)
  expect_equal(nrow(draws$nll_datamodel), iters)
  expect_equal(nrow(draws$nld_locationparam), iters)
})

test_that("drop burn-in works", {
  iters <- 2000
  burnin <- 1000
  draws <- fit_model(model_training_data = example_model_data$rjags_data, num_iters = iters)
  draws <- drop_burnin(draws, burn_in = burnin)
  
  # check named list
  expect_named(draws, c("thetas", "mus", "gammas", "rhos", "etas", "nll_datamodel", "nld_locationparam"))
  
  # check length
  expect_equal(nrow(draws$thetas), iters-burnin)
  expect_equal(nrow(draws$mus), iters-burnin)
  expect_equal(nrow(draws$gammas), iters-burnin)
  expect_equal(nrow(draws$rhos), iters-burnin)
  expect_equal(nrow(draws$etas), iters-burnin)
  expect_equal(nrow(draws$nll_datamodel), iters-burnin)
  expect_equal(nrow(draws$nld_locationparam), iters-burnin)
})

test_that("analyze questioned documents works", {
  iters <- 4000
  burnin <- 1000
  draws <- fit_model(model_training_data = example_model_data$rjags_data, num_iters = iters)
  draws <- drop_burnin(draws, burn_in = burnin)
  analysis <- analyze_questioned_documents(example_model_data, draws, example_questioned_data, num_cores = 2)
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities"))
  
  # check vote totals
  sapply(analysis$votes, function(x) expect_equal(sum(x), iters-burnin))

  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
  
})
