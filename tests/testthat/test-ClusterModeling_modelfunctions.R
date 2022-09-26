test_that("fit model works", {
  iters <- 2000
  draws <- fit_model(model_training_data = example_model_training_data, num_iters = iters)
  
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
  draws <- fit_model(model_training_data = example_model_training_data, num_iters = iters)
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
