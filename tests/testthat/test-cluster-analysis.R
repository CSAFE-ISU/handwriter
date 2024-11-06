# Single Chain ------------------------------------------------------------
test_that("calculate accuracy works on a single chain", {
  actual <- calculate_accuracy(example_analysis)
  
  expect_equal(actual, 1)
})
