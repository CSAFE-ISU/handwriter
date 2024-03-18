# Test case 1: Test with a uniform image (all pixels have the same intensity)
test_that("otsuBinarization works with uniform image", {
  img <- matrix(50, nrow = 10, ncol = 10)
  threshold <- otsuBinarization(img)
  expect_true(abs(threshold - 50) <= 10)
})

# Test case 2: Test with a binary image (only two intensity values)
test_that("otsuBinarization works with binary image", {
  img <- matrix(c(rep(0, 50), rep(255, 50)), nrow = 10, ncol = 10)
  threshold <- otsuBinarization(img)
  expect_true(threshold >= 0 && threshold <= 255)
})

# Test case 3: Test with a gradient image
test_that("otsuBinarization works with gradient image", {
  img <- matrix(rep(1:10, 10), nrow = 10, ncol = 10, byrow = TRUE)
  threshold <- otsuBinarization(img)
  expect_true(threshold > 0)
})

# Test case 4: Test with a negative value in the image
test_that("otsuBinarization works with negative value in the image", {
  img <- matrix(c(rep(-10, 25), rep(10, 75)), nrow = 10, ncol = 10)
  threshold <- otsuBinarization(img)
  expect_true(threshold > -10)
})

test_that("otsuBinarization works with custom breaks value", {
  img <- matrix(rep(1:10, 10), nrow = 10, ncol = 10, byrow = TRUE)
  threshold_default <- otsuBinarization(img)
  threshold_custom <- otsuBinarization(img, breaks = 256)
  expect_true(threshold_default >= 0 && threshold_default <= 255)
  expect_true(threshold_custom >= 0 && threshold_custom <= 255)
})
