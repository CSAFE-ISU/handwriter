# test otsuBinarization ---------------------------------------------------

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


# test readPNGBinary ------------------------------------------------------

testthat::test_that("readPNGBinary works with defaults", {
  actual <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"))
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "read_png_binary.rds"))
  testthat::expect_identical(actual, expected)
})

testthat::test_that("readPNGBinary works with defaults", {
  actual <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"), inversion = TRUE)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "read_png_binary_invert.rds"))
  testthat::expect_identical(actual, expected)
})

testthat::test_that("readPNGBinary works with positive cutoff adjustment", {
  actual <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"), cutoffAdjust = 5)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "read_png_binary_cut5.rds"))
  testthat::expect_identical(actual, expected)
})

testthat::test_that("readPNGBinary works with negative cutoff adjustment", {
  actual <- readPNGBinary(testthat::test_path("fixtures", "temp1qd", "data", "template_docs", "w0016_s01_pLND_r01.png"), cutoffAdjust = -0.5)
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "read_png_binary_cut-0.5.rds"))
  testthat::expect_identical(actual, expected)
})

testthat::test_that("readPNGBinary works with doc with an alpha channel", {
  actual <- readPNGBinary(testthat::test_path("fixtures", "alpha_channel", "w0030_s03_pWOZ_r01.png"))
  
  expected <- readRDS(testthat::test_path("fixtures", "alpha_channel", "alpha.rds"))
  testthat::expect_identical(actual, expected)
})
