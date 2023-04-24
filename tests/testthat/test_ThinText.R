
######################
#readPNGBinary Tests
######################
library(testthat)
library(here)
source("~/Desktop/Handwriter2023/hwriter/handwriter/R/ThinText.R")

test_readPNGBinary <- function() {

  # Helper function to compare images
  compare_images <- function(img1, img2) {
    return(all(dim(img1) == dim(img2)) && all(img1 == img2))
  }
  
  # Test 1: Basic functionality
  test_that("readPNGBinary reads and binarizes an image", {
    test_image_path <- here("examples", "csafe_data", "0001_4.png")
    binarized_image <- readPNGBinary(test_image_path)
    expect_is(binarized_image, "array")
  })
  
  # # Test 2: Testing with RData file
  # test_that("readPNGBinary reads and binarizes an RData image", {
  #   test_rdata_image_path <- "path/to/test/image.RData"
  #   binarized_image <- readPNGBinary(test_rdata_image_path)
  #   expect_is(binarized_image, "array")
  # })
  
  # Test 3: Test with inversion set to TRUE
  test_that("readPNGBinary inverts the image when inversion is TRUE", {
    test_image_path <- here("examples", "csafe_data", "0001_4.png")
    binarized_image <- readPNGBinary(test_image_path, inversion = TRUE)
    inverted_image <- readPNGBinary(test_image_path)
    expect_false(compare_images(binarized_image, inverted_image))
  })
  
  # Test 4: Test with crop set to FALSE
  test_that("readPNGBinary does not crop the image when crop is FALSE", {
    test_image_path <- here("examples", "csafe_data", "0001_4.png")
    binarized_image <- readPNGBinary(test_image_path, crop = FALSE)
    cropped_image <- readPNGBinary(test_image_path)
    expect_false(compare_images(binarized_image, cropped_image))
  })
  
  # Test 5: Test with clean set to FALSE
  test_that("readPNGBinary does not clean the image when clean is FALSE", {
    test_image_path <- here("examples", "csafe_data", "0001_4.png")
    binarized_image <- readPNGBinary(test_image_path, clean = FALSE)
    cleaned_image <- readPNGBinary(test_image_path)
    expect_false(compare_images(binarized_image, cleaned_image))
  })
  
  # Test 6: Test with a custom cutoffAdjust value
  test_that("readPNGBinary adjusts the threshold with cutoffAdjust", {
    test_image_path <- here("examples", "csafe_data", "0001_4.png")
    binarized_image <- readPNGBinary(test_image_path, cutoffAdjust = 0.1)
    default_image <- readPNGBinary(test_image_path)
    expect_false(compare_images(binarized_image, default_image))
  })
}

test_readPNGBinary()

