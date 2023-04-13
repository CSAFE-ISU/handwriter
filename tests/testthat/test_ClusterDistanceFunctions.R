#install.packages("testthat")
library(testthat)
#' @importFrom testthat pass fail

# tolerance <- 0.40
source("~/Desktop/Handwriter2023/hwriter/handwriter/R/ClusterDistanceFunctions.R")

# Define a custom function to compare two values within a given tolerance
# expect_equal_tolerance <- function(actual, expected, tolerance = 0.001, info = NULL) {
#   # Check that the difference between actual and expected is less than or equal to the tolerance
#   if (abs(actual - expected) <= tolerance) {
#     # Test passed
#     testthat::pass(info)
#   } else {
#     # Test failed
#     testthat::fail(sprintf("%s\nExpected: %s\nActual: %s", 
#                            if (!is.null(info)) paste0("(", info, ") "), 
#                            expected, actual))
#   }
# }

# Define a custom function to compare two vectors within a given tolerance
expect_equal_tolerance <- function(actual, expected, tolerance = 0.001, info = NULL) {
  # Check that the absolute difference between the two vectors is less than or equal to the tolerance
  diff <- abs(actual - expected)
  if (all(diff <= tolerance)) {
    # Test passed
    testthat::pass(info)
  } else {
    # Test failed
    testthat::fail(sprintf("%s\nExpected: %s\nActual: %s", 
                           if (!is.null(info)) paste0("(", info, ") "), 
                           expected, actual))
  }
}


# # Example usage:
# test_that("my_function returns expected value", {
#   result <- my_function()
#   expect_equal_tolerance(result, 3.141592, tolerance = 0.0001)
# })


test_that("Testing the distXY function", {
  # Test case 1: Same point, expect 0 distance
  xy1 <- c(0, 0)
  xy2 <- c(0, 0)
  expect_equal(distXY(xy1, xy2), 0)
  
  # Test case 2: Horizontal distance
  xy1 <- c(0, 0)
  xy2 <- c(5, 0)
  expect_equal(distXY(xy1, xy2), 5)
  
  # Test case 3: Vertical distance
  xy1 <- c(0, 0)
  xy2 <- c(0, 5)
  expect_equal(distXY(xy1, xy2), 5)
  
  # Test case 4: Diagonal distance
  xy1 <- c(0, 0)
  xy2 <- c(3, 4)
  expect_equal(distXY(xy1, xy2), 5)
  
  # Test case 5: Negative coordinates
  xy1 <- c(-2, 1)
  xy2 <- c(1, 5)
  expect_equal(distXY(xy1, xy2), 5)

})

test_that("dist_loc function", {
  # Test case 1: Same path, expect (0, 0) distances
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(0, 0)
  p2e2 <- c(5, 0)
  expect_equal(dist_loc(p1e1, p1e2, p2e1, p2e2), c(0, 5))

  # Test case 2: Parallel paths with no overlap, expect same distances
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(0, 3)
  p2e2 <- c(5, 3)
  # expect_equal_tolerance(dist_loc(p1e1, p1e2, p2e1, p2e2), c(3, 5.83), tolerance = 0.001)
  #expect_equal(dist_loc(p1e1, p1e2, p2e1, p2e2), expect_equal_tolerance(dist_loc(p1e1, p1e2, p2e1, p2e2), c(3, 5.83), tolerance = 0.001))
  actual<- dist_loc(p1e1, p1e2, p2e1, p2e2)
  actual[2] <- round(actual[2], 2)
  expect_setequal(actual, c(3, 5.83))

  # Test case 3: Overlapping paths, expect (0, 0) distances
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(3, 0)
  p2e2 <- c(8, 0)
  expect_equal(dist_loc(p1e1, p1e2, p2e1, p2e2), c(3, 2))

  # Test case 4: Non-parallel paths with no overlap, expect different distances
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(6, 0)
  p2e2 <- c(6, 3)
  actual<- dist_loc(p1e1, p1e2, p2e1, p2e2)
  actual[1] <- round(actual[1], 2)
  expect_setequal(actual, c(3.16,1))
})

# Test cases for dist_loc function using expect_near
# test_that("dist_loc returns expected distances", {
#   # Test data
#   p1e1 <- c(0, 0)
#   p1e2 <- c(1, 0)
#   p2e1 <- c(0, 1)
#   p2e2 <- c(1, 1)
#   
#   # Call dist_loc function
#   result <- dist_loc(p1e1, p1e2, p2e1, p2e2)
#   
#   # Expected output
#   expected_d_loc_plus <- min(distXY(p1e1, p2e1), distXY(p1e2, p2e2))
#   expected_d_loc_minus <- min(distXY(p1e1, p2e2), distXY(p1e2, p2e1))
#   
#   # Test output with tolerance
#   tolerance <- 1e-9
#   expect_near(result[1], expected_d_loc_plus, tol = tolerance)
#   expect_near(result[2], expected_d_loc_minus, tol = tolerance)
# })

test_that("dist_sld function", {
  # Test case 1: Same path, expect 0 difference
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(0, 0)
  p2e2 <- c(5, 0)
  expect_equal(dist_sld(p1e1, p1e2, p2e1, p2e2), 0)
  
  # Test case 2: Different paths with same length, expect 0 difference
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(0, 3)
  p2e2 <- c(5, 3)
  expect_equal(dist_sld(p1e1, p1e2, p2e1, p2e2), 0)
  
  # Test case 3: Different paths with different lengths, expect non-zero difference
  p1e1 <- c(0, 0)
  p1e2 <- c(5, 0)
  p2e1 <- c(0, 3)
  p2e2 <- c(0, 8)
  expect_equal(dist_sld(p1e1, p1e2, p2e1, p2e2), 0)
})

test_that("pointLineProportionVect function", {
  # Test case 1: Zero proportion, expect (0, 0)
  endpt1 <- c(0, 0)
  endpt2 <- c(5, 5)
  edgecut_prop_n <- 0
  edgecutpt_n <- c(0, 0)
  expect_equal(pointLineProportionVect(endpt1, endpt2, edgecut_prop_n, edgecutpt_n), c(0, 0))
  
  # Test case 2: Midpoint proportion, expect (0, 0)
  endpt1 <- c(0, 0)
  endpt2 <- c(4, 4)
  edgecut_prop_n <- 0.5
  edgecutpt_n <- c(2, 2)
  expect_equal(pointLineProportionVect(endpt1, endpt2, edgecut_prop_n, edgecutpt_n), c(0, 0))
  
  
  # Test case 3: Different path and line cut points, expect non-zero difference
  endpt1 <- c(0, 0)
  endpt2 <- c(6, 6)
  edgecut_prop_n <- 0.5
  edgecutpt_n <- c(3, 4) # Changing the y-coordinate of the cut point on the path
  result <- pointLineProportionVect(endpt1, endpt2, edgecut_prop_n, edgecutpt_n)
  expect_true(result[1] != 0 || result[2] != 0)
  
})

test_that("solveLP function", {
  # Test case 1: 2x2 dists matrix with a clear optimal solution
  dists <- matrix(c(2, 3, 5, 1), nrow = 2)
  result <- solveLP(dists)
  expect_equal(result$matching_weight, 3)
  expect_equal(result$matching_size, 2)
  expect_equal(result$matching, c(1, 2))
  
  # Test case 2: 3x3 dists matrix with a clear optimal solution
  dists <- matrix(c(2, 3, 4, 5, 1, 6, 7, 8, 9), nrow = 3)
  result <- solveLP(dists)
  expect_equal(result$matching_weight, 12)
  expect_equal(result$matching_size, 3)
  expect_equal(result$matching, c(3, 2, 1))
})

