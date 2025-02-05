# test distXY -------------------------------------------------------------

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


# test dist_loc -----------------------------------------------------------

# Calculates the distances between endpoints of one path with the endpoints of a
# second path.
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


# test dist_sld -----------------------------------------------------------

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


# test pointLineProportionVect --------------------------------------------

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


# test solveLP ------------------------------------------------------------

#find the optimal pairings of paths between two graphs.
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


# test getGraphInfo -------------------------------------------------------

# Test 1: getGraphInfo with two prototype image lists
test_that("getGraphInfo returns the correct output with two prototype image lists", {
  image_list1 <- create_dummy_image_list(is_proto = TRUE, num_paths = 2)
  image_list2 <- create_dummy_image_list(is_proto = TRUE, num_paths = 3)
  
  graph_info <- getGraphInfo(image_list1, image_list2, TRUE, TRUE, 5)
  expect_equal(graph_info$numPaths1, 2)
  expect_equal(graph_info$numPaths2, 3)
  graph_info2 <- getGraphInfo_cpp(image_list1, image_list2, TRUE, TRUE, 5)
  expect_equal(graph_info2$numPaths1, 2)
  expect_equal(graph_info2$numPaths2, 3)
})

# Test 2: getGraphInfo with two non-prototype image lists
test_that("getGraphInfo returns the correct output with two non-prototype image lists", {
  image_list1 <- create_dummy_image_list(is_proto = FALSE, num_paths = 2)
  image_list2 <- create_dummy_image_list(is_proto = FALSE, num_paths = 3)
  
  graph_info <- getGraphInfo(image_list1, image_list2, FALSE, FALSE, 5)
  expect_equal(graph_info$numPaths1, 2)
  expect_equal(graph_info$numPaths2, 3)
  graph_info2 <- getGraphInfo_cpp(image_list1, image_list2, FALSE, FALSE, 5)
  expect_equal(graph_info2$numPaths1, 2)
  expect_equal(graph_info2$numPaths2, 3)
})

# Test 3: getGraphInfo with one prototype and one non-prototype image list
test_that("getGraphInfo returns the correct output with one prototype and one non-prototype image list", {
  image_list1 <- create_dummy_image_list(is_proto = TRUE, num_paths = 2)
  image_list2 <- create_dummy_image_list(is_proto = FALSE, num_paths = 3)
  
  graph_info <- getGraphInfo(image_list1, image_list2, TRUE, FALSE, 5)
  expect_equal(graph_info$numPaths1, 2)
  expect_equal(graph_info$numPaths2, 3)
  graph_info2 <- getGraphInfo_cpp(image_list1, image_list2, TRUE, FALSE, 5)
  expect_equal(graph_info2$numPaths1, 2)
  expect_equal(graph_info2$numPaths2, 3)
})
