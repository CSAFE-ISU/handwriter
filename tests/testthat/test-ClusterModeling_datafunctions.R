# format_model_data -------------------------------------------------------
test_that("format model data works", {
  model_clusters <- readRDS(test_path("fixtures", "template", "data", "model_clusters.rds"))
  data <- format_model_data(model_clusters=model_clusters, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  
  # check names
  expect_named(data, c("graph_measurements", "cluster_fill_counts", "rjags_data"))
})

test_that("model data formated for rjags has the correct format", {  
  model_clusters <- readRDS(test_path("fixtures", "template", "data", "model_clusters.rds"))
  data <- format_model_data(model_clusters=model_clusters, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  
  expect_named(data$rjags_data, c("Y", "G", "D", "W", "docN", "docwriter", "zero_vec", "Gsmall", 
                                  "numletters", "pc_wrapped", "letterwriter", "lettercluster", 
                                  "zero_mat", "a", "b", "c", "d", "e"))
  
  # number of graphs sums to total number of graphs
  expect_equal(sum(data$rjags_data$docN), data$rjags_data$numletters)
  
  # check data frame
  expect_s3_class(data$rjags_data$Y, "data.frame")
  
  # check integer
  expect_type(data$rjags_data$G, "integer")
  expect_type(data$rjags_data$D, "integer")
  expect_type(data$rjags_data$W, "integer")
  expect_type(data$rjags_data$Gsmall, "integer")
  expect_type(data$rjags_data$numletters, "integer")

  # check vectors  
  expect_vector(data$rjags_data$docN, ptype=integer(), size=data$rjags_data$D)
  expect_vector(data$rjags_data$docwriter, ptype=integer(), size=data$rjags_data$D)
  expect_equal(data$rjags_data$zero_vec, rep(0, data$rjags_data$numletters)) 
  expect_vector(data$rjags_data$pc_wrapped, ptype=numeric(), size=data$rjags_data$numletters)
  expect_vector(data$rjags_data$letterwriter, ptype=integer(), size=data$rjags_data$numletters)
  expect_vector(data$rjags_data$lettercluster, ptype=integer(), size=data$rjags_data$numletters)
  
  # check that docwriter is not writer IDs but instead numbers writers 1,2,...,data$W
  expect_equal(1:data$rjags_data$W, unique(data$rjags_data$docwriter))
  expect_equal(1:data$rjags_data$W, unique(data$rjags_data$letterwriter))
  
  # check matrix
  expect_equal(data$rjags_data$zero_mat, matrix(0, nrow=data$rjags_data$W, ncol=data$rjags_data$Gsmall))
})

test_that("clusters in formatted model data are labeled sequentially",{
  model_clusters <- readRDS(test_path("fixtures", "template", "data", "model_clusters.rds"))
  data <- format_model_data(model_clusters=model_clusters, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  
  # check cluster labels
  clusters <- unique(data$rjags_data$lettercluster)
  expect_equal(sort(unique(clusters)), 1:max(clusters))
})


# format_questioned_data --------------------------------------------------
test_that("formatted questioned data is formatted correctly", {
  questioned_clusters <- readRDS(test_path("fixtures", "template", "data", "questioned_clusters.rds"))
  
  data <- format_questioned_data(model=example_model_1chain,
                                 questioned_clusters=questioned_clusters, 
                                 writer_indices=c(2,5), 
                                 doc_indices=c(7,18))
  # check names
  expect_named(data, c("graph_measurements", "cluster_fill_counts"))
  
  # check data frames
  expect_s3_class(data$graph_measurements, "data.frame")
  expect_s3_class(data$cluster_fill_counts, "data.frame")
  
  # check number of rows
  expect_equal(sum(data$cluster_fill_counts[,-c(1,2,3)]), nrow(data$graph_measurements))
  
  # check vectors
  expect_vector(data$graph_measurements$writer, ptype = integer())
  expect_vector(data$graph_measurements$doc, ptype = character())
  expect_vector(data$graph_measurements$cluster, ptype = integer())
  expect_vector(data$graph_measurements$slope, ptype = numeric())
  expect_vector(data$graph_measurements$pc_rotation, ptype = numeric())
  expect_vector(data$graph_measurements$pc_wrapped, ptype = numeric())
  expect_vector(data$cluster_fill_counts$writer, ptype = integer())
  expect_vector(data$cluster_fill_counts$doc, ptype = character())
  
  # check cluster labels
  expect_gte(min(data$graph_measurements$cluster), 1)
})
