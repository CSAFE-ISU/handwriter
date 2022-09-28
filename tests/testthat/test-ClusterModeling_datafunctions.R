# format_model_data -------------------------------------------------------
test_that("formatted model data is a named list with correct names", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  # check names
  expect_named(data, c("Y", "G", "D", "W", "docN", "docwriter", "zero_vec", "Gsmall", "numletters",
                       "pc_wrapped", "letterwriter", "lettercluster", "zero_mat", "a", "b", "c", "d", "e"))
})

test_that("in formatted model data docN sums to numletters ", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  expect_equal(sum(data$docN), data$numletters)
})
  
test_that("in formated model data Y is a data frame", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  # check data frame
  expect_s3_class(data$Y, "data.frame")
})

test_that("in formatted model data scalars are the correct type", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  expect_type(data$G, "integer")
  expect_type(data$D, "integer")
  expect_type(data$W, "integer")
  expect_type(data$Gsmall, "integer")
  expect_type(data$numletters, "integer")
})

test_that("in formatted model data vectors are the correct type and length", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  expect_vector(data$docN, ptype=integer(), size=data$D)
  expect_vector(data$docwriter, ptype=integer(), size=data$D)
  expect_equal(data$zero_vec, rep(0, data$numletters)) 
  expect_vector(data$pc_wrapped, ptype=numeric(), size=data$numletters)
  expect_vector(data$letterwriter, ptype=integer(), size=data$numletters)
  
  # check that docwriter is not writer IDs but instead numbers writers 1,2,...,data$W
  expect_equal(1:data$W, unique(data$docwriter))
  expect_equal(1:data$W, unique(data$letterwriter))
})

test_that("formatted model cluster data", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  
  # check vector of integers
  expect_vector(data$lettercluster, ptype=integer(), size=data$numletters)
  
  # check cluster labels
  clusters <- unique(data$lettercluster)
  expect_lte(max(clusters), data$G)
  expect_gte(min(clusters), 1)
})

test_that("formatted model zero_mat has correct dimensions", {
  data <- format_model_data(proc_list=example_model_proc_list, 
                            writer_indices=c(2,5), 
                            doc_indices=c(7,18), 
                            a=2, b=0.25, c=2, d=2, e=0.5)
  expect_equal(data$zero_mat, matrix(0, nrow=data$Gsmall, ncol=data$Gsmall))
})


# get_letter_measurements -------------------------------------------------
test_that("get letter measurements works", {
  df <- get_letter_measurements(proc_list=example_model_proc_list,
                                writer_indices=c(2,5), 
                                doc_indices=c(7,18))
  # check class
  expect_s3_class(df, "data.frame")
  # check columns
  expect_equal(colnames(df), c("writer", "doc", "cluster", "slope", "pc_rotation", "pc_wrapped"))
  expect_vector(df$writer, ptype = integer())
  expect_vector(df$doc, ptype = character())
  expect_vector(df$cluster, ptype = integer())
  expect_vector(df$slope, ptype = numeric())
  expect_vector(df$pc_rotation, ptype = numeric())
  expect_vector(df$pc_wrapped, ptype = numeric())
})


# get_cluster_fill_counts -------------------------------------------------
test_that("get cluster fill counts works", {
  cluster_counts <- get_cluster_fill_counts(proc_list=example_model_proc_list, 
                                            writer_indices=c(2,5), 
                                            doc_indices=c(7,18))
  # check data frame
  expect_s3_class(cluster_counts, "data.frame")
  # check column names
  expect_equal(colnames(cluster_counts), c("writer", "doc", "1", "2", "3", "4", "5"))
  # check type
  expect_vector(cluster_counts$writer, ptype = integer())
  expect_vector(cluster_counts$doc, ptype = character())
  expect_vector(cluster_counts$"1", ptype = integer())
  expect_vector(cluster_counts$"2", ptype = integer())
  expect_vector(cluster_counts$"3", ptype = integer())
  expect_vector(cluster_counts$"4", ptype = integer())
  expect_vector(cluster_counts$"5", ptype = integer())
})


# format_questioned_data --------------------------------------------------
test_that("formatted questioned data is a named list with correct names", {
  data <- format_questioned_data(proc_list=example_questioned_proc_list, 
                                 writer_indices=c(2,5), 
                                 doc_indices=c(7,18))
  # check names
  expect_named(data, c("graph_measurements", "cluster_fill_counts"))
  
  # check data frames
  expect_s3_class(data$graph_measurements, "data.frame")
  expect_s3_class(data$cluster_fill_counts, "data.frame")
  
  # check number of rows
  expect_equal(nrow(data$graph_measurements), sum(data$cluster_fill_counts[,-c(1,2)]))
  
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

