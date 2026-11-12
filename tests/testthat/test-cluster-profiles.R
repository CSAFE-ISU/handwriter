# test get_cluster_fill_counts --------------------------------------------

testthat::test_that("Get cluster fill counts works with writer and doc columns", {
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill counts works without writer and doc columns", {
  
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # drop writer and doc columns from clusters
  clusters <- clusters %>% dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc"))) %>%
    dplyr::group_by(docname)
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill counts sorts cluster columns numerically", {
  # The first document does not use clusters 2 or 10, so pivot_wider() would
  # put those columns last if they were not explicitly sorted (issue #231).
  df <- data.frame(
    docname = c(rep("doc1", 4), rep("doc2", 5)),
    writer = c(rep("w1", 4), rep("w2", 5)),
    doc = c(rep("s1", 4), rep("s1", 5)),
    cluster = c(1, 3, 3, 11, 2, 10, 1, 11, 2)
  )
  actual <- get_cluster_fill_counts(df)

  testthat::expect_equal(colnames(actual), c("docname", "writer", "doc", "1", "2", "3", "10", "11"))
  testthat::expect_equal(as.integer(actual[1, -c(1, 2, 3)]), c(1L, 0L, 2L, 0L, 1L))
  testthat::expect_equal(as.integer(actual[2, -c(1, 2, 3)]), c(1L, 2L, 0L, 1L, 1L))
})

testthat::test_that("Get cluster fill counts sorts cluster columns numerically with outliers", {
  df <- data.frame(docname = c("doc1", "doc1", "doc2", "doc2"), cluster = c(5, 2, -1, 1))
  actual <- get_cluster_fill_counts(df)
  testthat::expect_equal(colnames(actual), c("docname", "-1", "1", "2", "5"))
})

testthat::test_that("Get cluster fill counts works with only one document", {
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "questioned_clusters.rds"))
  actual <- get_cluster_fill_counts(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "questioned_counts.rds"))
  testthat::expect_equal(actual, expected)
})

# get_cluster_fill_rates -------------------------------------------------

testthat::test_that("Get cluster fill rates works with writer and doc columns", {
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill rates works without writer and doc columns", {
  
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
  # drop writer and doc columns from clusters
  clusters <- clusters %>% dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get cluster fill rates works with only one document", {
  clusters <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "questioned_clusters.rds"))
  actual <- get_cluster_fill_rates(clusters)
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "questioned_rates.rds"))
  testthat::expect_equal(actual, expected)
})


# get_writer_profiles -----------------------------------------------------

testthat::test_that("Get writer profiles works with counts with writer and doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs and model_clusters to tempdir to reduce processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "main_dir", "clusters")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    writer_indices = c(1,5),
    doc_indices = c(7,18),
    measure = "counts",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with counts without writer or doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs to tempdir to reduce processing time. NOTE: model_clusters
  # contain writer and doc indices, so recalculate clusters instead of copying
  # model_clusters files
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    measure = "counts",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc"))) %>%
    dplyr::group_by(docname)
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with rates with writer and doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs and model_clusters to tempdir to reduce processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_clusters"),
    output_dir = file.path(tempdir(), "main_dir", "clusters")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    writer_indices = c(1,5),
    doc_indices = c(7,18),
    measure = "rates",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  # example_cluster_template has 5 clusters, so drop the columns for clusters 6-40
  expected <- expected[, !(colnames(expected) %in% paste0("cluster", 6:40))]
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles works with rates without writer or doc indices", {
  empty_tempdir("main_dir")
  
  # copy model_graphs to tempdir to reduce processing time. NOTE: model_clusters
  # contain writer and doc indices, so recalculate clusters instead of copying
  # model_clusters files
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
    template = example_cluster_template,
    measure = "rates",
    output_dir = file.path(tempdir(), "main_dir")
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))
  # example_cluster_template has 5 clusters, so drop the columns for clusters 6-40
  expected <- expected[, !(colnames(expected) %in% paste0("cluster", 6:40))]
  # drop writer and doc columns from expected
  expected <- expected %>% 
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c("writer", "doc")))
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Get writer profiles throws an error if measure is not counts or rates", {
  empty_tempdir("main_dir")
  
  testthat::expect_error(
    get_writer_profiles(
      input_dir = testthat::test_path("fixtures", "temp1qd", "data", "model_docs"),
      template = example_cluster_template,
      measure = "frequency",
      output_dir = file.path(tempdir(), "main_dir")
    ),
    "measure must be 'counts' or 'rates'"
  )
})

testthat::test_that("Get writer profiles works if output_dir is not specified", {
  empty_tempdir("writer_profiles")
  
  # copy questioned_graphs and questioned_clusters to tempdir to reduce processing time
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "questioned_graphs"),
    output_dir = file.path(tempdir(), "writer_profiles", "graphs")
  )
  copy_files(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "questioned_clusters"),
    output_dir = file.path(tempdir(), "writer_profiles", "clusters")
  )
  
  actual <- get_writer_profiles(
    input_dir = testthat::test_path("fixtures", "temp1qd", "data", "questioned_docs"),
    template = example_cluster_template,
    writer_indices = c(1,5),
    doc_indices = c(7,18),
    measure = "counts"
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "questioned_counts.rds"))
  
  testthat::expect_equal(actual, expected)
  
  # check that tempdir was cleaned up
  testthat::expect_false(dir.exists(file.path(tempdir(), "writer_profiles")))
})

testthat::test_that("Get cluster fill rates uses num_clusters for missing clusters", {
  df <- data.frame(docname = c("doc1", "doc1", "doc2", "doc2"), cluster = c(1, 3, 3, 3))
  
  actual <- get_cluster_fill_rates(df, num_clusters = 5)
  testthat::expect_equal(colnames(actual), c("docname", "total_graphs", paste0("cluster", 1:5)))
  testthat::expect_equal(unname(unlist(actual[1, paste0("cluster", 1:5)])), c(0.5, 0, 0.5, 0, 0))
  
  # default is 40 clusters
  actual <- get_cluster_fill_rates(df)
  testthat::expect_equal(colnames(actual), c("docname", "total_graphs", paste0("cluster", 1:40)))
})

testthat::test_that("Get cluster fill rates errors if a cluster label is greater than num_clusters", {
  df <- data.frame(docname = c("doc1", "doc1"), cluster = c(1, 7))
  testthat::expect_error(get_cluster_fill_rates(df, num_clusters = 5),
                         "df contains cluster labels greater than num_clusters")
  testthat::expect_error(get_cluster_fill_rates(df, num_clusters = 2.5),
                         "num_clusters must be a single integer")
})
