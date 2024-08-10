devtools::load_all()

unlink(file.path(tempdir(), "clusters"), recursive = TRUE)
actual <- get_clusters_batch(example_cluster_template,
                             testthat::test_path("fixtures", "processHandwriting", "graphs"),
                             file.path(tempdir(), "clusters"),
                             writer_indices = c(7, 7),
                             doc_indices = c(9, 16),
                             num_cores = 1)
file.copy(file.path(tempdir(), "clusters", "all_clusters.rds"), testthat::test_path("fixtures", "clusters", "clusters.rds"), overwrite = TRUE)