save_proclist <- function(sample_name) {
  doc = list()
  doc$image <- readPNGBinary(testthat::test_path("fixtures", "processHandwriting", "samples", paste0(sample_name, ".png")))
  doc$thin = thinImage(doc$image)
  doc$process = processHandwriting(doc$thin, dim(doc$image))
  doc$docname <- sample_name
  saveRDS(doc, testthat::test_path("fixtures", "processHandwriting", "graphs", paste0(sample_name, "_proclist.rds")))
}

samples <- paste0("sample", 1:4)
for (sample in samples) {
  save_proclist(sample)
}

unlink(tempdir(), recursive = TRUE)
clusters <- get_clusters_batch(
  example_cluster_template,
  testthat::test_path("fixtures", "processHandwriting", "graphs"),
  tempdir())
saveRDS(clusters, testthat::test_path("fixtures", "processHandwriting", "clusters_wo_indices.rds"))
counts <- get_cluster_fill_counts(clusters)
saveRDS(counts, testthat::test_path("fixtures", "processHandwriting", "counts_wo_indices.rds"))

unlink(tempdir(), recursive = TRUE)
clusters <- get_clusters_batch(
  template = example_cluster_template,
  input_dir = testthat::test_path("fixtures", "processHandwriting", "graphs"),
  output_dir = tempdir(),
  writer_indices = c(2,5),
  doc_indices = c(7,18))
saveRDS(clusters, testthat::test_path("fixtures", "processHandwriting", "clusters.rds"))
counts <- get_cluster_fill_counts(clusters)
saveRDS(counts, testthat::test_path("fixtures", "processHandwriting", "counts.rds"))
