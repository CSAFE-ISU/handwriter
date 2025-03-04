main_dir <- testthat::test_path("fixtures", "temp1qd")

# readPNGBinary -----------------------------------------------------------

# make fixture for readPNGBinary
actual <- readPNGBinary(file.path(main_dir, "data", "template_docs", "w0016_s01_pLND_r01.png"))
saveRDS(actual, file.path(main_dir, "data", "read_png_binary.rds"))

# make fixture for readPNGBinary with inversion
actual <- readPNGBinary(file.path(main_dir, "data", "template_docs", "w0016_s01_pLND_r01.png"),
                        inversion = TRUE)
saveRDS(actual, file.path(main_dir, "data", "read_png_binary_invert.rds"))

# make fixture for readPNGBinary with cutoff = 5
actual <- readPNGBinary(file.path(main_dir, "data", "template_docs", "w0016_s01_pLND_r01.png"),
                        cutoffAdjust = 5)
saveRDS(actual, file.path(main_dir, "data", "read_png_binary_cut5.rds"))

# make fixture for readPNGBinary with cutoff = -0.5
actual <- readPNGBinary(file.path(main_dir, "data", "template_docs", "w0016_s01_pLND_r01.png"),
                        cutoffAdjust = -0.5)
saveRDS(actual, file.path(main_dir, "data", "read_png_binary_cut-0.5.rds"))


# template ----------------------------------------------------------------

# make template identical to example_cluster_template
make_clustering_template(main_dir = main_dir,
                         template_docs = file.path(main_dir, 'data', 'template_docs'),
                         writer_indices = c(1, 5),
                         K = 5,
                         num_dist_cores = 1,
                         max_iters = 3,
                         centers_seed = 100)

# make formatted template data with outliers
data <- format_template_data(example_cluster_template)
saveRDS(data, file.path(main_dir, "data", "template_data_w_outliers.rds"))

# make formatted template data with outliers
data <- format_template_data(example_cluster_template)
data$cluster_fill_counts <- data$cluster_fill_counts %>% dplyr::select(-tidyselect::all_of(c("-1")))
saveRDS(data, file.path(main_dir, "data", "template_data_wo_outliers.rds"))


# model -------------------------------------------------------------------

# make model with same settings as example_model. Even if we set the seed the
# models will not be identical.
model <- fit_model(main_dir = main_dir,
                   model_docs = file.path(main_dir, 'data', 'model_docs'),
                   num_iters = 200,
                   num_chains = 1,
                   num_cores = 1,
                   writer_indices = c(1, 5),
                   doc_indices = c(7, 18))

# make formatted model data
model_clusters <- readRDS(test_path("fixtures", "temp1qd", "data", "model_clusters.rds"))
data <- format_model_data(model_clusters=model_clusters, 
                          writer_indices=c(2,5), 
                          doc_indices=c(7,18), 
                          a=2, b=0.25, c=2, d=2, e=0.5)
saveRDS(data, test_path("fixtures", "temp1qd", "data", "model_data.rds"))

# make model cluster fill counts and cluster fill rates
counts <- get_cluster_fill_counts(model_clusters)
saveRDS(counts, testthat::test_path("fixtures", "temp1qd", "data", "model_counts.rds"))
rates <- get_cluster_fill_rates(model_clusters)
saveRDS(rates, testthat::test_path("fixtures", "temp1qd", "data", "model_rates.rds"))


# questioned documents ----------------------------------------------------

analysis <- analyze_questioned_documents(main_dir = main_dir,
                                         questioned_docs = file.path(main_dir, "data", "questioned_docs"),
                                         model = model,
                                         num_cores = 2,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))

# make formatted questioned data
model <- readRDS(testthat::test_path("fixtures", "temp1qd", "data", "model.rds"))
questioned_clusters <- readRDS(test_path("fixtures", "temp1qd", "data", "questioned_clusters.rds"))
data <- format_questioned_data(model=model,
                               questioned_clusters=questioned_clusters, 
                               writer_indices=c(1,5), 
                               doc_indices=c(7,18))
saveRDS(data, testthat::test_path("fixtures", "temp1qd", "data", "questioned_data.rds"))

# make questioned cluster fill rates
counts <- get_cluster_fill_counts(questioned_clusters)
saveRDS(counts, testthat::test_path("fixtures", "temp1qd", "data", "questioned_counts.rds"))
rates <- get_cluster_fill_rates(questioned_clusters)
saveRDS(rates, testthat::test_path("fixtures", "temp1qd", "data", "questioned_rates.rds"))
